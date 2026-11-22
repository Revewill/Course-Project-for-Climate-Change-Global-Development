# Run from the project directory containing data/.
packages <- c("tidyverse", "patchwork", "scales")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages)) install.packages(missing_packages)

library(tidyverse)
library(patchwork)
library(scales)

data_dir <- "data"
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

read_data <- function(stem) {
  paths <- list.files(
    data_dir, pattern = paste0("^", stem, "\\.csv$"),
    full.names = TRUE, recursive = TRUE
  )
  if (length(paths) != 1L) stop("Expected one CSV named ", stem, ".csv")
  read_csv(paths, show_col_types = FALSE)
}

carbon <- read_data("01_carbon_paths")
finance <- read_data("02_huaxin_group")
costs <- read_data("03_huaxin_cement_cost")
emissions <- read_data("05_emissions")
regional <- read_data("06_gnr_regional_approx")
retrofit <- read_data("07_retrofit")
market <- read_data("08_market_response")
mixes <- read_data("09_concrete_mix")
currency <- read_data("10_currency")

one <- function(x, label) {
  if (length(x) != 1L || !is.finite(x)) stop("Missing or ambiguous: ", label)
  x
}

get_value <- function(df, condition, column, label) {
  one(pull(filter(df, {{ condition }}), {{ column }}), label)
}

# Price year, currency and case boundaries
case_year <- 2024L
cement_label <- "\u6c34\u6ce5"
cpi <- filter(currency, source == 22)
fx <- filter(currency, source == 23)

cpi_at <- function(y) get_value(cpi, year == y, value, paste("US CPI", y))
fx_at <- function(y) get_value(fx, year == y, value, paste("CNY/USD", y))

to_case_cny <- function(usd, base_year) {
  usd * cpi_at(case_year) / cpi_at(base_year) * fx_at(case_year)
}

carbon_2030 <- filter(carbon, source == 1, year == 2030)
tax_usd2015 <- c(
  Low = one(carbon_2030$lower, "IPCC lower"),
  Central = one(carbon_2030$central, "IPCC central"),
  High = one(carbon_2030$upper, "IPCC upper")
)
tax_cny <- to_case_cny(tax_usd2015, 2015)
central_tax <- unname(tax_cny["Central"])

huaxin <- finance |>
  filter(product == cement_label) |>
  transmute(
    year, sales_t = sales_10000_units * 10000,
    revenue_cny = revenue_CNY, cost_cny = cost_of_sales_CNY,
    price = revenue_cny / sales_t,
    cost = cost_cny / sales_t,
    gross_profit = price - cost
  ) |>
  arrange(year)

case <- filter(huaxin, year == case_year)
price0 <- one(case$price, "case selling price")
cost0 <- one(case$cost, "case cost")
margin0 <- one(case$gross_profit, "case gross profit")
volume0 <- one(case$sales_t, "case sales volume")

intensity <- get_value(
  emissions,
  source == 6 & year_or_version == case_year & unit == "kgCO2/t\u6c34\u6ce5",
  value, "Huaxin domestic Scope 1 cement intensity"
) / 1000

tax_per_t <- central_tax * intensity
break_even_tax <- margin0 / intensity

cost_audit <- costs |>
  mutate(component_sum = materials_CNY + fuel_power_CNY +
           depreciation_amortization_CNY + labor_other_CNY) |>
  left_join(select(huaxin, year, cost_cny, sales_t), by = "year") |>
  mutate(difference_cny = component_sum - cost_cny)

stopifnot(nrow(case) == 1L, all(abs(cost_audit$difference_cny) <= 1))

tax_cases <- tibble(
  scenario = names(tax_cny),
  tax_usd2015_per_tco2 = as.numeric(tax_usd2015),
  tax_cny2024_per_tco2 = as.numeric(tax_cny)
) |>
  mutate(
    domestic_scope1_tco2_per_t = intensity,
    tax_cny_per_t_cement = tax_cny2024_per_tco2 * intensity,
    original_group_gross_profit = margin0,
    gross_profit_after_tax = margin0 - tax_cny_per_t_cement
  )

# Figure 1: evidence and static exposure
path_data <- carbon |>
  mutate(
    economy = case_when(
      source == 1 ~ "IPCC global C1",
      geography == "\u5148\u8fdb\u7ecf\u6d4e\u4f53" ~ "IEA advanced",
      str_starts(geography, "\u90e8\u5206") ~ "IEA selected emerging",
      TRUE ~ "IEA other emerging"
    ),
    multiplier = cpi_at(case_year) / cpi$value[match(usd_base_year, cpi$year)],
    across(c(central, lower, upper), ~ .x * multiplier)
  )

stopifnot(all(is.finite(path_data$central)))

region_names <- c(
  "\u5317\u7f8e" = "North America", "\u4e9a\u592a" = "Asia-Pacific",
  "EU27" = "EU27", "\u4e16\u754c" = "World"
)
regional <- regional |>
  mutate(region_en = unname(region_names[region]))
stopifnot(!anyNA(regional$region_en))

india_parts <- emissions |>
  filter(source == 10, indicator != "\u5408\u8ba1") |>
  mutate(
    component = case_when(
      indicator == "\u77f3\u7070\u77f3\u7145\u70e7" ~ "Calcination",
      indicator == "\u71c3\u6599\u71c3\u70e7" ~ "Fuel combustion",
      TRUE ~ "Electricity"
    )
  )

ink <- "#17324D"
teal <- "#007F79"
orange <- "#BE6227"
red <- "#AA3548"
blue <- "#477CAC"
grey <- "#647482"

theme_set(
  theme_minimal(base_size = 10.5) +
    theme(
      plot.title = element_text(face = "bold", colour = ink, size = 11),
      plot.subtitle = element_text(colour = grey, size = 9),
      axis.title = element_text(colour = ink),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      plot.tag = element_text(face = "bold", colour = ink),
      plot.margin = margin(7, 10, 6, 7)
    )
)

p1a <- ggplot(path_data, aes(year, central, colour = economy)) +
  geom_linerange(
    data = filter(path_data, source == 1),
    aes(ymin = lower, ymax = upper), linewidth = 1, alpha = 0.55
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "IPCC global C1" = ink, "IEA advanced" = blue,
    "IEA selected emerging" = teal, "IEA other emerging" = orange
  )) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = "Carbon prices along mitigation pathways",
       subtitle = "IPCC pathway range and IEA industrial scenario assumptions",
       x = NULL, y = "2024 USD per tonne of CO2")

p1b <- ggplot(regional, aes(year, approx_kgCO2_per_t_cementitious,
                            colour = region_en)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  scale_colour_manual(values = c(
    "Asia-Pacific" = teal, "EU27" = blue, "North America" = orange, "World" = ink
  )) +
  labs(title = "Regional differences in production intensity",
       subtitle = "GNR sample; approximate readings from published curves",
       x = NULL, y = "kg CO2 per tonne of cementitious material")

p1c <- ggplot(india_parts, aes(value, fct_reorder(component, value))) +
  geom_col(fill = teal, width = 0.58) +
  geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.25, size = 3.3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.17))) +
  labs(title = "Where cement emissions originate",
       subtitle = "India, 2023; electricity is an indirect source",
       x = "Tonnes CO2e per tonne of cement", y = NULL)

unit_history <- huaxin |>
  select(year, `Selling price` = price, `Cost of sales` = cost,
         `Gross profit` = gross_profit) |>
  pivot_longer(-year, names_to = "metric", values_to = "amount")

p1d <- ggplot(unit_history, aes(year, amount, colour = metric)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.1) +
  scale_x_continuous(breaks = huaxin$year) +
  scale_colour_manual(values = c(
    "Selling price" = ink, "Cost of sales" = orange, "Gross profit" = teal
  )) +
  labs(title = "Huaxin's unit economics",
       subtitle = "Group cement business; reporting-year prices",
       x = NULL, y = "Nominal CNY per tonne sold")

exposure <- tibble(
  tax = seq(0, max(tax_cny) * 1.05, length.out = 301),
  gross_profit = margin0 - intensity * tax
)

p1e <- ggplot(exposure, aes(tax, gross_profit)) +
  annotate("rect", xmin = min(tax_cny), xmax = max(tax_cny),
           ymin = -Inf, ymax = Inf, fill = orange, alpha = 0.13) +
  geom_hline(yintercept = 0, colour = grey) +
  geom_line(colour = ink, linewidth = 0.9) +
  geom_point(data = tax_cases,
             aes(tax_cny2024_per_tco2, gross_profit_after_tax),
             colour = orange, size = 2.2) +
  annotate("point", x = break_even_tax, y = 0, colour = teal, size = 2.8) +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Gross profit if the firm changes nothing",
       subtitle = "Shading: IPCC 2030 range; green point: zero gross profit",
       x = "Hypothetical tax (2024 CNY/t CO2)",
       y = "Gross profit (2024 CNY/t cement)")

eu_pass <- market |>
  filter(source == 18) |>
  mutate(
    item = case_when(
      str_detect(region, "\u6ce2\u5170") & str_detect(region, "\u6377\u514b") ~
        "Portland cement | Czechia, Poland",
      str_detect(product, "\u719f\u6599") ~ "Clinker | France, Germany, Poland",
      is.na(low) ~ "All cement | UK, Italy",
      TRUE ~ "All cement | France, Germany"
    ),
    item = fct_rev(factor(item))
  )

p1f <- ggplot(eu_pass, aes(y = item)) +
  geom_segment(data = filter(eu_pass, !is.na(low)),
               aes(x = low, xend = high, yend = item),
               linewidth = 3, colour = teal, lineend = "round") +
  geom_text(data = filter(eu_pass, is.na(low)), aes(x = 50),
            label = "Not statistically significant", size = 2.9, colour = grey) +
  scale_x_continuous(limits = c(0, 102), breaks = seq(0, 100, 25)) +
  labs(title = "Observed carbon-cost pass-through",
       subtitle = "Historical EU ETS estimates; ranges are indicative",
       x = "Carbon cost passed into price (%)", y = NULL)

# Short-run pricing: operating contribution versus temporary idling
# A cost classification scenario is needed because marginal cost is undisclosed.
c_case <- filter(cost_audit, year == case_year)
variable_cost <- function(material_share, energy_share, labour_other_share) {
  (material_share * c_case$materials_CNY +
     energy_share * c_case$fuel_power_CNY +
     labour_other_share * c_case$labor_other_CNY) / volume0
}

cost_scenarios <- tribble(
  ~cost_case,              ~materials, ~energy, ~labour_other,
  "Lower avoidable cost",        0.85,   0.75,           0.10,
  "Central assumption",          1.00,   0.90,           0.35,
  "Higher avoidable cost",       1.00,   1.00,           0.65
) |>
  mutate(vc = pmap_dbl(list(materials, energy, labour_other), variable_cost))

vc0 <- one(cost_scenarios$vc[cost_scenarios$cost_case == "Central assumption"],
           "central variable cost")

pricing_grid <- crossing(
  pass = seq(0, 1, length.out = 101),
  retained = seq(0, 1, length.out = 101)
) |>
  mutate(
    price = price0 + pass * tax_per_t,
    contribution = retained * (price - vc0 - tax_per_t),
    hold_contribution = price0 - vc0 - tax_per_t,
    gain_vs_best_alternative = contribution - pmax(0, hold_contribution)
  )

p2a <- ggplot(pricing_grid, aes(pass, retained)) +
  geom_raster(aes(fill = gain_vs_best_alternative)) +
  geom_contour(aes(z = gain_vs_best_alternative), breaks = 0,
               colour = ink, linewidth = 0.55) +
  scale_fill_gradient2(low = red, mid = "white", high = teal, midpoint = 0,
                       name = "CNY/t baseline sales") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "When does a price increase pay?",
       subtitle = "Gain over the better of holding price or temporarily idling",
       x = "Share of carbon cost passed through", y = "Baseline volume retained")

elasticities <- filter(market, source == 19)
elasticity_range <- range(c(elasticities$low, elasticities$high), na.rm = TRUE)
elasticity_mid <- one(
  elasticities$low[elasticities$low == elasticities$high],
  "EPA demand elasticity point estimate"
)

price_response <- crossing(
  pass = seq(0, 1, length.out = 101),
  elasticity = sort(unique(c(elasticity_range, elasticity_mid)))
) |>
  mutate(
    new_price = price0 + pass * tax_per_t,
    retained = (new_price / price0)^elasticity,
    contribution = retained * (new_price - vc0 - tax_per_t)
  )

p2b <- ggplot(price_response, aes(pass, contribution,
                                  colour = factor(elasticity))) +
  geom_hline(yintercept = 0, linetype = 2, colour = grey) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(values = c(blue, ink, orange), name = "Elasticity") +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "How much does demand response matter?",
       subtitle = "US market elasticities transferred as stress scenarios",
       x = "Share of carbon cost passed through",
       y = "Contribution per baseline tonne (CNY)")

# US engineering case: real 2022 USD, common horizon, no subsidy or price premium
# DOE capture is adjusted by an explicit avoided-to-captured ratio.
project_value <- function(key, column = "low") {
  rows <- filter(retrofit, source == 16, indicator == key)
  one(rows[[column]], key)
}

capex <- project_value("\u521d\u59cb\u8d44\u672c\u652f\u51fa")
fixed_om <- project_value("\u56fa\u5b9a\u8fd0\u7ef4")
variable_om <- project_value("\u53ef\u53d8\u8fd0\u7ef4")
energy_cost <- project_value("\u71c3\u6599\u7535\u529b")
captured <- project_value("\u5e74\u6355\u96c6\u91cf")
ts_key <- "\u8fd0\u8f93\u5c01\u5b58\u8d39\u7528"
ts_low <- project_value(ts_key)
ts_high <- project_value(ts_key, "high")
ts_mid <- mean(c(ts_low, ts_high))

tax_usd2022 <- unname(tax_usd2015["Central"]) * cpi_at(2022) / cpi_at(2015)
horizon <- 20L
rate <- 0.08
avoided_ratio <- 0.85
base_load <- 0.75

annuity <- function(n, r) {
  ifelse(r == 0, n, (1 - (1 + r)^(-n)) / r)
}

cash_flow <- function(tax, load, avoided = avoided_ratio, ts = ts_mid) {
  load * (captured * (tax * avoided - ts) - variable_om - energy_cost) - fixed_om
}

npv_now <- function(tax, load, avoided = avoided_ratio, ts = ts_mid,
                    r = rate, capex_factor = 1) {
  -capex * capex_factor + cash_flow(tax, load, avoided, ts) * annuity(horizon, r)
}

investment_grid <- crossing(
  tax = seq(0, 1.5 * tax_usd2022, length.out = 151),
  load = seq(0.2, 1, length.out = 101)
) |>
  mutate(npv_m = npv_now(tax, load) / 1e6)

p2c <- ggplot(investment_grid, aes(tax, load)) +
  geom_raster(aes(fill = npv_m)) +
  geom_contour(aes(z = npv_m), breaks = 0, colour = ink, linewidth = 0.65) +
  geom_vline(xintercept = tax_usd2022, colour = ink, linetype = 2) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_gradient2(low = red, mid = "white", high = teal, midpoint = 0,
                       name = "NPV (USD m)") +
  labs(title = "When does capture investment pay?",
       subtitle = "US retrofit benchmark; contour = zero incremental NPV",
       x = "Constant carbon price (2022 USD/t CO2)",
       y = "Throughput / DOE reference output")

# Waiting reveals future demand at the end of year one.
# All strategies end in year 20; a delayed project earns 19 years of cash flow.
low_load <- 0.40
high_load <- 0.95

wait_payoff <- function(load) {
  value_at_year1 <- -capex +
    cash_flow(tax_usd2022, load) * annuity(horizon - 1L, rate)
  pmax(0, value_at_year1) / (1 + rate)
}

now_payoff <- function(load) {
  -capex + cash_flow(tax_usd2022, base_load) / (1 + rate) +
    cash_flow(tax_usd2022, load) * annuity(horizon - 1L, rate) / (1 + rate)
}

timing <- tibble(prob_high = seq(0, 1, length.out = 101)) |>
  mutate(
    `Invest now` = prob_high * now_payoff(high_load) +
      (1 - prob_high) * now_payoff(low_load),
    `Wait and choose` = prob_high * wait_payoff(high_load) +
      (1 - prob_high) * wait_payoff(low_load),
    `Continue without retrofit` = 0
  ) |>
  pivot_longer(-prob_high, names_to = "strategy", values_to = "npv") |>
  mutate(npv_m = npv / 1e6)

p2d <- ggplot(timing, aes(prob_high, npv_m, colour = strategy)) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(values = c(
    "Invest now" = ink, "Wait and choose" = teal,
    "Continue without retrofit" = orange
  )) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "What is one year of information worth?",
       subtitle = "Demand states are scenarios; probabilities are varied",
       x = "Probability of high future throughput",
       y = "Expected incremental NPV (million 2022 USD)")

downstream <- crossing(
  mixes |> transmute(
    mix_id = row_number(),
    cement_kg_m3 = portland_kg_m3 + PLC_kg_m3
  ),
  pass = c(0.20, 0.40, 1.00)
) |>
  mutate(
    cost_cny_m3 = cement_kg_m3 / 1000 * tax_per_t * pass,
    pass_label = factor(pass, levels = c(.2, .4, 1),
                        labels = c("20%", "40%", "100%"))
  )

p2e <- ggplot(downstream, aes(cement_kg_m3, cost_cny_m3, colour = pass_label)) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_colour_manual(values = c(blue, teal, orange), name = "Pass-through") +
  labs(title = "What reaches the concrete purchaser?",
       subtitle = "Ten published mixes; other input prices held constant",
       x = "Purchased cement (kg per cubic metre)",
       y = "Extra cement cost (2024 CNY/m³)")

sensitivity <- tribble(
  ~parameter,              ~low, ~high,
  "Carbon price",          unname(tax_usd2015["Low"] / tax_usd2015["Central"]),
                           unname(tax_usd2015["High"] / tax_usd2015["Central"]),
  "Throughput",            0.40, 0.95,
  "Avoided/captured CO2",  0.70, 1.00,
  "Transport and storage", ts_low, ts_high,
  "Real discount rate",    0.05, 0.12,
  "Capital expenditure",   0.80, 1.20
)

evaluate_parameter <- function(parameter, value) {
  switch(parameter,
    "Carbon price"          = npv_now(tax_usd2022 * value, base_load),
    "Throughput"            = npv_now(tax_usd2022, value),
    "Avoided/captured CO2"  = npv_now(tax_usd2022, base_load, avoided = value),
    "Transport and storage" = npv_now(tax_usd2022, base_load, ts = value),
    "Real discount rate"    = npv_now(tax_usd2022, base_load, r = value),
    "Capital expenditure"   = npv_now(tax_usd2022, base_load, capex_factor = value)
  ) / 1e6
}

sensitivity <- sensitivity |>
  mutate(
    npv_at_low = map2_dbl(parameter, low, evaluate_parameter),
    npv_at_high = map2_dbl(parameter, high, evaluate_parameter),
    xmin = pmin(npv_at_low, npv_at_high),
    xmax = pmax(npv_at_low, npv_at_high),
    parameter = fct_reorder(parameter, xmax - xmin)
  )

p2f <- ggplot(sensitivity, aes(y = parameter)) +
  geom_vline(xintercept = 0, colour = grey) +
  geom_vline(xintercept = npv_now(tax_usd2022, base_load) / 1e6,
             colour = ink, linetype = 2) +
  geom_segment(aes(x = xmin, xend = xmax, yend = parameter),
               colour = blue, linewidth = 2.2) +
  geom_point(aes(x = npv_at_low), colour = orange, size = 2.3) +
  geom_point(aes(x = npv_at_high), colour = teal, size = 2.3) +
  labs(title = "Which assumptions can reverse the decision?",
       subtitle = "Orange/green: low/high input; dashed line: central NPV",
       x = "Incremental NPV (million 2022 USD)", y = NULL)

assemble <- function(panels, number, title, caption) {
  fig <- wrap_plots(panels, ncol = 2, byrow = TRUE) +
    plot_annotation(
      title = title, caption = caption, tag_levels = "a", tag_prefix = number,
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", colour = ink),
        plot.caption = element_text(size = 8, colour = grey, hjust = 0)
      )
    )
  fig
}

fig1 <- assemble(
  list(p1a, p1b, p1c, p1d, p1e, p1f), "1",
  "Carbon pricing and cement producers' exposure",
  paste(
    "Sources: IPCC AR6 WGIII; IEA NZE; GCCA/GNR via Cement Europe; NITI Aayog;",
    "Huaxin annual and ESG reports; European Commission pass-through study.",
    "Panel 1e combines group unit gross profit with domestic Scope 1 intensity.",
    sep = "\n"
  )
)

fig2 <- assemble(
  list(p2a, p2b, p2c, p2d, p2e, p2f), "2",
  "Pricing, investment timing and downstream costs",
  paste(
    "Panels 2a–b use Huaxin's 2024 baseline; 2c–d and 2f use a separate US DOE engineering benchmark.",
    "Panel 2e applies the Huaxin price shock to published mix designs. Pricing and investment panels are conditional simulations.",
    "Investment: 20-year horizon, 8% real discount rate, 85% avoided/captured CO2, no subsidy; terminal value and decommissioning cost set to zero.",
    sep = "\n"
  )
)

print(fig1)
print(fig2)

for (i in 1:2) {
  fig <- list(fig1, fig2)[[i]]
  stem <- file.path(output_dir, paste0("Figure_", i))
  ggsave(paste0(stem, ".pdf"), fig, width = 13.6, height = 13.5,
         device = grDevices::cairo_pdf, bg = "white")
  ggsave(paste0(stem, ".png"), fig, width = 13.6, height = 13.5,
         dpi = 320, bg = "white")
}

assumptions <- tribble(
  ~parameter, ~central, ~unit, ~basis,
  "Case year", case_year, "year", "Matched financial and domestic emissions disclosure",
  "Cement Scope 1 intensity", intensity, "t CO2/t cement", "Huaxin domestic proxy",
  "Discount rate", rate, "fraction", "Scenario assumption",
  "Project horizon", horizon, "years", "Scenario assumption",
  "Avoided/captured ratio", avoided_ratio, "fraction", "Scenario assumption",
  "Reference throughput fraction", base_load, "fraction", "Scenario assumption",
  "Low future throughput", low_load, "fraction", "Scenario assumption",
  "High future throughput", high_load, "fraction", "Scenario assumption",
  "Transport and storage", ts_mid, "2022 USD/t captured", "Midpoint of DOE range; approximate common price year",
  "Year-one investment delay", 1, "year", "Perfect information about future demand after one year",
  "Idle operating contribution", 0, "CNY/t baseline sales", "Fixed costs common; no shutdown or restart charge"
)

tables <- list(
  huaxin_unit_economics = huaxin,
  financial_cost_check = cost_audit,
  carbon_tax_scenarios = tax_cases,
  carbon_price_paths = path_data,
  avoidable_cost_assumptions = cost_scenarios,
  pricing_and_retention = pricing_grid,
  demand_response_stress_test = price_response,
  retrofit_npv_grid = investment_grid,
  investment_timing = timing,
  concrete_cost_incidence = downstream,
  investment_sensitivity = sensitivity,
  model_assumptions = assumptions
)

iwalk(tables, ~ write_csv(.x, file.path(output_dir, paste0(.y, ".csv"))))

print(tax_cases, width = Inf)
cat(sprintf(
  "\n2024 group gross profit: %.2f CNY/t cement\nDomestic Scope 1 proxy: %.3f t CO2/t cement\nZero-gross-profit carbon tax: %.2f CNY/t CO2\n",
  margin0, intensity, break_even_tax
))