packages <- c("ggplot2", "dplyr", "tidyr", "readr", "patchwork", "scales", "pdftools", "png")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(patchwork)
})

data_dir <- "data"
out_dir <- "output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
input_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)

read_input <- function(name) {
  hit <- input_files[basename(input_files) == paste0(name, ".csv")]
  if (length(hit) != 1L) stop("Expected one input file for ", name)
  read_csv(hit, show_col_types = FALSE, progress = FALSE)
}

single <- function(x, label) {
  if (length(x) != 1L || !is.finite(x)) stop("Invalid input: ", label)
  as.numeric(x)
}

paths_raw <- read_input("01_carbon_paths")
group_raw <- read_input("02_huaxin_group")
cost_raw <- read_input("03_huaxin_cement_cost")
emissions <- read_input("05_emissions")
gnr_raw <- read_input("06_gnr_regional_approx")
retrofit <- read_input("07_retrofit")
market <- read_input("08_market_response")
mix_raw <- read_input("09_concrete_mix")
currency <- read_input("10_currency")

# Carbon prices and company accounts
cpi <- function(y) {
  single(currency$value[currency$source == 22 & currency$year == y], paste("CPI", y))
}

fx <- single(currency$value[currency$source == 23 & currency$year == 2024], "2024 FX")
economy_names <- c(
  "全球" = "IPCC global C1",
  "先进经济体" = "IEA advanced",
  "部分新兴经济体（含中国）" = "IEA selected emerging",
  "其他新兴经济体" = "IEA other emerging"
)

paths <- paths_raw |>
  mutate(
    economy = unname(economy_names[geography]),
    multiplier = cpi(2024) / vapply(usd_base_year, cpi, numeric(1)),
    central_original = central,
    lower_original = lower,
    upper_original = upper,
    across(c(central, lower, upper), ~ .x * multiplier),
    output_unit = "2024 USD/t CO2"
  )

ipcc <- filter(paths, source == 1, year == 2030)
stopifnot(nrow(ipcc) == 1L, !anyNA(paths$economy))

finance <- group_raw |>
  filter(product == "水泥") |>
  transmute(
    year,
    sales_t = sales_10000_units * 10000,
    revenue_cny = revenue_CNY,
    cost_cny = cost_of_sales_CNY,
    price = revenue_cny / sales_t,
    cost = cost_cny / sales_t,
    gross_profit = price - cost
  ) |>
  arrange(year)

cost_check <- cost_raw |>
  mutate(component_sum = materials_CNY + fuel_power_CNY +
           depreciation_amortization_CNY + labor_other_CNY) |>
  left_join(select(finance, year, cost_cny, sales_t), by = "year") |>
  mutate(difference_cny = component_sum - cost_cny)

stopifnot(!anyDuplicated(finance$year), all(finance$sales_t > 0),
          all(abs(cost_check$difference_cny) <= 1))

base <- filter(finance, year == 2024)
base_cost <- filter(cost_check, year == 2024)
stopifnot(nrow(base) == 1L, nrow(base_cost) == 1L)

p0 <- base$price
gp0 <- base$gross_profit

intensity <- emissions |>
  filter(source == 6, indicator == "成品水泥直接排放", year_or_version == 2024) |>
  pull(value) |>
  single("Domestic cement Scope 1 intensity") / 1000

tax_cases <- tibble(
  scenario = c("Low", "Central", "High"),
  tax_usd2015_per_tco2 = c(ipcc$lower_original, ipcc$central_original, ipcc$upper_original)
) |>
  mutate(
    tax_cny2024_per_tco2 = tax_usd2015_per_tco2 * ipcc$multiplier * fx,
    domestic_scope1_tco2_per_t = intensity,
    tax_cny_per_t_cement = tax_cny2024_per_tco2 * intensity,
    original_group_gross_profit = gp0,
    gross_profit_after_tax = gp0 - tax_cny_per_t_cement
  )

carbon_cost <- tax_cases$tax_cny_per_t_cement[tax_cases$scenario == "Central"]
tax_break_even <- gp0 / intensity

# Pricing and demand
avoidable <- tibble(
  cost_case = c("Lower avoidable cost", "Central assumption", "Higher avoidable cost"),
  materials = c(0.85, 1, 1),
  energy = c(0.75, 0.90, 1),
  labour_other = c(0.10, 0.35, 0.65)
) |>
  mutate(
    vc = (materials * base_cost$materials_CNY +
            energy * base_cost$fuel_power_CNY +
            labour_other * base_cost$labor_other_CNY) / base_cost$sales_t,
    minimum_pass_through = (vc + carbon_cost - p0) / carbon_cost
  )

vc0 <- avoidable$vc[2]
pass_min <- avoidable$minimum_pass_through[2]
hold <- p0 - vc0 - carbon_cost
share_grid <- seq(0, 1, by = 0.01)

pricing <- expand_grid(pass = share_grid, retained = share_grid) |>
  mutate(
    price = p0 + pass * carbon_cost,
    contribution = retained * (price - vc0 - carbon_cost),
    hold_contribution = hold,
    gain_vs_best_alternative = contribution - max(hold, 0)
  )

elasticity_range <- filter(market, source == 19, low != high)
elasticity_point <- filter(market, source == 19, low == high)
elasticities <- sort(c(elasticity_range$low, elasticity_point$low, elasticity_range$high))
stopifnot(length(elasticities) == 3L, all(elasticities < 0))

demand <- expand_grid(pass = share_grid, elasticity = elasticities) |>
  mutate(
    new_price = p0 + pass * carbon_cost,
    retained = (new_price / p0)^elasticity,
    contribution = retained * (new_price - vc0 - carbon_cost)
  )

# US capture retrofit
engineering <- function(indicator_name, field = "low") {
  row <- filter(retrofit, source == 16, indicator == indicator_name)
  single(row[[field]], indicator_name)
}

capex <- engineering("初始资本支出")
fixed_om <- engineering("固定运维")
variable_om <- engineering("可变运维") + engineering("燃料电力")
captured <- engineering("年捕集量")
ts_low <- engineering("运输封存费用")
ts_high <- engineering("运输封存费用", "high")
ts_base <- mean(c(ts_low, ts_high))

horizon <- 20
rate <- 0.08
avoided_ratio <- 0.85
load_base <- 0.75
load_low <- 0.40
load_high <- 0.95
tax_us22 <- ipcc$central_original * cpi(2022) / cpi(2015)

annuity <- function(n, r) (1 - (1 + r)^(-n)) / r

cash_flow <- function(tax, load, ratio = avoided_ratio, storage = ts_base) {
  load * (captured * (ratio * tax - storage) - variable_om) - fixed_om
}

npv <- function(tax = tax_us22, load = load_base, ratio = avoided_ratio,
                storage = ts_base, r = rate, capital = capex, n = horizon) {
  -capital + cash_flow(tax, load, ratio, storage) * annuity(n, r)
}

base_npv <- npv() / 1e6
retro_grid <- expand_grid(tax = seq(0, 410, by = 2), load = seq(0.20, 1, by = 0.01)) |>
  mutate(npv_m = npv(tax = tax, load = load) / 1e6)

unit_saving <- captured * (avoided_ratio * tax_us22 - ts_base) - variable_om
load_break_even <- (capex / annuity(horizon, rate) + fixed_om) / unit_saving
retro_tax_break_even <- (ts_base +
  (variable_om + (fixed_om + capex / annuity(horizon, rate)) / load_base) /
    captured) / avoided_ratio

prob <- share_grid
cf_low <- cash_flow(tax_us22, load_low)
cf_high <- cash_flow(tax_us22, load_high)
future_cf <- (1 - prob) * cf_low + prob * cf_high
future_factor <- annuity(horizon - 1, rate)

now_value <- -capex + (cash_flow(tax_us22, load_base) +
                        future_cf * future_factor) / (1 + rate)
wait_low <- max(0, -capex + cf_low * future_factor)
wait_high <- max(0, -capex + cf_high * future_factor)
wait_value <- ((1 - prob) * wait_low + prob * wait_high) / (1 + rate)

timing <- tibble(
  prob_high = prob,
  `Invest now` = now_value,
  `Wait one year` = wait_value,
  `No retrofit` = 0
) |>
  pivot_longer(-prob_high, names_to = "strategy", values_to = "npv") |>
  mutate(npv_m = npv / 1e6)

sensitivity_row <- function(parameter, low, high, calc) {
  tibble(parameter, low, high, npv_at_low = calc(low) / 1e6,
         npv_at_high = calc(high) / 1e6)
}

sensitivity <- bind_rows(
  sensitivity_row("Carbon price", ipcc$lower_original / ipcc$central_original,
                  ipcc$upper_original / ipcc$central_original,
                  function(x) npv(tax = x * tax_us22)),
  sensitivity_row("Throughput", load_low, load_high, function(x) npv(load = x)),
  sensitivity_row("Avoided/captured CO2", 0.70, 1, function(x) npv(ratio = x)),
  sensitivity_row("Transport and storage", ts_low, ts_high, function(x) npv(storage = x)),
  sensitivity_row("Real discount rate", 0.05, 0.12, function(x) npv(r = x)),
  sensitivity_row("Capital expenditure", 0.80, 1.20, function(x) npv(capital = x * capex))
) |>
  mutate(xmin = pmin(npv_at_low, npv_at_high), xmax = pmax(npv_at_low, npv_at_high))

concrete <- mix_raw |>
  transmute(mix_id = row_number(), cement_kg_m3 = portland_kg_m3 + PLC_kg_m3) |>
  crossing(pass = c(0.20, 0.40, 1)) |>
  mutate(cost_cny_m3 = cement_kg_m3 / 1000 * pass * carbon_cost,
         pass_label = scales::percent(pass, accuracy = 1))

assumptions <- tribble(
  ~parameter, ~central, ~unit, ~basis,
  "Case year", 2024, "year", "Group financial data and domestic emissions proxy",
  "Cement Scope 1 intensity", intensity, "t CO2/t cement", "Huaxin domestic disclosure",
  "Real discount rate", rate, "fraction", "Scenario assumption",
  "Project horizon", horizon, "years", "Common end date for both investment strategies",
  "Avoided/captured ratio", avoided_ratio, "fraction", "Scenario assumption",
  "Reference throughput", load_base, "fraction", "First operating year if investing now",
  "Low future throughput", load_low, "fraction", "Persistent low-demand state",
  "High future throughput", load_high, "fraction", "Persistent high-demand state",
  "Transport and storage", ts_base, "2022 USD/t captured", "Midpoint of DOE cost range",
  "Investment delay", 1, "year", "Demand state observed before delayed investment",
  "Idle contribution", 0, "CNY/t baseline sales", "Common fixed costs, no restart charge",
  "Free allocation", 0, "fraction", "Hypothetical full tax on direct emissions",
  "Capture subsidy", 0, "USD/t captured", "Tax-only retrofit scenario",
  "Terminal value", 0, "USD", "No salvage credit at the common end date"
)

article <- tribble(
  ~metric, ~value, ~unit, ~interpretation,
  "2030 central carbon price", ipcc$central, "2024 USD/t CO2",
  "IPCC global C1 scenario benchmark, used as a hypothetical tax",
  "Baseline gross profit", gp0, "2024 CNY/t cement sold",
  "Huaxin group cement revenue less cost of sales, divided by cement sales",
  "Central carbon cost", carbon_cost, "2024 CNY/t cement",
  "Domestic direct emissions applied to group unit economics",
  "Central gross profit after tax", gp0 - carbon_cost, "2024 CNY/t cement",
  "Fixed product price, sales and technology",
  "Gross-profit break-even tax", tax_break_even, "2024 CNY/t CO2",
  "Carbon price at which baseline gross profit reaches zero",
  "Operating break-even pass-through", pass_min, "fraction of carbon cost",
  "Central avoidable cost, positive sales, zero idle contribution",
  "Full-pass-through sales retained", (1 + carbon_cost / p0)^elasticity_point$low,
  "fraction of baseline sales", "US market elasticity applied as a stress test",
  "Reference retrofit NPV", base_npv, "million 2022 USD",
  "US engineering case, constant central carbon price and reference throughput",
  "Retrofit break-even throughput", load_break_even, "fraction of reference capacity",
  "Central carbon price and engineering assumptions",
  "Retrofit break-even carbon price", retro_tax_break_even, "2022 USD/t CO2",
  "Reference throughput and engineering assumptions",
  "Minimum concrete cost increase at 40% pass-through",
  min(concrete$cost_cny_m3[concrete$pass == 0.4]), "2024 CNY/m3",
  "Purchased cement only, across the supplied mixes",
  "Maximum concrete cost increase at 40% pass-through",
  max(concrete$cost_cny_m3[concrete$pass == 0.4]), "2024 CNY/m3",
  "Purchased cement only, across the supplied mixes"
)

tables <- list(
  avoidable_cost_assumptions = avoidable,
  carbon_price_paths = paths,
  carbon_tax_scenarios = tax_cases,
  concrete_cost_incidence = concrete,
  demand_response_stress_test = demand,
  financial_cost_check = cost_check,
  huaxin_unit_economics = finance,
  investment_sensitivity = sensitivity,
  investment_timing = timing,
  model_assumptions = assumptions,
  pricing_and_retention = pricing,
  retrofit_npv_grid = retro_grid,
  article_results = article
)

for (name in names(tables)) {
  write_csv(tables[[name]], file.path(out_dir, paste0(name, ".csv")))
}

# Figure data
region_names <- c("北美" = "North America", "亚太" = "Asia-Pacific",
                  "世界" = "World", "EU27" = "EU27")
gnr <- gnr_raw |> mutate(region_name = unname(region_names[region]))

source_names <- c("石灰石煅烧" = "Calcination", "燃料燃烧" = "Fuel combustion",
                  "电力" = "Electricity")
india <- emissions |>
  filter(source == 10, indicator %in% names(source_names)) |>
  mutate(component = factor(unname(source_names[indicator]),
                            levels = c("Electricity", "Fuel combustion", "Calcination")))

series_names <- c(price = "Selling price", cost = "Cost of sales", gross_profit = "Gross profit")
finance_long <- finance |>
  select(year, all_of(names(series_names))) |>
  pivot_longer(-year) |>
  mutate(series = unname(series_names[name]))

market_labels <- c(
  "英国/意大利" = "All cement\nUK, Italy",
  "法国/德国" = "All cement\nFrance, Germany",
  "法国/德国/波兰" = "Clinker\nFrance, Germany, Poland",
  "捷克/波兰" = "Portland cement\nCzechia, Poland"
)
pass_evidence <- market |>
  filter(source == 18) |>
  mutate(
    label = factor(unname(market_labels[region]), levels = rev(unname(market_labels))),
    lo = low / 100, hi = high / 100
  )

stopifnot(!anyNA(gnr$region_name), !anyNA(pass_evidence$label), nrow(india) == 3L)

ink <- "#344563"
blue <- "#5878A6"
teal <- "#43887F"
rose <- "#BF596F"
mint <- "#A6D0C6"
pink <- "#F4A7AE"
peach <- "#FBD9C9"
grid_colour <- "#E4EAF0"

pct <- scales::label_percent(accuracy = 1)
num <- scales::label_number(big.mark = ",", accuracy = 1)
mm <- function(x) grid::unit(x, "mm")

theme_set(
  theme_minimal(base_family = "sans", base_size = 13) +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(colour = ink, size = 11.5, lineheight = 0.95),
      axis.text.x = element_text(margin = margin(t = 3)),
      axis.text.y = element_text(margin = margin(r = 3)),
      axis.title = element_text(size = 13, lineheight = 0.97),
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.title.y = element_text(margin = margin(r = 4)),
      axis.line = element_line(colour = "#A5AFBC", linewidth = 0.33),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = grid_colour, linewidth = 0.27),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10.5),
      legend.key.width = mm(7),
      legend.key.height = mm(4),
      legend.key.spacing.x = mm(2.1),
      legend.key.spacing.y = mm(0.5),
      legend.box.spacing = mm(1.2),
      legend.margin = margin(1, 0, 0, 0),
      plot.tag = element_text(size = 16, face = "bold", hjust = 0, colour = ink),
      plot.tag.position = "topleft",
      plot.tag.location = "plot",
      plot.margin = margin(5, 9, 7, 7)
    )
)

tight_y <- theme(axis.title.y = element_text(margin = margin(r = 1.5)))
line_points <- list(geom_line(linewidth = 0.64), geom_point(size = 1.6))
percent_x <- function() scale_x_continuous(breaks = seq(0, 1, 0.25), labels = pct)
percent_y <- function() scale_y_continuous(breaks = seq(0, 1, 0.25), labels = pct)
two_rows <- guide_legend(nrow = 2, byrow = TRUE)

heat_scale <- function(label) {
  scale_fill_gradient2(
    low = rose, mid = "#F7F9FB", high = teal, midpoint = 0,
    name = label, labels = num,
    guide = guide_colourbar(
      display = "rectangles", nbin = 256,
      theme = theme(
        legend.title.position = "top",
        legend.title = element_text(size = 10.5, hjust = 0.5),
        legend.key.width = mm(48),
        legend.key.height = mm(3.2)
      )
    )
  )
}
heat_theme <- theme(panel.grid = element_blank(),
                    legend.title = element_text(size = 10.5))
legend_title <- theme(legend.title = element_text(size = 10.5))

# Figure 1
a1 <- ggplot(paths, aes(year, central, colour = economy)) +
  geom_linerange(data = filter(paths, !is.na(lower), !is.na(upper)),
                 aes(ymin = lower, ymax = upper), linewidth = 0.65, alpha = 0.70) +
  line_points +
  scale_colour_manual(values = c("IPCC global C1" = ink, "IEA advanced" = blue,
                                 "IEA selected emerging" = teal, "IEA other emerging" = rose)) +
  guides(colour = two_rows) +
  labs(x = NULL, y = "Carbon price\n(2024 USD/t CO₂)") +
  tight_y

b1 <- ggplot(gnr, aes(year, approx_kgCO2_per_t_cementitious, colour = region_name)) +
  line_points +
  scale_colour_manual(values = c("North America" = rose, "Asia-Pacific" = teal,
                                 "World" = ink, "EU27" = blue)) +
  guides(colour = two_rows) +
  labs(x = NULL, y = "Gross emissions intensity\n(kg CO₂/t cementitious material)")

c1 <- ggplot(india, aes(value, component, fill = component)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.23,
            colour = ink, size = 4.0) +
  scale_fill_manual(values = c("Calcination" = blue, "Fuel combustion" = mint,
                               "Electricity" = pink), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Emissions (t CO₂e/t cement)", y = NULL)

d1 <- ggplot(finance_long, aes(year, value, colour = series)) +
  line_points +
  scale_x_continuous(breaks = finance$year) +
  scale_colour_manual(values = c("Selling price" = ink, "Cost of sales" = rose,
                                 "Gross profit" = teal)) +
  labs(x = NULL, y = "CNY/t cement sold")

tax_line <- tibble(
  tax = seq(0, max(tax_cases$tax_cny2024_per_tco2) * 1.05, length.out = 200),
  profit = gp0 - tax * intensity
)

e1 <- ggplot(tax_line, aes(tax, profit)) +
  annotate("rect", xmin = min(tax_cases$tax_cny2024_per_tco2),
           xmax = max(tax_cases$tax_cny2024_per_tco2),
           ymin = -Inf, ymax = Inf, fill = peach, alpha = 0.55) +
  geom_hline(yintercept = 0, colour = "#A5AFBC", linewidth = 0.36) +
  geom_line(colour = blue, linewidth = 0.72) +
  geom_point(data = tax_cases, aes(tax_cny2024_per_tco2, gross_profit_after_tax),
             shape = 21, fill = pink, colour = rose, size = 2.25, stroke = 0.45) +
  annotate("point", x = tax_break_even, y = 0, shape = 23, fill = mint,
           colour = teal, size = 2.6, stroke = 0.45) +
  scale_x_continuous(labels = num) +
  scale_y_continuous(labels = num) +
  labs(x = "Carbon price (2024 CNY/t CO₂)", y = "Gross profit\n(2024 CNY/t cement)") +
  tight_y

f1 <- ggplot(pass_evidence, aes(y = label)) +
  geom_segment(data = filter(pass_evidence, is.finite(lo)),
               aes(x = lo, xend = hi, yend = label), colour = teal,
               linewidth = 1.95, lineend = "round") +
  geom_text(data = filter(pass_evidence, is.na(lo)), aes(x = 0.02),
            label = "No significant estimate", hjust = 0, colour = ink, size = 3.8) +
  percent_x() +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Observed cost pass-through", y = NULL)

# Figure 2
a2 <- ggplot(pricing, aes(pass, retained, fill = gain_vs_best_alternative)) +
  geom_tile(width = 0.01, height = 0.01, colour = NA) +
  geom_vline(xintercept = pass_min, linetype = 2, colour = ink, linewidth = 0.5) +
  percent_x() + percent_y() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  heat_scale("Contribution gain (CNY/t baseline sales)") + heat_theme +
  labs(x = "Carbon cost passed through", y = "Baseline sales retained")

b2 <- ggplot(demand, aes(pass, contribution, colour = factor(elasticity),
                          linetype = factor(elasticity))) +
  geom_hline(yintercept = 0, colour = "#A5AFBC", linewidth = 0.36) +
  geom_line(linewidth = 0.72) +
  scale_colour_manual(name = "Elasticity", values = c(blue, ink, rose)) +
  scale_linetype_manual(name = "Elasticity", values = c("dashed", "solid", "dotdash")) +
  legend_title + percent_x() + scale_y_continuous(labels = num) +
  labs(x = "Carbon cost passed through", y = "Contribution\n(CNY/t baseline sales)")

c2 <- ggplot(retro_grid, aes(tax, load)) +
  geom_tile(aes(fill = npv_m), width = 2, height = 0.01, colour = NA) +
  geom_contour(aes(z = npv_m), breaks = 0, colour = ink, linewidth = 0.62) +
  geom_vline(xintercept = tax_us22, colour = ink, linetype = 2, linewidth = 0.5) +
  scale_y_continuous(labels = pct) +
  coord_cartesian(xlim = range(retro_grid$tax), ylim = range(retro_grid$load),
                  expand = FALSE) +
  heat_scale("Incremental NPV (million 2022 USD)") + heat_theme +
  labs(x = "Carbon price (2022 USD/t CO₂)", y = "Share of reference output")

strategy_colours <- c("Invest now" = blue, "Wait one year" = teal, "No retrofit" = rose)
d2 <- ggplot(timing, aes(prob_high, npv_m, colour = strategy, linetype = strategy)) +
  geom_line(linewidth = 0.72) +
  scale_colour_manual(values = strategy_colours) +
  scale_linetype_manual(values = c("Invest now" = "solid", "Wait one year" = "longdash",
                                   "No retrofit" = "dotted")) +
  percent_x() + scale_y_continuous(labels = num) +
  labs(x = "Probability of high future output", y = "Expected NPV\n(million 2022 USD)")

mix_plot <- concrete |>
  distinct(cement_kg_m3, pass, cost_cny_m3, pass_label) |>
  mutate(pass_label = factor(pass_label, levels = c("20%", "40%", "100%")))

e2 <- ggplot(mix_plot, aes(cement_kg_m3, cost_cny_m3,
                           colour = pass_label, shape = pass_label)) +
  line_points +
  scale_colour_manual(name = "Pass-through", values = c(blue, teal, rose)) +
  scale_shape_manual(name = "Pass-through", values = c(16, 17, 15)) +
  legend_title +
  labs(x = "Purchased cement (kg/m³ concrete)", y = "Added cement cost\n(2024 CNY/m³)")

sens_plot <- sensitivity |>
  arrange(xmax - xmin) |>
  mutate(parameter = factor(parameter, levels = parameter))

sens_points <- sens_plot |>
  pivot_longer(c(npv_at_low, npv_at_high), names_to = "endpoint", values_to = "value") |>
  mutate(endpoint = if_else(endpoint == "npv_at_low", "Lower input", "Higher input"))

f2 <- ggplot(sens_plot, aes(y = parameter)) +
  geom_vline(xintercept = base_npv, linetype = 2, colour = ink, linewidth = 0.5) +
  geom_segment(aes(x = xmin, xend = xmax, yend = parameter),
               colour = "#A7C7E7", linewidth = 1.25) +
  geom_point(data = sens_points, aes(x = value, colour = endpoint, shape = endpoint),
             size = 2.25) +
  scale_colour_manual(values = c("Lower input" = blue, "Higher input" = rose)) +
  scale_shape_manual(values = c("Lower input" = 16, "Higher input" = 17)) +
  scale_x_continuous(labels = num) +
  labs(x = "Incremental NPV (million 2022 USD)", y = NULL)

# Keep each panel's axis-title and tick-label widths local to its cell.
make_figure <- function(panels) {
  cells <- lapply(seq_along(panels), function(i) {
    panel <- panels[[i]] + labs(tag = letters[i])
    wrap_elements(full = ggplotGrob(panel), clip = FALSE)
  })
  wrap_plots(cells, ncol = 2, nrow = 3, byrow = TRUE,
             widths = c(1, 1), heights = c(1, 1, 1)) +
    plot_annotation(theme = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(5, 6, 5, 6)
    ))
}

figures <- list(
  make_figure(list(a1, b1, c1, d1, e1, f1)),
  make_figure(list(a2, b2, c2, d2, e2, f2))
)

figure_width <- 13.4
figure_height <- 13.0

for (i in seq_along(figures)) {
  stem <- file.path(out_dir, paste0("Figure_", i))
  if (capabilities("cairo")) {
    ggsave(paste0(stem, ".pdf"), figures[[i]], device = grDevices::cairo_pdf,
           width = figure_width, height = figure_height, units = "in", bg = "white")
  } else {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      install.packages("svglite", repos = "https://cloud.r-project.org")
    }
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      install.packages("rsvg", repos = "https://cloud.r-project.org")
    }
    svg_tmp <- tempfile(fileext = ".svg")
    ggsave(svg_tmp, figures[[i]], device = svglite::svglite,
           width = figure_width, height = figure_height, units = "in", bg = "white")
    rsvg::rsvg_pdf(svg_tmp, file = paste0(stem, ".pdf"),
                   width = figure_width * 72, height = figure_height * 72)
    unlink(svg_tmp)
  }
  ggsave(paste0(stem, ".png"), figures[[i]], width = figure_width,
         height = figure_height, units = "in", dpi = 600, bg = "white")
}

captions <- c(
  "Figure 1. Carbon pricing and cement producers' exposure.",
  "(a) Global IPCC C1 marginal abatement costs and regional carbon prices from the IEA 2021 net-zero scenario, expressed in 2024 USD. Vertical bars show the reported IPCC pathway range. Lines connect reported years.",
  "(b) Regional gross CO2 intensity per tonne of cementitious material. Values are approximate readings from published GCCA/GNR curves.",
  "(c) Indian cement emissions by source in 2023. Electricity is an indirect source.",
  "(d) Huaxin group cement selling price, cost of sales and gross profit per tonne sold, in each year's nominal CNY.",
  "(e) Huaxin gross profit under a hypothetical full tax on direct emissions, at fixed 2024 selling price, sales and technology. Domestic Scope 1 intensity is used as a proxy for the group. Shading spans the 2030 IPCC range. Circles mark the low, central and high scenarios. The diamond marks zero gross profit.",
  "(f) Historical EU ETS pass-through estimates by product and market. Segments show ranges reported across the indicated markets. The UK and Italy entry records a lack of statistical significance.",
  "",
  "Figure 2. Pricing, investment timing and downstream cost under a hypothetical carbon tax.",
  "(a) Operating contribution relative to the better of holding price at baseline sales and idling. Values are per tonne of baseline sales. The dashed line marks the pass-through needed to cover avoidable costs at positive sales. Sales retention and pass-through vary independently in this panel.",
  "(b) Contribution when sales retention follows constant-elasticity demand. Three historical US market elasticity estimates are applied as stress tests to Huaxin's 2024 baseline. The large price changes extrapolate beyond the baseline and do not establish firm-specific demand.",
  "(c) Incremental NPV of capture at a US reference cement plant. The solid contour marks zero NPV. The dashed line marks the central carbon price in 2022 USD. Throughput scales capture, avoided emissions and variable operating costs.",
  "(d) Expected NPV over a common 20-year horizon. Immediate investment earns the first year's cash flow at reference throughput. Waiting reveals the persistent demand state after one year and permits the firm to decline investment. The remaining operating life is then 19 years. No retrofit has zero incremental NPV.",
  "(e) Added cement expenditure per cubic metre for ten published concrete mixes, under three cement cost pass-through assumptions. Mixes with equal purchased cement content coincide. The calculation holds mix composition fixed and excludes separately purchased slag and fly ash.",
  "(f) One-at-a-time sensitivity of retrofit NPV. The dashed line marks the central case. Circles and triangles mark the lower and higher input, respectively. The underlying input ranges are in investment_sensitivity.csv.",
  "",
  "Monetary conventions and scope",
  "US CPI-U puts scenario prices on a common dollar basis. The 2024 annual-average CNY/USD exchange rate converts the Huaxin tax scenarios. US engineering costs are treated on an approximate common 2022 USD basis. The retrofit case assumes an 8% real discount rate, a 0.85 avoided-to-captured CO2 ratio, no capture subsidy and no terminal value. The Huaxin and US plant results describe separate cases.",
  "",
  "Source identifiers in data/00_sources.csv",
  "Figure 1a: 1, 2, 22. Figure 1b: 15. Figure 1c: 10. Figure 1d: 3-5. Figure 1e: 1, 4, 6, 22, 23. Figure 1f: 18.",
  "Figure 2a: 1, 4, 6, 22, 23. Figure 2b: same inputs plus 19. Figure 2c, d and f: 1, 16, 22 and stated scenario assumptions. Figure 2e: 1, 4, 6, 21-23 and stated pass-through assumptions."
)

writeLines(enc2utf8(captions), file.path(out_dir, "captions.txt"), useBytes = TRUE)
message("Saved 13 CSV tables, two vector PDFs, two 600 dpi PNGs and captions.txt to output/.")

export_figures <- function() {
  out_dir <- normalizePath("output", mustWork = TRUE)
  stems <- c("Figure_1", "Figure_2")
  width <- 8040L
  height <- 7800L
  dpi <- 600L

  stage <- tempfile("png_export_", tmpdir = out_dir)
  if (!dir.create(stage)) stop("Could not create the temporary export directory.")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  check_png <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con))

    header <- readBin(con, "raw", n = 24L)
    signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
    if (length(header) != 24L || !identical(header[1:8], signature) ||
        rawToChar(header[13:16]) != "IHDR") {
      stop("Invalid PNG header in ", basename(path))
    }

    size <- readBin(header[17:24], integer(), n = 2L, size = 4L, endian = "big")
    if (!identical(size, c(width, height))) {
      stop(sprintf("Unexpected dimensions in %s: %d x %d",
                   basename(path), size[1], size[2]))
    }

    seek(con, where = -12, origin = "end")
    trailer <- readBin(con, "raw", n = 12L)
    png_end <- as.raw(c(0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130))
    if (!identical(trailer, png_end)) stop("Incomplete PNG: ", basename(path))
  }

  render_one <- function(stem) {
    pdf_path <- file.path(out_dir, paste0(stem, ".pdf"))
    png_path <- file.path(stage, paste0(stem, ".png"))

    if (!file.exists(pdf_path)) stop("Missing PDF: ", pdf_path)
    if (pdftools::pdf_info(pdf_path)$pages != 1L) {
      stop("Expected a single-page PDF: ", pdf_path)
    }

    bitmap <- pdftools::pdf_render_page(
      pdf_path, page = 1L, dpi = dpi, numeric = FALSE, antialias = TRUE
    )
    dims <- dim(bitmap)
    if (typeof(bitmap) != "raw" || length(dims) != 3L || dims[1] != 4L) {
      stop("Unexpected RGBA bitmap format in ", stem)
    }

    dx <- width - dims[2]
    dy <- height - dims[3]
    if (dx < 0L || dy < 0L || dx > 9L || dy > 9L) {
      stop(sprintf("%s rendered at %d x %d. Check the PDF page size.",
                   stem, dims[2], dims[3]))
    }

    if (dx > 0L || dy > 0L) {
      canvas <- array(as.raw(255), dim = c(4L, width, height))
      x <- seq_len(dims[2]) + dx %/% 2L
      y <- seq_len(dims[3]) + dy %/% 2L
      canvas[, x, y] <- bitmap
      rm(bitmap)
    } else {
      canvas <- bitmap
      rm(bitmap)
    }

    # Raw RGBA uses channel, width, height.
    png::writePNG(canvas, target = png_path, dpi = dpi)
    check_png(png_path)
  }

  for (stem in stems) {
    render_one(stem)
    invisible(gc())
  }

  for (stem in stems) {
    name <- paste0(stem, ".png")
    dest <- file.path(out_dir, name)
    if (!file.rename(file.path(stage, name), dest)) {
      stop("Could not replace ", dest)
    }
    check_png(dest)
  }

  message("Verified Figure_1.png and Figure_2.png: 8040 x 7800 pixels at 600 dpi.")
}

export_figures()
