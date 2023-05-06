library(fpp3)
library(fpp2)
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::filter)

## fpp3
# aus_accommodation Australian accommodation data
# aus_airpassengers Air Transport Passengers Australia
# aus_arrivals      International Arrivals to Australia
# bank_calls        Call volume for a large North American commercial bank
# boston_marathon   Boston marathon winning times since 1897
# canadian_gas      Monthly Canadian gas production
# guinea_rice       Rice production (Guinea)
# insurance         Insurance quotations and advertising expenditure
# prices            Price series for various commodities
# souvenirs         Sales for a souvenir shop
# us_change         Percentage changes in economic variables in the USA.
# us_employment     US monthly employment data
# us_gasoline       US finished motor gasoline product supplied.

## tsbbledata
# PBS             Monthly Medicare Australia prescription data
# ansett          Passenger numbers on Ansett airline flights
# aus_livestock   Australian livestock slaughter
# aus_production  Quarterly production of selected commodities in Australia.
# aus_retail      Australian retail trade turnover
# gafa_stock      GAFA stock prices
# global_economy  Global economic indicators
# hh_budget       Household budget characteristics
# nyc_bikes       NYC Citi Bike trips
# olympic_running Fastest running times for Olympic races
# pelt            Pelt trading records
# vic_elec        Half-hourly electricity demand for Victoria, Australia


# TIME SERIES GRAPHICKS ---------------------------------------------------


# tsibbles ----------------------------------------------------------------


the_data <- economics %>% 
  pivot_longer(!date, names_to = "variable") %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date, key = variable)

a10 <- fpp2::a10 %>% as_tsibble() %>% mutate(Cost = value)

# Time Plots --------------------------------------------------------------

autoplot(the_data, value)

melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# Seasonal plots ----------------------------------------------------------

a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

the_data %>% filter(variable == "pce") %>% gg_season(value)

## Multiple seasonal periods

vic_elec

vic_elec |> gg_season(Demand, period = "year") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

vic_elec |> gg_season(Demand, period = "month") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")


# Seasonal subseries plots ------------------------------------------------

a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

## Example: Australian holiday tourism

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays

autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

holidays |>
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# Scaterrplots ------------------------------------------------------------

vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

# Scatterplot matrices 

visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)


# Lag plots ---------------------------------------------------------------


recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# Autocorrelation ---------------------------------------------------------

recent_production |> ACF(Beer, lag_max = 9)

recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title="Australian beer production")

# Trend and seasonality in ACF plots

a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

# White Noise -------------------------------------------------------------

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")

y |>
  ACF(wn) |>
  autoplot() + labs(title = "White noise")

# TIME SERIES DECOMPOSITION -----------------------------------------------

# Transformations and Adjustments -----------------------------------------


# Calendar adjustments

# For example, if you are studying the total monthly sales in a retail store, 
# there will be variation between the months simply because of the different 
# numbers of trading days in each month, in addition to the seasonal variation 
# across the year. It is easy to remove this variation by computing average sales 
# per trading day in each month, rather than total sales in the month. Then we 
# effectively remove the calendar variation.

# Population adjustments
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP/Population) +
  labs(title= "GDP per capita", y = "$US")

# Inflation adjustments
print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

aus_economy <- global_economy |>
  filter(Code == "AUS")

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") |>
  mutate(name = factor(name,
                       levels=c("Turnover","Adjusted_turnover"))) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

# Mathematical transformations

lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)

aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

aus_production %>% 
  select(Gas) %>% 
  mutate(b_c = box_cox(Gas, lambda)) %>% 
  mutate(inv_bc = (lambda * b_c + 1)^(1/lambda))


# Time Series Components --------------------------------------------------

# Example: Employment in the US retail sector
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp)

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) |> autoplot()

# Seasonally adjusted data
components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


# Moving averages ---------------------------------------------------------

# Moving average smoothing
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

# Moving averages of moving averages
beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)
beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )

# Estimating the trend-cycle with seasonal data

# Example: Employment in the US retail sector
us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# Weighted moving averages


# Classical decomposition -------------------------------------------------

# Additive decomposition

us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")

# Multiplicative decomposition
us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "multiplicative")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical multiplicative decomposition of total
                  US retail employment")


# Methods used by official statistics agencies ----------------------------

# X-11 method
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")

x11_dcmp |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

# SEATS method
seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")

# STL decomposition -------------------------------------------------------

us_retail_employment |>
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

# Ver https://otexts.com/fpp3/stl.html

# TIME SERIES FEATURES ----------------------------------------------------


# Some simple statistics --------------------------------------------------

tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

tourism |> features(Trips, quantile)

# ACF features ------------------------------------------------------------

tourism |> features(Trips, feat_acf)

# STL Features ------------------------------------------------------------

tourism |>
  features(Trips, feat_stl)

tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

# Exploring Australian tourism data ---------------------------------------

tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))

library(glue)
tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))

library(broom)
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)

outliers |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  mutate(Series = glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")

# THE FORECASTER TOOLBOX --------------------------------------------------


