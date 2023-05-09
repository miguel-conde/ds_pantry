
library(tidyverse)
library(fpp2)
library(fpp3)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(fabletools::accuracy)

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

# A tidy forecasting workflow ---------------------------------------------

# Data preparation (tidy)
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

# Plot the data (visualise)
gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# Define a model (specify)
TSLM(GDP_per_capita ~ trend())

# Train the model (estimate)
fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

# Check model performance (evaluate)

# Produce forecasts (forecast)
fit |> forecast(h = "3 years")

fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")


# Some simple forecasting methods -----------------------------------------

bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)

# Mean method
fit <- bricks |> model(MEAN(Bricks))

fit |>
  forecast(h = "3 years") |>
  autoplot(bricks) +
  labs(y = "Bricks", title = "Clay bricks production in Australia")

# Naïve method
fit <- bricks |> model(NAIVE(Bricks))

fit |>
  forecast(h = "3 years") |>
  autoplot(bricks) +
  labs(y = "Bricks", title = "Clay bricks production in Australia")

# Seasonal naïve method
fit <- bricks |> model(SNAIVE(Bricks ~ lag("year")))

fit |>
  forecast(h = "3 years") |>
  autoplot(bricks) +
  labs(y = "Bricks", title = "Clay bricks production in Australia")

# Drift method
fit <- bricks |> model(RW(Bricks ~ drift()))

fit |>
  forecast(h = "3 years") |>
  autoplot(bricks) +
  labs(y = "Bricks", title = "Clay bricks production in Australia")

# Example: Australian quarterly beer production

# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> forecast(h = 14)
# Plot forecasts against actual values
beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Example: Google’s daily closing stock price

# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

# Fitted values and residuals ---------------------------------------------

augment(beer_fit)

# Residual diagnostics ----------------------------------------------------

# A good forecasting method will yield innovation residuals with the following 
# properties:
# 
# The innovation residuals are uncorrelated. If there are correlations between 
# innovation residuals, then there is information left in the residuals which 
# should be used in computing forecasts.
# The innovation residuals have zero mean. If they have a mean other than zero, 
# then the forecasts are biased.
#
# In addition to these essential properties, it is useful (but not necessary) 
# for the residuals to also have the following two properties.
# 
# The innovation residuals have constant variance. This is known as 
# “homoscedasticity”.
# The innovation residuals are normally distributed.

# Example: Forecasting Google daily closing stock prices
autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")

aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the naïve method")

google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

# Portmanteau tests for autocorrelation

# lag is the maximum lag being considered.
# We suggest using lag = 10 for non-seasonal data and lag = 2m for seasonal data, 
# where m is the period of seasonality. However, the test is not good when  lag
# is large, so if these values are larger than T/5, then use lag = T/5
#
# H0: las autocorrelaciones vienen de ruido  blanco (residuos no correlados)

aug |> features(.innov, box_pierce, lag = 10)

aug |> features(.innov, ljung_box, lag = 10)

# the results are not significant (i.e., the  p-values are relatively large)

# An alternative simple approach that may be appropriate for forecasting the 
# Google daily closing stock price is the drift method.
fit <- google_2015 |> model(RW(Close ~ drift()))
tidy(fit)

augment(fit) |> features(.innov, ljung_box, lag=10)


# Distributional forecasts and prediction intervals -----------------------

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  hilo()

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

# Prediction intervals from bootstrapped residuals
fit <- google_2015 |>
  model(NAIVE(Close))
sim <- fit |> generate(h = 30, times = 5, bootstrap = TRUE)
sim

google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10, bootstrap = TRUE, times = 1000) |>
  hilo()


# Forecasting using transformations ---------------------------------------

# When forecasting from a model with transformations, we first produce forecasts 
# of the transformed data.

prices |>
  filter(!is.na(eggs)) |>
  model(RW(log(eggs) ~ drift())) |>
  forecast(h = 50) |>
  autoplot(prices |> filter(!is.na(eggs)),
           level = 80, point_forecast = lst(mean, median)
  ) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

# Forecasting with decomposition ------------------------------------------

# To forecast a decomposed time series, we forecast the seasonal component,  
# and the seasonally adjusted component separately. 
# It is usually assumed that the seasonal component is unchanging, or changing 
# extremely slowly, so it is forecast by simply taking the last year of the 
# estimated component. In other words, a seasonal naïve method is used for the 
# seasonal component.
# 
# To forecast the seasonally adjusted component, any non-seasonal forecasting 
# method may be used.

# Example: Employment in the US retail sector
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment |>
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) |>
  components() |>
  select(-.model)
dcmp |>
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment - Season Adjust")

fit_dcmp <- us_retail_employment |>
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp |>
  forecast() |>
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp |> gg_tsresiduals()

# The ACF of the residuals, shown in Figure 5.20, displays significant 
# autocorrelations. These are due to the naïve method not capturing the changing 
# trend in the seasonally adjusted series.

# Evaluating point forecast accuracy --------------------------------------

# Functions to subset a time series
aus_production |> filter(year(Quarter) >= 1995)
aus_production |> filter_index("1995 Q1" ~ .)
aus_production |>
  slice(n()-19:0)
aus_retail |>
  group_by(State, Industry) |>
  slice(1:12)

# Forecast errors
# Scale-dependent errors - MAE, RMSE
# Percentage errors - MAPE
# Scaled errors - MASE, RMSSE

# Examples

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
beer_train <- recent_production |>
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit |>
  forecast(h = 10)

beer_fc |>
  autoplot(
    aus_production |> filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

fabletools::accuracy(beer_fit)
fabletools::accuracy(beer_fc, recent_production)

# Non seasonal example
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit |>
  forecast(google_jan_2016)

google_fc |>
  autoplot(bind_rows(google_2015, google_jan_2016),
           level = NULL) +
  labs(y = "$US",
       title = "Google closing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

fabletools::accuracy(google_fit)
fabletools::accuracy(google_fc, google_stock)

# Evaluating distributional forecast accuracy -----------------------------

# Quantile scores
google_fc |>
  filter(.model == "Naïve") |>
  autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
  labs(y = "$US",
       title = "Google closing stock prices")

google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  fabletools::accuracy(google_stock, list(qs=quantile_score), probs=0.10)

# Winkler Score
google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  fabletools::accuracy(google_stock,
           list(winkler = winkler_score), level = 80)

# Continuous Ranked Probability Score
google_fc |>
  fabletools::accuracy(google_stock, list(crps = CRPS))

# Scale-free comparisons using skill scores
google_fc |>
  fabletools::accuracy(google_stock, list(skill = skill_score(CRPS)))

# Time series cross-validation --------------------------------------------

# Time series cross-validation accuracy
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1) |>
  relocate(Date, Symbol, .id)
google_2015_tr

# TSCV accuracy
google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 1) |>
  fabletools::accuracy(google_2015)
# Training set accuracy
google_2015 |>
  model(RW(Close ~ drift())) |>
  fabletools::accuracy()

# Example: Forecast horizon accuracy with cross-validation
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 8) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)
fc |>
  fabletools::accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()

# TIME SERIES REGRESSION MODELS -------------------------------------------

# The linear model --------------------------------------------------------

# Simple linear regression

# Example: US consumption expenditure
us_change |>
  pivot_longer(c(Consumption, Income), names_to="Series") |>
  autoplot(value) +
  labs(y = "% change")

us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

# Multiple linear regression

# Example: US consumption expenditure
us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

us_change |>
  GGally::ggpairs(columns = 2:6)

# Least squares estimation ------------------------------------------------

# Example: US consumption expenditure
fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))
report(fit_consMR)

# Fitted values
augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

# Goodness-of-fit

# Evaluating the regression model -----------------------------------------
fit_consMR |> gg_tsresiduals()

augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

# Residual plots against predictors
us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# Residual plots against fitted values
augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

# Outliers and influential observations

# Spurious regression
# Regressing non-stationary time series can lead to spurious regressions.
# Cases of spurious regression might appear to give reasonable short-term 
# forecasts, but they will generally not continue to work into the future.
fit <- aus_airpassengers |>
  filter(Year <= 2011) |>
  left_join(guinea_rice, by = "Year") |>
  model(TSLM(Passengers ~ Production))
report(fit)

fit |> gg_tsresiduals()


# Some useful predictors --------------------------------------------------

# Trend
# Dummy variables
# Dummy seasonal variables

# Example: Australian quarterly beer production
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

# We can model this data using a regression model with a linear trend and 
# quarterly dummy variables
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

# Intervention variables
# Trading days
# Distributed lags
# Easter
# Fourier Series

fourier_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

# Selecting predictors ----------------------------------------------------

glance(fit_consMR) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)

# Adjusted R2
# Cross validation
# Akaike’s Information Criterion
# Corrected Akaike’s Information Criterion
# Schwarz’s Bayesian Information Criterion

# Which measure should we use?

# Consequently, we recommend that one of the AICc, AIC, or CV statistics be used, 
# each of which has forecasting as their objective. 

# Example: US consumption

# Best subset regression
# Stepwise regression
# Beware of inference after selecting predictors


# Forecasting with regression ---------------------------------------------

# Example: Australian quarterly beer production

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))

fc_beer <- forecast(fit_beer)

fc_beer |>
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )

# Scenario based forecasting
fit_consBest <- us_change |>
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

# Building a predictive regression model

# Prediction intervals
# Example
fit_cons <- us_change |>
  model(TSLM(Consumption ~ Income))

new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) |>
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) |>
    mutate(Income = 12),
  names_to = "Scenario"
)

fcast <- forecast(fit_cons, new_cons)

us_change |>
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(title = "US consumption", y = "% change")

# Nonlinear regression ----------------------------------------------------

# Forecasting with a nonlinear trend

# Example: Boston marathon winning times
boston_men <- boston_marathon |>
  filter(Year >= 1924) |>
  filter(Event == "Men's open division") |>
  mutate(Minutes = as.numeric(Time)/60)

trnd_lm <- boston_men %>% 
  model(TSLM(Minutes ~ trend()))

boston_men %>% autoplot(Minutes) + 
  geom_line(data = fitted(trnd_lm), mapping = aes(x = Year, y = .fitted)) +
  geom_smooth(method = "lm", se = FALSE)

resid(trnd_lm) %>% ggplot(aes(x = Year, y = .resid)) + geom_line()

fit_trends <- boston_men |>
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends |> forecast(h = 10)

boston_men |>
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

# EXPONENTIAL SMOOTHING ---------------------------------------------------


# Simple exponential smoothing --------------------------------------------

# The simplest of the exponentially smoothing methods is naturally called simple 
# exponential smoothing (SES)14. This method is suitable for forecasting data 
# with no clear trend or seasonal pattern.
algeria_economy <- global_economy |>
  filter(Country == "Algeria")
algeria_economy |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

# Estimate parameters
fit <- algeria_economy |>
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit |>
  forecast(h = 5)

fc |>
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

# Methods with trend ------------------------------------------------------

# Holt’s linear trend method

# Example: Australian population
aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

fit <- aus_economy |>
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )
fc <- fit |> forecast(h = 10)

# Damped trend methods

# Example: Australian Population (continued)
aus_economy |>
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))
