library(tidyverse)
library(forecast)
library(fpp3) # https://otexts.com/fpp3/

PBS

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6) -> a10

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

a10 %>%
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

holidays %>%
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")


# Non-seasonal ARIMA models -----------------------------------------------

global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

# Fit a non-seasonal ARIMA model automatically
fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))

report(fit)

fit %>% forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot() # => no ma

global_economy %>%
  filter(Code == "EGY") %>%
  PACF(Exports) %>%
  autoplot() # => 4,0,0


fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)


# Example: Central African Republic exports -------------------------------

# 1. Plot the data. Identify unusual observations. Understand patterns.

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

# The time plot shows some non-stationarity, with an overall decline. The 
# improvement in 1994 was due to a new government which overthrew the military 
# junta and had some initial success, before unrest caused further economic 
# decline.

# 2. If necessary, use a Box-Cox Transformation to stabilize the variance.
# 
# There is no evidence of changing variance, so we will not do a Box-Cox 
# transformation.
# 
# AT THIS POITN WE CAN FIT AN AUTO ARIMA TO USE AS BENCHMARK
#

# 3. If necessary, difference the data until it appears non-stationary. Use
#    unit-root tests if you are unsure.
#
# To address the non-stationarity, we will take a first difference of the 
# data.


# 4. Plot the ACF / PACF of the differenced data and try to determine possible 
#    candidate models

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')

# The PACF shown in Figure 9.14 is suggestive of an AR(2) model; so an initial 
# candidate model is an ARIMA(2,1,0). The ACF suggests an MA(3) model; so an 
# alternative candidate is an ARIMA(0,1,3).

# 5. Try your chosen model(s) and use the AICc to search for a better model
# 
# We fit both an ARIMA(2,1,0) and an ARIMA(0,1,3) model along with two 
# automated model selections, one using the default stepwise procedure, and one 
# working harder to search a larger model space.

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")

# The four models have almost identical AICc values. Of the models fitted, the 
# full search has found that an ARIMA(3,1,0) gives the lowest AICc value, 
# closely followed by the ARIMA(2,1,0) and ARIMA(0,1,3) — the latter two being 
# the models that we guessed from the ACF and PACF plots. The automated stepwise 
# selection has identified an ARIMA(2,1,2) model, which has the highest AICc 
# value of the four models.

# 6. Check the residuals from your chosen model by plotting the ACF of the 
#    residuals, and doing a portmanteau test of the residuals.
#    
#        - Do the residuals look like white noise ?
#          YES => calculate forecasts 7
#           NO => go back to 4

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

# 
# The ACF plot of the residuals from the ARIMA(3,1,0) model shows that all 
# autocorrelations are within the threshold limits, indicating that the 
# residuals are behaving like white noise.

augment(caf_fit) %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

# A portmanteau test returns a large p-value, also suggesting that the 
# residuals are white noise.

# 7. Forecast
caf_fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)

# Note that the mean forecasts look very similar to what we would get with a 
# random walk (equivalent to an ARIMA(0,1,0)). The extra work to include AR and 
# MA terms has made little difference to the point forecasts in this example, 
# although the prediction intervals are much narrower than for a random walk 
# model.


gg_arma(caf_fit %>% select(Country, search))


# Seasonal ARIMA models ---------------------------------------------------


# Example: Monthly US leisure and hospitality employment ------------------


