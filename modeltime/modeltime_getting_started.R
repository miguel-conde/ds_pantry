# https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html

library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

# This toggles plots from plotly (interactive) to ggplot (static)
interactive <- FALSE


# STEP 1 - Collect data and split into training and test sets. ------------

# Data
m750 <- m4_monthly %>% filter(id == "M750")

m750 %>%
  plot_time_series(date, value, .interactive = interactive)

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)


# STEP 2 - Create & Fit Multiple Models -----------------------------------

## Important note: Handling Date Features

# Modeltime models (e.g. arima_reg()) are created with a date or date time 
# feature in the model. You will see that most models include a formula like 
# fit(value ~ date, data).
#
# Parsnip models (e.g. linear_reg()) typically should not have date features, 
# but may contain derivatives of dates (e.g. month, year, etc). You will often 
# see formulas like fit(value ~ as.numeric(date) + month(date), data).

# Model 1: auto_arima (Modeltime) ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date, data = training(splits))

# Model 2: arima_boost (Parsnip) ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), 
                                               ordered = F),
      data = training(splits))

# Model 3: ets (Modeltime) ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(value ~ date, data = training(splits))

# Model 4: prophet (Modeltime) ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date, data = training(splits))

# Model 5: lm (Parsnip) ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))

# Model 6: earth (Workflow) ----
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
  step_date(date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(date)) %>%
  step_normalize(date_num) %>%
  step_rm(date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

# STEP 3 - Add fitted models to a Model Table. ----------------------------

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl

# STEP 4 - Calibrate the model to a testing set. --------------------------

# Calibrating adds a new column, .calibration_data, with the test predictions 
# and residuals inside. A few notes on Calibration:
#  
# - Calibration is how confidence intervals and accuracy metrics are determined
# - Calibration Data is simply forecasting predictions and residuals that are 
#   calculated from out-of-sample data.
# - After calibrating, the calibration data follows the data through the 
#   forecasting workflow.

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl


# STEP 5 - Testing Set Forecast & Accuracy Evaluation ---------------------

# 5A - Visualizing the Forecast Test --------------------------------------

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = m750
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


# 5B - Accuracy Metrics ---------------------------------------------------

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )


# STEP 6 - Refit to Full Dataset & Forecast Forward -----------------------

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = m750)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = m750) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )
