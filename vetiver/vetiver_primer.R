# https://www.rstudio.com/blog/announce-vetiver/
# https://www.youtube.com/watch?v=oFQANK13-k4
# https://isabelizimm.github.io/rstudioconf2022-mlops/#/section

library(tidyverse)
library(vetiver)


# Getting Started ---------------------------------------------------------


## Train a model ----------------------------------------------------------

library(tidymodels)

car_mod <-
  workflow(mpg ~ ., linear_reg()) %>%
  fit(mtcars)

## Create a vetiver model -------------------------------------------------

v <- vetiver_model(car_mod, "cars_mpg")
v

# Think of this vetiver model as a deployable model object.


# Version -----------------------------------------------------------------


## Store and version your model -------------------------------------------

library(pins)

model_board <- board_temp(versioned = TRUE)
model_board %>% vetiver_pin_write(v)

# Let’s train a new kind of model for mtcars, a decision tree instead of our 
# original linear model.

car_mod <-
  workflow(mpg ~ ., decision_tree(mode = "regression")) %>%
  fit(mtcars)

v <- vetiver_model(car_mod, "cars_mpg")

model_board %>% vetiver_pin_write(v)

# Both versions are stored, and we have access to both.
model_board %>% pin_versions("cars_mpg")


# Deploy ------------------------------------------------------------------


## Create a REST API for deployment ---------------------------------------

library(plumber)

pr() %>%
  vetiver_api(v)

vetiver_write_plumber(model_board, "cars_mpg", file = "vetiver/plumber.R")


# Generate a Dockerfile ---------------------------------------------------

vetiver_write_docker(v, plumber_file = "vetiver/plumber.R", path = "vetiver/")

# Predict from your model endpoint ----------------------------------------

endpoint <- vetiver_endpoint("http://127.0.0.1:8080/predict")
endpoint

# Beforehand source entrypoint.R as local job 
new_car <- tibble(cyl = 4,  disp = 200, 
                  hp = 100, drat = 3,
                  wt = 3,   qsec = 17, 
                  vs = 0,   am = 1,
                  gear = 4, carb = 2)
predict(endpoint, new_car)
