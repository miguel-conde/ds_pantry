library(tidyverse)
library(tidymodels)


# Load the data -----------------------------------------------------------


library(modeldata) # This is also loaded by the tidymodels package
data(ames)

# or, in one line:
data(ames, package = "modeldata")

dim(ames)

# Exploring the data ------------------------------------------------------

library(tidymodels)

# For convenience, tidymodels contains a function that captures most of the 
# common naming conflicts that we might encounter
tidymodels_prefer(quiet = FALSE)

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50)

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50) +
  scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


# RESAMPLE ----------------------------------------------------------------


# Splitting the data ------------------------------------------------------

## SIMPLE RANDOM SAMPLING

# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(123)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80) # From rsample package
ames_split

# The object ames_split is an rsplit object and only contains the partitioning 
# information; to get the resulting data sets, we apply two more functions:

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

## STRATIFIED SAMPLING
set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

# The rsample package contains a function called initial_time_split() that is 
# very similar to initial_split(). Instead of using random sampling, the prop 
# argument denotes what proportion of the first part of the data should be used 
# as the training set; the function assumes that the data have been pre-sorted 
# in an appropriate order.


# RECIPEs -----------------------------------------------------------------


# Feature engineering with recipes ----------------------------------------

## Write RECIPE - define the preprocessing
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())

simple_ames

# Other selectors specific to the recipes package are: all_numeric_predictors(),
# all_numeric(), all_predictors(), and all_outcomes(). As with dplyr, one or 
# more unquoted expressions, separated by commas, can be used to select which 
# columns are affected by each step.

# PREP recipe - estimate (from the training set) any quantities required by the
# steps
simple_ames <- prep(simple_ames, training = ames_train)
simple_ames

# One important argument to prep() is retain. When retain = TRUE (the default), 
# the prepared version of the training set is kept within the recipe. This data 
# set has been pre-processed using all of the steps listed in the recipe. Since 
# prep() has to execute the recipe as it proceeds, it may be advantageous to 
# keep this version of the training set so that, if that data set is to be used 
# later, redundant calculations can be avoided. However, if the training set is 
# big, it may be problematic to keep such a large amount of data in memory. Use 
# retain = FALSE to avoid this.

## BAKE the recipe - apply the preprocessing operations to a data set 
test_ex <- bake(simple_ames, new_data = ames_test)
names(test_ex) %>% head()

# Trick:
bake(simple_ames, ames_test, starts_with("Neighborhood_"))

# To get the processed version of the training set,:
bake(simple_ames, new_data = NULL) %>% nrow()

ames_train %>% nrow()

# Example
complex_ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20) 

complex_ames_prepped <- complex_ames_rec %>% 
  prep(training = ames_train) 

complex_ames_train_prepped <- complex_ames_prepped %>% 
  bake(new_data = NULL)

complex_ames_test_prepped <- complex_ames_prepped %>% 
  bake(ames_test)


# Using with a traditional model ------------------------------------------

# Fit the model; Note that the column Sale_Price has already been
# log transformed.
lm_fit <- lm(Sale_Price ~ ., data = complex_ames_train_prepped)

glance(lm_fit)

tidy(lm_fit)

predict(lm_fit, complex_ames_test_prepped %>% head())


# Tidy a recipe -----------------------------------------------------------

tidy(complex_ames_rec)

complex_ames_prepped %>% tidy(id = "dummy_6HPZn")

complex_ames_prepped %>% tidy(number = 2)


# Column roles ------------------------------------------------------------

complex_ames_train_prepped %>% select(address)

complex_ames_rec <- 
  recipe(Sale_Price ~ Street + Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20) %>% 
  update_role(Street, new_role = "street address")

complex_ames_prepped <- complex_ames_rec %>% 
  prep(training = ames_train) 

complex_ames_train_prepped <- complex_ames_prepped %>% 
  bake(new_data = NULL)

complex_ames_test_prepped <- complex_ames_prepped %>% 
  bake(ames_test)


# PARSNIP -----------------------------------------------------------------


# Create models -----------------------------------------------------------

#
linear_reg() %>% set_engine("lm") # Type of model + engine for fitting it

linear_reg() %>% set_engine("glmnet") 

linear_reg() %>% set_engine("stan")

#
linear_reg() %>% set_engine("lm") %>% translate()
linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()
linear_reg() %>% set_engine("stan") %>% translate()

#
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit

lm_xy_fit

# 
rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger", verbose = TRUE) %>% 
  set_mode("regression") %>% 
  translate()


# Model results -----------------------------------------------------------

# 
lm_form_fit %>% pluck("fit")
lm_form_fit %>% pluck("fit") %>% summary()
lm_form_fit %>% pluck("fit") %>% plot()
lm_form_fit %>% pluck("fit") %>% vcov()

#
model_res <- 
  lm_form_fit %>% 
  pluck("fit") %>% 
  summary()

model_res

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)

param_est

#
tidy(lm_form_fit)


# Predictions -------------------------------------------------------------

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

# Only changes "tree", anything else remains equal
tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))


# Tools for model specification -------------------------------------------

## usemodels
library(usemodels)

use_xgboost(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
              Latitude + Longitude, 
            data = ames_train,
            # Don't create the model tuning code:
            tune = FALSE,
            # Add comments explaining some of the code:
            verbose = TRUE)

# 
parsnip_addin()



# WORKFLOWS ---------------------------------------------------------------



# Basics ------------------------------------------------------------------

# 1 - add_model()
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow

# 2a - add_formula()
lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3))

# Both the model and preprocessor can be removed or updated
lm_fit %>% update_formula(Sale_Price ~ Longitude)


# Recipes -----------------------------------------------------------------


lm_wflow %>% 
  add_recipe(complex_ames_rec)

# 2b - add_recipe()
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_recipe(complex_ames_rec)

lm_wflow

# Does `prep()`, `bake()`, and `fit()` in one step:
lm_fit <- fit(lm_wflow, ames_train)

# Does `bake()` and `predict()` automatically:
predict(lm_fit, ames_test %>% slice(1:3))

# Get the recipe and run `tidy()` method: 
lm_fit %>% 
  pull_workflow_prepped_recipe() %>% 
  tidy()

# To tidy the model fit: 
lm_fit %>% 
  # This returns the parsnip object:
  pull_workflow_fit() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)

# 2c - add_variables()
lm_wflow <- 
  lm_wflow %>% 
  remove_recipe() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm_wflow


# Special formulas --------------------------------------------------------

data(Orthodont, package = "nlme")

library(lme4)
lmer(distance ~ Sex + (age | Subject), data = Orthodont)

library(multilevelmod)

parametric_model <- 
  linear_reg() %>% 
  set_engine("lmer")
  