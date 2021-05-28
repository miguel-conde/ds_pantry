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

parametric_workflow <- 
  workflow() %>% 
  # Pass the data along as-is:
  # add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_recipe(recipe(distance ~ Sex + age + Subject, Orthodont)) %>% 
  add_model(parametric_model,
            # This formula is given to the model
            formula = distance ~ Sex + (age | Subject))

parametric_fit <- fit(parametric_workflow, data = Orthodont)
parametric_fit


# WORKFLOWSETS ------------------------------------------------------------

# Multiple workflows at once

# List with a set of formulas
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

library(workflowsets)
location_models <- workflow_set(preproc = location, 
                                models = list(lm = lm_model))
location_models

location_models$info[[1]]
location_models$option[[1]]
location_models$result[[1]]

pull_workflow(location_models, id = "coords_lm")

# Model fits
location_models <-
  location_models %>%
  mutate(fit = map(info, ~ fit(.x$workflow[[1]], ames_train)))
location_models

location_models$fit[[1]]

# YARDSTICK ---------------------------------------------------------------

# Models performance

# Regression --------------------------------------------------------------

ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res

ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res

ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)


# Binary Classification ---------------------------------------------------

data(two_class_example)
str(two_class_example)

conf_mat(two_class_example, truth = truth, estimate = predicted)

accuracy(two_class_example, truth = truth, estimate = predicted)

# Matthews correlation coefficient:
mcc(two_class_example, truth, predicted)

# F1 metric:
f_meas(two_class_example, truth, predicted)

# For binary classification data sets, these functions have a standard argument
# called event_level. The default is that the first level of the outcome factor 
# is the event of interest.
f_meas(two_class_example, truth, predicted, event_level = "second")

# There are numerous classification metrics that use the predicted probabilities
# as inputs rather than the hard class predictions. For example, the receiver 
# operating characteristic (ROC) curve computes the sensitivity and specificity 
# over a continuum of different event thresholds. The predicted class column is 
# not used. There are two yardstick functions for this method: roc_curve() 
# computes the data points that make up the ROC curve and roc_auc() computes 
# the area under the curve.
# The interfaces to these types of metric functions use the ... argument 
# placeholder to pass in the appropriate class probability column. For 
# two-class problems, the probability column for the event of interest is 
# passed into the function:

two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve

roc_auc(two_class_example, truth, Class1)

autoplot(two_class_curve)

# There are a number of other functions that use probability estimates, 
# including gain_curve(), lift_curve(), and pr_curve().
autoplot(lift_curve(two_class_example, truth, Class1))


# Multiclass Classification -----------------------------------------------

data(hpc_cv)
str(hpc_cv)

accuracy(hpc_cv, obs, pred)

mcc(hpc_cv, obs, pred)

# There are methods for using metrics that are specific to outcomes with two 
# classes for data sets with more than two classes.
class_totals <- 
  count(hpc_cv, obs, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))

class_totals

cell_counts <- 
  hpc_cv %>% 
  group_by(obs, pred) %>% 
  count() %>% 
  ungroup()

cell_counts

one_versus_all <- 
  cell_counts %>% 
  filter(obs == pred) %>% 
  full_join(class_totals, by = "obs") %>% 
  mutate(sens = n / totals)

one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )

sensitivity(hpc_cv, obs, pred, estimator = "macro")
sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")
sensitivity(hpc_cv, obs, pred, estimator = "micro")

autoplot(roc_curve(hpc_cv, obs, VF, F, M, L))
roc_auc(hpc_cv, obs, VF, F, M, L)

roc_auc(hpc_cv, obs, VF, F, M, L, estimator = "macro_weighted")

# Finally, all of these performance metrics can be computed using dplyr 
# groupings. Recall that these data have a column for the resampling groups. 
# Passing a grouped data frame to the metric function will compute the metrics 
# for each group:
hpc_cv %>% 
  group_by(Resample) %>% 
  accuracy(obs, pred)

hpc_cv %>% 
  group_by(Resample) %>% 
  roc_curve(obs, VF, F, M, L) %>% 
  autoplot()


# RSAMPLE -----------------------------------------------------------------

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

rf_fit <- rf_wflow %>% fit(data = ames_train)

estimate_perf <- function(model, dat) {
  # Capture the names of the objects used
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>% 
    predict(dat) %>% 
    bind_cols(dat %>% select(Sale_Price)) %>% 
    reg_metrics(Sale_Price, .pred) %>% 
    select(-.estimator) %>% 
    mutate(object = obj_name, data = data_name)
}

estimate_perf(rf_fit, ames_train)
estimate_perf(lm_fit, ames_train)

estimate_perf(rf_fit, ames_test)
estimate_perf(lm_fit, ames_test)

# Resampling Methods ------------------------------------------------------


# 1 - Cross Validation ----------------------------------------------------

set.seed(55)
ames_folds <- vfold_cv(ames_train, v = 10)
ames_folds

# For the first fold:
ames_folds$splits[[1]] %>% analysis() %>% dim()
ames_folds$splits[[1]] %>% assessment() %>% dim()


# 1.1 - Repeated Cross Validation -----------------------------------------
vfold_cv(ames_train, v = 10, repeats = 5)


# 1.2 - Leave-one-out Cross Validation ------------------------------------


# 1.3 - Montecarlo Cross Validation ---------------------------------------
mc_cv(ames_train, prop = 9/10, times = 20)



# 2 - Validations sets ----------------------------------------------------

set.seed(12)
val_set <- validation_split(ames_train, prop = 3/4)
val_set


# 3 - Bootsrapping --------------------------------------------------------

bootstraps(ames_train, times = 5)


# 4 - Rolling Forecasting origin resampling -------------------------------

time_slices <- 
  tibble(x = 1:365) %>% 
  rolling_origin(initial = 6 * 30, assess = 30, skip = 29, cumulative = FALSE)

data_range <- function(x) {
  summarize(x, first = min(x), last = max(x))
}

map_dfr(time_slices$splits, ~   analysis(.x) %>% data_range())

map_dfr(time_slices$splits, ~ assessment(.x) %>% data_range())


# Estimating Performance --------------------------------------------------

# Let’s reconsider the previous random forest model contained in the rf_wflow 
# object. The tune::fit_resamples() function is analogous to fit(), but instead  
# of having a data argument, fit_resamples() has resamples which expects an rset 
# object like the ones shown above. 

# For our example, let’s save the predictions in order to visualize the model 
# fit and residuals:

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(130)
rf_res <- 
  rf_wflow %>% 
  fit_resamples(resamples = ames_folds, control = keep_pred)

rf_res

collect_metrics(rf_res)

collect_metrics(rf_res, summarize = FALSE)

# To obtain the assessment set predictions
assess_res <- collect_predictions(rf_res)
assess_res

# Let’s compare the observed and predicted values
assess_res %>% 
  ggplot(aes(x = Sale_Price, y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(col = "red") + 
  coord_obs_pred() + 
  ylab("Predicted")

over_predicted <- 
  assess_res %>% 
  mutate(residual = Sale_Price - .pred) %>% 
  arrange(desc(abs(residual))) %>% 
  slice(1)
over_predicted

ames_train %>% 
  slice(over_predicted$.row) %>% 
  select(Gr_Liv_Area, Neighborhood, Year_Built, Bedroom_AbvGr, Full_Bath)


# Parallel Processing -----------------------------------------------------

# The tune package uses the foreach package to facilitate parallel computations. 
# These computations could be split across processors on the same computer or 
# across different computers, depending on the chosen technology.

# For computations conducted on a single computer, the number of possible 
# “worker processes” is determined by the parallel package:

# The number of physical cores in the hardware:
parallel::detectCores(logical = FALSE)

# The number of possible independent processes that can 
# be simultaneously used:  
parallel::detectCores(logical = TRUE)

# The difference between these two values is related to the computer’s processor. 
# For example, most Intel processors use hyper-threading which creates two 
# virtual cores for each physical core. While these extra resources can improve 
# performance, most of the speed-ups produced by parallel processing occur when 
# processing uses fewer than the number of physical cores.

# Unix and macOS only
# library(doMC)
# registerDoMC(cores = 2)

# Now run fit_resamples()...

# To reset the computations to sequential processing:
# registerDoSEQ(

# # All operating systems
# library(doParallel)
# 
# # Create a cluster object and then register: 
# cl <- makePSOCKcluster(2)
# registerDoParallel(cl)
# 
# # Now run fit_resamples()`...
# 
# stopCluster(cl)


# Saving the resampled objects --------------------------------------------

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <-  
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(linear_reg() %>% set_engine("lm")) 

lm_fit <- lm_wflow %>% fit(data = ames_train)

# Select the recipe: 
pull_workflow_prepped_recipe(lm_fit)

get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

# Test it using: 
# get_model(lm_fit)

ctrl <- control_resamples(extract = get_model)

lm_res <- lm_wflow %>%  fit_resamples(resamples = ames_folds, control = ctrl)
lm_res

lm_res$.extracts[[1]]

# To get the results
lm_res$.extracts[[1]][[1]]

all_coef <- map_dfr(lm_res$.extracts, ~ .x[[1]][[1]])
# Show the replicates for a single predictor:
filter(all_coef, term == "Year_Built")


# Summary code ------------------------------------------------------------

library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

set.seed(55)
ames_folds <- vfold_cv(ames_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(130)
rf_res <- rf_wflow %>% fit_resamples(resamples = ames_folds, control = keep_pred)


# COMPARING MODELS --------------------------------------------------------

basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec
  )

lm_models <- workflow_set(preproc, list(lm = lm_model), cross = FALSE)
lm_models

lm_models <- 
  lm_models %>% 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               seed = 1101, verbose = TRUE,
               # Options to `fit_resamples()`: 
               resamples = ames_folds, 
               # metrics = metric_set(rmse, rsq, mae), 
               control = keep_pred)

collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")

# What about the random forest model from the previous chapter? We can add it 
# to the set by first converting it to its own workflow set then binding rows. 
# This requires that, when the model was resampled, the save_workflow = TRUE 
# option was set in the control function.

four_models <- 
  as_workflow_set(random_forest = rf_res) %>% 
  bind_rows(lm_models)
four_models

autoplot(four_models, metric = "rsq")
