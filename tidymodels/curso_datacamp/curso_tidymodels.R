library(tidyverse)
library(tidymodels)


# A. BASIC ----------------------------------------------------------------


# 1. Linear Regression ----------------------------------------------------


# Initial split -----------------------------------------------------------

mpg

mpg_split <- initial_split(mpg, 
                           prop = .75,
                           strata = hwy)

mpg_training <- mpg_split %>% 
  training()

mpg_test <- mpg_split %>% 
  testing()


# Model specification -------------------------------------------------------

lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")


# Model Fitting -----------------------------------------------------------

lm_fit <- lm_model %>% 
  fit(hwy ~ cty, 
      data = mpg_training)

tidy(lm_fit)

# Model predictions -------------------------------------------------------

# Predictions
hwy_predictions <- lm_fit %>% 
  predict(mpg_test)

hwy_predictions

mp_test_results <- mpg_test %>% 
  select(hwy, cty) %>% 
  bind_cols(hwy_predictions)

mp_test_results


# Evaluate ----------------------------------------------------------------

mp_test_results %>% 
  rmse(truth = hwy, estimate = .pred)

mp_test_results %>% 
  rsq(truth = hwy, estimate = .pred)

ggplot(mp_test_results, aes(x = hwy, y = .pred)) +
  geom_point() +
  geom_abline(color = "blue", linetype = 2) +
  coord_obs_pred() +
  labs(title = "R-Squared Plot",
       y = "Predicted Highway MPG",
       x = "Actual Highway MPG")


# All-in-one --------------------------------------------------------------

# Initial resample
# set.seed(2023)
mpg_split <- initial_split(mpg, 
                           prop = .75,
                           strata = hwy)

# Spec model
lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# Create training and test dataset
# Fit the model to the training data
# Calculate metrics and predictions on the testing data
# Retrun an object with all the results
lm_last_fit <- lm_model %>% 
  last_fit(hwy ~ cty,
           split = mpg_split)

# Metrics
lm_last_fit %>% 
  collect_metrics()

# Predcitions
lm_last_fit %>% 
  collect_predictions()


# 2. Logistic Regression --------------------------------------------------

leads_df <- readRDS("./data/tidymodels/leads_df.rds")
leads_df


# Initial split -----------------------------------------------------------

# set.seed(2023)
leads_split <- leads_df %>% 
  initial_split(prop = 0.75,
                strata = purchased)

leads_training <- leads_split %>% 
  training()

leads_test <- leads_split %>% 
  testing()


# Model Specification -----------------------------------------------------

logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")


# Model Fitting -----------------------------------------------------------

logistic_fit <- logistic_model %>% 
  fit(purchased ~ total_visits + total_time,
      data = leads_training)

# Model Predictions -------------------------------------------------------

class_preds <- logistic_fit %>% 
  predict(new_data = leads_test,
          type = "class")

class_preds

prob_preds <- logistic_fit %>% 
  predict(new_data = leads_test,
          type = "prob")

prob_preds

leads_results <- leads_test %>% 
  select(purchased) %>% 
  bind_cols(class_preds, prob_preds)

leads_results

# Evaluate ----------------------------------------------------------------

conf_mat(leads_results, 
         truth = purchased,
         estimate = .pred_class)

accuracy(leads_results, 
         truth = purchased,
         estimate = .pred_class)

sens(leads_results, 
     truth = purchased,
     estimate = .pred_class)

spec(leads_results, 
     truth = purchased,
     estimate = .pred_class)

#
custom_metrics <- metric_set(accuracy, sens, spec)
custom_metrics(leads_results, 
               truth = purchased,
               estimate = .pred_class)

#
conf_mat(leads_results, 
         truth = purchased,
         estimate = .pred_class) %>% 
  summary()

#
conf_mat(leads_results, 
         truth = purchased,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

conf_mat(leads_results, 
         truth = purchased,
         estimate = .pred_class) %>% 
  autoplot(type = "mosaic")

## ROC CURVE
leads_results %>% 
  roc_curve(truth = purchased, .pred_yes)

leads_results %>% 
  roc_curve(truth = purchased, .pred_yes) %>% 
  autoplot()

leads_results %>% 
  roc_auc(truth = purchased, .pred_yes)

# All-in_one workflow ---------------------------------------------------

# set.seed(2023)
leads_split <- leads_df %>% 
  initial_split(prop = 0.75,
                strata = purchased)

leads_training <- leads_split %>% 
  training()

leads_test <- leads_split %>% 
  testing()

# Spec
logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# 
logistic_last_fit <- logistic_model %>% 
  last_fit(purchased ~ total_visits + total_time,
           leads_split)

logistic_last_fit %>% 
  collect_metrics()

#
last_fit_results <- logistic_last_fit %>% 
  collect_predictions()

last_fit_results

custom_metrics <- metric_set(accuracy, sens, spec, roc_auc)
custom_metrics(last_fit_results,
               truth = purchased,
               estimate = .pred_class,
               .pred_yes)

# B. FEATURE ENGINEERING --------------------------------------------------


# 1. Overall process ------------------------------------------------------


# Specify variables types and roles ---------------------------------------

# recipe()

leads_log_rec <- recipe(purchased ~ .,
                        data = leads_training)

leads_log_rec

# Data preprocessing steps ------------------------------------------------

## step_xxx()

leads_log_rec <- recipe(purchased ~ .,
                        data = leads_training) %>% 
  step_log(total_time, base = 10)

leads_log_rec

leads_log_rec %>% summary()

# Train preprocessing steps on training data ------------------------------

# prep()

leads_log_rec_prep <- leads_log_rec %>% 
  prep(training = leads_training)

leads_log_rec_prep

# Apply trained preprocessing steps on new data ---------------------------

# bake()

leads_log_rec_prep %>% 
  bake(new_data = NULL)

leads_log_rec_prep %>% 
  bake(new_data = leads_test)


# 2. Specific transformations ---------------------------------------------


# a. Numeric predictors ----------------------------------------------------


# Correlated predictors ---------------------------------------------------

ggplot(leads_training,
       aes(x = pages_per_visit, y = total_clicks)) +
  geom_point() +
  labs(title = 'Total Clicks vs Average Page Visits',
       y = 'Total Clicks', x = 'Average Pages per Visit')

leads_training %>%
  select_if(is.numeric) %>%
  cor()

#
leads_cor_rec <- recipe(purchased ~ .,
                        data = leads_training) %>% 
  step_corr(total_visits, total_time, pages_per_visit, total_clicks,
            threshold = 0.9)

leads_cor_rec

#
leads_cor_rec <- recipe(purchased ~ .,
                        data = leads_training) %>% 
  step_corr(all_numeric(),
            threshold = 0.9)

leads_cor_rec

leads_cor_rec %>% 
  prep(training = leads_training) %>% 
  bake(new_data = leads_test)


# Normalization -----------------------------------------------------------

leads_norm_rec <- recipe(purchased ~ .,
                         data = leads_training) %>% 
  step_corr(all_numeric(), threshold = .9) %>% 
  step_normalize(all_numeric())

leads_norm_rec

leads_norm_rec %>% 
  prep(training = leads_training) %>% 
  bake(new_data = leads_test)

# b. Nominal predictors ---------------------------------------------------


# Dummy Variables ---------------------------------------------------------

recipe(purchased ~ .,
       data = leads_training) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep(training = leads_training) %>% 
  bake(new_data = leads_test)


# C. SUMMARY: COMPLETE WORKFLOW -------------------------------------------


# Data Resampling ---------------------------------------------------------

leads_split <- initial_split(leads_df, 
                             prop = .75,
                             strata = purchased)

leads_training <- leads_split %>% 
  training()

leads_test <- leads_split %>% 
  testing()

# Model Specification -----------------------------------------------------

logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Feature Engineering -----------------------------------------------------

leads_recipe <- 
  # Variable types and roles
  recipe(purchased ~ ., 
         data = leads_training) %>% 
  # Preprocessing steps
  step_corr(all_numeric(), threshold = .9) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

# Recipe training
leads_recipe_prep <- leads_recipe %>% 
  prep(training = leads_training)

# Preprocess training data
leads_training_prep <- leads_recipe_prep %>% 
  bake(new_data = NULL)

# Preprocess test data
leads_test_prep <- leads_recipe_prep %>% 
  bake(new_data = leads_test)

# Model fitting and predictions -------------------------------------------

logistic_fit <- logistic_model %>% 
  fit(purchased ~ .,
      data = leads_training_prep)

class_preds <- predict(logistic_fit, 
                       new_data = leads_test_prep, 
                       type = "class")

prob_preds <- predict(logistic_fit, 
                      new_data = leads_test_prep, 
                      type = "prob")

leads_results <- leads_test %>% 
  select(purchased) %>% 
  bind_cols(class_preds, prob_preds)

# Model Evaluation --------------------------------------------------------

leads_results %>% 
  conf_mat(truth = purchased, estimate = .pred_class) 

leads_results %>% 
  conf_mat(truth = purchased, estimate = .pred_class) %>% 
  summary()


# D. WORKFLOWS ------------------------------------------------------------

# Data Resampling ---------------------------------------------------------

leads_split <- initial_split(leads_df, 
                             prop = .75,
                             strata = purchased)

# Decision tree model specification ---------------------------------------

dt_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")


# Feature Engineering -----------------------------------------------------

leads_recipe <- 
  # Variable types and roles
  recipe(purchased ~ ., 
         data = leads_training) %>% 
  # Preprocessing steps
  step_corr(all_numeric(), threshold = .9) %>% 
  step_normalize(all_numeric()) %>% 
  step_dummy(all_nominal(), -all_outcomes())


# Workflow Specification --------------------------------------------------

leads_wkfl <- workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(leads_recipe)

leads_wkfl


# Model Fitting -----------------------------------------------------------

leads_wkfl_fit <- leads_wkfl %>% 
  last_fit(split = leads_split)

leads_wkfl_preds <- leads_wkfl_fit %>% 
  collect_predictions()

leads_wkfl_preds


# Evaluate ----------------------------------------------------------------

leads_metrics <- metric_set(accuracy, sens, spec, roc_auc)

leads_wkfl_preds %>% 
  leads_metrics(truth = purchased,
                estimate = .pred_class,
                .pred_yes)

# E. CROSS VALIDATION -----------------------------------------------------

# Creating CV folds -------------------------------------------------------

set.seed(124)
lead_folds <- vfold_cv(data = leads_training, 
                       v = 10, 
                       strata = purchased)

lead_folds


# Model training with CV --------------------------------------------------

leads_rs_fit <- leads_wkfl %>% 
  fit_resamples(resamples = lead_folds, 
                metrics = leads_metrics)

leads_rs_fit %>% 
  collect_metrics()

rs_metrics <- leads_rs_fit %>%
  collect_metrics(summarize = FALSE)

rs_metrics

rs_metrics %>%
  group_by(.metric) %>%
  summarize(min = min(.estimate),
            median = median(.estimate),
            max = max(.estimate),
            mean = mean(.estimate),
            sd = sd(.estimate))

# Models trained with fit_resamples() are
# not able to provide predictions on new data
# sources
# 
# Purpose of fit_resample()
# - Explore and compare the performance
#   profile of different model types
# - Select best performing model type and
#   focus on model

# F. HYPERPARAMETERS TUNING -----------------------------------------------

# Decision tree model
dt_tune_model <- decision_tree(cost_complexity = tune(), 
                               tree_depth      = tune(), 
                               min_n           = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_tune_model

# Update workflow with the decision tree model
leads_tune_wkfl <- leads_wkfl %>% 
  update_model(dt_tune_model)

leads_tune_wkfl

# Grid search
parameters(dt_tune_model)
extract_parameter_set_dials(dt_tune_model)

set.seed(124)
dt_grid <- grid_random(extract_parameter_set_dials(dt_tune_model),
                       size = 5)
dt_grid

# Hyperparameter tuning with cross validation
dt_tuning <- leads_tune_wkfl %>% 
  tune_grid(resamples = lead_folds, 
            grid = dt_grid, 
            metrics = leads_metrics, 
            control = control_grid(save_pred = TRUE))

dt_tuning

dt_tuning %>% 
  collect_metrics()

dt_tuning %>% 
  collect_metrics(summarize = FALSE)

dt_tuning %>% 
  collect_metrics(summarize = FALSE) %>% 
  filter(.metric == "roc_auc") %>% 
  group_by(id) %>% 
  summarise(min = min(.estimate),
            ci_low = mean(.estimate) - 1.96 * sd(.estimate),
            median = median(.estimate),
            mean = mean(.estimate),
            ci_high = mean(.estimate) + 1.96 * sd(.estimate),
            max = max(.estimate))

# Select best model
dt_tuning %>% 
  show_best(metric = "roc_auc", n = 5)

best_dt_model <- dt_tuning %>% 
  select_best(metric = "roc_auc", n = 5)

# Finalize workflow
final_leads_wkflw <- leads_tune_wkfl %>% 
  finalize_workflow(best_dt_model)

final_leads_wkflw

leads_final_fit <- final_leads_wkflw %>% 
  last_fit(split = leads_split)

leads_final_fit %>% 
  collect_metrics()

leads_final_fit %>% 
  collect_predictions()

# To use workflow with new data:

leads_final_fit$.workflow[[1]] %>% predict(leads_training)
leads_final_fit$.workflow[[1]] %>% predict(leads_training, type = "prob")

leads_final_fit$.workflow[[1]] %>% predict(leads_test)
leads_final_fit$.workflow[[1]] %>% predict(leads_test, type = "prob")


