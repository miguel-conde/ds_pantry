library(tidyverse)

# SHAPR -------------------------------------------------------------------


library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
# model <- xgboost(
#   data = x_train,
#   label = y_train,
#   nround = 20,
#   verbose = FALSE
# )
f <- as.formula(paste(y_var, "~", paste(x_var, collapse = "+")))
model <- lm(f, data = Boston[-1:-6, ])

# Prepare the data for explanation
explainer <- shapr(x_train, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- shapr::explain(
  x_train,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

contribs_model <- model.matrix(f, Boston[-1:-6, ]) %>% sweep(2, coef(model), "*") %>% 
  as_tibble() %>% mutate(y_hat = rowSums(.))
contribs_model

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)
#>      none     lstat         rm       dis      indus
#> 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#> 2: 22.446 0.1671903 -0.7088405 0.9689007  0.3786871
#> 3: 22.446 5.9888016  5.5450861 0.5660136 -1.4304350
#> 4: 22.446 8.2142203  0.7507569 0.1893368  1.8298305
#> 5: 22.446 0.5059890  5.6875106 0.8432240  2.2471152
#> 6: 22.446 1.9929674 -3.6001959 0.8601984  3.1510530

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))


contribs_expalanation <- explanation$dt %>% 
  as_tibble() %>% 
  mutate(baseline = predict(model, x_train %>% as_tibble() %>% mutate_all(~ 0)),
         .after = 1) %>% 
  mutate(a_repartir = none - baseline, .after = baseline) %>%
  rowwise() %>%
  mutate(s = sum(c_across(4:ncol(.)))) %>%
  ungroup() %>%
  mutate_at(vars(4:ncol(.)), ~ . + . * a_repartir / s) %>%
  select(-a_repartir, -none, -s) %>% 
  mutate(yhat = rowSums(.))
contribs_expalanation


contribs_model <- 
  model.matrix(~ lstat + rm + dis + indus, as.data.frame(x_train)) %>% 
  sweep(2, coef(model), "*") %>% 
  as_tibble() %>% mutate(y_hat = rowSums(.))
contribs_model

explanation <- shapr::explain(
  x_train,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

phi <- tibble(
  beta_0    = predict(model, x_train %>% as_tibble() %>% mutate_all(~ 0)) %>% mean(),
  phi_lstat = predict(model, x_train %>% as_tibble %>% mutate(lstat = mean(lstat)) %>% mutate_at(vars(-lstat), ~ 0)) %>% mean(),
  phi_rm    = predict(model, x_train %>% as_tibble %>% mutate(rm = mean(rm)) %>% mutate_at(vars(-rm), ~ 0)) %>% mean(),
  phi_dis   = predict(model, x_train %>% as_tibble %>% mutate(dis = mean(dis)) %>% mutate_at(vars(-dis), ~ 0)) %>% mean(),
  phi_indus = predict(model, x_train %>% as_tibble %>% mutate(indus = mean(indus)) %>% mutate_at(vars(-indus), ~ 0)) %>% mean()
) %>% 
  mutate_at(vars(-beta_0), ~ . - beta_0)

explanation$dt %>% as.matrix() %>% sweep(2, as.numeric(phi), "+") %>% as_tibble() %>% 
  mutate(none = phi$beta_0) %>% 
  mutate(pred = rowSums(.)) %>% 
  head()

predict(model) %>% head
contribs_model %>% head()


# FASTSHAP ----------------------------------------------------------------
library(ranger)
library(fastshap)


get_shap_contribs <- function(in_data, shap_values, in_model, pred_wrapper, pred = FALSE) {
  
  delta_vector <- rep(NA, ncol(shap_values)+1)
  names(delta_vector) <- c("baseline", colnames(shap_values))
  
  delta_vector["baseline"] <- 
    pred_wrapper(in_model, in_data %>% mutate_all(~ 0)) %>% .[1]
  
  for (v in colnames(shap_values)) {
    
    aux <- in_data %>% 
      mutate_at(vars(-all_of(v)), ~ 0) %>% 
      mutate_at(all_of(v), ~ mean(.))
    
    delta_vector[v] <- pred_wrapper(in_model, aux[1,]) - delta_vector["baseline"]
  }
  
  shap_contribs <- shap_values %>% 
    as.data.frame() %>% 
    mutate(baseline = 0, .before = 1) %>% 
    sweep(2, delta_vector[c("baseline", colnames(shap_values))], "+") %>% 
    as_tibble()
  
  if (pred == TRUE) shap_contribs <- shap_contribs %>% 
    mutate(y_hat = rowSums(shap_contribs))
  
  return(shap_contribs)
}

get_shap_contribs_2 <- function(in_data, shap_values, in_model, pred_wrapper, pred = FALSE) {
  
  shap_values %>% 
    as_tibble() %>% 
    mutate(sum_phi = rowSums(shap_values)) %>% 
    mutate(phi_0 = pred_wrapper(in_model, in_data) - sum_phi,
           .before = 1) %>% 
    mutate(baseline = pred_wrapper(in_model, in_data %>% mutate_all(~ 0)),
           .after = phi_0) %>% 
    mutate(to_distribute = phi_0 - baseline,
           .after = baseline) %>% 
    mutate_at(vars(-phi_0, -baseline, -to_distribute, -sum_phi),
              ~ . + ./sum_phi * to_distribute) %>% 
    select(-phi_0, -to_distribute, -sum_phi)
}


data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

Boston_train <- Boston[-1:-6, c(x_var, y_var)]
Boston_test  <- Boston[1:6, x_var]

f <- as.formula(paste(y_var, "~", paste(x_var, collapse = "+")))
model_lm <- lm(f, data = Boston_train)
model_rf <- ranger(f, data = Boston_train)

## LM
contribs_lm <- model.matrix(f, Boston_train) %>% 
  sweep(2, coef(model_lm), "*") %>% 
  as_tibble() %>% 
  mutate(pred = rowSums(.))
head(contribs_lm)

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap_lm <- fastshap::explain(model_lm, 
                             X = subset(Boston_train, select = -medv), 
                             nsim = 1000, 
                             pred_wrapper = predict, 
                             adjust = TRUE)
shap_lm

shap_contribs_lm <- get_shap_contribs(Boston_train, shap_lm, model_lm, predict, pred = TRUE)
shap_contribs_lm
head(contribs_lm)

plot(Boston_train$lstat, shap_contribs_lm$lstat)

## RF

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap_rf <- fastshap::explain(model_rf, 
                             X = subset(Boston_train, select = -medv), 
                             nsim = 10, 
                             pred_wrapper = function(object, newdata) predict(object, newdata)$predictions, 
                             adjust = TRUE)
shap_rf

shap_contribs_rf <- get_shap_contribs(Boston_train, shap_rf, 
                                      model_rf,
                                      function(object, newdata) predict(object, newdata)$predictions, 
                                      pred = TRUE)
shap_contribs_rf

plot(Boston_train$lstat, shap_contribs_rf$lstat)
plot(Boston_train$rm, shap_contribs_rf$rm)
plot(Boston_train$dis, shap_contribs_rf$dis)
plot(Boston_train$indus, shap_contribs_rf$indus)

###
predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions +
  (predict(model_rf, tibble(lstat = mean(Boston_train$lstat), rm = 0, dis = 0, indus = 0))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions) +
  (predict(model_rf, tibble(lstat = 0, rm =  mean(Boston_train$rm), dis = 0, indus = 0))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions) +
  (predict(model_rf, tibble(lstat = 0, rm = 0, dis =  mean(Boston_train$dis), indus = 0))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions) +
  (predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus =  mean(Boston_train$indus)))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions)

predict(model_rf, tibble(lstat = mean(Boston_train$lstat), 
                         rm    = mean(Boston_train$rm), 
                         dis   = mean(Boston_train$dis), 
                         indus =  mean(Boston_train$indus)))$predictions

####
predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions +
  (predict(model_rf, tibble(lstat = Boston_train$lstat[1], rm = 0, dis = 0, indus = 0))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions) +
  (predict(model_rf, tibble(lstat = 0, rm =  Boston_train$rm[1], dis = 0, indus = 0))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions) +
  (predict(model_rf, tibble(lstat = 0, rm = 0, dis =  Boston_train$dis[1], indus = 0))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions) +
  (predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus =  Boston_train$indus[1]))$predictions -
     predict(model_rf, tibble(lstat = 0, rm = 0, dis = 0, indus = 0))$predictions)

predict(model_rf, Boston_train[1,])$predictions


####
probe <- list(lstat = data.frame(lstat = Boston_train$lstat, yhat = shap_contribs_rf$lstat) %>% 
                arrange(lstat))
kk <- fit_contrib_curve(probe$lstat, tgt_var = "lstat")
kk$curve_type
plot(probe$lstat$lstat, kk$fit_fun(probe$lstat$lstat, kk$pars), type = "l", lwd = 2, col = "red")
lines(probe$lstat, add = TRUE, type = "p")
