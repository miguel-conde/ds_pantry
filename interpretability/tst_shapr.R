library(tidyverse)

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
# p <- coef(model)['(Intercept)']

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

contribs_model <- model.matrix(f, Boston[1:6, ]) %>% sweep(2, coef(model), "*") %>% 
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
  mutate(baseline = predict(model, x_test %>% as_tibble() %>% mutate_all(~ 0)),
         .after = 1) %>% 
  mutate(a_repartir = none - baseline, .after = baseline) %>%
  rowwise() %>%
  mutate(s = sum(c_across(4:ncol(.)))) %>%
  ungroup() %>%
  mutate_at(vars(4:ncol(.)), ~ . + . * a_repartir / s) %>%
  select(-a_repartir, -none, -s) %>% 
  mutate(yhat = rowSums(.))
contribs_expalanation
