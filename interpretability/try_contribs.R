library(tidyverse)
library(ranger)
library(caret)

library(pdp)

source("interpretability/utils_pdp_contrib.R")

# DATA --------------------------------------------------------------------

data("Boston", package = "MASS")


# MODELS ------------------------------------------------------------------


# Linear model ------------------------------------------------------------

m_lm <- lm(medv ~ ., Boston)

contribs_lm <- model.matrix(medv ~., Boston) %>% sweep(2, coef(m_lm), "*") %>% 
  as_tibble() %>% 
  mutate(y_hat = rowSums(.))

predictions_lm <- fitted(m_lm)
Metrics::rmse(Boston$medv, predictions_lm)
plot(Boston$medv, fitted(m_lm), xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1)

# Random Forest -----------------------------------------------------------

m_rf <- ranger(medv ~ ., Boston)

predictions_rf <- predict(m_rf, Boston)$predictions
Metrics::rmse(Boston$medv, predictions_rf)

plot(Boston$medv, predicitons_rf, xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1)

# Caret - XgBoost ---------------------------------------------------------

the_grid <- data.frame(nrounds = 500,
                       max_depth = 6,
                       eta = 0.3,
                       gamma = 0, # ??
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 1)

m_xgboost <- train(medv ~ ., Boston, 
                   method = "xgbTree",
                   tuneGrid = the_grid,
                   trControl = trainControl(method = "none"))

predictions_xgboost <- predict(m_xgboost$finalModel, 
                               Boston %>% select(-medv) %>% as.matrix()) 
Metrics::rmse(Boston$medv, predictions_xgboost)

plot(Boston$medv, predictions_xgboost, xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1)

# MY PDP ------------------------------------------------------------------

pdp_var <- function(in_data_features, tgt_var, n_grid = 20) {
  
  enquo_tgt_var <- enquo(tgt_var)
  browser()
  x <- in_data_features %>% pull(!!enquo_tgt_var)
  x_min <- min(x)
  x_max <- max(x)
  
  x_grid <- seq(x_min, x_max, length.out = n_grid)
  
  out <- rep(NA, n_grid)
  for (i in seq_along(x_grid)) {
    
    X_i <- in_data_features %>% 
      mutate(!!enquo_tgt_var := x_grid[i])
    
    x_m_lm <- lm(medv ~ . - 1, X_i)
    
    out[i] <- mean(fitted(x_m_lm))
  }
  
  return(out)
  
}


pdp_var(Boston, lstat)

# PDP ---------------------------------------------------------------------

# https://bradleyboehmke.github.io/HOML/iml.html

# Custom prediction function wrapper
pdp_pred <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata)))
  return(results)
}

# Compute partial dependence values
pd_values <- partial(
  m_lm,
  train = Boston, 
  pred.var = "lstat",
  pred.fun = pdp_pred,
  grid.resolution = 20
)
head(pd_values)  # take a peak

autoplot(pd_values) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

# Construct c-ICE curves
partial(
  m_lm,
  train = Boston, 
  pred.var = "lstat",
  pred.fun = pdp_pred,
  grid.resolution = 20,
  plot = TRUE,
  center = TRUE,
  plot.engine = "ggplot2"
)  +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

# La distancia es fitted - fitted cuando todos valen 0 menos el de interés
mean(fitted(m_lm) - Boston$lstat * coef(m_lm)["lstat"])
mean(fitted(m_lm)) - mean(Boston$lstat) * coef(m_lm)["lstat"]

# UTILS -------------------------------------------------------------------


# Linear Model ------------------------------------------------------------

res <- pdp_1_contrib(m_lm, Boston, "lstat", pdp_pred_lm)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), 
              color = "red", linetype = 2) +
  coord_cartesian(ylim = c(-20, 30))

res <- pdp_1_contrib(m_lm, Boston, "black", pdp_pred_lm)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["black"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))


# Random Forest -----------------------------------------------------------

res_rf <- pdp_1_contrib(m_rf, Boston, "lstat", pdp_pred_rf)

autoplot(res_rf) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res_rf <- pdp_1_contrib(m_rf, Boston, "black", pdp_pred_rf)

autoplot(res_rf) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["black"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res_rf <- pdp_1_contrib(m_rf, Boston, "dis", pdp_pred_rf)

autoplot(res_rf) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["dis"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res_rf <- pdp_1_contrib(m_rf, Boston, "ptratio", pdp_pred_rf)

autoplot(res_rf) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["ptratio"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))


##

res_all_rf <- pdp_contribs(m_rf, Boston, 
                           Boston %>% select(-medv) %>% names(), 
                           pdp_pred_rf)

autoplot(res_all_rf$contrib_grid$crim) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["crim"]), color = "red") +
  geom_point(data = tibble(x = seq(0:100), y = res_all$contrib_funs$crim(seq(0:100))),
             aes(x = x, y = y), alpha = .1) +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all_rf$contrib_grid$tax) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["tax"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all_rf$contrib_grid$zn) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["zn"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all_rf$contrib_grid$indus) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["indus"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all_rf$contrib_grid$chas) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["chas"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

probe_rf <- res_all_rf$contribs %>% mutate(y_hat = rowSums(.))
probe_rf

plot(predicitons_rf, probe$y_hat)
abline(a= 0, b = 1)

probe_rf %>% summarise_all(~ sum(.))
sum(predicitons_rf)

contribs_lm %>% summarise_all(~ sum(.))

curve(res_all_rf$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim))
curve(res_all_rf$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn))
curve(res_all_rf$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(res_all_rf$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas))
curve(res_all_rf$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox))
curve(res_all_rf$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm))
curve(res_all_rf$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(res_all_rf$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis))
curve(res_all_rf$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad))
curve(res_all_rf$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax))
curve(res_all_rf$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio))
curve(res_all_rf$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black))
curve(res_all_rf$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat))


### LM
lm_res_all <- pdp_contribs(m_lm, Boston, 
                        Boston %>% select(-medv) %>% names(), 
                        pdp_pred_lm)

probe_lm <- lm_res_all$contribs %>% mutate(y_hat = rowSums(.))
probe_lm

plot(predictions_lm, probe_lm$y_hat)
abline(a= 0, b = 1)

probe_lm %>% summarise_all(~ sum(.))
sum(predictions_lm)

contribs_lm %>% summarise_all(~ sum(.))

curve(lm_res_all$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim))
curve(lm_res_all$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn))
curve(lm_res_all$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(lm_res_all$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas))
curve(lm_res_all$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox))
curve(lm_res_all$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm))
curve(lm_res_all$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(lm_res_all$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis))
curve(lm_res_all$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad))
curve(lm_res_all$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax))
curve(lm_res_all$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio))
curve(lm_res_all$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black))
curve(lm_res_all$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat))


# LM vs RF ----------------------------------------------------------------
curve(res_all_rf$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim),
      ylim = c(-2, 1.5))
curve(lm_res_all$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn),
      ylim = c(-0.2, 4.5))
curve(lm_res_all$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(lm_res_all$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas),
      ylim = c(0, 3))
curve(lm_res_all$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox),
      ylim = c(-14, 0.8))
curve(lm_res_all$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm),
      ylim = c(-1, 35))
curve(lm_res_all$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(lm_res_all$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis),
      ylim = c(-20, 3))
curve(lm_res_all$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad),
      ylim = c(-0.2, 7))
curve(lm_res_all$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax),
      ylim = c(-9, -1))
curve(lm_res_all$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio),
      ylim = c(-22, 0.5))
curve(lm_res_all$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black),
      ylim = c(0, 4))
curve(lm_res_all$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black),
      add = TRUE, col = "red")

curve(res_all_rf$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat),
      ylim = c(-20, 2))
curve(lm_res_all$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat),
      add = TRUE, col = "red")

# XgBoost -----------------------------------------------------------------

res_all_xgboost <- pdp_contribs(m_xgboost, Boston, 
                                Boston %>% select(-medv) %>% names(), 
                                pdp_pred_xgboost)

res_all_xgboost$contribs

probe_xgboost <- res_all_xgboost$contribs %>% mutate(y_hat = rowSums(.))
probe_xgboost

plot(predictions_xgboost, probe_xgboost$y_hat)
abline(a= 0, b = 1)

probe_xgboost %>% summarise_all(~ sum(.))
sum(predictions_xgboost)

contribs_lm %>% summarise_all(~ sum(.))

curve(res_all_xgboost$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim))
curve(res_all_xgboost$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn))
curve(res_all_xgboost$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(res_all_xgboost$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas))
curve(res_all_xgboost$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox))
curve(res_all_xgboost$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm))
curve(res_all_xgboost$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(res_all_xgboost$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis))
curve(res_all_xgboost$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad))
curve(res_all_xgboost$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax))
curve(res_all_xgboost$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio))
curve(res_all_xgboost$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black))
curve(res_all_xgboost$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat))


# Plots -------------------------------------------------------------------

p1 <- ggplot_1_contrib(res_all_rf, Boston, "crim", 
                 x_units = "\n(per capita crime rate by town)",
                 y_units = "\n(to the median value of owner-occupied homes in $1000s)")
p1

p2 <- ggplot_1_contrib(res_all_rf, Boston, "lstat", 
                 x_units = "\n(lower status of the population (percent))",
                 y_units = "\n(to the median value of owner-occupied homes in $1000s)")
p2

p3 <- ggplot_1_contrib(res_all_rf, Boston, "rm", 
                       x_units = "\n(average number of rooms per dwelling)",
                       y_units = "\n(to the median value of owner-occupied homes in $1000s)")
p3

p <- p1 + p2 + p3 + p2 + p1 + p3 + p1 + p2 + p3 + p2 + p1 + p3 + p1
p
