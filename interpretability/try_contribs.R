library(tidyverse)
library(ranger)

data("Boston", package = "MASS")

m_lm <- lm(medv ~ ., Boston)
m_rf <- ranger(medv ~ ., Boston)

contribs_lm <- model.matrix(medv ~., Boston) %>% sweep(2, coef(m_lm), "*") %>% 
  as_tibble() %>% 
  mutate(y_hat = rowSums(.))

# My PDP ------------------------------------------------------------------


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

library(pdp)

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

pdp_1_contrib <- function(in_model, in_data, pred_var, pred_fun, grid_resolution = 20) {
  pd_values <- partial(
    in_model,
    train = in_data, 
    pred.var = pred_var,
    pred.fun = pred_fun,
    grid.resolution =  grid_resolution
  )
  
  avg_y_hat <- pred_fun(in_model, in_data)
  avg_x <- in_data %>% pull(!!sym(pred_var)) %>% mean()
  
  X_aux_0 <- in_data %>% mutate_all(~ 0)
  X_aux_1 <- X_aux_0  %>% mutate(!!sym(pred_var) := avg_x)
  
  # aux_y <- avg_y_hat - pred_fun(in_model, X_aux)
  
  dist <- avg_y_hat - (pred_fun(in_model, X_aux_1) - pred_fun(in_model, X_aux_0))
  out <- pd_values %>% 
    mutate(yhat = yhat - dist)
  
  return(out)
}

res <- pdp_1_contrib(m_lm, Boston, "lstat", pdp_pred)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), 
              color = "red", linetype = 2) +
  coord_cartesian(ylim = c(-20, 30))

res <- pdp_1_contrib(m_lm, Boston, "black", pdp_pred)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["black"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))


###
pdp_pred_rf <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata)$predictions))
  return(results)
}

res <- pdp_1_contrib(m_rf, Boston, "lstat", pdp_pred_rf)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res <- pdp_1_contrib(m_rf, Boston, "black", pdp_pred_rf)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["black"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res <- pdp_1_contrib(m_rf, Boston, "dis", pdp_pred_rf)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["dis"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res <- pdp_1_contrib(m_rf, Boston, "ptratio", pdp_pred_rf)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["ptratio"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))


pdp_contribs <- function(in_model, 
                         in_data, 
                         pred_vars, 
                         pred_fun, 
                         grid_resolution = 20) {
    
    # Lista con las contribuciones de cada variable en su grid
    contrib_grid <- vector(mode = "list", length = length(pred_vars))
    
    # Lista con las funciones para interpolar la contribucion de cada variable
    # en su grid
    contrib_funs <- vector(mode = "list", length = length(pred_vars))
    
    # Lista con las contribuciones de cada variable en in_data
    tbl_contribs <- vector(mode = "list", length = length(pred_vars))
    
    names(contrib_grid) <- names(contrib_funs) <- names(tbl_contribs) <- pred_vars
    
    for (i in seq_along(pred_vars)) {
      
      var_i <- pred_vars[i]
      
      # Contribucion de var_i en su grid
      contrib_grid[[var_i]] <- pdp_1_contrib(in_model, in_data,var_i, 
                                             pred_fun, grid_resolution)
      
      # Función para interpolar la contribución de var_i en su grid
      contrib_funs[[var_i]] <- approxfun(contrib_grid[[var_i]])
      
      # Tibble de 1 columna con la contribución de var_i en in_data
      contribs_i <- contrib_funs[[var_i]](in_data %>% pull(!!sym(var_i)))
      tbl_contribs[[var_i]] <- tibble(!!sym(var_i) := contribs_i)
    }
    
    tbl_contribs <- bind_cols(tbl_contribs) %>% 
      mutate(baseline = pred_fun(in_model, 
                                 in_data %>% mutate_all(~ 0)), .before = 1)
    
    out <- list(contribs     = tbl_contribs, 
                contrib_grid = contrib_grid, 
                contrib_funs = contrib_funs)
    
    return(out)
  }

res_all <- pdp_contribs(m_rf, Boston, 
                        Boston %>% select(-medv) %>% names(), 
                        pdp_pred_rf)

autoplot(res_all$contrib_grid$crim) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["crim"]), color = "red") +
  geom_point(data = tibble(x = seq(0:100), y = res_all$contrib_funs$crim(seq(0:100))),
             aes(x = x, y = y), alpha = .1) +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all$contrib_grid$tax) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["tax"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all$contrib_grid$zn) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["zn"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all$contrib_grid$indus) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["indus"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

autoplot(res_all$contrib_grid$chas) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["chas"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

probe <- res_all$contribs %>% mutate(y_hat = rowSums(.))
probe

rf_predictions <- predict(m_rf, Boston)$predictions
plot(rf_predictions, probe$y_hat)
abline(a= 0, b = 1)

probe %>% summarise_all(~ sum(.))
sum(rf_predictions)

contribs_lm %>% summarise_all(~ sum(.))

curve(res_all$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim))
curve(res_all$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn))
curve(res_all$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(res_all$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas))
curve(res_all$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox))
curve(res_all$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm))
curve(res_all$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(res_all$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis))
curve(res_all$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad))
curve(res_all$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax))
curve(res_all$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio))
curve(res_all$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black))
curve(res_all$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat))


### LM
lm_res_all <- pdp_contribs(m_lm, Boston, 
                        Boston %>% select(-medv) %>% names(), 
                        pdp_pred)

probe_lm <- lm_res_all$contribs %>% mutate(y_hat = rowSums(.))
probe_lm

lm_predictions <- predict(m_lm, Boston)
plot(lm_predictions, probe_lm$y_hat)
abline(a= 0, b = 1)

probe_lm %>% summarise_all(~ sum(.))
sum(lm_predictions)

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


###
curve(res_all$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim),
      ylim = c(-2, 0.3))
curve(lm_res_all$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn),
      ylim = c(-0.2, 4.5))
curve(lm_res_all$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(lm_res_all$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas),
      ylim = c(0, 3))
curve(lm_res_all$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox),
      ylim = c(-14, 0.8))
curve(lm_res_all$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm),
      ylim = c(-1, 35))
curve(lm_res_all$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(lm_res_all$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis),
      ylim = c(-20, 3))
curve(lm_res_all$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad),
      ylim = c(-0.2, 7))
curve(lm_res_all$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax),
      ylim = c(-9, -1))
curve(lm_res_all$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio),
      ylim = c(-22, 0.5))
curve(lm_res_all$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black),
      ylim = c(0, 4))
curve(lm_res_all$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black),
      add = TRUE, col = "red")

curve(res_all$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat),
      ylim = c(-20, 2))
curve(lm_res_all$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat),
      add = TRUE, col = "red")

#########
library(caret)

m_xgboost <- train(medv ~ ., Boston, 
                   method = "xgbTree",
                   trControl = trainControl(method = "none"))

res_all_xgboost <- pdp_contribs(m_xgboost, Boston, 
                                Boston %>% select(-medv) %>% names(), 
                                pdp_pred_xgboost)

res_all_xgboost$contribs

probe_xgboost <- res_all_xgboost$contribs %>% mutate(y_hat = rowSums(.))
probe_xgboost

xgboost_predictions <- predict(m_xgboost, Boston)
plot(xgboost_predictions, probe_xgboost$y_hat)
abline(a= 0, b = 1)

probe_xgboost %>% summarise_all(~ sum(.))
sum(xgboost_predictions)

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
