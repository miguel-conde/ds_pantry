library(pdp)
library(dplyr)
library(ranger)

pdp_pred_lm <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata)))
  return(results)
}

pdp_pred_rf <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata)$predictions))
  return(results)
}

pdp_pred_xgboost <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata)))
  return(results)
}


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
