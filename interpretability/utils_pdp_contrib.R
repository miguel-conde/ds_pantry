#
# Idea sacada de : https://bradleyboehmke.github.io/HOML/iml.html


# LIBRARIES and SOURCES ---------------------------------------------------

library(pdp)
library(dplyr)
library(ranger)
library(ggplot2)
library(patchwork)


# AVERAGE PREDICTIONS FOR PDP ---------------------------------------------


pdp_pred_lm <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata)))
  return(results)
}

pdp_pred_glm <- function(object, newdata)  {
  results <- mean(as.vector(predict(object, newdata, type = "response")))
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

pdp_pred <- function(object, newdata) {
  results <- mean(as.vector(FUN_PRED(object, newdata)))
  return(results)
}


# CONTRIBUTIONS -----------------------------------------------------------


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
  
  # La distancia es la media de:
  #          fitted - fitted cuando todos valen 0 menos el de interés
  dist <- avg_y_hat - (pred_fun(in_model, X_aux_1) - pred_fun(in_model, X_aux_0))
  
  # aux_class <- class(pd_values)
  # out <- pd_values %>%
  #   as_tibble() %>%
  #   mutate(yhat = yhat - dist) %>% 
  #   as.data.frame()
  # class(out) <- aux_class
  
  out <- pd_values
  out$yhat <- out$yhat - dist
  
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
  
  # Regularización para que las contribuciones sumen lo mismo que las 
  # predicciones
  k <- pred_fun(in_model, in_data) * nrow(in_data) / sum(tbl_contribs)
  
  tbl_contribs <- (tbl_contribs * k) %>% as_tibble()
  
  out <- list(contribs     = tbl_contribs, 
              contrib_grid = contrib_grid, 
              contrib_funs = contrib_funs)
  
  return(out)
}


# Plots -------------------------------------------------------------------


ggplot_1_contrib <- function(res_all, tgt_var, 
                             title = NULL,
                             x_units = "", y_units = "", 
                             n_x = 100,
                             the_theme = theme_bw) {
  
  enquo_tgt_var <- enquo(tgt_var)
  name_tgt_var  <- quo_name(enquo_tgt_var)
  
  # the_x_data    <- pull(in_data, !!enquo_tgt_var)
  the_x_data    <- pull(res_all$contrib_grid[[name_tgt_var]], !!enquo_tgt_var)
  
  data_tbl <- tibble(x = seq(from       = min(the_x_data), 
                             to         = max(the_x_data),
                             length.out = n_x),
                     y = res_all$contrib_funs[[name_tgt_var]](x))
  
  the_title <-  ifelse(is.null(title), 
                       paste(name_tgt_var, ""), 
                       title)
  x_lab <- paste(name_tgt_var,   x_units)
  y_lab <- paste("", y_units)
  
  p <- ggplot(data = data_tbl, mapping = aes(x, y)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(title = the_title, x = x_lab, y = y_lab) +
    the_theme()
  
  return(p)
  
}

ggplot_contribs <- function(res_all, tgt_vars = NULL, y_units = "", ...) {
  
  tgt_vars = if(is.null(tgt_vars)) {
    names(res_all$contrib_grid)
  } else {
    tgt_vars
  }
  
  p_list <- vector(mode = "list", length = length(tgt_vars))
  names(p_list) <- tgt_vars
  for (i in seq_along(p_list)) {
    p_list[[i]] <- ggplot_1_contrib(res_all, !!sym(tgt_vars[[i]]),
                                    y_units = y_units, ...)
  }
  
  out <- p_list[[1]]
  for (i in 2:length(p_list)) out <- out + p_list[[i]]
  
  return(out)
}


# ROIs --------------------------------------------------------------------

pdp_1_roi <- function(in_contribs, in_model, in_data, pred_var, pred_fun, delta = 0.01) {
  
  k <- 1 + delta
  
  out <- pdp_1_contrib(in_model, in_data, 
                       pred_var = pred_var, pred_fun = pred_fun) %>% 
    as_tibble() %>%  
    mutate(pred_var_plus = k * !!sym(pred_var),
           y_hat_plus1 = in_contribs$contrib_funs[[pred_var]](pred_var_plus)) %>% 
    drop_na() %>% 
    mutate(roi = (y_hat_plus1 - yhat) / delta / !!sym(pred_var)) %>% 
    mutate(roi = ifelse(is.na(roi), 0, roi)) %>% 
    mutate(roi = ifelse(is.infinite(roi), 0, roi)) %>% 
    select(-pred_var_plus, -y_hat_plus1, -yhat)
  
  return(out)
}

pdp_rois <- function(in_contribs, 
                     in_model, 
                     in_data, 
                     pred_vars, 
                     pred_fun, 
                     delta = .01) {
  
  # Lista con los ROIs de cada variable en su grid
  rois_grid <- vector(mode = "list", length = length(pred_vars))
  
  # Lista con las funciones para interpolar los ROIs de cada variable
  # en su grid
  rois_funs <- vector(mode = "list", length = length(pred_vars))
  
  names(rois_grid) <- names(rois_funs) <- pred_vars
  
  for (i in seq_along(pred_vars)) {
    
    var_i <- pred_vars[i]
    
    # Contribucion de var_i en su grid
    rois_grid[[var_i]] <- pdp_1_roi(in_contribs, in_model, in_data, var_i, 
                                    pred_fun, delta)
    
    # Función para interpolar la contribución de var_i en su grid
    rois_funs[[var_i]] <- approxfun(rois_grid[[var_i]])
  }
  
  out <- list(rois_grid = rois_grid, 
              rois_funs = rois_funs)
  
  return(out)
}

# Plots -------------------------------------------------------------------

ggplot_1_roi <- function(rois_all, tgt_var, 
                         title = NULL,
                         x_units = "", y_units = "", 
                         n_x = 100,
                         the_theme = theme_bw) {
  
  enquo_tgt_var <- enquo(tgt_var)
  name_tgt_var  <- quo_name(enquo_tgt_var)
  
  # the_x_data    <- pull(in_data, !!enquo_tgt_var)
  the_x_data    <- pull(rois_all$rois_grid[[name_tgt_var]], !!enquo_tgt_var)
  
  data_tbl <- tibble(x = seq(from       = min(the_x_data), 
                             to         = max(the_x_data), 
                             length.out = n_x),
                     y = rois_all$rois_funs[[name_tgt_var]](x))
  
  the_title <-  ifelse(is.null(title), 
                       paste(name_tgt_var, ""), 
                       title)
  x_lab <- paste(name_tgt_var,   x_units)
  y_lab <- paste("", y_units)
  
  p <- ggplot(data = data_tbl, mapping = aes(x, y)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(title = the_title, x = x_lab, y = y_lab) +
    the_theme()
  
  return(p)
  
}

ggplot_rois <- function(rois_all, tgt_vars = NULL, y_units = "", ...) {
  
  tgt_vars = if(is.null(tgt_vars)) {
    names(rois_all$rois_grid)
  } else {
    tgt_vars
  }
  
  p_list <- vector(mode = "list", length = length(tgt_vars))
  names(p_list) <- tgt_vars
  for (i in seq_along(p_list)) {
    p_list[[i]] <- ggplot_1_roi(rois_all, !!sym(tgt_vars[[i]]),
                                y_units = y_units, ...)
  }
  
  out <- p_list[[1]]
  for (i in 2:length(p_list)) out <- out + p_list[[i]]
  
  return(out)
}
