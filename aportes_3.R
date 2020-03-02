library(iml)
library(tidyverse)

est_aportes <- function(model = NULL, dataset, y = NULL,
                        predict.fun = NULL, type = NULL,
                        vars_2_est, ...) {
  
  mod = Predictor$new(model, data = dataset, y = y,
                      predict.fun = predict.fun, type = type)
  
  effect = FeatureEffect$new(mod, vars_2_est[1], ...)
  
  out <- lapply(vars_2_est, function(x) {
    effect$set.feature(x)
    effect$results
  }) 
  
  names(out) <- vars_2_est
  
  out <- list(est_contrib = out,
              mod_predictor = mod)
  
  out
}

x_aporte <- function(x_var, x_value, ale_aportes) {
  # browser()
  aux <- ale_aportes$est_contrib[[x_var]]
  
  idx <- which(aux[[x_var]] == x_value)
  
  if (length(idx == 1)) return(aux[[1]][idx])
  
  if(x_value > max(aux[[x_var]]) | x_value < min(aux[[x_var]])) {
    stop("Out of range")
  }
  
  idx_slot <- c(max(which(aux[[x_var]] <= x_value)), 
                min(which(aux[[x_var]] > x_value)))
  x_range <- aux[[x_var]][idx_slot]
  aporte_range <- aux[[1]][idx_slot]
  
  prop <- (x_value - x_range[1]) / (x_range[2] - x_range[1])
  
  out <- prop * (aporte_range[2] - aporte_range[1]) + aporte_range[1]
  
  out
}

x_aportes <- function(xs, x_var, ale_aportes) {
  sapply(xs, function(x) {
    x_aporte(x_var, x, ale_aportes)
  })
}

est_model_contrib <- function(mod_fit, tb_data, vars_2_est, ...) {
  
  ale_aportes <- est_aportes(mod_fit,
                             data = tb_data,
                             vars_2_est = vars_2_est,
                             ...)
  
  out <- sapply(vars_2_est, function(x_var) {
    x_aportes(tb_data %>% pull(x_var), x_var, ale_aportes)
  })
  
  aux_sum_contrib <- rowSums(out)
  aux_mod_fit_pred <- ale_aportes$mod_predictor$predict(tb_data) %>% 
    pull(1)
  aux <- aux_mod_fit_pred / aux_sum_contrib
  
  out <- out %>% apply(2, function(x) {
    x * aux
  })
  
  out
}