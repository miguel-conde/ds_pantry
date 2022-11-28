library(tidyverse)
library(lme4)

get_mlmer_contribs_old <- function(in_model, new_data = NULL, pred = FALSE, ...) {
  
  # in_model
  # a fitted model object
  #
  # new_data
  # data frame for which to evaluate predictions.
  #
  # pred
  # (logical) add a column with the sum of the contributions (i.e., the 
  # prediction)?
  #
  # ...
  #    re.form	
  #    (formula, NULL, or NA) specify which random effects to condition on when 
  #    predicting. If NULL, include all random effects; if NA or ~0, include no 
  #    random effects.
  #   
  #    random.only	
  #    (logical) ignore fixed effects, making predictions only using random 
  #    effects?
  
  if (is.null(new_data)) new_data <- in_model@frame
  
  model_vars <- coef(in_model) %>% 
    lapply(names) %>% 
    unlist() %>% 
    str_split(":") %>% 
    unlist() %>% 
    unique()
  
  has_intcpt <- "(Intercept)" %in% model_vars
  
  out_length <- length(model_vars) + ifelse(has_intcpt == TRUE, -1, 0)
  out <- vector(mode = "list", length = out_length)
  names(out) <- setdiff(model_vars, "(Intercept)")
  orig_pred <- predict(in_model, newdata = new_data, ...)
  for ( v in names(out)) {
    
    aux <- new_data %>% mutate(!!sym(v) := 0)
    out[[v]] = orig_pred - predict(in_model, newdata = aux, ...)
    
  }
  
  out <- bind_cols(out)
  
  if (has_intcpt == TRUE) {
    
    aux <- new_data %>% 
      mutate_if(is.numeric, ~ 0) 
    
    out <- out %>% mutate(`(Intercept)` = predict(in_model, newdata = aux, ...),
                          .before = 1)
  }
  
  model_vars <- model_vars %>% intersect(c(names(new_data), "(Intercept)"))
  out <- out %>% select(all_of(model_vars))
  
  if ("(Intercept)" %in% names(out)) {
    out <- out %>% relocate(`(Intercept)`, .before = 1)
  }
  
  if (pred == TRUE) {
    
    out <- out %>% 
      rowwise() %>% 
      mutate(pred = sum(c_across())) %>% 
      ungroup()
  }
  
  return(out)
}

get_mlmer_contribs <- function(in_model, new_data = NULL, pred = FALSE, ...) {
  
  # in_model
  # a fitted model object
  #
  # new_data
  # data frame for which to evaluate predictions.
  #
  # pred
  # (logical) add a column with the sum of the contributions (i.e., the 
  # prediction)?
  #
  # ...
  #    re.form	
  #    (formula, NULL, or NA) specify which random effects to condition on when 
  #    predicting. If NULL, include all random effects; if NA or ~0, include no 
  #    random effects.
  #   
  #    random.only	
  #    (logical) ignore fixed effects, making predictions only using random 
  #    effects?
  
  if (is.null(new_data)) new_data <- in_model@frame
  
  model_vars <- coef(in_model) %>% 
    lapply(names) %>% 
    unlist() %>% 
    str_split(":") %>% 
    unlist() %>% 
    unique()
  
  has_intcpt <- "(Intercept)" %in% model_vars
  names_vars <- setdiff(model_vars, "(Intercept)")
  
  if (has_intcpt == TRUE) {
    
    aux <- new_data %>% mutate_if(is.numeric, ~ 0) 
    out <- tibble(`(Intercept)` = predict(in_model, newdata = aux, ...))
    
    if (length(names_vars) > 0) {
      
      orig_pred <- predict(in_model, newdata = new_data, ...)
      for (v in names_vars) {
        aux <- new_data %>% mutate(!!sym(v) := 0)
        out <- out %>% mutate(!!sym(v) := orig_pred - predict(in_model, newdata = aux, ...))
      }
    }
  } else {
    
    out_length <- length(names_vars) 
    out <- vector(mode = "list", length = out_length)
    orig_pred <- predict(in_model, newdata = new_data, ...)
    for (v in names_vars) {
      
      aux <- new_data %>% mutate(!!sym(v) := 0)
      out[[v]] = orig_pred - predict(in_model, newdata = aux, ...)
    }
    
    out <- bind_cols(out)
  }
  
  model_vars <- model_vars %>% intersect(c(names(new_data), "(Intercept)"))
  out <- out %>% select(all_of(model_vars))
  
  if (pred == TRUE) {
    
    out <- out %>% 
      rowwise() %>% 
      mutate(pred = sum(c_across())) %>% 
      ungroup()
  }
  
  return(out)
}