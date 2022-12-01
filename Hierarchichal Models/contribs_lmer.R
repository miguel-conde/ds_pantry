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

calc_contribs <- function(m_h_lm, base_emisiones) {
  
  str_f <- paste0(format(summary(m_h_lm)$call$formula), collapse = "")
  
  str_response <- str_f %>% str_remove("~.*$") %>% str_trim()
  
  fe_str_f <- str_f %>% 
    str_remove("^.*~") %>% 
    str_remove("\\(.*$") %>% 
    str_remove("[:space:]*\\+[:space:]*$") %>% 
    str_squish()
  
  
  re_str_f <- str_f %>% 
    str_extract("\\(.*\\)") %>% 
    str_split("\\+[:digit:]*\\(") %>% 
    map(~ .x %>% str_remove("\\|.*\\)") %>% 
          str_remove("\\(|\\)") %>% 
          str_squish)
  
  contrib_f <- 
    as.formula(paste(str_response, "~ ", 
                     paste(unique(c(unlist(str_split(fe_str_f, "\\+")),
                                    unlist(str_split(re_str_f, "\\+")))),
                           collapse = "+")))
  
  mm <- model.matrix(contrib_f, base_emisiones)
  mm <- mm[, order(colnames(mm))]
  
  progs <- unique(base_emisiones$name_edit)
  progs <- progs[order(progs)]
  contribs_lst <- vector(mode = "list", length = length(progs))
  names(contribs_lst) <- progs
  
  for (prog in progs) {
    
    idx_prog <- which(base_emisiones$name_edit == prog)
    
    m_matrix_prog <- mm[idx_prog, , drop = FALSE]
    coefs_prog <- coef(m_h_lm)$name_edit[prog, colnames(m_matrix_prog)]
    coefs_prog <- coefs_prog[, order(names(coefs_prog))] %>% 
      unlist()
    
    contribs_lst[[prog]] <- base_emisiones %>% 
      filter(name_edit == prog) %>% 
      select(date, name_edit, class_ytb) %>% 
      bind_cols(m_matrix_prog %>% sweep(2, coefs_prog, "*")) %>% 
      as_tibble()
    
  }
  
  contribs <- bind_rows(contribs_lst)
  
  return(contribs)
  
}