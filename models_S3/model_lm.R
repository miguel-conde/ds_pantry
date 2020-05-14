## Define model lm class

#' Model lm class
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
"model_lm" <-
  function(x, ...){
    UseMethod("model_lm")
  }

# CONSTRUCTORS ------------------------------------------------------------

new_model_lm.default <- function(f, base, ...) {
  # browser()
  out <- model(f, base)
  
  assign("model_type", "lm", out$get_env())
  
  class(out) <- append("model_lm", class(out))
  
  ###
  # new_this <- get("this", out$get_env())
  # class(new_this) <- class(out)
  # new_this$fit <- function() return(fit(get("this", out$get_env())))
  # out <- assign("this", new_this, out$get_env(), inherits = TRUE)
  ###
  
  return(out)
}

validate_model_lm.default <- function(f, base, ...) {
  
  out <- new_model_lm.default(f, base)
  
  return(out)
}

model_lm.default <- function(f = NULL, base = NULL, ...) {
  
  out <- validate_model_lm.default(f, base)
  
  return(out)
}

# METHODS -----------------------------------------------------------------

# make_copy ---------------------------------------------------------------

make_copy.model_lm <- function(object, ...)
{
  message("In model_lm and making a copy")
  
  NextMethod(object)
}


# fit ---------------------------------------------------------------------

fit.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  if(!inherits(object$get_model_formula(), "formula")) {
    stop("Need a formula.")
  }
  if(!inherits(object$get_model_base(), "data.frame")) {
    stop("Need a data.frame as base.")
  }
  
  aux_lm <- lm(formula = object$get_model_formula(), 
               data = object$get_model_base(),
               ...)
  # assign("fitted_object", aux_lm, object$get_env(), inherits = TRUE)
  assign("fitted_object", aux_lm, object$get_env())
  
  invisible(object)
}


# summary -----------------------------------------------------------------

summary.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  summary(object$get_fitted_model())
}



# fitted ------------------------------------------------------------------

fitted.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  fitted(object$get_fitted_model())
}

# residuals ---------------------------------------------------------------
residuals.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  residuals(object$get_fitted_model())
}

# plot --------------------------------------------------------------------
plot.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  plot(object$get_fitted_model())
}

# coef --------------------------------------------------------------------
coef.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  coef(object$get_fitted_model())
}


# predict -----------------------------------------------------------------

predict.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  out <- predict(object$get_fitted_model(), ...)
  
  return(out)
}

# confint -----------------------------------------------------------------


# contributions -----------------------------------------------------------
contributions.model_lm <- function(object, ...) {
  
  if (!inherits(object, "model_lm")) {
    warning("This is not a model_lm objct. Passing to next method.")
    NextMethod(object)
  }
  
  if (!object$is_fitted()) {
    stop("This model is not fitted yet.")
    NextMethod(object)
  }
  
  ### TO DO
  
  return(out)
}