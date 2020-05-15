## Define model default class

#' Models default class
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
"model" <-
  function(x, ...){
    UseMethod("model")
  }


# CONSTRUCTORS ------------------------------------------------------------

new_model.default <- function(f, base, ...) {
  
  ## Basic checks
  
  stopifnot(is.null(f) | inherits(f, "formula"))
  stopifnot(is.null(base) | inherits(base, "data.frame"))
  
  ## Attributes
  
  # Get the environment for this instance of the function.
  this_env <- environment()
  
  model_formula <- f
  model_base <- base
  
  fitted_object <- NULL
  model_type <- NA
  
  
  # Create the list used to represent an object for this class
  this <- structure(
    list(
      ## Accesors
      
      # For Read-Only attributes
      get_env = function() {
        return(get("this_env", this_env))
      },
      get_fitted_model = function() {
        return(get("fitted_object", this_env))
      },
      is_fitted = function() {
        return(!is.null(get("fitted_object", this_env)))
      },
      get_model_type = function() {
        return(get("model_type", this_env))
      },
      
      # For R/W atrributes
      get_model_formula = function() {
        return(get("model_formula", this_env))
      },
      set_model_formula = function(f) {
        stopifnot(is.null(f) | inherits(f, "formula"))
        return(assign("model_formula", f, this_env)) 
      },
      get_model_base = function() {
        return(get("model_base", this_env))
      },
      set_model_base = function(base) {
        stopifnot(is.null(base) | inherits(base, "data.frame"))
        return(assign("model_base", base, this_env)) 
      },
      
      ## Methods
      copy = function() {
        return(make_copy(this))
      },
      fit = function(...) {
        return(fit(this, ...))
      },
      summary = function(...) {
        return(summary(this, ...))
      },
      fitted = function(...) {
        return(fitted(this, ...))
      },
      residuals = function(...) {
        return(residuals(this, ...))
      },
      coef = function(...) {
        return(coef(this, ...))
      },
      plot = function(...) {
        return(plot(this, ...))
      },
      predict = function(...) {
        return(predict(this, ...))
      }#,
      # confint = function(...) {
      #   return(confint(this, ...))
      # },
      # contributions = function(...) {
      #   return(contributions(this, ...))
      # }
    ),
    class = c("model", "list")
  )
  
  return(this)
}

validate_model.default <- function(f, base, ...) {
  
  ## In-deep checks
  
  out <- new_model.default(f, base)
  
  return(out)
}

model.default <- function(f = NULL, base = NULL, ...) {
  
  out <- validate_model.default(f, base)
  
  return(out)
}


# METHODS -----------------------------------------------------------------

# make_copy ---------------------------------------------------------------

#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
make_copy <- function(object, ...)
{
  message("Calling the base make_copy function")
  UseMethod("make_copy", object)
  message("Note this is not executed!")
}

make_copy.default <- function(object, ...)
{
  stop("I do not know how to make a copy of this object.")
  return(object)
}

make_copy.model <- function(object, ...)
{
  message("In model and making a copy")
  print(class(object))
  new_object <- model(
    f = object$get_model_formula(),
    base = object$get_model_base())
  
  assign("model_type", object$get_model_type(), new_object$get_env())
  assign("fitted_object", object$get_fitted_model(), new_object$get_env())
  
  class(new_object) <- class(object)
  
  return(new_object)
}

# fit ---------------------------------------------------------------------


#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
"fit" <-
  function(x, ...){
    UseMethod("fit")
  }

fit.default <- function(object, ...) {
  
  warning("I do not know how to fit this object. Passing to next method.")
  return(object)
}

# summary -----------------------------------------------------------------

#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
summary.model <- function(object, ...)
{
  warning("I do not know how to make a summary of this object. Passing to next method.")
  NextMethod(object)
}


# fitted ------------------------------------------------------------------
fitted.model <- function(object, ...)
{
  warning("I do not know how to get the fitted values of this object. Passing to next method.")
  NextMethod(object)
}

# residuals ---------------------------------------------------------------
residuals.model <- function(object, ...)
{
  warning("I do not know how to get the residuals of this object. Passing to next method.")
  NextMethod(object)
}


# plot --------------------------------------------------------------------
plot.model <- function(object, ...)
{
  warning("I do not know how to plot this object. Passing to next method.")
  NextMethod(object)
}


# coef --------------------------------------------------------------------
fitted.model <- function(object, ...)
{
  warning("I do not know how to get the coefficients of this object. Passing to next method.")
  NextMethod(object)
}


# predict -----------------------------------------------------------------
predict.model <- function(object, ...)
{
  warning("I do not know how to predict with this object. Passing to next method.")
  NextMethod(object)
}


# confint -----------------------------------------------------------------


# contributions -----------------------------------------------------------

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
"contributions" <-
  function(x, ...){
    UseMethod("fit")
  }

contributions.default <- function(object, ...) {
  
  warning("I do not know how to extract contributions from this object. Passing to next method.")
  return(object)
}
