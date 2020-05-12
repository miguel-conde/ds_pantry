# S3
# https://adv-r.hadley.nz/s3.html


library(sloop)


# BASICS ------------------------------------------------------------------


# An S3 object is a base type with at least a class attribute
f <- factor(c("a", "b", "c"))
f
attributes(f)
class(f)

# Get the underlying base type
typeof(f) 
unclass(f)

# An S3 object behaves differently from its underlying base type whenever 
# it’s passed to a generic (short for generic function).
ftype(print)

ftype(str)

ftype(unclass)

print(f)
print(unclass(f))

# The generic is a middleman: its job is to define the interface (i.e. the 
# arguments) then find the right implementation for the job. The implementation 
# for a specific class is called a method, and the generic finds that method by 
# performing method dispatch.
s3_dispatch(print(f))

ftype(t.test)
ftype(t.data.frame)

ftype(weighted.mean)
weighted.mean
weighted.mean.Date

s3_get_method(weighted.mean.Date)


# CLASSES -----------------------------------------------------------------

# To make an object an instance of a class, you simply set the class attribute. 
# You can do that during creation with structure(), 
# or after the fact with class<-():
x <- structure(list(), class = "my_class")
x

x <- list()
class(x) <- "my_class"
x

# Determine the class of an S3 object
class(x)

# See if an object is an instance of a class
inherits(x, "my_class")

inherits(x, "your_class")

# I recommend that you usually provide three functions:
#
#   - A low-level constructor, new_myclass(), that efficiently creates new 
#     objects with the correct structure.
#   - A validator, validate_myclass(), that performs more computationally 
#     expensive checks to ensure that the object has correct values.
#   - A user-friendly helper, myclass(), that provides a convenient way for 
#     others to create objects of your class.


# Constructors ------------------------------------------------------------

# The constructor should follow three principles:
# 
#    - Be called new_myclass().
#    - Have one argument for the base object, and one for each attribute.
#    - Check the type of the base object and the types of each attribute.

new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

new_Date(c(-1, 0, 1))

new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  
  structure(x,
            class = "difftime",
            units = units
  )
}

new_difftime(c(1, 10, 3600), "secs")

new_difftime(52, "weeks")


# Validators --------------------------------------------------------------

new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

new_factor(1:5, "a")

new_factor(0:1, "a")

validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  x
}

validate_factor(new_factor(1:5, "a"))

validate_factor(new_factor(0:1, "a"))


# Helpers -----------------------------------------------------------------

# A helper should always:
# 
#    - Have the same name as the class, e.g. myclass().
#    - Finish by calling the constructor, and the validator, if it exists.
#    - Create carefully crafted error messages tailored towards an end-user.
#    - Have a thoughtfully crafted user interface with carefully chosen default 
#      values and useful conversions.
#
# The last bullet is the trickiest, and it’s hard to give general advice. 
# However, there are three common patterns:
# 
#    - Sometimes all the helper needs to do is coerce its inputs to the desired 
#      type. For example, new_difftime() is very strict, and violates the usual 
#      convention that you can use an integer vector wherever you can use a 
#      double vector:

new_difftime(1:10)

difftime <- function(x = double(), units = "secs") {
  x <- as.double(x)
  new_difftime(x, units = units)
}

difftime(1:10)

# Often, the most natural representation of a complex object is a string. For 
# example, it’s very convenient to specify factors with a character vector. The 
# code below shows a simple version of factor(): it takes a character vector, 
# and guesses that the levels should be the unique values. This is not always 
# correct (since some levels might not be seen in the data), but it’s a useful 
# default.

factor <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}

factor(c("a", "a", "b"))

# Some complex objects are most naturally specified by multiple simple
# components. For example, I think it’s natural to construct a date-time by 
# supplying the individual components (year, month, day etc). That leads me to 
# this POSIXct() helper that resembles the existing ISODatetime() function

POSIXct <- function(year = integer(), 
                    month = integer(), 
                    day = integer(), 
                    hour = 0L, 
                    minute = 0L, 
                    sec = 0, 
                    tzone = "") {
  ISOdatetime(year, month, day, hour, minute, sec, tz = tzone)
}

POSIXct(2020, 1, 1, tzone = "America/New_York")


# Generics and methods ----------------------------------------------------


# Object styles -----------------------------------------------------------




# Inheritance -------------------------------------------------------------

# S3 classes can share behaviour through a mechanism called inheritance. 
# Inheritance is powered by three ideas:
# 
#    - The class can be a character vector. For example, the ordered and 
#      POSIXct classes have two components in their class:

class(ordered("x"))

class(Sys.time())

# If a method is not found for the class in the first element of the vector, 
# R looks for a method for the second class (and so on):

s3_dispatch(print(ordered("x")))

s3_dispatch(print(Sys.time()))

# A method can delegate work by calling NextMethod(). We’ll come back to that 
# very shortly; for now, note that s3_dispatch() reports delegation with ->.

s3_dispatch(ordered("x")[1])

s3_dispatch(Sys.time()[1])

# Before we continue we need a bit of vocabulary to describe the relationship between the classes that appear together in a class vector. We’ll say that ordered is a subclass of factor because it always appears before it in the class vector, and, conversely, we’ll say factor is a superclass of ordered.

S3 imposes no restrictions on the relationship between sub- and superclasses but your life will be easier if you impose some. I recommend that you adhere to two simple principles when creating a subclass:
  
  The base type of the subclass should be that same as the superclass.

The attributes of the subclass should be a superset of the attributes of the superclass.

# Examples ----------------------------------------------------------------

ftype(coefficients)
ftype(coef)
ftype(coefficients.lm)
ftype(summary)
ftype(summary.lm)
ftype(plot)
ftype(print)
ftype(fitted)
ftype(residuals)


# Local Environmet Approach -----------------------------------------------

NordAmericain <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  
  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## The Methods for this class normally go here but are discussed
    ## below. A simple placeholder is here to give you a teaser....
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    }
    
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"NordAmericain")
  return(me)
}

bubba <- NordAmericain()
bubba

get("hasBreakfast",bubba$getEnv())
get("hasBreakfast",bubba$thisEnv)


bubba <- NordAmericain(myFavorite="oatmeal")
get("favoriteBreakfast",bubba$getEnv())

louise <- bubba
assign("favoriteBreakfast","toast",louise$getEnv())
get("favoriteBreakfast",louise$getEnv())

get("favoriteBreakfast",bubba$getEnv())


# TOY: MODELS -------------------------------------------------------------

#' Title
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

new_model.default <- function(f, base, ...) {

  stopifnot(is.null(f) | inherits(f, "formula"))
  stopifnot(is.null(base) | inherits(base, "data.frame"))
  
  ## Get the environment for this instance of the function.
  this_env <- environment()
  
  ## Public Attributes
  model_formula <- f
  model_base <- base
  
  # Private attributes
  fitted_object <- NULL
  model_type <- NA
  
  ## Create the list used to represent an object for this class
  out <- structure(
    list(
      ## Public atrributes
      get_env = function() {
        return(get("this_env", this_env))
      },
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
      get_fitted_model = function() {
        return(get("fitted_object", this_env))
      },
      ## Private attributes
      is_fitted = function() {
        return(!is.null(get("fitted_object", this_env)))
      },
      get_model_type = function() {
        return(get("model_type", this_env))
      }
    ),
    class = c("model", "list")
  )
  
  ## Define the value of the list within the current environment.
  assign('this', out, envir = this_env)
  
  return(out)
}

validate_model.default <- function(f, base, ...) {
  
  out <- new_model.default(f, base)
  
  return(out)
}

model.default <- function(f = NULL, base = NULL, ...) {
  
  out <- validate_model.default(f, base)
  
  return(out)
}

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

fit.default <- function(model) {
  
  return(model)
}

kk <- model(y ~ x)
get("model_formula", kk$get_env())
get("base", kk$get_env())

kk$get_model_formula()
kk$get_model_base()

kk$set_model_formula(NULL)
kk$set_model_base(tibble(a = 1:10, b = letters[1:10]))

kk$get_model_formula()
kk$get_model_base()

kk$is_fitted()
kk$get_model_type()

make_copy <- function(elObjeto)
{
  print("Calling the base make_copy function")
  UseMethod("make_copy", elObjeto)
  print("Note this is not executed!")
}

make_copy.default <- function(elObjeto)
{
  print("You screwed up. I do not know how to handle this object.")
  return(elObjeto)
}


make_copy.model <- function(elObjeto)
{
  print("In model and making a copy")
  newObject <- model(
    f = elObjeto$get_model_formula(),
    base = elObjeto$get_model_base())
  return(newObject)
}

kk <- model(y ~ x)
kk2 <- make_copy(kk)


#' Title
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

new_model_lm.default <- function(f, base, ...) {
  browser()
  out <- model(f, base)
  
  assign("model_type", "lm", out$get_env())
  
  class(out) <- append("model_lm", class(out))
  
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

kk_lm <- model_lm(y ~ x)
get("model_formula", kk_lm$get_env())
get("base", kk_lm$get_env())

class(kk_lm)

kk_lm$get_model_formula()
kk_lm$get_model_base()

kk_lm$set_model_formula(NULL)
kk_lm$set_model_base(tibble(a = 1:10, b = letters[1:10]))

kk_lm$get_model_formula()
kk_lm$get_model_base()

kk_lm$is_fitted()
kk_lm$get_model_type()

kk_lm <- model_lm(y ~ x)
kk_lm_2 <- make_copy(kk)

fit.model_lm <- function(model, ...) {
  browser()
  aux_lm <- lm(formula = model$get_model_formula(), 
               data = model$get_model_base(),
               ...)
  assign("fitted_object", aux_lm, model$get_env())
  
  return(model)
}

kk_lm <- model_lm(f = y ~ x, base = tibble(x = 1:100, y = 5*(1:100) + rnorm(100)))

kkk <- fit(kk_lm)
summary(kkk$get_fitted_model())
