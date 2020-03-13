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
