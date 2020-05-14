library(tidyverse)

source(here::here("models_S3", "model_default.R"))

# CONSTRUCTORS ------------------------------------------------------------

kk <- model(y ~ x)
get("model_formula", kk$get_env())
get("base", kk$get_env())

identical(get("this", kk$get_env()), kk)

kk$get_model_formula()
kk$get_model_base()

kk$set_model_formula(NULL)
kk$set_model_base(tibble(a = 1:10, b = letters[1:10]))

kk$get_model_formula()
kk$get_model_base()

kk$is_fitted()
kk$get_model_type()


# METHODS -----------------------------------------------------------------


# make_copy ---------------------------------------------------------------

kk <- model(y ~ x)
kk2 <- make_copy(kk)

identical(kk, kk2)

identical(kk$get_env(), kk2$get_env())
identical(kk$get_fitted_model(), kk2$get_fitted_model())
identical(kk$is_fitted(), kk2$is_fitted())
identical(kk$get_model_type(), kk2$get_model_type())
identical(kk$get_model_formula(), kk2$get_model_formula())
identical(kk$get_model_base(), kk2$get_model_base())

