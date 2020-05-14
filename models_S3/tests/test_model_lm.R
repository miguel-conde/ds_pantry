library(tidyverse)

source(here::here("models_S3", "model_default.R"))
source(here::here("models_S3", "model_lm.R"))


# CONSTRUCTORS ------------------------------------------------------------

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


# METHODS -----------------------------------------------------------------


# make_copy ---------------------------------------------------------------


kk_lm <- model_lm(y ~ x)
kk_lm_2 <- make_copy(kk_lm)

identical(kk_lm, kk_lm_2)

identical(kk_lm$get_env(), kk_lm_2$get_env())
identical(kk_lm$get_fitted_model(), kk_lm_2$get_fitted_model())
identical(kk_lm$is_fitted(), kk_lm_2$is_fitted())
identical(kk_lm$get_model_type(), kk_lm_2$get_model_type())
identical(kk_lm$get_model_formula(), kk_lm_2$get_model_formula())
identical(kk_lm$get_model_base(), kk_lm_2$get_model_base())


# fit ---------------------------------------------------------------------

kk_lm <- model_lm(f = y ~ x, base = tibble(x = 1:100, y = 5*(1:100) + rnorm(100)))

kk_lm <- fit(kk_lm)

identical(kk_lm$is_fitted(), kk_lm_2$is_fitted())

summary(kk_lm$get_fitted_model())

# kk_lm$fit()

# summary -----------------------------------------------------------------

summary(kk_lm)

summary(kk_lm_2)

# fitted ------------------------------------------------------------------

fitted(kk_lm)

fitted(kk_lm_2)

# residuals ---------------------------------------------------------------

residuals(kk_lm)

residuals(kk_lm_2)

# plot --------------------------------------------------------------------

plot(kk_lm)

plot(kk_lm_2)

# coef --------------------------------------------------------------------

coef(kk_lm)

coef(kk_lm_2)


# predict -----------------------------------------------------------------

predict(kk_lm)

predict(kk_lm, new_data = tibble(x = 101:200, y = 5*(1:100) + rnorm(100)))

predict(kk_lm_2)

predict(kk_lm, new_data = 1:100)
