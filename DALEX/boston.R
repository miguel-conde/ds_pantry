library(tidyverse)
library(DALEX)

data("Boston", package = "MASS")

model_boston_lm <- lm(medv ~ ., Boston) 
summary(model_boston_lm)

mm <- model.matrix(model_boston_lm)
contribs_boston_lm <- mm %>% 
  sweep(2, coef(model_boston_lm)[colnames(mm)], "*") %>% 
  as_tibble()

# Explainer
explain_boston_rf <- explain(model_boston_lm, 
                              data = Boston,
                              y = Boston$medv, 
                              label = "LM",
                              colorize = FALSE)

# Variable importance plots
vi_lm <- model_parts(model_boston_lm)
head(vi_lm)
plot(vi_lm)

# Variable effects
vr_crim  <- model_profile(explain_boston_rf, variables =  "crim")
head(vr_crim)
plot(vr_crim)
vr_crim$cp_profiles %>% as_tibble()
vr_crim$agr_profiles %>% as_tibble()
vr_crim$agr_profiles %>% as_tibble() %>% select(3,4) %>% plot()

vr_zn  <- model_profile(explain_boston_rf, variables =  "zn")
plot(vr_zn)

vr_indus  <- variable_profile(explain_boston_rf, variables =  "indus")
plot(vr_indus)

vr_chas  <- model_profile(explain_boston_rf, variables =  "chas")
plot(vr_chas)

# Instance level explanations
sp_lm <- predict_parts(explain_boston_rf, Boston[1, ]) # break_down
plot(sp_lm)
sp_lm %>% as_tibble()

sp_lm <- predict_parts(explain_boston_rf, Boston[1, ], type = "shap")
plot(sp_lm)
sp_lm %>% as_tibble()

sp_lm <- predict_parts(explain_boston_rf, Boston[1, ], type = "oscillations")
plot(sp_lm)
sp_lm %>% as_tibble()

sp_lm <- predict_parts(explain_boston_rf, Boston[1, ], type = "oscillations_uni")
plot(sp_lm)
sp_lm %>% as_tibble()

sp_lm <- predict_parts(explain_boston_rf, Boston[1, ], type = "oscillations_emp")
plot(sp_lm)
sp_lm %>% as_tibble()

sp_lm <- predict_parts(explain_boston_rf, Boston[1, ], type = "break_down_interactions")
plot(sp_lm)
sp_lm %>% as_tibble()

