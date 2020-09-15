library(tidyverse)
library(bsts)

# http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html

# Example 1 - Nowcasting --------------------------------------------------


library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

plot(initial.claims$iclaimsNSA)

ss <- AddLocalLinearTrend(list(), initial.claims$iclaimsNSA)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52)
model1 <- bsts(initial.claims$iclaimsNSA,
               state.specification = ss,
               niter = 1000)

names(model1)

plot(model1)
plot(model1, "components")  # plot(model1, "comp") works too!
plot(model1, "help")

pred1 <- predict(model1, horizon = 12)
plot(pred1, plot.original = 156)


# Regression with spike and slab priors -----------------------------------

# Fit a bsts model with expected model size 1, the default.
model2 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims)


# Fit a bsts model with expected model size 5, to include more coefficients.
model3 <- bsts(iclaimsNSA ~ .,
               state.specification = ss,
               niter = 1000,
               data = initial.claims,
               expected.model.size = 5)  # Passed to SpikeSlabPrior.

plot(model2, "comp")
plot(model3, "comp")

plot(model2, "coef")
plot(model3, "coef")


# Model diagnostics: Did the Google data help? ----------------------------

CompareBstsModels(list("Model 1" = model1,
                       "Model 2" = model2,
                       "Model 3" = model3),
                  colors = c("black", "red", "blue"))


# Example 2: Long term forecasting ----------------------------------------

sp500 <- ts(cumsum(SP500)+1250,start = c(2012, 1), frequency = 360)
plot(sp500)

ss1 <- AddLocalLinearTrend(list(), sp500)
model1 <- bsts(sp500, state.specification = ss1, niter = 1000)
pred1 <- predict(model1, horizon = 360)


ss2 <- AddSemilocalLinearTrend(list(), sp500)
model2 <- bsts(sp500, state.specification = ss2, niter = 1000)
pred2 <- predict(model2, horizon = 360)

old_par = par(mfrow = c(1,2))
plot(pred2, plot.original = 360, ylim = range(pred1))
plot(pred1, plot.original = 360, ylim = range(pred1))
par(old_par)


# PROBATINA ALSHAYA -------------------------------------------------------

base_modeler <- read.csv("C:/Users/mcondedesimon/Documents/PROYECTOS/alshaya_mmm_pottery_barn_ksa/DELIVERY/data/base_final_bayes.csv")
base_modeler <- base_modeler %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .))

op_base_online <- base_modeler %>% 
  filter(date > as.Date("2019-06-30") + 7 - 1) %>% 
  mutate_if(is.logical, as.numeric)

# TARGET_VAR <- "online_magento_orders"
# VARS_MODEL <- c("sessions",  
#                 "online_promos_lockdown_ecom_mss",
#                 "covid19_lockdown_on",
#                 "week_day_thursday",
#                 "online_promo_event_on",
#                 "online_promo_free_product_on",
#                 "online_promo_sale_on",
#                 "xmas_period",
#                 "covid19_lockdown_on_week_day_friday",
#                 "covid19_lockdown_on_week_day_saturday")
# 
# f <- as.formula(paste(TARGET_VAR, "~",
#                       paste(VARS_MODEL, collapse="+"),
#                       "- 1"))

TARGET_VAR <- "sessions"
VARS_MODEL <- c("criteo_impressions_h_as" ,          "facebook_impressions_h_as" ,       
                "gdn_impressions_h_as"      ,        # "goog_trends"     ,                 
                "non_brand_search_impressions_h_as", "online_promo_discount_on" ,        
                "online_promo_sale_on"        ,      "ramadan_on"        ,               
                "snapchat_impressions_h_as" )
f <- as.formula(paste(TARGET_VAR, "~",
                      paste(VARS_MODEL, collapse="+"),
                      "- 1"))

ss <- AddLocalLinearTrend(list(), op_base_online %>% pull(TARGET_VAR))
# ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 365)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 7)
model_alshaya <- bsts(f,
                      state.specification = ss,
                      data = op_base_online,
                      niter = 1000,
                      expected.model.size = 9)

names(model_alshaya)

plot(model_alshaya)
lines(model_alshaya$original.series)
plot(model_alshaya, "components")  

# pred1 <- predict(model_alshaya, horizon = 30)
# plot(pred1, plot.original = 120)

mape <- mean(abs(model_alshaya$one.step.prediction.errors / 
                  sum(model_alshaya$original.series))^2)
mape

model_alshaya$coefficients %>% colMeans()

reg_contr_array <- array(dim = c(319,8, 0))
for(i in 1:1000) {
  reg_contr_array <- sweep(model_alshaya$predictors, 
                         MARGIN = 2, 
                         STATS = model_alshaya$coefficients[i, ], 
                         FUN = "*") %>% 
    abind::abind(reg_contr_array, along = 3)
}

reg_contr_array %>% apply(1:2, mean) %>% rowSums() %>% plot(type = "l")

all_contr <- abind::abind(model_alshaya$state.contributions, along = 2)

(all_contr %>% apply(2:3, mean) %>% t)[, "regression"]  %>% plot(type = "l")



# Reproducción de los resultados de Alshaya -------------------------------
TARGET_VAR <- "sessions"
VARS_MODEL <- c("criteo_impressions_h_as" ,          "facebook_impressions_h_as" ,       
                "gdn_impressions_h_as"      ,        "goog_trends"     ,                 
                "non_brand_search_impressions_h_as", "online_promo_discount_on" ,        
                "online_promo_sale_on"        ,      "ramadan_on"        ,               
                "snapchat_impressions_h_as" )
f <- as.formula(paste(TARGET_VAR, "~",
                      paste(VARS_MODEL, collapse="+"),
                      "- 1"))

ss <- list()
ss <- AddSeasonal(ss, op_base_online %>% pull(TARGET_VAR), nseasons = 7)
model_alshaya <- bsts(f,
                      state.specification = ss,
                      data = op_base_online,
                      niter = 1000,
                      expected.model.size = 9)

plot(model_alshaya)
lines(model_alshaya$original.series)
plot(model_alshaya, "components")  

model_alshaya$coefficients %>% colMeans()

mape <- mean(abs(model_alshaya$one.step.prediction.errors / 
                   sum(model_alshaya$original.series))^2)
mape

# Dynamic Regression ------------------------------------------------------

TARGET_VAR <- "sessions"
VARS_MODEL <- c("criteo_impressions_h_as" ,          "facebook_impressions_h_as" ,       
                "gdn_impressions_h_as"      ,        # "goog_trends"     ,                 
                "non_brand_search_impressions_h_as", "online_promo_discount_on" ,        
                "online_promo_sale_on"        ,      "ramadan_on"        ,               
                "snapchat_impressions_h_as" )
f <- as.formula(paste(TARGET_VAR, "~",
                      paste(VARS_MODEL, collapse="+"),
                      "- 1"))

ss <- AddLocalLinearTrend(list(), op_base_online %>% pull(TARGET_VAR))
ss <- AddSeasonal(ss, op_base_online %>% pull(TARGET_VAR), nseasons = 7)
ss <- AddDynamicRegression(ss, f,data = op_base_online)
model_alshaya <- bsts(op_base_online %>% pull(TARGET_VAR),
                      state.specification = ss,
                      # data = op_base_online,
                      niter = 1000,
                      expected.model.size = 9)

names(model_alshaya)

plot(model_alshaya)
lines(model_alshaya$original.series)
plot(model_alshaya, "components")  

# pred1 <- predict(model_alshaya, horizon = 30)
# plot(pred1, plot.original = 120)

mape <- mean(abs(model_alshaya$one.step.prediction.errors / 
                   sum(model_alshaya$original.series))^2)
mape

model_alshaya$dynamic.regression.coefficients %>% dim()
model_coefs <- model_alshaya$dynamic.regression.coefficients %>% 
  apply(2:3, mean) %>% t
model_coefs[, "facebook_impressions_h_as"] %>% plot(type = "l")
model_coefs[, "non_brand_search_impressions_h_as"] %>% plot(type = "l")
model_coefs[, "criteo_impressions_h_as"] %>% plot(type = "l")
model_coefs[, "gdn_impressions_h_as"] %>% plot(type = "l")
model_coefs[, "snapchat_impressions_h_as"] %>% plot(type = "l")
model_coefs[, "online_promo_discount_on"] %>% plot(type = "l")
model_coefs[, "online_promo_sale_on"] %>% plot(type = "l")
model_coefs[, "ramadan_on"] %>% plot(type = "l")

reg_contr_array <- array(dim = c(319,8, 0))
for(i in 1:1000) {
  data_predictors <- op_base_online %>% dplyr::select(all_of(VARS_MODEL))
  reg_contr_array <- (data_predictors * 
    t(model_alshaya$dynamic.regression.coefficients[i,,])) %>% 
    abind::abind(reg_contr_array, along = 3)
}

reg_contr_array %>% apply(1:2, mean) %>% rowSums() %>% plot(type = "l")

model_alshaya$state.contributions %>% 
  apply(2:3, mean) %>% 
  tail() %>% 
  t %>% 
  tail()
reg_contr_array %>% apply(1:2, mean) %>% rowSums() %>% tail()

all_contr <- abind::abind(model_alshaya$state.contributions, along = 2)

(all_contr %>% apply(2:3, mean) %>% t)[, "regression"]  %>% plot(type = "l")
