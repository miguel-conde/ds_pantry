library(tidyverse)

library(brms)

data("kidney")
head(kidney, n = 3)

fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease
            + (1 + age|patient),
            data = kidney, family = lognormal(),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))


S# ALSHAYA -----------------------------------------------------------------

library(bayesm3)

base_modeler <- read.csv("C:/Users/mcondedesimon/Documents/PROYECTOS/alshaya_mmm_pottery_barn_ksa/DELIVERY/data/base_final_bayes.csv")
base_modeler <- base_modeler %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .))

op_base_online <- base_modeler %>% 
  filter(date > as.Date("2019-06-30") + 7 - 1) %>% 
  mutate_if(is.logical, as.numeric)

TARGET_VAR <- "sessions+1"
VARS_MODEL <- c("criteo_impressions_h_as" ,          "facebook_impressions_h_as" ,       
                "gdn_impressions_h_as"      ,        "goog_trends"     ,                 
                "non_brand_search_impressions_h_as", "online_promo_discount_on" ,        
                "online_promo_sale_on"        ,      "ramadan_on"        ,               
                "snapchat_impressions_h_as" )
f <- as.formula(paste(TARGET_VAR, "~",
                      paste0("(",
                             paste(VARS_MODEL, collapse="+"),
                      "- 1"),
                      "| date)"))

fit1 <- brm(formula = f,
            data = op_base_online, 
            family = lognormal(),
            prior = c(# set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 2000, chains = 4,
            control = list(adapt_delta = 0.95))
