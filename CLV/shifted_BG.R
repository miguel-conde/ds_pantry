# https://danielweitzenfeld.github.io/passtheroc/blog/2015/01/19/s-b-g/

library(tidyverse)
library(rstan)
library(tidybayes)

df <- tribble(~cohorte, ~t, ~n,
              1, 0, 1000,
              1, 1, 840,
              1, 2, 618,
              1, 3, 570,
              1, 4, 519,
              2, 0, 1000,
              2, 1, 810,
              2, 2, 678,
              2, 3, 560,
              3, 0, 1000,
              3, 1, 900,
              3, 2, 790,
              4, 0, 1000,
              4, 1, 780)

df <- tribble(~cohorte, ~t, ~n,
              1, 0, 1000,
              1, 1, 869,
              1, 2, 743,
              1, 3, 653,
              1, 4, 593,
              1, 5, 551,
              1, 6, 517,
              1, 7, 491,
              1, 8, 468,
              1, 9, 445,
              1, 10, 427,
              1, 11, 409,
              1, 12, 394,
              2, 0, 1000,
              2, 1, 631,
              2, 2, 468,
              2, 3, 382,
              2, 4, 326,
              2, 5, 289,
              2, 6, 262,
              2, 7, 241,
              2, 8, 223,
              2, 9, 207,
              2, 10, 194,
              2, 11, 183,
              2, 12, 173)

model_data_all <- df %>% 
  group_by(cohorte) %>% 
  group_map(~ .x %>% arrange(t) %>% 
              mutate(lag_n = lag(n)) %>% 
              mutate(r = n / lag_n ) %>% 
              drop_na() %>% 
              mutate(S = cumprod(r)), 
            .keep = TRUE) %>% 
  bind_rows()

model_data <- model_data %>% filter(cohorte < 8)

STAN_FILE <- here::here("clv", "s-BG.stan")
# STAN_FILE <- here::here("clv", "tst_stan.stan")

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)

model_sbg <- stan_model(file = STAN_FILE)

sbg_model <- 
  sampling(model_sbg, 
           iter = 10000, chains = 1, 
           data = list(N = nrow(model_data),
                       r = model_data$r,
                       G = length(model_data$cohorte %>% unique()),
                       g = model_data$cohorte,
                       t = model_data$t),
           verbose = TRUE)

sbg_model


print(sbg_model, c("betas"))
sbg_model %>% rstan::extract(c("betas"))
sbg_model %>% rstan::extract(c("alphas"))
sbg_model %>% rstan::extract(c("mu_r")) %>% .[[1]] %>% colMeans()
model_data$r

plot(model_data$r,
     sbg_model %>% rstan::extract(c("mu_r")) %>% .[[1]] %>% colMeans())
abline(a=0, b=1)

plot(cumprod(model_data$r),
     sbg_model %>% rstan::extract(c("mu_r")) %>% .[[1]] %>% colMeans() %>% cumprod())
abline(a=0, b=1)

curve(dbeta(x, 0.0001156209, 7.364960), 0, 1)
curve(dbeta(x, 0.0001018031, 2.905218), 0, 1, add = TRUE, color = "blue")

curve(dbeta(x, 0.688, 3.806), 0, 1)
curve(dbeta(x, 0.704, 1.182), 0, 1, add = TRUE, color = "blue")

res <- (sapply(1:12, function(x) x + as.matrix(colMeans(rstan::extract(sbg_model, c("betas"))[[1]]))) - 2) /
  ((sapply(1:12, function(x) x + as.matrix(colMeans(rstan::extract(sbg_model, c("betas"))[[1]]) + 
                                             colMeans(rstan::extract(sbg_model, c("alphas"))[[1]]))) - 1))
  
res

res %>% t() %>% as.data.frame() %>% 
  mutate(V1 = cumprod(V1), V2 = cumprod(V2))
