library(tidyverse)
library(rethinking)
library(brms)
library(tidybayes)
library(bayesplot)

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)


data(chimpanzees)

d <- chimpanzees %>% as_tibble()

# Response: pulled_left - El lado de la palanca que movió el chimpancé
#
# Predictors:
#              prosoc_left: en qué lado estaba la opción prosocial
#              condition: si había o no otro chimpancé al otro lado de la mesa

# Variable índice
#
# 1 - prosoc_left = 0 - condition = 0 2 food right / no partner
# 2 - prosoc_left = 1 - condition = 0 2 food right / no partner
# 3 - prosoc_left = 0 - condition = 1 2 food left / partner present
# 4 - prosoc_left = 1 - condition = 1 2 food left / partner present
#

d <- d %>% mutate(treatment = 1 + prosoc_left + 2 * condition)

xtabs( ~ treatment + prosoc_left + condition, d)

binom_m_1_code <- "
data {
  int<lower = 1> N_actors;
  int<lower = 1> N_treatments;
  int<lower = 1> n;
  int<lower = 1, upper = N_treatments> treatment[n]; // Predictor
  int<lower = 1, upper = N_actors>     actor[n];     // Predictor
  int<lower = 0, upper = 1> pulled_left[n];          // Response
  
}

parameters {
  real alpha[N_actors];
  real beta[N_treatments];
}

transformed parameters {
  vector[n] p;
  
  for (i in 1:n) {
    p[i] =  alpha[actor[i]] + beta[treatment[i]];
    p[i] =  inv_logit(p[i]);
  }
}

model {

  pulled_left ~ binomial(1, p); // L ~ bernouilli(p);
  
  alpha ~ normal(0, 1.5);
  beta ~ normal(0, 0.5);
  
  pulled_left ~ binomial(1, p);
}

generated quantities {
  vector[n] log_lik;
  vector[n] sim_p;
  int sim_pulled_left[n];
  
  for (i in 1:n) {
  
    sim_p[i] =  alpha[actor[i]] + beta[treatment[i]];
    sim_p[i] =  inv_logit(p[i]);
    
    sim_pulled_left[i] = binomial_rng(1, sim_p[i]);
    
    log_lik[i] = binomial_lpmf(pulled_left[i] | 1, p[i]);
  }
}
"

binom_m_1 <- stan_model(model_code = binom_m_1_code)

fit_linear_D_A <- sampling(binom_m_1, 
                           iter = 1000, chains = 1, 
                           data = compose_data(d %>% select(treatment, actor, pulled_left)),
                           verbose = TRUE)

print(fit_linear_D_A, pars = c("alpha", "beta", "sigma"))
precis(fit_linear_D_A)

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment))

stan_binom_m_1 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ), data = dat_list, chains = 4, log_lik = TRUE)

precis(stan_binom_m_1, depth = 2)