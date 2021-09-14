library(tidyverse)

library(rstan)

library(rethinking)


## Vamos a hacer un ejercicio sencillo de inferencia bayesiana paso a paso,
## desde la generación de datos fake a la creación del modelo, primero "a mano"
## y lugo con rstan y rethinking


# 1 - MODELO 1 ------------------------------------------------------------


# 1.1 - FAKE DATA ---------------------------------------------------------

## Tenemos 10 bolas (7 azules y 3 verdes) en 1 bolsa y sacamos 9 con reposición. 
## Vamos a hacer que
## tenemos .
## Nuestros datos fake serán una extracción aleatoria:

N_azul <- 7
N_verde <- 3
N_bolas <- N_azul + N_verde

p <- N_azul / N_bolas

n_obs <- 1
n_bernouillis_x_obs <- 9

# n_obs <- número de observaciones
# size <- número de experimentos de Bernouilli en cada observación
# p <- probabilidad de éxito de cada experimento de Bernouilli
# dato_num_azules <- es el dato: número de éxitos en toda la secuencia de 
# experimentos de Bernouilli
dato_num_azules <- rbinom(n = n_obs, size = n_bernouillis_x_obs, prob = p) 

## Evidentemente, haremos como que no conocemos como se han generado los datos

# 1.2 - Modelo ------------------------------------------------------------

# Nos dicen que hab generado 1 observación consistente en n_bernouillis_x_obs
# extracciones de bolas de una bolsa en la que hay bolas azules y verdes.
# Nos piden que estimemos la proporción de bolas azules sabiendo que en esa 
# observación han salido num_azules.
#
# Partimos de nuestros a prioris acerca de como se han generado los datos: tiene
# que ser:
#
# - Likelihood: será una binomial de parámetro p
# - Prior del parámetro: uniforme de 0 a 1
#

# MÉTODO GRID
ps_grid <- seq(0, 1, length.out = 100) # La grid de p's

prior <- dunif(ps_grid, 0, 1)
likelihood <- dbinom(x = dato_num_azules, 
                     size = n_bernouillis_x_obs, 
                     prob = ps_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(ps_grid, posterior, type = "o")
ps_grid[which.max(posterior)]

# Método HMCMH - rstan

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)


stan_code <- "
data {
  int<lower=0> N;
  int y;
}

parameters {
  real<lower=0, upper=1> p;
}

model {
  p ~ uniform(0, 1); 
  y ~ binomial(N, p);
}
"

fit <- stan(model_code = stan_code, 
            iter = 1000, chains = 1, 
            data = list(N = n_bernouillis_x_obs, y = dato_num_azules),
            verbose = TRUE)

fit
precis(fit)

stan_posterior <- rstan::extract(fit)$p

hist(stan_posterior)


# 2 - MODELO 2 ------------------------------------------------------------

n_obs <- 100
n_bernouillis_x_obs <- 9

dato_num_azules <- rbinom(n = n_obs, size = n_bernouillis_x_obs, prob = p) 

## MÉTODO GRID
ps_grid2 <- seq(0, 1, length.out = 1000) # La grid de p's

posterior2 <- sapply(ps_grid2,
               function(p) {
                 log_likelihood <- sum(dbinom(x = dato_num_azules,
                                              size = n_bernouillis_x_obs,
                                              prob = p,
                                              log = TRUE))
                 likelihood <- exp(log_likelihood)
                 post <- likelihood * dunif(p, 0, 1)
                 post
               })
posterior2 <- posterior2 / sum(posterior2)

plot(ps_grid2, posterior2, type = "o")
ps_grid2[which.max(posterior2)]

# Método HMCMH - rstan

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)


## MÉTODO HMCMC - stan

stan_code2 <- "
data {
  int<lower=0> N;
  int<lower=0> N_obs;
  int y[N_obs];
}

parameters {
  real<lower=0, upper=1> p;
}

model {
  p ~ uniform(0, 1); 
  y ~ binomial(N, p);
}
"

fit2 <- stan(model_code = stan_code2, 
            iter = 1000, chains = 1, 
            data = list(N = n_bernouillis_x_obs, 
                        N_obs = n_obs,
                        y = dato_num_azules),
            verbose = TRUE)

fit2
precis(fit2)

stan_posterior2 <- rstan::extract(fit2)$p

hist(stan_posterior2)

# 3 - MODELO 3 ------------------------------------------------------------

# Con 2 parámetros ahora

## MÉTODO GRID
ps <- seq(0, 1, length.out = 100) # La grid de p's
Ns <- 0:30
pars_grid <- expand_grid(ps, Ns)

posterior <- sapply(1:nrow(pars_grid),
                    function(idx) {
                      log_likelihood <- sum( dbinom(x = dato_num_azules,
                                                    size = pars_grid$Ns[idx],
                                                    prob = pars_grid$ps[idx],
                                                    log = TRUE))
                      likelihood <- exp(log_likelihood)
                      post <- likelihood * dunif(p, 0, 1)
                      post
                    })
posterior <- posterior / sum(posterior)

plot(pars_grid$ps, posterior, type = "o")
plot(pars_grid$Ns, posterior, type = "o")
pars_grid[which.max(posterior),]


# Método HMCMH - rstan

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)


## MÉTODO HMCMC - stan



stan_code3 <- "
data {
  int<lower=0> N_obs;
  int y[N_obs];
}

parameters {
  real<lower=0, upper=1> p;
  real<lower = 0> N;
}

transformed parameters {
int<lower=0> int_N;

int_N = round(N);
}

model {
  p ~ uniform(0, 1); 
  N ~ uniform(1, 20);
  y ~ binomial(int_N, p);
}
"

fit3 <- stan(model_code = stan_code3, 
             iter = 1000, chains = 1, 
             data = list(N_obs = n_obs,
                         y = dato_num_azules),
             verbose = TRUE)

fit3
precis(fit3)

stan_posterior3 <- rstan::extract(fit3)$p

hist(stan_posterior3)



# 4 - SAMPLING ------------------------------------------------------------

## Del modelo 2
## 10000 muestras de la posterior:
post_samples_2 <- sample(x = ps_grid2, 
                         size = 10000, 
                         prob = posterior2,
                         replace = TRUE)
plot(post_samples_2)
density(post_samples_2) %>% plot()
