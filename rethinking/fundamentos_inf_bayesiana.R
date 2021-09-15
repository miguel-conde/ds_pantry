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

n_obs2 <- 100
n_bernouillis_x_obs2 <- 9

dato_num_azules2 <- rbinom(n = n_obs2, size = n_bernouillis_x_obs2, prob = p) 

## MÉTODO GRID
ps_grid2 <- seq(0, 1, length.out = 1000) # La grid de p's

posterior2 <- sapply(ps_grid2,
               function(p) {
                 log_likelihood <- sum(dbinom(x = dato_num_azules2,
                                              size = n_bernouillis_x_obs2,
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
            data = list(N = n_bernouillis_x_obs2, 
                        N_obs = n_obs2,
                        y = dato_num_azules2),
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

posterior3 <- sapply(1:nrow(pars_grid),
                    function(idx) {
                      log_likelihood <- sum( dbinom(x = dato_num_azules2,
                                                    size = pars_grid$Ns[idx],
                                                    prob = pars_grid$ps[idx],
                                                    log = TRUE))
                      likelihood <- exp(log_likelihood)
                      post <- likelihood * dunif(p, 0, 1)
                      post
                    })
posterior3 <- posterior3 / sum(posterior)

plot(pars_grid$ps, posterior3, type = "o")
plot(pars_grid$Ns, posterior3, type = "o")
pars_grid[which.max(posterior3),]


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

# 4.1 - Muestreo de la posterior ------------------------------------------

## Del modelo 2
## 10000 muestras de la posterior:
post_samples_2 <- sample(x = ps_grid2, 
                         size = 10000, 
                         prob = posterior2,
                         replace = TRUE)
plot(post_samples_2)
density(post_samples_2) %>% plot()

# 4.2 - Aplicaciones del muestreo de la posterior -------------------------


# 4.2.1 - Probabilidad de que un parámetro esté acotado entre límites -----

# Probabilidad posterior de que p < 0.75
sum(posterior2[ps_grid2 < 0.75]) # Exacta, casi nunca la podremos calcular
mean(post_samples_2 < 0.75)      # De la posterior, casi igual
mean(rstan::extract(fit2)$p < 0.75)

# Probabilidad posterior de que p < 0.7 y p > 0.6
sum(posterior2[ps_grid2 < 0.7 & ps_grid2 > 0.6]) # Exacta, casi nunca la podremos calcular
mean(post_samples_2 < 0.7 & post_samples_2 > 0.6)      # De la posterior, casi igual
mean(rstan::extract(fit2)$p < 0.7 & 
       rstan::extract(fit2)$p > 0.6)


# 4.2.2 - Intervalos de masa definida - Intervalos de credibilidad --------

# Entre qué 2 valores de p se encuentra el 89% de la probabilidad
quantile(post_samples_2, probs = c((1-.89)/2, 1-(1-.89)/2))
quantile(rstan::extract(fit2)$p, probs = c((1-.89)/2, 1-(1-.89)/2))

# Este tipo de intervalos asignan la misma masa de probailiadd a cada cola.
# Se llaman "PERCENTILE INTERVALS", PI.
quantile(post_samples_2, probs = c(0.1, 1 - 0.1))
quantile(post_samples_2, probs = c(0.2, 1 - 0.2))
quantile(post_samples_2, probs = c(0.3, 1 - 0.3))
quantile(post_samples_2, probs = c(0.25, 1 - 0.25))

# Funcionan bien si la posterior es bastante simétrica. Si está bastante sesgada,
# mjor usar HISGHEST POSTERIOR DENSITY INTERVALS (HDPI) = el intervalo más estrecho
# que contiene la masa de probabilidad especificada
rethinking::HPDI(post_samples_2, prob = 0.5)


# 4.2.3 - Estimaciones puntuales ------------------------------------------

## Sirven como resúmenes de la posterior

## 1 - Estimación MAP (Maximum A Posteriori) - MODA
ps_grid2[which.max(posterior2)] # Exacta, no siempre podremos calcularla
rethinking::chainmode(post_samples_2, adj = 0.01)
rethinking::chainmode(rstan::extract(fit2)$p, adj = 0.01)

## 2 - Media
sum(ps_grid2 * posterior2)
mean(post_samples_2)
mean(rstan::extract(fit2)$p)

## 3 - Mediana
quantile(post_samples_2, prob = .5)
  quantile(rstan::extract(fit2)$p, prob = .5)

## 4 - Function loss

# Minimizamos una medida de la incertidumbre sobre el valor real

## L1 - Coincide con la MEDIANA
loss <-sapply(ps_grid2, function(d) sum(posterior2 * abs(d - ps_grid2)))
ps_grid2[which.min(loss)]

## L2 - Coincide con la media
loss <-sapply(ps_grid2, function(d) sum(posterior2 * (d - ps_grid2)^2))
ps_grid2[which.min(loss)]


# 4.3 - Muestreo para simular predicciones --------------------------------


# 4.3.1 - Dummy data ------------------------------------------------------

# Es lo que hemos hecho para los datos de los modelos de arriba

# Comprobación:
# Probabilidades de cada caso:
dbinom(0:n_bernouillis_x_obs2, size = n_bernouillis_x_obs2, prob = p)

# Simulamos dummy data
dummy_data <- rbinom(n = 1e6, size = n_bernouillis_x_obs2, prob = p)
table(dummy_data) / 1e6


# 4.3.2 - Model Checking --------------------------------------------------


# 4.3.2.1 - Posterior Predictive Distribution -----------------------------

## PPS (Posterior Predictive Sampling)
w <- rbinom(1e5, size = n_bernouillis_x_obs2, prob = post_samples_2)

# Con Stan
stan_code2_pps <- "
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

generated quantities {
  int y_pps[N_obs];
  
  for (n in 1:N_obs) {
    y_pps[n] = binomial_rng(N, p);
  }
    
}
"

fit2_pps <- stan(model_code = stan_code2_pps, 
             iter = 1000, chains = 1, 
             data = list(N = n_bernouillis_x_obs2, 
                         N_obs = n_obs2,
                         y = dato_num_azules2),
             verbose = TRUE)

# Salen 500 (1 por cada post-warmup draws) distribuciones de cada simulación predictiva

extract(fit2_pps)$y_pps %>% apply(1, function(x) table(x)/100) %>% bind_rows()
