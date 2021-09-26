library(tidyverse)

library(rethinking)

library(tidybayes)

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)


# A - ASOCIACIONES ESPÚREAS -----------------------------------------------

data("WaffleDivorce")

d <- WaffleDivorce %>% as_tibble()
glimpse(d)

ggplot(data = d, mapping = aes(x = Marriage, y = Divorce)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = d, mapping = aes(x = MedianAgeMarriage, y = Divorce)) +
  geom_point() +
  geom_smooth(method = "lm")

d %>% select(Marriage, Divorce, MedianAgeMarriage) %>% cor()

# Marriage (rate) y MedianAgeMarriage son buenos PREDICTORES de Divorce (rate),
# hay una clara CORRELACiÓN, están ASOCIADOS  Pero ¿la relación es CAUSAL?

d <- d %>% mutate(D = standardize(Divorce),
                  M = standardize(Marriage),
                  A = standardize(MedianAgeMarriage))

sc_linear_D_A <- " // Stan code dor lineal model D ~ A
data {
  int<lower=1> n;
  vector[n] A;
  vector[n] D;
}

parameters {
  real<lower=0> sigma;
  real<lower=0> alpha;
  real beta;
  
}

transformed parameters {
  vector[n] mu;
  mu = alpha + beta * A;
}

model {

  sigma ~ exponential(1);
  alpha ~ normal(0, 0.2);
  beta ~ normal(0, 0.5); // solo con 5% de las betas mu más allá de 1 desv. 
                         // estándar de la tasa de divorcio media
  
  D ~ normal(mu, sigma);
}

generated quantities {
  vector[n] mu_sim;
  real D_sim;
  
  // PPS
  for (i in 1:n) {
    mu_sim[i] = alpha + beta * A[i];
    D_sim = normal_rng(mu_sim[i], sigma);
  }
}
"

# PRIOR PREDICTIVE SAMPLING
prior_sigma <- rexp(1e5, 1)
prior_beta <- rnorm(1e5, 0, 0.5)
prior_alpha <- abs(rnorm(1e5, 0, 0.2))

seq_A <- seq(-2, 2, length.out = 100)

prior_mu <- sapply(seq_A, function(x) prior_alpha + prior_beta * x) %>% t()
prior_D <- prior_mu %>% apply(2, function(x) rnorm(x, prior_sigma))

prior_sample_cols <- sample(ncol(prior_mu), 100)

p <- ggplot(data = tibble(A = seq_A), aes(x = A))

for (i in 1:100) {
  print(head(prior_mu[,i]))
    p <- p + geom_line(aes(y = prior_mu[,i]))
}

p

## Fit the model

model_linear_D_A <- stan_model(model_code = sc_linear_D_A)

fit_linear_D_A <- sampling(model_linear_D_A, 
                           iter = 1000, chains = 1, 
                           data = compose_data(d %>% select(D, A)),
                           verbose = TRUE)

print(fit_linear_D_A, pars = c("alpha", "beta", "sigma"))
precis(fit_linear_D_A)

## POSTERIRO PREDICTIVE SAMPLING

post <- extract(fit_linear_D_A, c("alpha", "beta", "sigma"))
post_mu <-  sapply(seq_A, function(x) post$alpha + post$beta * x) %>% t()
prior_D <- post_mu %>% apply(2, function(x) rnorm(x, prior_sigma))

post_mu_mean <- apply(post_mu, 1, mean)
post_mu_PI <- apply(post_mu, 1, PI)

plot(D ~ A, data = d, col = rangi2)
lines(seq_A, post_mu_mean, lwd = 2)
shade(post_mu_PI, seq_A)


# 1. DAGs e independencias condicionales -----------------------------------


# THINK BEFORE REGRESS
# TESTABLE IMPLICATIONS

library(dagitty)

DAG_1 <- dagitty('
                 dag{D <- A -> M -> D}
                 ')

plot(DAG_1)

DAG_2 <- dagitty('
                 dag{D <- A -> M}
                 ')

plot(DAG_2)

impliedConditionalIndependencies(DAG_1)
impliedConditionalIndependencies(DAG_2)


# 2 - Regresión multivariable ---------------------------------------------

sc_linear_D_A_M <- " // Stan code dor lineal model D ~ A
data {
  int<lower=1> n;
  vector[n] A;
  vector[n] D;
  vector[n] M;
}

parameters {
  real<lower=0> sigma;
  real<lower=0> alpha;
  real beta_A;
  real beta_M;
  
}

transformed parameters {
  vector[n] mu;
  mu = alpha + beta_A * A + beta_M * M;
}

model {

  sigma ~ exponential(1);
  alpha ~ normal(0, 0.2);
  beta_A ~ normal(0, 0.5); 
  beta_M ~ normal(0, 0.5); 
  
  D ~ normal(mu, sigma);
}

generated quantities {
  vector[n] mu_sim;
  real D_sim;
  
  // PPS
  for (i in 1:n) {
    mu_sim[i] = alpha + beta_A * A[i]+ beta_M * A[i];
    D_sim = normal_rng(mu_sim[i], sigma);
  }
}
"

## Fit the model

model_linear_D_A_M <- stan_model(model_code = sc_linear_D_A_M)

fit_linear_D_A_M <- sampling(model_linear_D_A_M, 
                           iter = 1000, chains = 1, 
                           data = compose_data(d %>% select(D, A, M)),
                           verbose = TRUE)

print(fit_linear_D_A_M, pars = c("alpha", "beta_A", "beta_M", "sigma"))
precis(fit_linear_D_A_M)

plot(fit_linear_D_A_M, pars = c("alpha", "beta_A", "beta_M", "sigma"))

# 3 - Plotting multivariate posteriors ------------------------------------


# a. Predictor residual plots ---------------------------------------------


# b. Posterios prediction plots -------------------------------------------


# c. Counterfactual plots -------------------------------------------------


# B - RELACIONES OCULTAS --------------------------------------------------

data("milk")

d <- milk

glimpse(milk)
summary(milk)

# C - VARIABLES CATEGÓRICAS -----------------------------------------------


# EJERCICIOS --------------------------------------------------------------


# Medium ------------------------------------------------------------------


# 5M1 ---------------------------------------------------------------------

# Invent your own example of a spurious correlation. An outcome variable 
# should be correlated with both predictor variables. But when both predictors 
# are entered in the same model, the correlation between the outcome and one 
# of the predictors should mostly vanish (or at least be greatly reduced).

# SIMULACIÓN del MODELO REAL
x1 <- rnorm(1000)
# x1 -> x2
x2 <- rnorm(1000, x1) # x1 es CAUSA de x2 => estarán correladas
# x1 -> y
y  <- rnorm(1000, x1) # x1 también es CAUSA de la respuesta y => x1 e y estarán
                      # correladas; pero también estarán correladas x2 e y 
                      # debido a su origen común, aunque x2 e y no son CAUSA 
                      # una de Otra.

# Comprobémoslo:
cor(tibble(x1, x2, y))

# Si encontramos un dataset así - como en el caso de la edad (A = x1), la tasa 
# de matrimonio (M = x2) y la de Divorcio (D = y) - la primera hipótesis sería:

DAG_H1 <- dagitty(('
                 dag{x1 -> x2 -> y
                     x1 -> y}
                 '))

plot(DAG_H1)

# En este dag hay implicaciones que podemos comprobar empíricamente con los 
# datos:

# 1. x1 y x2 no son independientes (porque hay una flecha entre ellas) => 
#    => están correladas. Si lo comprobamos y resulta que no están correladas,
#    nos echaría por tierra al menos esta flecha en el modelo.
# 2. x1 e y no son independientes - idem
# 3. x2 e y no son independientes - idem

# Estas 3 ya las hemos testado arriba y el modelo hipotético ha sobrevivido.
# También podríamos haber hecho:
summary(lm(x2 ~ x1))
summary(lm(y ~ x1))
summary(lm(y ~ x2))

# En los 3 casos los coeficientes son significativos => hay correlación

# Veamos ahora independencias condicionadas
dagitty::impliedConditionalIndependencies(DAG_H1)

# No tiene - lógico porque hay flecha entre todas los posibles pares de 
# variables. 
# Comprobemos con los datos:

# ¿Siguen siendo independientes x1 y x2 si condicionamos en y?
summary(lm(x2 ~ x1 + y))

# Si, la correlación entre x2 y x1 persiste

# Y ¿x1 con y si condicionamos en x2?
summary(lm(y ~ x1 + x2))

# Si, la correlación entre x1 e y persiste.

# Por último, ¿siguen siendo independientes y y x2 si condicionamos en x1?
# En el mismo modelo anterior se ve que no (porque el coeficiente de x2 no
# es siginificativo). Por tanto, y en contra de nuestra primera hipótesis (el
# modelo DAG_H1) si hay una independencia condicional: x2 e y son independientes
# si condicionamos en x1. Y si la hipótesis es falsa, el modelo no es correcto.

# Lo modificamos:
DAG_H2 <- dagitty(('
                 dag{x1 -> x2
                     x1 -> y}
                 '))

plot(DAG_H2)

dagitty::impliedConditionalIndependencies(DAG_H2)

# Como se ve, esta segunda hipótesis lleva implícita la independencia condicional
# de que x2 e y condicionada a x1.

# 5M2 ---------------------------------------------------------------------

# Invent your own example of a masked relationship. An outcome variable should 
# be correlated with both predictor variables, but in opposite directions. And 
# the two predictor variables should be correlated with one another.

# SIMULACIÓN del MODELO REAL
x1 <- rnorm(1000)
# x1 -> x2
x2 <- rnorm(1000, x1)
# x1 -> y <- x2
y  <- rnorm(1000, x2 - x1)

cor(tibble(x1, x2, y))

# En este caso, aunque x1 es causa de y, su correlación aparece débil, 
# enmascarada su relación por x2, que es causada también por x1 y es a su vez
# la segunda causa de y.
# De esas correlaciones podríamos pensar que el modelo correcto es:

DAG_H1 <- dagitty('
                  y <- x2 -> x1
                  ')
plot(DAG_H1)

# o cualquiera de sus equivalentes de Markov

equivalenceClass(DAG_H1)

# 5M3 ---------------------------------------------------------------------

# It is sometimes observed that the best predictor of fire risk is the presence 
# of firefighters—State and localities with many firefighters also have more 
# fires. Presumably firefighters do not cause fires. Nevertheless, this is not a 
# spurious correlation. Instead fires cause firefighters. Consider the same 
# reversal of causal inference in the context of the divorce and marriage data. 
# How might a high divorce rate cause a higher marriage rate? Can you think of a 
# way to evaluate this relationship, using multiple regression?

# Hard --------------------------------------------------------------------


# 5H1 ---------------------------------------------------------------------

# In the divorce example, suppose the DAG is:  M → A → D
# What are the implied conditional independencies of the graph? Are the data 
# consistent with it?

# 5H2 ---------------------------------------------------------------------

#  Assuming that the DAG for the divorce example is indeed  M → A → D, fit a 
# new model and use it ot estimate the counterfactual effect of halving a 
# State’s marriage rate M. Using the counterfactual example from the chapter 
# (starting on page 140) as a template.


# 5H3 ---------------------------------------------------------------------

# Return to the milk energy model, m5.7. Suppose that the true causal 
# relationship among the variables is: M -> N -K, M -> N
# Now compute the counterfactual effect on K of doubling M. You will need to 
# account for both the direct and indirect paths of causation. Use the 
# counterfactual example from the chapter (starting on page 140) as a template.
