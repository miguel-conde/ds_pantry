library(tidyverse)

# La técnica de bootstrap proviene de la necesidad de tener que calcular una 
# desviación estándar en situaciones en las que es difícil o imposible hacerlo. 
# Ejemplo: los errores estándar de los coeficientes de una regresión lineal 
# pueden estimarse mediante bootstrap.

# ESTIMATING STANDARD DEVIATION OF A SAMPLE -------------------------------

pop_mean <- 0
pop_sd <- .96574

N <- 1000
set.seed(123)
data_sample <- rnorm(N, pop_mean, pop_sd)

sample_mean <- mean(data_sample)
sample_sd <- sqrt(1/(N-1)*sum((data_sample - sample_mean)^2))

sample_sd
sd(data_sample)

sample_se <- sample_sd / sqrt(N)
sample_se

## NOW WITH BOOTSTRAP

B <- 10000

bootstrap_mns <- c()

set.seed(123)
for(i in 1:B) {
  data_bootstrap <- sample(data_sample, size = N, replace = TRUE)
  
  bootstrap_mns <- c(bootstrap_mns, mean(data_bootstrap))
}

bootstrap_se_estimate <- 
    sqrt(1 / (B-1) * sum((bootstrap_mns - mean(bootstrap_mns))^2))

bootstrap_se_estimate


# LM COEFS ESTIMATES ------------------------------------------------------

N <- 10000

boot_coefs <- vector(mode = "list", length = N)

set.seed(1)
for (i in 1:N) {
  
  boot_sample = cars[sample(nrow(cars), replace = TRUE), ]
  
  lm_i <- lm(dist ~ speed, boot_sample)
  boot_coefs[[i]] <- tibble(speed = coef(lm_i)["speed"] %>% as.numeric,
                            intcpt = coef(lm_i)[1] %>% as.numeric)
}

mns <- boot_coefs %>% bind_rows() %>% 
  summarise_all(mean)

ses <- boot_coefs %>% bind_rows() %>% 
  summarise_all(sd) 

# Bootstrap 95% CI for R-Squared ------------------------------------------

library(boot)

# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")


# Bootstrap 95% CI for regression coefficients ----------------------------

library(boot)

# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}

# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)

# view results
results

plot(results, index=1) # intercept
plot(results, index=2) # wt
plot(results, index=3) # disp

# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # wt
boot.ci(results, type="bca", index=3) # disp
