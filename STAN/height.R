library(rstan)

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)


# Heights Simple Example --------------------------------------------------

# Fake data
Y <- rnorm(10, 1.5, 0.2)

fit <- stan(here::here("STAN", "height.stan"), 
            iter = 200, chains = 4, data = list(N = length(Y), y = Y))

print(fit, probs = c(0.25, 0.5, 0.75))

library(ggplot2) 

mu <- extract(fit, "mu")[[1]] 

qplot(mu)

library(shinystan) 

aFit <- as.shinystan(fit) 

launch_shinystan(aFit)


# Heights and weights Example ---------------------------------------------

N <- 100

# Fake data
X <- rnorm(N, 60, 10)
beta <- 0.3
sigma <- 0.3

Y <- beta * log(X) + rnorm(N, 0, sigma)

fit <- stan(here::here("STAN", "heights_weights.stan"), 
            iter = 200, chains = 4, data = list(N = length(Y), y = Y, x = X))

print(fit, probs = c(0.25, 0.5, 0.75))

mu <- extract(fit, "mu")[[1]] 

qplot(mu)

aFit <- as.shinystan(fit) 

launch_shinystan(aFit)


# Heights with transformed parameters and transformed data ----------------

# Fake data
Y <- rnorm(10, 1.6, sqrt(5.1))

fit <- stan(here::here("STAN", "height_2.stan"), 
            iter = 1000, chains = 4, data = list(N = length(Y), y = Y))

print(fit, probs = c(0.25, 0.5, 0.75))

library(ggplot2) 

mu <- extract(fit, "mu")[[1]] 

qplot(mu)

library(shinystan) 

aFit <- as.shinystan(fit) 

launch_shinystan(aFit)


# Independent samples -----------------------------------------------------

fit <- stan(here::here("STAN", "ind_samples.stan"), 
            data = list(mu = 10, kappa = 5), 
            algorithm = "Fixed_param", 
            iter = 4000, 
            chains = 1)

Y <- extract(fit, "y")[[1]] 

qplot(Y) + geom_histogram(binwidth = 2)


# Translate and compile a model for later use -----------------------------

aModel <- stan_model(here::here("STAN", "ind_samples.stan")) 

fit <- sampling(aModel, 
                data = list(mu = 10, kappa = 5), 
                algorithm = "Fixed_param", 
                iter = 4000, 
                chains = 1)

# GROUPS ------------------------------------------------------------------

# Suppose that you have individual data for three studies of individuals’ 
# heights. In particular, imagine that you have the following data in R, and you 
# want to generate a separate estimate of the mean population height for each of 
# the three cases: 
X_1 <- c( 1.53, 1.67, 1.52) 
X_2 <- c( 1.75, 1.62, 1.87, 1.95) 
X_3 <- c( 1.25, 1.75)

# What is the best way to pass this data to Stan?

### SOLUTION 1

# A nice trick is to combine all data into one long data vector. We then create 
# helper arrays in R that indicate the size of each data set and its starting 
# position in the long array: 
Y     <- c(1.53, 1.67, 1.52, 1.75, 1.62, 1.87, 1.95, 1.25, 1.75) 
S     <- c(3, 4 ,2)     # sample sizes of each study 
index <- c(1, 4, 8)     # start position of each

# Fake data
N <- 100
Y <- rnorm(N, 1.5, 0.2)
S <- c(20, 50, 30)
index = c(1, 21, 71)

fit <- stan(here::here("STAN", "heights_groups_1.stan"), 
            data = list(N = N, K = 3, Y = Y, S = S, index = index), 
            iter = 1000, 
            chains = 1, 
            control = list(adapt_delta = 0.95, stepsize = 0.01))


mu <- extract(fit, "mu")[[1]] 

print(fit, probs = c(0.25, 0.5, 0.75))

### SOLUTION 2

# An alternative way to estimate this type of model is to pass an array which 
# identifies the group to which each observation belongs:

groups = c(rep(1, 20), rep(2, 50), rep(3, 30))

fit <- stan(here::here("STAN", "heights_groups_2.stan"), 
            data = list(N = N, K = 3, Y = Y, groups = groups), 
            iter = 1000, 
            chains = 1, 
            control = list(adapt_delta = 0.95, stepsize = 0.01))


mu <- extract(fit, "mu")[[1]] 

print(fit, probs = c(0.25, 0.5, 0.75))
