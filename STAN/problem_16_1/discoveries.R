library(rstan)
library(tidyverse)

eval_disc <- readr::read_csv(here::here("STAN", "data", "evaluation_discoveries.csv"))

X <- eval_disc$discoveries
N <- length(X)

set.seed = 1

fit <- stan(here::here("STAN", "problem_16_1", "discoveries.stan"), 
            data = list(N = N, X = X), 
            iter = 1000, 
            chains = 4, 
            warmup = 500)

# Lambda and lp should both have a value of Rˆ ≈ 1.
print(fit)

lambda_post <- rstan::extract(fit, 'lambda')[[1]]

# Find the central posterior 80% credible interval for λ.
quantile(X_post, probs = c(0.1, 0.9))

print(fit, pars='lambda', probs = c(0.1, 0.9))

qplot(lambda_post)

plot(eval_disc, type = "o")
