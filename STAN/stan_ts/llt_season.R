library(rstan)
library(bayesplot)
library(tidybayes)
library(tidyverse)

sales_ts <- readRDS(here::here("STAN", "data", "sales_ts.Rds"))

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)

# LLT ---------------------------------------------------------------------

Y = sales_ts$IT_existing

fit_llt <- stan(here::here("STAN", "stan_ts", "llt.stan"), 
                data = list(N = length(Y), y = Y),
                iter = 200, 
                chains = 4)
