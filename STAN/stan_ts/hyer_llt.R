library(rstan)
library(bayesplot)
library(tidybayes)
library(tidyverse)

sales_ts <- readRDS(here::here("STAN", "data", "sales_ts.Rds")) %>% 
  gather(ctry_segment, value, -date) %>% 
  separate(ctry_segment, into = c("country", "segment")) %>% 
  mutate_at(vars(country, segment), ~ factor(.))

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)

# LLT ---------------------------------------------------------------------

Y = sales_ts$value %>% scale() %>% as.numeric()

N = length(Y)

K_ctries <- sales_ts$country %>% unique %>% length()

ctries <- sales_ts$country %>% as.integer()

fit_hyer_llt <- stan(here::here("STAN", "stan_ts", "hyer_llt.stan"), 
                  data = compose_data(sales_ts %>% 
                                        filter(segment == "existing") %>% 
                                        dplyr::select(-date, -segment) %>%
                                        rename(y = value)),
                  iter = 2000, 
                  chains = 4)

plot(Y, type = "l")

as.data.frame(fit_llt_1) %>% 
  select(starts_with("level[1]")) %>% 
  colMeans() %>% 
  lines(col = "blue")




library(loo)

logLikelihood_1 <- extract_log_lik(fit_llt_1, "logLikelihood")
WAIC_1 <- waic(logLikelihood_1)

logLikelihood_2 <- extract_log_lik(fit_llt_1, "logLikelihood")
WAIC_2 <- waic(logLikelihood_2)

logLikelihood_mine <- extract_log_lik(fit_llt, "logLikelihood")
WAIC_mine <- waic(logLikelihood_mine)

compare(WAIC_1, 
        WAIC_2, 
        WAIC_mine)
loo_compare(WAIC_1, 
            WAIC_2, 
            WAIC_mine)

LOO_1 <- loo(logLikelihood_1)
LOO_2 <- loo(logLikelihood_2)
LOO_mine <- loo(logLikelihood_mine)

compare(LOO_1, 
        LOO_2, 
        LOO_mine)
loo_compare(LOO_1, 
            LOO_2, 
            LOO_mine)
