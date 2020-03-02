library(tidyverse)

### TLC

set.seed(123)

N <- 10000

## POPULATION

pop <- runif(10*N)

mu_X <- mean(pop)

sd_X <- sd(pop)


samples <- sapply(1:10,
                  function(x) {
                    sample(pop, N, TRUE)
                  }) %>% as_tibble

avgs_samples <- samples %>%
  summarise_all(list(avg = mean)) %>% unlist()

est_mu_X <- avgs_samples %>% mean

est_mu_X

mu_X

est_sd_X <- (avgs_samples %>% sd) * sqrt(N)

est_sd_X

sd_X

### BIAS / VARIANCE

set.seed(123)

X <- rnorm(N)

var_irr <- 2
f <- function(X) 3*X

Y <- f(X) + rnorm(N, mean = 0, sd = var_irr^.5)

ds <- tibble(X, Y)

lm_fit <- lm(Y ~ X, ds)

y_hat <- predict(lm_fit, newdata = ds)

# https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff

mse <- Metrics::mse(f(X), y_hat)

mse

bias <- mean(f(X) - mean(y_hat))

var_est <- sd(y_hat)^2

bias^2 + var_est + var_irr

library(caret)

build_resample_y_hat <- function(ds, model_obj, idx) {
  unique_idx <- unique(idx)
  
  x <- ds[unique_idx, ]
  y_hat <- predict(model_obj, newdata = x)
  
  out <- tibble(idx = unique_idx) %>% 
    bind_cols(x) %>% 
    bind_cols(tibble(y_hat = y_hat))
  
  return(out)
}

calc_bias_vars <- function(Y, ds, idx_resamps, res_resample) {
  
  # browser()
  res_bootstrap <- tibble(idx =1:length(Y)) %>% 
    bind_cols(ds) 
  
  for(i in seq_along(idx_resamps)) {
    res_bootstrap <- res_bootstrap %>% 
      full_join(res_resample[[i]], by = c("idx", "X", "Y"))
  }
  
  bootstrap_cols <- (names(res_bootstrap) %>% 
                       str_detect("\\..*$"))
  names(res_bootstrap)[bootstrap_cols] <- 
    paste0("y_hat_", 1:length(idx_resamps))
  
  varianzas <- (res_bootstrap %>% select(starts_with("y_hat")) %>% 
                  apply(1, sd, na.rm = TRUE))^2
  bias_2 <- (res_bootstrap$Y - (res_bootstrap %>% 
                                  select(starts_with("y_hat")) %>% 
                                  apply(1, mean, na.rm = TRUE)))^2
  
  out <- res_bootstrap %>% select(-starts_with("y_hat")) %>% 
    bind_cols(tibble(bias_2 = bias_2, var = varianzas)) %>% 
    mutate(mse_ests = bias_2 + var)
  
  return(out)
} 


idx_resamps <- createResample(Y, times = 10, list = TRUE)

res <- idx_resamps %>% lapply(function(idx) {
  lm_fit <- lm(Y ~ X, ds[idx,])
  
  out <- build_resample_y_hat(ds, lm_fit, idx)
  
  return(out)
})

out <- calc_bias_vars(Y, ds, idx_resamps, res)
out %>% summarise_all(~mean(.x, na.rm = TRUE))

#### WITH CV
# https://daviddalpiaz.github.io/r4sl/biasvariance-tradeoff.html
# https://www.cs.upc.edu/~belanche/Docencia/dm2/MATERIAL/1.%20ParamEst%20-%20Bias%20&%20Var/DMII-Class1.pdf
# https://cedar.buffalo.edu/~srihari/CSE676/5.4%20MLBasics-Estimators.pdf
# http://users.monash.edu/~webb/Files/WebbConilione06.pdf

folds <- createFolds(ds$Y)

res <- vector(mode = "list", length = length(folds))

for (f in seq_along(folds)) {
  
  idx_train <- folds[[f]]
  train_set <- ds[idx_train, ]
  valid_set <- ds[-folds[[f]], ]
  
  idx_train_resamps <- createResample(train_set$Y, times = 10, list = TRUE)
  idx_valid_resamps <- createResample(valid_set$Y, times = 10, list = TRUE)
  
  aux <- mapply(FUN = function(idx_train, idx_valid) {
    lm_fit <- lm(Y ~ X, ds[idx_train,])
    
    out_train <- build_resample_y_hat(ds, lm_fit, idx_train)
    
    out_valid <- build_resample_y_hat(ds, lm_fit, idx_valid)
    
    out <- list(out_train = out_train,
         out_valid = out_valid)
    
    return(out)
  }, idx_train_resamps, idx_valid_resamps)
  # browser()
  out_train <- calc_bias_vars(ds[idx_train,"Y", drop = TRUE], 
                              ds[idx_train,], 
                              idx_train_resamps, 
                              aux["out_train", ]) 
  avg_train <- out_train %>% select(4:6) %>% summarise_all(mean, na.rm = TRUE)
  sd_train  <- out_train %>% select(4:6) %>% summarise_all(sd, na.rm = TRUE)
  
  out_valid <- calc_bias_vars(ds[-idx_train,"Y", drop = TRUE], 
                              ds[-idx_train,], 
                              idx_valid_resamps, 
                              aux["out_valid", ]) 
  avg_valid <- out_valid %>% select(4:6) %>% summarise_all(mean, na.rm = TRUE)
  sd_valid  <- out_valid %>% select(4:6) %>% summarise_all(sd, na.rm = TRUE)
  
  res[[f]] <- list(avg_train = avg_train,
                   sd_train = sd_train,
                   avg_valid = avg_valid,
                   sd_valid = sd_valid,
                   out_train = out_train,
                   out_valid = out_valid)
}

res_train <- lapply(res, function(x) x$avg_train) %>% bind_rows()
res_train %>% summarise_all(mean)
res_train %>% summarise_all(sd)

res_valid <- lapply(res, function(x) x$avg_valid) %>% bind_rows()
res_valid %>% summarise_all(mean)
res_valid %>% summarise_all(sd)

res_train - res_valid

####
tb_data <- tibble(a1 = 1:10, a2 = 11:20, b1 = 21:30, b2 = 31:40, c= 41:50)

str_start <- "a"
pattern <- paste0("^", str_start), 
tb_data %>% 
  mutate(!!sym(str_start) := rowSums(tb_data %>% 
                                      select(starts_with(str_start)))) %>% 
  select(-starts_with(str_start))

