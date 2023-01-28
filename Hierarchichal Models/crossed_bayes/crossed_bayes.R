library(tidyverse)
library(rstan)
library(tidybayes)

xxx <- function(fit) {
  check_treedepth(fit)
  check_energy(fit)
  check_divergences(fit)
  # check_n_eff(fit)
  # check_rhat(fit)  
  check_hmc_diagnostics(fit)
}

STAN_FILE <- here::here("Hierarchichal Models", "crossed_bayes", "crossed_1.stan")

get_mode <- function(v, method = c("first", "all")) {
  
  method <- match.arg(method)
  
  uniqv <- unique(v)
  tab <- tabulate(match(v, uniqv))
  
  if (method == "first") out <- uniqv[which.max(tab)]
  else out <- uniqv[tab == max(tab)]
  
  return(out)
}

data("heights", package = "modelr")
heights
summary(heights)

weight_lm <- lm(weight ~ height, heights %>% drop_na)

tgt_var <- "income"
group_vars <- c("marital", "sex")
group_id_vars <- paste0("id_", group_vars)
pred_vars <-  c(
  "height",
  "weight",
  "age",     
  "education",
  "afqt" 
) %>% 
  sort()
names_coefs <- c(paste0("beta_", pred_vars))
names_contribs <- c(paste0("contrib_", pred_vars))
names_perc_contribs <- paste0("perc_", names_contribs)


model_data <- heights %>% 
  select(all_of(c(tgt_var, group_vars,pred_vars))) %>% 
  rename_at(all_of(group_vars), ~ paste0("id_", .)) %>% 
  mutate(sex = id_sex, .before = id_sex) %>% 
  mutate(marital = id_marital, .before = id_marital) %>% 
  mutate(afqt = ifelse(is.na(afqt), median(afqt, na.rm = TRUE), afqt)) %>% 
  mutate_at(all_of(c(group_id_vars)), ~ as.numeric(factor(.)))

model_data$education[is.na(model_data$education)] <- get_mode(heights$education)          
model_data$weight[is.na(model_data$weight)] <- 
  predict(weight_lm, heights %>% filter(is.na(weight)))

model_data_scaled <- model_data %>% 
  mutate_if(is.numeric, ~ (. -mean(.)) / sd(.))

options(mc.cores = parallel:: detectCores())
rstan_options(auto_write = TRUE)

model_multi <- stan_model(file = STAN_FILE)

fit_crossed_sex_marital <- 
  sampling(model_multi, 
           iter = 100, chains = 1, 
           data = list(n         = nrow(model_data),
                       Y         = model_data$income,
                       num_preds = length(pred_vars) + 1,
                       X         = as.matrix(model_data_scaled %>% 
                                               select(all_of(pred_vars)) %>% 
                                               mutate(baseline = 1, .before = 1)),
                       N_sex     = length(model_data$id_sex %>% unique()),
                       N_marital = length(model_data$marital %>% unique()),
                       marital   = model_data$id_marital,
                       sex       = model_data$id_sex),
           verbose = TRUE)


print(fit_linear_multi_12, c("bar_beta", "sigma_beta"))
print(fit_crossed_sex_marital, c("betas_marital"))
print(fit_crossed_sex_marital, c("betas_sex"))

print(fit_linear_multi_12, c("sigma_beta"))

pairs(fit_linear_multi_12, pars = c("bar_beta", "sigma_beta"))

precis(fit_linear_multi_12)

            