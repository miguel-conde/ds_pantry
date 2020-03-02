library(tidyverse)


sample_probe <- c(50, 60, 60, 64, 66, 66, 67, 69, 70, 74, 
                  76, 76, 77, 79, 79, 79, 81, 82, 82, 89)

z_test_2s <- function(x_sample, mu_0, sd_0) {
  
  sample_mean <- mean(x_sample)
  
  N <- length(x_sample)
  
  sem_0 <- sd_0 / sqrt(N)
  
  z_statistic <- (sample_mean - mu_0) / sem_0
  
  p_value <- 2*pnorm(q = z_statistic, lower.tail = FALSE)
  
  out <- c(z_statistic = z_statistic, 
           sem = sem_0,
           mean_estimate = sample_mean,
           p_value = p_value)
  
  return(out)
}

z_test_2s(sample_probe, mu_0 = 67.5, sd_0 = 9.5)

t_test_2s <- function(x_sample, mu_0, alpha = .05) {
  
  sample_mean <- mean(x_sample)
  
  sample_sd <- sd(x_sample)
  
  N <- length(x_sample)
  
  sem_sample <- sample_sd / sqrt(N)
  
  t_statistic <- (sample_mean - mu_0) / sem_sample
  
  p_value <- 2*pt(q = t_statistic, df = N - 1, lower.tail = FALSE)
  
  ci <- sample_mean + c(-1, 1) * qt(1 - alpha/2, N - 1, lower.tail = TRUE) * sem_sample 
  
  out <- c(t_statistic = t_statistic, 
           df = N - 1,
           mean_estimate = sample_mean,
           sd_estimate = sample_sd,
           ci = ci,
           sem = sem_sample,
           p_value = p_value)
  
  return(out)
}

t_test_2s(sample_probe, mu_0 = 67.5)

t.test(sample_probe, mu = 67.5)


####
lm_model <- lm(Sepal.Length ~ ., iris)
sum_lm_model <- summary(lm_model)

t_statistic <- sum_lm_model$coefficients[which(rownames(sum_lm_model$coefficient) == "Petal.Width"), "t value"]
d_f <- sum_lm_model$df[2]

2*pt(q = abs(t_statistic), df = d_f, lower.tail = FALSE)

sum_lm_model$coefficients[which(rownames(sum_lm_model$coefficient) == "Petal.Width"), "Pr(>|t|)"]

resid <- residuals(lm_model)
est_resid_variance <- sum(resid^2) / d_f
X_mat <- model.matrix(Sepal.Length ~ ., iris)

est_cov_mat_resid <- est_resid_variance * solve(t(X_mat) %*% X_mat)
diag(est_cov_mat_resid) %>% sqrt

sum_lm_model$coefficients[,2]

tab_coefs <- function(lm_model) {
  
  d_f <- lm_model$df.residual
  resid <- residuals(lm_model)
  est_resid_variance <- sum(resid^2) / d_f
  X_mat <- model.matrix(lm_model)
  
  se <- (est_resid_variance * solve(t(X_mat) %*% X_mat)) %>% 
    diag() %>% 
    sqrt
  
  estimates <- coef(lm_model)
  
  t_statistics <- estimates / se
  
  p_values <- 2*pt(q = abs(t_statistics), df = d_f, lower.tail = FALSE)
  
  out <- tibble(coefficients = names(estimates),
                estimate = estimates,
                std_error = se,
                t_value = t_statistics,
                p_value = p_values)
  
  out
}
