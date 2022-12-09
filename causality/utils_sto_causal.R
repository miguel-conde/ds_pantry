# https://royalsocietypublishing.org/doi/10.1098/rspa.2021.0835

library(tidyverse)

make_probe_x_y <- function(in_data, causa, efecto, J = 10) {
  
  probe_x_y <- in_data
  for (i in -J:J) {
    FUN <- ifelse(i > 0, lag, lead)
    
    probe_x_y <- probe_x_y %>% 
      mutate(!!sym(paste0("lag_", i)) := FUN(!!sym(causa), n = abs(i)))
  }
  
  probe_x_y <- probe_x_y %>% drop_na()
  
  return(probe_x_y)
}

cost_fun <- function(g, L, J, X, Y, psi, lambda) {
  
  v_t <- Y - X %*% g
  mu_v <- mean(v_t)
  mu <- rep(mu_v, L - 2*J) %>% as.matrix
  
  t(Y - mu - X %*% g) %*% (Y - mu - X %*% g) + 
    lambda * t(g) %*% t(psi) %*% psi %*% g
  
}

make_psi <- function(J) {
  
  out <- matrix(0, nrow = 2*J-1, ncol = 2*J+1)
  
  for (i in 1:nrow(out)) {
    for (j in 1:ncol(out)) {
      if (j == i+1) out[i, j] <- 2
      else if (abs(j-i-1) == 1) out[i,j] <- 1
    }
  }
  return(out)
}

est_causal <- function(in_data, causa, efecto, J = 20, lambda = 10) {
  
  L <- nrow(in_data)
  probe <- make_probe_x_y(in_data %>% select(all_of(c(causa, efecto))), causa, efecto, J = J)
  X <- probe %>% select(-all_of(c(causa, efecto))) %>% as.matrix()
  Y <- probe %>% select(all_of(efecto)) %>% as.matrix()
  psi <- make_psi(J)
  
  res_optim <- optim(rep(0, 2*J+1), fn = cost_fun, 
                     L = L, J= J, X= X, Y = Y, psi = psi, lambda = lambda,
                     method = "L-BFGS-B", lower = 0)
  
  out <- list(X = X,
              Y = Y,
              L = L,
              J = J,
              psi = psi,
              res_optim = res_optim)
  
  class(out) <- c("est_causal", class(out))
  
  return(out)
}

evr <- function(obj) {
  # Explained variance ratio
  
  v_t <- obj$Y - obj$X %*% matrix(obj$res_optim$par)
  mu_v <- mean(v_t)
  mu <- rep(mu_v, obj$L - 2*obj$J) %>% as.matrix()
  hat_gamma_nu <- 1 / (obj$L - 2*obj$J) * t(obj$Y - mu - obj$X %*% matrix(obj$res_optim$par)) %*% 
    (obj$Y - mu - obj$X %*% matrix(obj$res_optim$par))
  hat_gamma_y = var(obj$Y)
  
  e <- 1 - hat_gamma_nu / hat_gamma_y 
  
  return(e)
}

autoplot.est_causal <- function(obj) {
  
  gg_res_optim <- tibble(x = 1:ncol(obj$X), 
                         time_lag = colnames(obj$X), 
                         irf = obj$res_optim$par)
  
  ggplot(data = gg_res_optim, mapping = aes(x = x, y = irf, color = x > obj$J)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = obj$J+1, linetype = 2) +
    scale_x_continuous(breaks = 1:(2*obj$J+1), labels = gg_res_optim$time_lag) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


