library(tidyverse)


# LLT ---------------------------------------------------------------------


## 
N <- 1000

set.seed(2020)

epsilon_t <- rnorm(N, 0, 10)
eta_mu_t <- rnorm(N, 0, 1000)
eta_delta_t <- rnorm(N, 0, 1)

mu_0 <- 10
delta_0 <- 1

delta_t_1 <- (c(delta_0, rep(0, N-1) + eta_delta_t) %>% cumsum())

mu_t_1 <- (c(mu_0, rep(0, N-1) + delta_t_1 + eta_mu_t) %>% cumsum())

y_t_1 = mu_t_1 + epsilon_t

plot(y_t_1, type = "l")

llt <- function(N = 1000, 
                eta_delta_mn = 0, eta_delta_sigma = 1, 
                eta_mu_sigma = 1000, 
                epsilon_sigma = 10, 
                mu_0 = 10,
                delta_0 = 1, 
                seed = 2020) {
  set.seed(seed)
  # browser()
  epsilon_t <- rnorm(N, 0, epsilon_sigma)
  eta_mu_t <- rnorm(N, 0, eta_mu_sigma)
  eta_delta_t <- rnorm(N, eta_delta_mn, eta_delta_sigma)
  
  delta_t_1 <- (c(delta_0, rep(0, N-1)) + eta_delta_t) %>% cumsum()
  
  mu_t_1 <- (c(mu_0, rep(0, N-1)) + delta_t_1 + eta_mu_t) %>% cumsum()
  
  y_t_1 = mu_t_1 + epsilon_t
  
  y_t_1

}


plot(llt(), type = "l")

library(manipulate)
manipulate(plot(llt(eta_delta_mn = mn_delta,
                    eta_delta_sigma = s_delta,
                    eta_mu_sigma = s_mu,
                    epsilon_sigma = s_epsilon,
                    mu_0 = lvl_0), type = "l"),
           mn_delta = slider(-1, 1, 0, step = .1),
           s_delta = slider(0, 10, 1, step = .1),
           s_mu = slider(0, 2000, 1000, step = 100),
           s_epsilon = slider(0, 20, 10, step = 1),
           lvl_0 = slider(-100, 100, 0, step = 10),
           slope_0 = slider(-1, 1, 0, step = .1))


# SEASONALITY -------------------------------------------------------------

simul_season <- function(t, lambda, alpha, beta) {
  
  alpha * cos(lambda*t) + beta * sin(lambda*t)
}

simul_season(1:(2*12), 2*pi/12, 1, 1) %>% plot(type = "o")


season_matrix <- function(lambda) {
  matrix(c(cos(lambda), sin(lambda),
           -sin(lambda), cos(lambda)),
         byrow = TRUE, nrow = 2)
}

season_matrix(2*pi/12)

phi_0 <- 1 # alpha
conf_phi_0 <-  1 # beta

phi_0 <- matrix(c(phi_0, conf_phi_0))


phi_t_1 <- phi_0

phi_t <- vector(mode = 'list', length = 24)

for (i in 1:24) {
  phi_t[[i]] <- season_matrix(2*pi/12) %*% phi_t_1  + matrix(rnorm(2))
  phi_t_1 <- phi_t[[i]]
}

phi_t %>% map_dbl(~ .[1,1]) %>% plot(type = "o")