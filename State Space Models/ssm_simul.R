

## LLT
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
