library(tidyverse)
library(quantmod)
library(timetk)

goog = getSymbols("GOOG",src="yahoo", auto.assign = FALSE)
saveRDS(goog, here::here("Monte Carlo", "data", "goog.Rds"))
dji = getSymbols("^DJI",src="yahoo", auto.assign = FALSE)
saveRDS(dji, here::here("Monte Carlo", "data", "dji.Rds"))
ibex = getSymbols("^IBEX",src="yahoo", auto.assign = FALSE)
saveRDS(ibex, here::here("Monte Carlo", "data", "ibex.Rds"))
sp500 = getSymbols("^GSPC",src="yahoo", auto.assign = FALSE)
saveRDS(sp500, here::here("Monte Carlo", "data", "sp500.Rds"))

# probe <- dji$DJI.Close
probe <- sp500$GSPC.Close
periodicity(probe)

plot(probe)
acf(diff(probe)[-1])
pacf(diff(probe)[-1])

probe_returns <- diff(log(probe))[-1]
acf(probe_returns)
pacf(probe_returns)
plot(probe_returns)
plot((probe_returns+1) %>% cumprod)

plot(probe_returns)
returns_sd_ma_30 <- zoo::rollapply(probe_returns, 30, sd)[-(1:30)]
lines(returns_sd_ma_30, col = "red", lty = 2)
acf(returns_sd_ma_30)
pacf(returns_sd_ma_30)

arima_returns_sd_ma_30 <- returns_sd_ma_30 %>% forecast::auto.arima()
summary(arima_returns_sd_ma_30)

plot(returns_sd_ma_30)
lines(as.xts(fitted(arima_returns_sd_ma_30) %>% as.numeric, 
             order.by = index(returns_sd_ma_30)), col = "blue")

R2 <- 1 - var(residuals(arima_returns_sd_ma_30)) / var(returns_sd_ma_30)
R2


# MC1 ---------------------------------------------------------------------

N <- length(probe_returns)

total_returns <- vector(mode = "list", length = 100)
for (i in 1:100) {
  # Bootstrap
  returns_i <- sample(as.numeric(probe_returns), size = N, replace = TRUE)
  
  total_returns[[i]] <- cumprod(returns_i+1)
}

M <- max(sapply(total_returns, max))
m <- min(sapply(total_returns, min))
plot(x=1:N, y=rep(0, N), type ="n", ylim = c(m, M))
for (i in 1:100) {
  
  lines(1:N, total_returns[[i]], col = i)
}

sapply(total_returns, function(x) x[N]) %>% hist()
sapply(total_returns, function(x) x[N]) %>% mean()
sapply(total_returns, function(x) x[N]) %>% sd()
(sapply(total_returns, function(x) x[N]) < 0) %>% mean()
(sapply(total_returns, function(x) x[N]) > 1) %>% mean()
(sapply(total_returns, function(x) x[N]) > 5) %>% mean()
(sapply(total_returns, function(x) x[N]) > 10) %>% mean()
(sapply(total_returns, function(x) x[N]) > 20) %>% mean()

# MC2 ---------------------------------------------------------------------

### Cauchy

N <- length(probe_returns)

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_code_cauchy <- "
data {
  int<lower=0> N;         // number of returns 
  real y[N];              // returns
}

parameters {
  real location;          // Cauchy distr. parameters
  real scale;      
}

model {
  y ~ cauchy(location, scale);
}
"

fit_cauchy <- stan(model_code = stan_code_cauchy, 
                   data = list(N = N,
                               y = as.numeric(probe_returns)))
fit_cauchy

location_cauchy <- rstan::extract(fit_cauchy, "location")[[1]] %>% mean()
scale_cauchy <- rstan::extract(fit_cauchy, "scale")[[1]] %>% mean()

probe_returns %>% hist(breaks = 100, freq = FALSE)
lines(density(probe_returns), col = "blue")
lines(seq(-.1, .1, .001), 
      dcauchy(seq(-.1, .1, .001), 
              location = location_cauchy, 
              scale = scale_cauchy), 
      col = "red")

plot(x=1:N, y=rep(0, N), type ="n", ylim = c(-1000000, 1000000))
for(i in 1:100) {
  i_returns <- (rcauchy(N, 
                        location = location_cauchy, 
                        scale = scale_cauchy) + 1) %>% cumprod()
  lines(1:N, i_returns, col = i)
}

### Laplace
probe_returns %>% hist(breaks = 100, freq = FALSE)
lines(density(probe_returns), col = "blue")
N <- length(probe_returns)

stan_code_laplace <- "
data {
  int<lower=0> N;         // number of returns 
  real y[N];              // returns
}

parameters {
  real location;          // double exponential (Laplace) distr. parameters
  real scale;      
}

model {
  y ~ double_exponential(location, scale);
}
"

fit_laplace <- stan(model_code = stan_code_laplace, 
                    data = list(N = N,
                                y = as.numeric(probe_returns)))
fit_laplace

location_laplace <- rstan::extract(fit_laplace, "location")[[1]] %>% mean()
scale_laplace <- rstan::extract(fit_laplace, "scale")[[1]] %>% mean()


probe_returns %>% hist(breaks = 100, freq = FALSE)
lines(density(probe_returns), col = "blue")
library(nimble)
lines(seq(-.1, .1, .001), 
      ddexp(seq(-.1, .1, .001), 
            location = location_laplace, 
            scale = scale_laplace), 
      col = "red")

plot(x=1:N, y=rep(0, N), type ="n", ylim = c(0, 50))
total_returns <- vector(mode = "list", length = 100)
for(i in 1:100) {
  total_returns[[i]] <- (rdexp(N, 
                     location = location_laplace, 
                     scale = scale_laplace) + 1) %>% cumprod()
  lines(1:N, total_returns[[i]], col = i)
}

sapply(total_returns, function(x) x[N]) %>% hist(freq = FALSE)
sapply(total_returns, function(x) x[N]) %>% density %>% lines(col = "blue")
sapply(total_returns, function(x) x[N]) %>% mean()
sapply(total_returns, function(x) x[N]) %>% sd()
(sapply(total_returns, function(x) x[N]) < 0) %>% mean()
(sapply(total_returns, function(x) x[N]) > 1) %>% mean()
(sapply(total_returns, function(x) x[N]) > 5) %>% mean()
(sapply(total_returns, function(x) x[N]) > 10) %>% mean()
(sapply(total_returns, function(x) x[N]) > 20) %>% mean()
