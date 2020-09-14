library(tidyverse)
library(dlm)

# vignette - dlm: an R package for Bayesian analysis of Dynamic Linear Models

# DLM ---------------------------------------------------------------------

# y[t] = F[t] theta[t] + v[t]
# theta[t] = F[t] theta[t-1] + w[t]

# RW plus noise model (aka Local Level) -----------------------------------

# y[t] = theta[t] + v[t]
# theta[t] = theta[t-1] + w[t]

dlm(FF = 1, V = 0.8, GG = 1, W = 0.1, m0 = 0, C0 = 100)
dlmModPoly(order = 1, dV = 0.8, dW = 0.1, C0 = 100)

myMod <- dlmModPoly()
FF(myMod)
W(myMod)
GG(myMod)
W(myMod)
m0(myMod)
C0(myMod)

# dlmModARMA ARMA process
# dlmModPoly nth order polynomial DLM
# dlmModReg Linear regression
# dlmModSeas Periodic { Seasonal factors
# dlmModTrig Periodic { Trigonometric form

# With the exception of dlmModARMA, which handles also the multivariate case, 
# the other creator functions are limited to the case of univariate observations. 
# More complicated DLMs can be explicitely defined using the general function dlm.


# Combining models: sums and outer sums -----------------------------------


# For example, suppose one wants to model a time series as a sum of a
# stochastic linear trend and a quarterly seasonal component, observed with
# noise. The model can be set up as follows:

myMod <- dlmModPoly() + dlmModSeas(4)

# two time series, the frst following a
# stochastic linear trend and the second a random noise plus a quarterly seasonal
# component, both series being observed with noise. A joint DLM for
# the two series, assuming independence, can be set up in R as follow
dlmModPoly(dV = 0.2, dW = c(0, 0.5)) %+%
  (dlmModSeas(4, dV = 0, dW = c(0, 0, 0.35)) +
     dlmModPoly(1, dV = 0.1, dW = 0.03))


# Time-varying models -----------------------------------------------------

# A object m of class dlm may contain components named JFF,
# JV, JGG, JW, and X. The first four are matrices of integers of the same
# dimension as FF, V, GG, W, respectively, while X is an n by m matrix,
# where n is the number of observations in the data. Entry (i; j) of JFF is
# zero if the corresponding entry of FF is time invariant, or k if the vector
# of values of Ft[i; j] at diferent times is stored in X[,k]

# For example, a dynamic linear regression
# can be modeled as

# y[t] = alpha[t] + x[t] beta[t] + v[t]
# alpha[t] = alpha[t-1] + w_alpha[t]
# beta[t] = beta[t-1] + w_beta[t]

u <- rnorm(25)
myMod <- dlmModReg(u, dV = 14.5)
myMod$JFF
head(myMod$X)


# Maximum likelihood estimation -------------------------------------------

# Consider the Nile river data set. 
plot(Nile)

# A reasonable model can be a random
# walk plus noise, with unknown system and observation variances. Parametrizing
# variances on a log scale, to ensure positivity, the model can be build using
# the function defined below.
buildFun <- function(x) {
  dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2]))
}

# Starting the optimization from the arbitrary (0; 0) point, the MLE of the
# parameter can be found as follows.
fit <- dlmMLE(Nile, parm = c(0,0), build = buildFun)
fit$conv

dlmNile <- buildFun(fit$par)

V(dlmNile)

W(dlmNile)

# As a less trivial example, suppose one wants to take into account a jump
# in the floow of the river following the construction of Ashwan dam in 1898.
# This can be done by inflating the system variance in 1899 using a multiplier
# bigger than one.

buildFun <- function(x) {
  m <- dlmModPoly(1, dV = exp(x[1]))
  m$JW <- matrix(1)
  m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile))
  j <- which(time(Nile) == 1899)
  m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))
  return(m)
}


dlmNileJump <- buildFun(fit$par)
V(dlmNileJump)
dlmNileJump$X[c(1, which(time(Nile) == 1899)), 1]

# Filtering, smoothing and forecasting ------------------------------------

# Filtering ---------------------------------------------------------------

# Taking the estimated parameters as known, we can compute the filtering 
# distribution using dlmFilter. If n is the number of observations in the data 
# set, dlmFilter returns the mean and variance of the n+1 Filtering distributions
# that can be computed from the data, i.e., the distribution of theta[t] given 
# y1:t for t = 0,1, ..., n (for t = 0, this is by convention the prior 
# distribution of theta[0]).
nileJumpFilt <- dlmFilter(Nile, dlmNileJump)
plot(Nile, type = 'o', col = "seagreen")
lines(dropFirst(nileJumpFilt$m), type = 'o',
      pch = 20, col = "brown")

v <- unlist(dlmSvd2var(nileJumpFilt$U.C, nileJumpFilt$D.C))
pl <- dropFirst(nileJumpFilt$m) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(nileJumpFilt$m) + qnorm(0.95, sd = sqrt(v[-1]))

lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")


# Smoothing ---------------------------------------------------------------

nileJumpSmooth <- dlmSmooth(nileJumpFilt)
plot(Nile, type = 'o', col = "seagreen")
lines(dropFirst(nileJumpSmooth$s), type = 'o', pch = 20, col = "brown")
v <- unlist(dlmSvd2var(nileJumpSmooth$U.S, nileJumpSmooth$D.S))
pl <- dropFirst(nileJumpSmooth$s) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(nileJumpSmooth$s) + qnorm(0.95, sd = sqrt(v[-1]))
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")


# As a second example, consider the UK gas consumption data set. On
# a logarithmic scale, this can be reasonably modeled by a DLM containing
# a quarterly seasonal component and a local linear trend, in the form of an
# integrated random walk. We first estimate the unknown variances by ML.
lGas <- log(UKgas)
dlmGas <- dlmModPoly() + dlmModSeas(4)
buildFun <- function(x) {
  diag(W(dlmGas))[2:3] <- exp(x[1:2])
  V(dlmGas) <- exp(x[3])
  return(dlmGas)
}
(fit <- dlmMLE(lGas, parm = rep(0, 3), build = buildFun))$conv

dlmGas <- buildFun(fit$par)
drop(V(dlmGas))
diag(W(dlmGas))[2:3]

gasSmooth <- dlmSmooth(lGas, mod = dlmGas)
x <- cbind(lGas, dropFirst(gasSmooth$s[,c(1,3)]))
colnames(x) <- c("Gas", "Trend", "Seasonal")
plot(x, type = 'o', main = "UK Gas Consumption")


# Forecasting -------------------------------------------------------------

gasFilt <- dlmFilter(lGas, mod = dlmGas)
gasFore <- dlmForecast(gasFilt, nAhead = 20)
sqrtR <- sapply(gasFore$R, function(x) sqrt(x[1,1]))
pl <- gasFore$a[,1] + qnorm(0.05, sd = sqrtR)
pu <- gasFore$a[,1] + qnorm(0.95, sd = sqrtR)
x <- ts.union(window(lGas, start = c(1982, 1)),
              window(gasSmooth$s[,1], start = c(1982, 1)),
              gasFore$a[,1], pl, pu)
plot(x, plot.type = "single", type = 'o', pch = c(1, 0, 20, 3, 3),
     col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"),
     ylab = "Log gas consumption")
legend("bottomright", legend = c("Observed",
                                 "Smoothed (deseasonalized)",
                                 "Forecasted level", "90% probability limit"),
       bty = 'n', pch = c(1, 0, 20, 3, 3), lty = 1,
       col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"))
