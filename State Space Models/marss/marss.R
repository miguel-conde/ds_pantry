library(tidyverse)
library(MARSS)



# Users Guide -------------------------------------------------------------


harborSealWA

harborSealWA[, -1] %>% ts(start = harborSeal[1,1]) %>% forecast::autoplot()

dat <- t(harborSealWA)
dat <- dat[2:nrow(dat), ] # remove the year row

dat

# One hidden state process for each observation time series

kemfit <- MARSS(dat)


kemfit$marss
kemfit$states

summary(kemfit)
summary(kemfit$model)

broom::tidy(kemfit)
broom::glance(kemfit)

coef(kemfit)

parvec <- coef(kemfit, type = "vector")
parvec


fitted(kemfit)
residuals(kemfit)
print(kemfit, what = "par")
print(kemfit, what = "Q")
tidy::tidy(kemfit)
tidy.marssMLE(kemfit)
logLik(kemfit)
AIC(kemfit)
residuals(kemfit) 
predict(kemfit, n.ahead = 12)
ggplot2::autoplot(predict(kemfit, n.ahead = 12))
forecast(kemfit, n.ahead = 12) 
ggplot2::autoplot(forecast(kemfit, n.ahead = 12))
plot(kemfit)
ggplot2::autoplot(kemfit)

# Kalman filter and smoother output. Expected value of X (states) conditioned on 
# all data, data 1 to t or data 1 to t −1. 
# MARSSkf(fit) returns the same in a list of matrices
# All the standard Kalman filter and smoother output (along with the lag-one
# covariance smoother output) is available using the tsSmooth and MARSSkf
# function. tsSmooth returns a data frame in long form. You need to
# pass in the type of conditioning you want (on all data, data 1 to t or data 1
# to t −1).
tsSmooth(kemfit)
# MARSSkf returns a list with all the filter and smoother output (including 
# variance matrices) in matrix and array form.
MARSSkf(kemfit)

# Analogous to MARSSkf(fit) but for the y equation
MARSShatyt(kemfit) # Analogous to:

kem.with.hess.CIs <- MARSSparamCIs(kemfit)
kem.with.hess.CIs

kem.w.boot.CIs <- MARSSparamCIs(kemfit, method = "parametric", nboot = 10)
# nboot should be more like 1000, but set low for example's sake
print(kem.w.boot.CIs)


## EXPLICITELY:
B <- Z <- diag(1, 5)
U <- matrix(c("u1", "u2", "u3", "u4", "u5"), 5, 1)
x0 <- A <- matrix(0, 5, 1)
R <- Q <- matrix(list(0), 5, 5)
diag(R) <- "r"
diag(Q) <- c("q1", "q2", "q3", "q4", "q5")

kemfit_2 <- MARSS(dat, 
                  model = list(B = B,
                               Z = Z,
                               U = U,
                               # x0 = x0,
                               A= A,
                               R = R,
                               Q = Q),
                  fit = TRUE, 
                  form = "marxss")

# With Optim
kemfit_bfgs <- MARSS(dat, method = "BFGS")

kemfit_bfgs2 <- MARSS(dat, method = "BFGS", inits = kemfit$par)

# Bootstrap parameter estimates
boot.params <- MARSSboot(kemfit,
                         nboot = 20, 
                         output = "parameters", 
                         sim = "parametric")$boot.params

# Data simulation
sim.data <- MARSSsimulate(kemfit, nsim = 2, tSteps = 100)$sim.data
kem.sim.1 <- MARSS(sim.data[, , 1])

kem.sim.2 <- kem.sim.1
kem.sim.2$model$data <- sim.data[, , 2]
MARSSkf(kem.sim.2)$logLik


# Replicate BSTS examples -------------------------------------------------


# Nowcasting --------------------------------------------------------------

library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

# Local Linear Trend + Seasonal

# Data
dat = data.frame(Yr = floor(lubridate::year(time(initial.claims)) + .Machine$double.eps),
                 Qtr = lubridate::quarter(time(initial.claims)), 
                 initial.claims) %>% 
  janitor::clean_names() %>% 
  t()

# Model
Z <- matrix(list(1,0), nrow = 1)
A <- matrix(list(0), nrow = 1)
R <- matrix(list("r"), nrow = 1)
B <- matrix(list(1, 0, 1, 1), nrow = 2)
U <- matrix(list(0, 0), nrow = 2)
Q <- matrix(list(0), nrow =2, ncol = 2)
diag(Q) <- c("s_mu", "s_beta")
x0 <- matrix(list(pi_mu, pi_beta), nrow = 1)
V0 <- matrix(list(0), nrow =2, ncol = 2)
diag(V0) <- c("s_mu_0", "s_beta_0")

llt_spec <- list(Z = Z,
                 A = A,
                 R = R,
                 B = B,
                 U = U,
                 Q = Q,
                 x0 = x0,
                 V0 = V0 #,
                 # G = G,
                 # H = H,
                 # L = L ???
                 )


llt_fit <- MARSS(dat["iclaims_nsa", ], 
                 model = llt_spec,
                 fit = TRUE, 
                 form = "marxss")

print(llt_fit, what = "R")
print(llt_fit, what = "Q")
print(llt_fit, what = "V0")

ggplot2::autoplot(predict(llt_fit, n.ahead = 52))

broom::tidy(llt_fit)
broom::glance(llt_fit)
