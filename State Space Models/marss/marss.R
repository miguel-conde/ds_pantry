library(tidyverse)
library(MARSS)



# 1 Users Guide -----------------------------------------------------------


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
# tidy::tidy(kemfit)
tidy.marssMLE(kemfit)
logLik(kemfit)
AIC(kemfit)
residuals(kemfit) 
predict(kemfit, n.ahead = 12)
ggplot2::autoplot(predict(kemfit, n.ahead = 12))
# forecast(kemfit, n.ahead = 12) 
# ggplot2::autoplot(forecast(kemfit, n.ahead = 12))
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



# 1.X Covariates ----------------------------------------------------------

fulldat <- lakeWAplanktonTrans
years <- fulldat[, "Year"] >= 1965 & fulldat[, "Year"] < 1975
dat <- t(fulldat[years, c("Greens", "Bluegreens")])
the.mean <- apply(dat, 1, mean, na.rm = TRUE)
the.sigma <- sqrt(apply(dat, 1, var, na.rm = TRUE))
dat <- (dat - the.mean) * (1 / the.sigma)

covariates <- rbind(
  Temp = fulldat[years, "Temp"],
  TP = fulldat[years, "TP"]
)
# z.score the covariates
covariates <- zscore(covariates)

# Multivariate linear regression with autocorrelated errors
Z <- "identity"
Q <- "unconstrained"
B <- "diagonal and unequal"
A <- U <- x0 <- "zero"
R <- "diagonal and equal"
d <- covariates

D <- "unconstrained"
y <- dat
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A,
                   R = R, D = D, d = d, x0 = x0)
control.list <- list(maxit = 1500)
kem <- MARSS(y, model = model.list, control = control.list)


# 1.Y Time-varying parameters ---------------------------------------------

dat <- t(harborSealWA)
dat <- dat[2:nrow(dat), ] # remove the year row

# Time-varying parameters are specified by passing in an array of matrices (list,
# numeric or character) where the 3rd dimension of the array is time and must
# be the same value as the 2nd (time) dimension of the data matrix. No text
# shortcuts are allowed for time-varying parameters; you need to use the matrix
# form.
# For example, let's say we wanted a different u for the first half versus
# second half of the harbor seal time series. We would pass in an array for u as
# follows:
U1 <- matrix("t1", 5, 1)
U2 <- matrix("t2", 5, 1)

Ut <- array(U2, dim = c(dim(U1), dim(dat)[2]))
TT <- dim(dat)[2]
Ut[, , 1:floor(TT / 2)] <- U1
kemfit.tv <- MARSS(dat, model = list(U = Ut, Q = "diagonal and equal"))

U1 <- matrix(c(rep("t1", 4), "hc"), 5, 1)
U2 <- matrix(c(rep("t2", 4), "hc"), 5, 1)
Ut <- array(U2, dim = c(dim(U1), dim(dat)[2]))
Ut[, , 1:floor(TT / 2)] <- U1
kemfit.tv <- MARSS(dat, model = list(U = Ut, Q = "diagonal and equal"))


# 1.Z DLM -----------------------------------------------------------------

data(SalmonSurvCUI)
years <- SalmonSurvCUI[, 1]
TT <- length(years)
# response data: logit(survival)
dat <- matrix(SalmonSurvCUI[, 2], nrow = 1)

CUI <- SalmonSurvCUI[, "CUI.apr"]
CUI.z <- zscore(CUI)
# number of state = # of regression params (slope(s) + intercept)
m <- 1 + 1

# for process eqn
B <- diag(m) # 2x2; Identity

U <- matrix(0, nrow = m, ncol = 1) # 2x1; both elements = 0
Q <- matrix(list(0), m, m) # 2x2; all 0 for now
diag(Q) <- c("q1", "q2") # 2x2; diag = (q1,q2)

# for observation eqn
Z <- array(NA, c(1, m, TT)) # NxMxT; empty for now
Z[1, 1, ] <- rep(1, TT) # Nx1; 1's for intercept
Z[1, 2, ] <- CUI.z # Nx1; regr variable
A <- matrix(0) # 1x1; scalar = 0
R <- matrix("r") # 1x1; scalar = r

# only need starting values for regr parameters
inits.list <- list(x0 = matrix(c(0, 0), nrow = m))
# list of model matrices & vectors
mod.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)

dlm1 <- MARSS(dat, inits = inits.list, model = mod.list)

dlm1$states

# 2 - Replicate BSTS examples ---------------------------------------------


# 2.1 Nowcasting ----------------------------------------------------------

library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

# Local Linear Trend + Seasonal

# Data
dat = data.frame(Yr = floor(lubridate::year(time(initial.claims)) + .Machine$double.eps),
                 Qtr = lubridate::quarter(time(initial.claims)), 
                 initial.claims) %>% 
  janitor::clean_names() %>% 
  t()


# 2.2 LLT Model -----------------------------------------------------------


var_y <- var(dat["iclaims_nsa", ])
Z <- matrix(c(1,0), nrow = 1, ncol = 2)
A <- matrix(list(0), nrow = 1)
G <- diag(nrow = 2) # Default
R <- matrix(list("r"), nrow = 1)
B <- matrix(c(1, 0, 1, 1), nrow = 2, ncol = 2)
U <- "zero"
H <- diag(nrow = 1) # Default
Q <- ldiag(c("s_mu", "s_beta"))
x0 <- matrix(c(dat["iclaims_nsa", 1], 0), nrow = 2, ncol = 1)
V0 <- matrix(list(0), nrow =2, ncol = 2) + diag(1e-10, 2)
# diag(V0) <- c("s_mu_0", "s_beta_0")
diag(V0) <- c(1e+04*var_y, 1e+04*var_y)

llt_spec <- list(Z = Z,
                 A = A,
                 R = R,
                 B = B,
                 U = U,
                 Q = Q,
                 x0 = x0,
                 V0 = V0,
                 G = G,
                 H = H#,
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

llt_fit$model$fixed$Z %*% print(llt_fit, what = "states", silent = TRUE) %>% 
  t %>% tail()
Z %*% print(llt_fit, what = "states", silent = TRUE) %>% 
  t %>% tail()
fitted(llt_fit) %>% tail()

# 
# 

# 2.3 LLT + Season(52) ----------------------------------------------------


makeB <- function(nf) {
  B <- matrix(0, nf + 1L, nf + 1L)
  B[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
  B[3L, ] <- c(0, 0, rep(-1, nf - 1L))
  if (nf >= 3L) {
    ind <- 3:nf
    B[cbind(ind + 1L, ind)] <- 1
  }
  return(B)
}

S <- 52
var_y <- var(dat["iclaims_nsa", ])/100
Z <- matrix(c(1,0,1, rep(0, S-2)), nrow = 1, ncol = 2+S-1)
A <- "zero"
G <- "identity" # Default
R <- matrix(list("r"), nrow = 1)
B <- makeB(S)
U <- "zero"
H <- "identity" # Default
Q <- ldiag(c(list("s_mu", "s_beta", "s_w"), as.list(rep(0, S-2))))
x0 <- matrix(c(dat["iclaims_nsa", 1], rep(0, S)),  ncol = 1)
V0 <- diag(1e+06*var_y, S+1) + diag(1e-10, S+1)


llt_spec <- list(Z = Z,
                 A = A,
                 R = R,
                 B = B,
                 U = U,
                 Q = Q,
                 x0 = x0,
                 V0 = V0,
                 G = G,
                 H = H,
                 # L = L ???
                 tinitx = 0
                 )

llt_seas52_fit <- MARSS(dat["iclaims_nsa", ], 
                 model = llt_spec,
                 fit = TRUE, 
                 form = "marxss",
                 # method = "kem")
                 method = "BFGS")

print(llt_seas52_fit, what = "R")
print(llt_seas52_fit, what = "Q")
print(llt_seas52_fit, what = "V0")

ggplot2::autoplot(predict(llt_seas52_fit, n.ahead = 52))

broom::tidy(llt_seas52_fit)
broom::glance(llt_seas52_fit)

llt_seas52_fit$model$fixed$Z %*% print(llt_seas52_fit, 
                                       what = "states", silent = TRUE) %>% 
  t %>% tail()
Z %*% print(llt_seas52_fit, what = "states", silent = TRUE) %>% 
  t %>% tail()
fitted(llt_seas52_fit) %>% tail()
fitted(llt_seas52_fit, type = "ytT") %>% tail()
fitted(llt_seas52_fit, type = "xtT") %>% tail()
fitted(llt_seas52_fit, type = "ytt") %>% tail()
fitted(llt_seas52_fit, type = "xtt1") %>% tail()


# 2.4 LLT + Season(52) + COVARIATES ---------------------------------------


covariates <- dat[c("michigan_unemployment"     ,
                    "idaho_unemployment",
                    "pennsylvania_unemployment",
                    "unemployment_filing",
                    "new_jersey_unemployment"   ,
                    "department_of_unemployment",
                    "illinois_unemployment" ,
                    "rhode_island_unemployment",
                    "unemployment_office"       ,
                    "filing_unemployment"), ]

S <- 52
var_y <- var(dat["iclaims_nsa", ])/100

B <- makeB(S)
U <- "zero"
# C
# c
G <- "identity" # Default
Q <- ldiag(c(list("s_mu", "s_beta", "s_w"), as.list(rep(0, S-2))))


Z <- matrix(c(1,0,1, rep(0, S-2)), nrow = 1, ncol = 2+S-1)
A <- "zero"
D <- "unconstrained"
d <- covariates
H <- "identity" # Default
R <- matrix(list("r"), nrow = 1)


x0 <- matrix(c(dat["iclaims_nsa", 1], rep(0, S)),  ncol = 1)
V0 <- diag(1e+06*var_y, S+1) + diag(1e-10, S+1)


llt_spec <- list(Z = Z,
                 A = A,
                 D = D,
                 d = d,
                 H = H,
                 R = R,
                 B = B,
                 U = U,
                 # C = c,
                 # c = c,
                 G = G,
                 Q = Q,
                 x0 = x0,
                 V0 = V0,
                 # L = L ???
                 tinitx = 0
)

llt_seas52_cov_fit <- MARSS(dat["iclaims_nsa", ], 
                        model = llt_spec,
                        fit = TRUE, 
                        form = "marxss",
                        # method = "kem")
                        method = "BFGS")

print(llt_seas52_cov_fit, what = "R")
print(llt_seas52_cov_fit, what = "Q")
print(llt_seas52_cov_fit, what = "V0")

ggplot2::autoplot(predict(llt_seas52_cov_fit, 
                          n.ahead = 52,
                          newdata = d[,1:52]))

broom::tidy(llt_seas52_cov_fit)
broom::glance(llt_seas52_cov_fit)

llt_seas52_cov_fit$model$fixed$Z %*% print(llt_seas52_cov_fit, 
                                       what = "states", silent = TRUE) %>% 
  t %>% tail()
Z %*% print(llt_seas52_cov_fit, what = "states", silent = TRUE) %>% 
  t %>% tail()
fitted(llt_seas52_cov_fit) %>% tail()
fitted(llt_seas52_cov_fit, type = "ytT") %>% tail()
fitted(llt_seas52_cov_fit, type = "xtT") %>% tail()
fitted(llt_seas52_cov_fit, type = "ytt") %>% tail()
fitted(llt_seas52_cov_fit, type = "xtt1") %>% tail()

# for (j in 1:5) {
plot.ts(MARSSresiduals(llt_seas52_cov_fit, type = "tt1")$model.residuals[1,],
        ylab = "Residual", main = "Model Residuals"
)
abline(h = 0, lty = "dashed")
acf(MARSSresiduals(llt_seas52_cov_fit, type = "tt1")$model.residuals[1,],
    na.action = na.pass)
# }
# 


# 2.5 LLT + Season(52) + TIME VARYING COVARIATES --------------------------

covariates <- dat[c("michigan_unemployment"     ,
                    "idaho_unemployment",
                    "pennsylvania_unemployment",
                    "unemployment_filing",
                    "new_jersey_unemployment"   ,
                    "department_of_unemployment",
                    "illinois_unemployment" ,
                    "rhode_island_unemployment",
                    "unemployment_office"       ,
                    "filing_unemployment"), ]

S <- 52
var_y <- var(dat["iclaims_nsa", ])/100

make2 <- function(B1, B2) {
  #browser()
  out1 <- cbind(B1, matrix(0, nrow = nrow(B1), ncol = ncol(B2)))
  out2 <- cbind(matrix(0, nrow = nrow(B2), ncol = ncol(B1)), B2)
  
  rbind(out1, out2)
}

B <- make2(makeB(S), diag(1, nrow(covariates)))
U <- "zero"
# C
# c
G <- "identity" # Default
Q <- make2(ldiag(c(list("s_mu", "s_beta", "s_w"), as.list(rep(0, S-2)))),
           ldiag(paste0("q_", 1:nrow(covariates))))

aux <- covariates %>% rownames()

# Z <- matrix(c(1,0,1, rep(0, S-2)), nrow = 1, ncol = 2+S-1)
Z <- array(NA, c(1, 2+S-1+length(aux), ncol(dat)))
for(i in 1:ncol(dat)) {
  Z[1,1:(2+S-1),i] <-  c(c(1,0,1), rep(0, S-2))
  Z[1,(2+S):(2+S-1+length(aux)),i] <- covariates[,i]
}

A <- "zero"
D <- "zero"
d <- "zero"
H <- "identity" # Default
R <- matrix(list("r"), nrow = 1)


x0 <- matrix(c(dat["iclaims_nsa", 1], 
               rep(0, S),  
               rep(0, length(aux))), 
             ncol = 1)
V0 <- diag(1e+06*var_y, S+1+length(aux)) + diag(1e-10, S+1+length(aux))


llt_spec <- list(Z = Z,
                 A = A,
                 D = D,
                 d = d,
                 H = H,
                 R = R,
                 B = B,
                 U = U,
                 # C = c,
                 # c = c,
                 G = G,
                 Q = Q,
                 x0 = x0,
                 V0 = V0,
                 # L = L ???
                 tinitx = 0
)

llt_seas52_cov_t_fit <- MARSS(dat["iclaims_nsa", ], 
                            model = llt_spec,
                            fit = TRUE, 
                            form = "marxss",
                            # method = "kem")
                            method = "BFGS")

print(llt_seas52_cov_t_fit, what = "R")
print(llt_seas52_cov_t_fit, what = "Q")
print(llt_seas52_cov_t_fit, what = "V0")

llt_seas52_cov_t_fit$states[54:63,]

ggplot2::autoplot(predict(llt_seas52_cov_t_fit, 
                          n.ahead = 52))

broom::tidy(llt_seas52_cov_t_fit)
broom::glance(llt_seas52_cov_t_fit)

llt_seas52_cov_t_fit$model$fixed$Z[,1,] %*% 
  t(print(llt_seas52_cov_t_fit,  what = "states", silent = TRUE)) %>% 
  t %>% tail()
Z[1,,] %*% t(print(llt_seas52_cov_t_fit, what = "states", silent = TRUE)) %>% 
  t %>% tail()
fitted(llt_seas52_cov_t_fit) %>% tail()
fitted(llt_seas52_cov_t_fit, type = "ytT") %>% tail()
fitted(llt_seas52_cov_t_fit, type = "xtT") %>% tail()
fitted(llt_seas52_cov_t_fit, type = "ytt") %>% tail()
fitted(llt_seas52_cov_t_fit, type = "xtt1") %>% tail()

# for (j in 1:5) {
plot.ts(MARSSresiduals(llt_seas52_cov_t_fit, type = "tt1")$model.residuals[1,],
        ylab = "Residual", main = "Model Residuals"
)
abline(h = 0, lty = "dashed")
acf(MARSSresiduals(llt_seas52_cov_t_fit, type = "tt1")$model.residuals[1,],
    na.action = na.pass)
# }
# 


# 2.5 - CLEAN -------------------------------------------------------------

## KEYS:
## 
## Local Level Trend  (LLT) + Season: Ch 19 Structural Time Series Models,
##                                    MARSS User's Guide.
## Dynamic linear models (DLMs): Ch 16 Structural Time Series Models,
##                                    MARSS User's Guide.
##                                    

library(tidyverse)
library(MARSS)

# Data --------------------------------------------------------------------

library(bsts)     # load the bsts package
data(iclaims)     # bring the initial.claims data into scope

dat = data.frame(Yr = floor(lubridate::year(time(initial.claims)) + .Machine$double.eps),
                 Qtr = lubridate::quarter(time(initial.claims)), 
                 initial.claims) %>% 
  janitor::clean_names() %>% 
  t()


# Aux funs ----------------------------------------------------------------

make_LLT_B <- function() {
  #
  # Make B matrix for Local Linear Trend (LLT)
  # 
  
  matrix(c(1, 0, 1, 1), nrow = 2, ncol = 2)
}

make_season_B <- function(nf) {
  #
  # Make B matrix for a seasonal component
  # nf = Seasonal frequency
  # 
  
  B <- matrix(0, nf - 1L, nf -1L)
  
  B[1L, ] <- rep(-1, nf - 1L)
  
  if (nf >= 3L) {
    ind <- (3:nf) - 2L
    B[cbind(ind + 1L, ind)] <- 1
  }
  return(B)
}

make_LLT_season_B <- function(nf) {
  #
  # Make B matrix  to model Local Level Trend (LLT) + seasonal components 
  # nf = Seasonal frequency
  # 
  # Ch 19 Structural Time Series Models, p. 264 - MARSS User's Guide
  # 
  B <- matrix(0, nf + 1L, nf + 1L)
  B[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
  B[3L, ] <- c(0, 0, rep(-1, nf - 1L))
  if (nf >= 3L) {
    ind <- 3:nf
    B[cbind(ind + 1L, ind)] <- 1
  }
  return(B)
}

make_dynamic_covariates_B <- function(n_covariates) {
  diag(1, n_covariates)
}

make_LLT_Q <- function() {
  ldiag(c("s_mu", "s_beta"))
}

make_season_Q <- function(nf, suffix = "") {
  
  ldiag(c(list(paste0("s_w", suffix)), 
          as.list(rep(0, nf - 2))))
}

make_covariates_Q <- function(n_covariates) {
  
  ldiag(paste0("q_", 1:n_covariates))
}

make_B <- function(B1, B2) {
  #
  # Make B matrix from B1 (e.g., LLT + Season) and B2 (e.g., time variant 
  # covariates) matrices.
  #
  out1 <- cbind(B1, matrix(0, nrow = nrow(B1), ncol = ncol(B2)))
  out2 <- cbind(matrix(0, nrow = nrow(B2), ncol = ncol(B1)), B2)
  
  rbind(out1, out2)
}


# Aux vars ----------------------------------------------------------------

COVARIATES <- c("michigan_unemployment",
                "idaho_unemployment",
                "pennsylvania_unemployment",
                "unemployment_filing",
                "new_jersey_unemployment",
                "department_of_unemployment",
                "illinois_unemployment",
                "rhode_island_unemployment",
                "unemployment_office",
                "filing_unemployment")

# Covariates-only matrix
covariates <- dat[COVARIATES, ] 

TGT_VARS <- c("iclaims_nsa")

targets <- dat[TGT_VARS, ]

var_y <- var(tartgets)/100 ## TO DO MULTIVARIATE

N_T_PERIODS <- ncol(dat)             # T
N_TARGETS <- length(TGT_VARS)        # n
N_COVARIATES <- nrow(covariates)
STATIONALITIES <- c(yearly = 52)
SUM_STATIONALITIES = sum(STATIONALITIES)
N_STATIONALITIES <- length(STATIONALITIES)
N_STATES <- 2 +  # LLT               # m
  SUM_STATIONALITIES - N_STATIONALITIES  + N_COVARIATES


# Model Specification -----------------------------------------------------

# Ch 1 Overview, pp. 1-2 - MARSS User's Guide
# 

## State Process
## 
## x[t] = B[t] X[t-1] + u[t] + C[t] c[t] + G[t] w[t], w[t] ~ MVN(0, Q[t])
## 
## x: m X T - N_STATES X N_T_PERIODS
## w: m X T - N_STATES X N_T_PERIODS
## 

# m X m - N_STATES X N_STATES - Default="identity"
B <- make_LLT_B() %>% # LLT
  make_B(make_season_B(STATIONALITIES["yearly"])) %>% # Season
  make_B(make_dynamic_covariates_B(N_COVARIATES)) # Time-variant covariates coefs

# m X 1 - N_STATES X 1 - Default="unconstrained"
U <- "zero"

# m X p - Default="zero"
C <- "zero"

# p X T - Default="zero"
c <- "zero"

# m X m - N_STATES X N_STATES - Default="identity"
G <- "identity" # Default

# m X m - N_STATES X N_STATES - Default="diagonal and unequal"
Q <- make_LLT_Q() %>% 
  make_B(make_season_Q(STATIONALITIES["yearly"])) %>% 
  make_B(make_covariates_Q(N_COVARIATES))
            

## Observation Process
## 
## y[t] = Z[t] X[t-1] + a[t] + D[t] d[t] + H[t] v[t], v[t] ~ MVN(0, R[t])
## 
## y: n X T - N_TARGETS X N_T_PERIODS
## v: n X T - N_TARGETS X N_T_PERIODS
## 

# n X m X T - N_TARGETS X N_STATES X N_T_PERIODS - Default="identity"
Z <- array(NA, c(N_TARGETS, N_STATES, N_T_PERIODS))
for(i in 1:N_T_PERIODS) {
  Z[1:N_TARGETS, 1:(N_STATES - N_COVARIATES), i] <-  
    c(c(1,0,1), # LLT
      rep(0,    # Seasonalities
          SUM_STATIONALITIES - 2*N_STATIONALITIES))
  Z[1:N_TARGETS, (N_STATES - N_COVARIATES + 1):N_STATES, i] <- 
    covariates[,i] # Time-variant covariates coefs
}

# n X 1 - N_TARGETS X 1 - Default="scaling"
A <- "zero"

# n X q - Default="zero"
D <- "zero"

# q X T - Default="zero"
d <- "zero"

# n X n - N_TARGETS X N_TARGETS - Default="identity"
H <- "identity" # Default

# n X n - N_TARGETS X N_TARGETS - Default="diagonal and equal"
R <- matrix(list("r"), nrow = N_TARGETS)

## Initial States
## 
## X[1] ~ MVN(pi, lambda) ó X[1] ~ MVN(pi, lambda)
## 

# m X 1 - N_STATES X 1 - Default="unconstrained"
x0 <- matrix(c(targets[1], 
               rep(0, STATIONALITIES["yearly"]),  
               rep(0, N_COVARIATES)), 
             ncol = 1)

# m X m - N_STATES X N_STATES - Default="zero"
V0 <- diag(1e+06*var_y, N_STATES) + diag(1e-10, N_STATES)

# Default=0
tinitx = 0

llt_spec <- list(Z = Z,
                 A = A,
                 D = D,
                 d = d,
                 H = H,
                 R = R,
                 B = B,
                 U = U,
                 C = c,
                 c = c,
                 G = G,
                 Q = Q,
                 x0 = x0,
                 V0 = V0,
                 # L = L ???
                 tinitx = tinitx
)


# Model fit ---------------------------------------------------------------
llt_seas52_cov_t_fit <- MARSS(targets, 
                              model = llt_spec,
                              fit = TRUE, 
                              form = "marxss",
                              # method = "kem")
                              method = "BFGS")

# Model check -------------------------------------------------------------


