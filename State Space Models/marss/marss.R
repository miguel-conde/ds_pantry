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



# 1.1 Covariates ----------------------------------------------------------

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


# 1.2 Time-varying parameters ---------------------------------------------

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


# 1.3 DLM -----------------------------------------------------------------

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


# 1.4 - Seasonalities -----------------------------------------------------

# Here we show a few approaches for including seasonal effects using the
# Lake Washington plankton data, which were collected monthly. The following
# examples will use all five phytoplankton species from Lake Washington. First,
# let's set up the data.
years <- fulldat[, "Year"] >= 1965 & fulldat[, "Year"] < 1975
phytos <- c(
"Diatoms", "Greens", "Bluegreens",
"Unicells", "Other.algae"
)
dat <- t(fulldat[years, phytos])
# z.score data again because we changed the mean when we subsampled
dat <- zscore(dat)
# number of time periods/samples
TT <- ncol(dat)


# 1.4.1 - Seasonal effects as fixed factors ---------------------------------
# number of "seasons" (e.g., 12 months per year)
period <- 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st <- 1
# create factors for seasons
c.in <- diag(period)
for (i in 2:(ceiling(TT / period))) {
  c.in <- cbind(c.in, diag(period))
}
# trim c.in to correct start & length
c.in <- c.in[, (1:TT) + (per.1st - 1)]

C <- matrix(month.abb, 5, 12, byrow = TRUE)
C

# better row names
rownames(c.in) <- month.abb

# Notice, that C only has 12 values in it, the 12 common month effects. However,
# for this example, we will let each taxon have a different month effect thus
# allowing different seasonality for each taxon. For this model, we want each
# value in C to be unique:
C <- "unconstrained"

# Now C has 5 x 12 = 60 separate effects.

# Each taxon has unique density-dependence
B <- "diagonal and unequal"
# Independent process errors
Q <- "diagonal and unequal"
# We have demeaned the data & are fitting a mean-reverting model
# by estimating a diagonal B, thus
U <- "zero"
# Each obs time series is associated with only one process
Z <- "identity"
# The data are demeaned & fluctuate around a mean
A <- "zero"

# Observation errors are independent, but they
# have similar variance due to similar collection methods
R <- "diagonal and equal"
# No covariate effects in the obs equation
D <- "zero"
d <- "zero"
# Now we can set up the model list for MARSS and fft the model (results
# are not shown since they are verbose with 60 different month effects).
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R,
                   C = C, c = c.in, D = D, d = d)
seas.mod.1 <- MARSS(dat, model = model.list, control = list(maxit = 1500))
# Get the estimated seasonal effects
# rows are taxa, cols are seasonal effects
seas.1 <- coef(seas.mod.1, type = "matrix")$C
rownames(seas.1) <- phytos
colnames(seas.1) <- month.abb


# 1.4.2 - Seasonal effects as a polynomial ---------------------------------
# The fixed factor approach required estimating 60 effects. Another approach is
# to model the month effect as a 3rd-order (or higher) polynomial: 
# a+bxm+cxm2+dxm3 where m is the month number. This approach has less 
# exibility but requires only 20 estimated parameters (i.e., 4 regression parameters
# times 5 taxa). To do so, we create a 4 x T covariate matrix c with the rows
# corresponding to 1, m, m2, and m3, and the columns again corresponding to
# the time points. Here is how to set up this matrix:

# number of "seasons" (e.g., 12 months per year)
period <- 12
# first "season" (e.g., Jan = 1, July = 7)
per.1st <- 1
# order of polynomial
poly.order <- 3
# create polynomials of months
month.cov <- matrix(1, 1, period)
for (i in 1:poly.order) {
  month.cov <- rbind(month.cov, (1:12)^i)
}

# our c matrix is month.cov replicated once for each year
c.m.poly <- matrix(month.cov, poly.order + 1, TT + period, byrow = FALSE)
# trim c.in to correct start & length
c.m.poly <- c.m.poly[, (1:TT) + (per.1st - 1)]
# Everything else remains the same as in the previous example
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R,
                   C = C, c = c.m.poly, D = D, d = d)
seas.mod.2 <- MARSS(dat, model = model.list, control = list(maxit = 1500))

# The effect of month m for taxon i is ai+bixm+cixm2+dixm3, where ai, bi,
# ci and di are in the i-th row of C. We can now calculate the matrix of seasonal
# effects as follows, where each row is a taxon and each column is a month:
C.2 <- coef(seas.mod.2, type = "matrix")$C
seas.2 <- C.2 %*% month.cov
rownames(seas.2) <- phytos
colnames(seas.2) <- month.abb


# 1.4.3 - Seasonal effects as a Fourier series ----------------------------

# The factor approach required estimating 60 effects, and the 3rd order polynomial
# model was an improvement at only 20 parameters. A third option is to
# use a discrete Fourier series, which is combination of sine and cosine waves; it
# would require only 10 parameters. Specifically, the effect of month m on taxon
# i is aixcos(2pm=p)+bixsin(2pm=p), where p is the period (e.g., 12 months,
# 4 quarters), and ai and bi are contained in the i-th row of C.
# We begin by defining the 2 x T seasonal covariate matrix c as a combination
# of 1 cosine and 1 sine wave:
cos.t <- cos(2 * pi * seq(TT) / period)
sin.t <- sin(2 * pi * seq(TT) / period)
c.Four <- rbind(cos.t, sin.t)
# Everything else remains the same and we can fit this model as follows:
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R,
                   C = C, c = c.Four, D = D, d = d)
seas.mod.3 <- MARSS(dat, model = model.list, control = list(maxit = 1500))
# We make our seasonal effect matrix as follows:
C.3 <- coef(seas.mod.3, type = "matrix")$C
# The time series of net seasonal effects
seas.3 <- C.3 %*% c.Four[, 1:period]
rownames(seas.3) <- phytos
colnames(seas.3) <- month.abb
  
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
V0 <- matrix(0, nrow =2, ncol = 2) + diag(1e-10, 2)
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

source(here::here("State Space Models", "marss", "marss_utils.R"))


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

targets <- dat[TGT_VARS, , drop = FALSE]

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
  dbind(make_season_B(STATIONALITIES["yearly"])) %>% # Season
  dbind(make_dynamic_covariates_B(N_COVARIATES)) # Time-variant covariates coefs

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
  dbind(make_season_Q(STATIONALITIES["yearly"])) %>% 
  dbind(make_covariates_Q(N_COVARIATES))
            

## Observation Process
## 
## y[t] = Z[t] X[t-1] + a[t] + D[t] d[t] + H[t] v[t], v[t] ~ MVN(0, R[t])
## 
## y: n X T - N_TARGETS X N_T_PERIODS
## v: n X T - N_TARGETS X N_T_PERIODS
## 

# n X m X T - N_TARGETS X N_STATES X N_T_PERIODS - Default="identity"
Z_old <- array(NA, c(N_TARGETS, N_STATES, N_T_PERIODS))
for(i in 1:N_T_PERIODS) {
  Z_old[1:N_TARGETS, 1:(N_STATES - N_COVARIATES), i] <-  
    c(c(1,0,1), # LLT
      rep(0,    # Seasonalities
          SUM_STATIONALITIES - 2*N_STATIONALITIES))
  Z_old[1:N_TARGETS, (N_STATES - N_COVARIATES + 1):N_STATES, i] <- 
    covariates[,i] # Time-variant covariates coefs
}

Z_LLT_season <- make_LLT_Z() %>% 
  cbind(make_season_Z(STATIONALITIES["yearly"])) %>% 
  array(dim = c(N_TARGETS, N_STATES - N_COVARIATES, N_T_PERIODS))
Z_covariates <- array(covariates, 
                      dim = c(N_TARGETS, 
                              N_COVARIATES, 
                              N_T_PERIODS))
Z <- abind::abind(Z_LLT_season, Z_covariates, along = 2)

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
x0 <- matrix(c(targets[TGT_VARS[1], 1], 
               rep(0, STATIONALITIES["yearly"]),  
               rep(0, N_COVARIATES)), 
             ncol = 1)

x0 <- make_LLT_x0(targets[TGT_VARS[1], ]) %>% 
  rbind(make_season_x0(STATIONALITIES["yearly"])) %>% 
  rbind(make_covariates_x0(covariates))

# m X m - N_STATES X N_STATES - Default="zero"
V0 <- diag(1e+06*var_y, N_STATES) + diag(1e-10, N_STATES)

V0 <- make_LLT_V0(targets[TGT_VARS[1], ]) %>% 
  dbind(make_season_V0(targets[TGT_VARS[1], ], 
                       nf = STATIONALITIES["yearly"])) %>% 
  dbind(make_covariates_V0(covariates))

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

plot(llt_seas52_cov_t_fit, plot.type = "model.ytT")

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

aux <- fitted(llt_seas52_cov_t_fit, type = "xtT")

states_evol <- aux %>% 
  dplyr::select(t, .rownames, .fitted) %>% 
  filter(.rownames %in% c(t, "X1", "X3", paste0("X", 54:63))) %>% 
  spread(.rownames, .fitted)  %>% 
  mutate(l_s = X1 + X3) # %>% 
  # rowwise() %>% 
  # mutate(cov = sum(X54:X63)) %>% 
  # ungroup %>% 
  # mutate(fitted = l_s + cov)

  
  
beta_coefs <- states_evol %>% dplyr::select(X54:X63)
contrib_covariates <- (covariates * t(beta_coefs)) %>% 
  t %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("date") %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

contrib_covariates <- contrib_covariates %>% 
  mutate(all_covariates = contrib_covariates %>% dplyr::select(-date) %>% rowSums()) 

contrib_evol <- states_evol %>% 
  dplyr::select(trend = X1, season = X3, trend_season = l_s) %>% 
  bind_cols((contrib_covariates)) %>% 
  mutate(fitted = trend + season + all_covariates,
         y = targets[1,],
         residuals = y - fitted)

plot(contrib_evol %>% dplyr::select(date, y), type = "l")
lines(contrib_evol%>% dplyr::select(date, trend), col = "blue") # Trend

plot(contrib_evol[1:52, c("date", "season")], type = "l") # Stationality

plot(contrib_evol %>% dplyr::select(date, y), type = "l")
lines(contrib_evol%>% dplyr::select(date, trend_season), col = "blue") # Trend + Stationality

plot(contrib_evol %>% dplyr::select(date, all_covariates), 
     col = "blue", type = "l") # Covariates 

plot(contrib_evol %>% dplyr::select(date, y), type = "l")
lines(contrib_evol%>% dplyr::select(date, fitted), col = "blue") # Fitted

plot(contrib_evol %>% dplyr::select(date, residuals))
qqnorm(scale(contrib_evol %>% pull(residuals)))
abline(a = 0, b = 1)


# 2.6 - 2 RESPONSES -------------------------------------------------------------

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

source(here::here("State Space Models", "marss", "marss_utils.R"))


# Aux vars ----------------------------------------------------------------

COVARIATES <- c("michigan_unemployment",
                "idaho_unemployment",
                "pennsylvania_unemployment",
                "unemployment_filing",
                "new_jersey_unemployment",
                # "department_of_unemployment",
                "illinois_unemployment",
                "rhode_island_unemployment",
                "unemployment_office",
                "filing_unemployment")

# Covariates-only matrix
covariates <- dat[COVARIATES, ] 

TGT_VARS <- c("iclaims_nsa", "department_of_unemployment")

targets <- dat[TGT_VARS, , drop = FALSE]

# var_y <- var(tartgets)/100 ## TO DO MULTIVARIATE

N_T_PERIODS <- ncol(dat)             # T
N_TARGETS <- length(TGT_VARS)        # n
N_COVARIATES <- nrow(covariates)
STATIONALITIES <- c(yearly = 52)
SUM_STATIONALITIES = sum(STATIONALITIES)
N_STATIONALITIES <- length(STATIONALITIES)
N_STATES <- N_TARGETS * (2  +  # LLT               # m
                             SUM_STATIONALITIES - N_STATIONALITIES  + 
                             N_COVARIATES)


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
  dbind(make_LLT_B()) %>% 
  dbind(make_season_B(STATIONALITIES["yearly"])) %>% # Season
  dbind(make_season_B(STATIONALITIES["yearly"])) %>% 
  dbind(make_dynamic_covariates_B(N_COVARIATES)) %>% # Time-variant covariates coefs
  dbind(make_dynamic_covariates_B(N_COVARIATES))

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
  dbind(make_LLT_Q()) %>% 
  dbind(make_season_Q(STATIONALITIES["yearly"])) %>% 
  dbind(make_season_Q(STATIONALITIES["yearly"])) %>% 
  dbind(make_covariates_Q(N_COVARIATES)) %>% 
  dbind(make_covariates_Q(N_COVARIATES))


## Observation Process
## 
## y[t] = Z[t] X[t-1] + a[t] + D[t] d[t] + H[t] v[t], v[t] ~ MVN(0, R[t])
## 
## y: n X T - N_TARGETS X N_T_PERIODS
## v: n X T - N_TARGETS X N_T_PERIODS
## 

# n X m X T - N_TARGETS X N_STATES X N_T_PERIODS - Default="identity"

Z_LLT_season <- make_LLT_Z() %>% 
  dbind(make_LLT_Z()) %>% 
  cbind(make_season_Z(STATIONALITIES["yearly"]) %>% 
          dbind(make_season_Z(STATIONALITIES["yearly"]))) %>% 
  array(dim = c(N_TARGETS, N_STATES - N_COVARIATES, N_T_PERIODS))
Z_covariates <- array(rbind(covariates, covariates), 
                      dim = c(N_TARGETS, 
                              N_COVARIATES, 
                              N_T_PERIODS))
Z <- abind::abind(Z_LLT_season, Z_covariates, along = 2)

# n X 1 - N_TARGETS X 1 - Default="scaling"
A <- "zero"

# n X q - Default="zero"
D <- "zero"

# q X T - Default="zero"
d <- "zero"

# n X n - N_TARGETS X N_TARGETS - Default="identity"
H <- "identity" # Default

# n X n - N_TARGETS X N_TARGETS - Default="diagonal and equal"
R <- make_R(N_TARGETS)

## Initial States
## 
## X[1] ~ MVN(pi, lambda) ó X[1] ~ MVN(pi, lambda)
## 

# m X 1 - N_STATES X 1 - Default="unconstrained"
# x0 <- matrix(c(targets[TGT_VARS[1], 1], 
#                rep(0, STATIONALITIES["yearly"]),  
#                rep(0, N_COVARIATES)), 
#              ncol = 1)

x0 <- make_LLT_x0(targets[TGT_VARS[1], ]) %>% 
  rbind(make_LLT_x0(targets[TGT_VARS[2], ])) %>% 
  rbind(make_season_x0(STATIONALITIES["yearly"])) %>% 
  rbind(make_season_x0(STATIONALITIES["yearly"])) %>% 
  rbind(make_covariates_x0(covariates)) %>% 
  rbind(make_covariates_x0(covariates))

# m X m - N_STATES X N_STATES - Default="zero"
# V0 <- diag(1e+06*var_y, N_STATES) + diag(1e-10, N_STATES)

V0 <- make_LLT_V0(targets[TGT_VARS[1], ]) %>% 
  dbind(make_LLT_V0(targets[TGT_VARS[2], ])) %>% 
  dbind(make_season_V0(targets[TGT_VARS[1], ], 
                       nf = STATIONALITIES["yearly"])) %>% 
  dbind(make_season_V0(targets[TGT_VARS[2], ], 
                       nf = STATIONALITIES["yearly"])) %>% 
  dbind(make_covariates_V0(covariates)) %>% 
  dbind(make_covariates_V0(covariates))

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
llt_seas52_cov_t_2y_fit <- MARSS(targets, 
                              model = llt_spec,
                              fit = TRUE, 
                              form = "marxss",
                              # method = "kem")
                              method = "BFGS")

# Model check -------------------------------------------------------------

plot(llt_seas52_cov_t_2y_fit, plot.type = "model.ytT")

# print(llt_seas52_cov_t_2y_fit, what = "R")
# print(llt_seas52_cov_t_2y_fit, what = "Q")
# print(llt_seas52_cov_t_2y_fit, what = "V0")
# 
# llt_seas52_cov_t_2y_fit$states[54:63,]

ggplot2::autoplot(predict(llt_seas52_cov_t_2y_fit, 
                          n.ahead = 52))

broom::tidy(llt_seas52_cov_t_2y_fit)
broom::glance(llt_seas52_cov_t_2y_fit)

llt_seas52_cov_t_2y_fit$model$fixed$Z[,1,] %*% 
  t(print(llt_seas52_cov_t_2y_fit,  what = "states", silent = TRUE)) %>% 
  t %>% tail()
Z[1,,] %*% t(print(llt_seas52_cov_t_2y_fit, what = "states", silent = TRUE)) %>% 
  t %>% tail()
fitted(llt_seas52_cov_t_2y_fit) %>% tail()
fitted(llt_seas52_cov_t_2y_fit, type = "ytT") %>% tail()
fitted(llt_seas52_cov_t_2y_fit, type = "xtT") %>% tail()
fitted(llt_seas52_cov_t_2y_fit, type = "ytt") %>% tail()
fitted(llt_seas52_cov_t_2y_fit, type = "xtt1") %>% tail()

# for (j in 1:5) {
plot.ts(MARSSresiduals(llt_seas52_cov_t_2y_fit, type = "tt1")$model.residuals[1,],
        ylab = "Residual", main = "Model Residuals"
)
abline(h = 0, lty = "dashed")
acf(MARSSresiduals(llt_seas52_cov_t_2y_fit, type = "tt1")$model.residuals[1,],
    na.action = na.pass)

aux <- fitted(llt_seas52_cov_t_2y_fit, type = "xtT")

states_evol <- aux %>% 
  dplyr::select(t, .rownames, .fitted) %>% 
  filter(.rownames %in% c(t, "X1", "X3", paste0("X", 54:63))) %>% 
  spread(.rownames, .fitted)  %>% 
  mutate(l_s = X1 + X3) # %>% 
# rowwise() %>% 
# mutate(cov = sum(X54:X63)) %>% 
# ungroup %>% 
# mutate(fitted = l_s + cov)



beta_coefs <- states_evol %>% dplyr::select(X54:X63)
contrib_covariates <- (covariates * t(beta_coefs)) %>% 
  t %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("date") %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

contrib_covariates <- contrib_covariates %>% 
  mutate(all_covariates = contrib_covariates %>% dplyr::select(-date) %>% rowSums()) 

contrib_evol <- states_evol %>% 
  dplyr::select(trend = X1, season = X3, trend_season = l_s) %>% 
  bind_cols((contrib_covariates)) %>% 
  mutate(fitted = trend + season + all_covariates,
         y = targets[1,],
         residuals = y - fitted)

plot(contrib_evol %>% dplyr::select(date, y), type = "l")
lines(contrib_evol%>% dplyr::select(date, trend), col = "blue") # Trend

plot(contrib_evol[1:52, c("date", "season")], type = "l") # Stationality

plot(contrib_evol %>% dplyr::select(date, y), type = "l")
lines(contrib_evol%>% dplyr::select(date, trend_season), col = "blue") # Trend + Stationality

plot(contrib_evol %>% dplyr::select(date, all_covariates), 
     col = "blue", type = "l") # Covariates 

plot(contrib_evol %>% dplyr::select(date, y), type = "l")
lines(contrib_evol%>% dplyr::select(date, fitted), col = "blue") # Fitted

plot(contrib_evol %>% dplyr::select(date, residuals))
qqnorm(scale(contrib_evol %>% pull(residuals)))
abline(a = 0, b = 1)