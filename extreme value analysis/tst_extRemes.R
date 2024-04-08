library(tidyverse)

library(extRemes)

# Port Jervis, New York winter maximum and minimum
# temperatures (degrees centigrade).
data(PORTw)

summary(PORTw$TMX1) # maximum winter temperature (degrees centigrade).


# GEV ---------------------------------------------------------------------


fit1 <- fevd(TMX1, PORTw, units="deg C")
fit1
plot(fit1)
plot(fit1, "trace")
return.level(fit1)
return.level(fit1, do.ci=TRUE)
ci(fit1, return.period=c(2,20,100)) # Same as above.

# lr.test(fit0, fit1)
ci(fit1, type="parameter")
par(mfrow=c(1,1))
ci(fit1, type="parameter", which.par=3, xrange=c(-0.4,0.01),
   nint=100, method="proflik", verbose=TRUE)

# 100-year return level
ci(fit1, method="proflik", xrange=c(22,28), verbose=TRUE)

plot(fit1, "probprob")
plot(fit1, "qq")
plot(fit1, "hist")
plot(fit1, "hist", ylim=c(0,0.25))


# Non-stationary model. ---------------------------------------------------


# Location as a function of AO index.

fit2 <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit2
plot(fit2)
plot(fit2, "trace")
# warnings are not critical here.
# Sometimes the nllh or gradients
# are not defined.

return.level(fit2)

v <- make.qcov(fit2, vals=list(mu1=c(-1, 1)))
return.level(fit2, return.period=c(2, 20, 100), qcov=v)

# Note that first row is for AOindex = -1 and second
# row is for AOindex = 1.

lr.test(fit1, fit2)
# Also compare AIC and BIC

look1 <- summary(fit1, silent=TRUE)
look1 <- c(look1$AIC, look1$BIC)

look2 <- summary(fit2, silent=TRUE)
look2 <- c(look2$AIC, look2$BIC)

# Lower AIC/BIC is better.
names(look1) <- names(look2) <- c("AIC", "BIC")
look1
look2

par(mfrow=c(1,1))
plot(fit2, "rl")


# GP ----------------------------------------------------------------------

## Fitting the GP df to threshold excesses.

# Hurricane damage data.

data(damage)

ny <- tabulate(damage$Year)
# Looks like only, at most, 5 obs per year.

ny <- mean(ny[ny > 0])
fit0 <- fevd(Dam, damage, threshold=6, type="Exponential", time.units="2.05/year")
fit0
plot(fit0)
plot(fit0, "trace") # ignore the warning.

fit1 <- fevd(Dam, damage, threshold=6, type="GP", time.units="2.05/year")
fit1
plot(fit1) # ignore the warning.
plot(fit1, "trace")

return.level(fit1)

# lr.test(fit0, fit1)

# Fort Collins, CO precipitation

data(Fort)

## GP df

fit <- fevd(Prec, Fort, threshold=0.395, type="GP", units="inches", verbose=TRUE)
fit
plot(fit)
plot(fit, "trace")

ci(fit, type="parameter")
par(mfrow=c(1,1))
ci(fit, type="return.level", method="proflik", xrange=c(4,7.5), verbose=TRUE)
# Can check using locator(2).

ci(fit, type="parameter", which.par=2, method="proflik", xrange=c(0.1, 0.3),
   verbose=TRUE) 
# Can check using locator(2).


# GEV vs GP ---------------------------------------------------------------

data(PORTw)

summary(PORTw$TMX1) # maximum winter temperature (degrees centigrade).


# GEV ---------------------------------------------------------------------

# GEV
fit_gev <- fevd(TMX1, PORTw, units="deg C")
fit_gev
plot(fit_gev)
plot(fit_gev, "trace")
return.level(fit_gev)
return.level(fit_gev, do.ci=TRUE)
ci(fit_gev, return.period=c(2,20,100)) # Same as above.

# lr.test(fit0, fit_gev)
ci(fit_gev, type="parameter")
par(mfrow=c(1,1))
ci(fit_gev, type="parameter", which.par=3, xrange=c(-0.4,0.01),
   nint=100, method="proflik", verbose=TRUE)

# 100-year return level
ci(fit_gev, method="proflik", xrange=c(22,28), verbose=TRUE)

plot(fit_gev, "probprob")
plot(fit_gev, "qq")
plot(fit_gev, "hist")
plot(fit_gev, "hist", ylim=c(0,0.25))

# GP
fit_gp <- fevd(TMX1, PORTw, type = "GP", threshold = 2, units="deg C")
fit_gp
plot(fit_gp)
plot(fit_gp, "trace")
return.level(fit_gp)
return.level(fit_gp, do.ci=TRUE)
ci(fit_gp, return.period=c(2,20,100)) # Same as above.
