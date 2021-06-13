library(tidyverse)

library(quantmod)
library(timetk)

FECHA_ADQUISICIÓN <- "2005-06-02"
FECHA_TRANSMISION <- "2020-01-06"

rep = getSymbols("REP.MC", src="yahoo", auto.assign = FALSE, from = "2000-01-01")
san = getSymbols("san.MC", src="yahoo", auto.assign = FALSE, from = "2000-01-01")

probe_rep <- rep$REP.MC.Adjusted
probe_san <- san$SAN.MC.Adjusted

plot(probe_rep)
plot(probe_san)


# N <- 1000
# y <- y - min(y)

y <- probe_rep %>% 
  timetk::tk_tbl() %>% 
  fill(REP.MC.Adjusted) %>% 
  pull(2) #timetk::tk_zoo() #cumsum(rnorm(N))

N <- length(y)

retornos <- diff(log(y)) %>% tail(-1)

dens_ret <- density(retornos)

plot(dens_ret)

retornos_simulados <- sample(dens_ret$x, N, replace = TRUE, prob = dens_ret$y)

y_simulado <- exp(cumsum(c(log(y[1]), retornos_simulados)))

plot(y, type = "l", ylim = c(min(y, y_simulado), max(y, y_simulado)))
lines(y_simulado, col = "red")
                  