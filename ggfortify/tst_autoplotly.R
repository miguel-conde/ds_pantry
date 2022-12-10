# https://github.com/sinhrks/ggfortify
# https://github.com/terrytangyuan/autoplotly
# https://terrytangyuan.github.io/2018/02/12/autoplotly-intro/

library(tidyverse)
library(autoplotly)

library(dlm)
form <- function(theta){
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))
}
model <- form(dlmMLE(Nile, parm = c(1, 1), form)$par)
filtered <- dlmFilter(Nile, model)
autoplotly(filtered)

library(strucchange)
autoplotly(breakpoints(Nile ~ 1), ts.colour = "blue", ts.linetype = "dashed",
           cpt.colour = "dodgerblue3", cpt.linetype = "solid")


