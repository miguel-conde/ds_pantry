library(tidyverse)

# https://stats.stackexchange.com/questions/136563/linear-regression-with-individual-constraints-in-r

set.seed(123)
X <- cbind(1, matrix(rnorm(20*2), nrow = 20, ncol = 2))
betas <- c(2, 3, -1)
y <- X %*% betas + rnorm(20)

min.RSS <- function(data, par){
  with(data, sum((par[1]*x1 + par[2]*x2 + par[3]*x3 - y)^2))
}
# All data required is stored in a data frame, column one and two contains 
# regressors x1,x2, and column three contains the observations y:
  
# dat = data.frame(x1=c(1,2), x2=c(2,3), y=c(5,6))
dat <- data.frame(cbind(X, y))
names(dat) <- c("x1", "x2", "x3", "y")

# Then the constrained optimization can be conducted via

UI <- diag(c(1,1,1))
CI <- c(0,0,0.5)

myObj        = constrOptim(theta = c(1,1,1), 
                           f = min.RSS, 
                           grad = NULL, 
                           data = dat, 
                           ui = UI, 
                           ci = CI)
coefficients = myObj$par
