# Modelling Dependence with Copulas in R
# https://www.r-bloggers.com/2015/10/modelling-dependence-with-copulas-in-r/

# Risk estimation using copulas
# http://www.ub.edu/rfa/R/copulas.html

# Elegir Cópulas Matemáticas en R. Ejemplo en Forex
# https://ricovictor.com/index.php/2021/09/21/elegir-copulas-matematicas-en-r-ejemplo-en-forex/


# 1 - Modelling Dependence with Copulas in R ------------------------------


# 1.1 - How copulas work (roughly) ----------------------------------------

library(mvtnorm)

set.seed(100)


n <- 2000
Sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow = 3)
m <- nrow(Sigma)

# z <- mvrnorm(n,mu=rep(0, m),Sigma=sigma,empirical=T)

library(mvtnorm)
z <- rmvnorm(n, mean = rep(0, m), sigma = Sigma)

library(psych)
cor(z,method='spearman')
pairs.panels(z,method='spearman')

# And now comes the magic trick: recall that if (X) is a random variable with 
# distribution (F) then (F(X)) is uniformly distributed in the interval [0, 1]. 
# In our toy example we already know the distribution (F) for each of the three 
# random variables so this part is quick and straightforward.
u <- pnorm(z)
pairs.panels(u)

library(rgl)
plot3d(u[,1],u[,2],u[,3],pch=20,col='navyblue')

x1 <- qgamma(u[,1],shape=2,scale=1)
x2 <- qbeta(u[,2],2,2)
x3 <- qt(u[,3],df=5)
plot3d(x1,x2,x3,pch=20,col='blue')

df <- cbind(x1,x2,x3)
pairs.panels(df,method='spearman')
cor(df,meth='spearman')


# 1.2 - Using the copula package ------------------------------------------

library(copula)
set.seed(100)
myCop <- normalCopula(param=c(0.4,0.2,-0.8), dim = 3, dispstr = "un")
myMvd <- mvdc(copula=myCop, margins=c("gamma", "beta", "t"),
              paramMargins=list(list(shape=2, scale=1),
                                list(shape1=2, shape2=2), 
                                list(df=5)) )


Z2 <- rMvdc(2000, myMvd)
colnames(Z2) <- c("x1", "x2", "x3")
pairs.panels(Z2)
