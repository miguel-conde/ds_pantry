library(tidyverse)
library(infotheo)

mutinformation(discretize(iris))

mutinformation(discretize(iris[1]), discretize(iris[5]))
mutinformation(discretize(iris[1:75, 1]), discretize(iris[1:75, 5]))
mutinformation(discretize(iris[76:150, 1]), discretize(iris[76:150, 5]))

# https://www.math.uzh.ch/pages/varrank/articles/varrank.html

library(varrank)

mi.data(X = iris[1], Y = iris[5], discretization.method = "kmeans") 

res_varrank <- varrank(data.df = iris, 
                       method = "estevez", 
                       variable.important = "Species", 
                       discretization.method = "sturges", 
                       algorithm = "forward", 
                       scheme="mid", 
                       verbose = FALSE)
res_varrank

plot(res_varrank)

entropy.data(freqs.table = table(discretization(data.df = iris[, "Species"], 
                                                discretization.method = "rice", 
                                                freq = FALSE)))

# https://bioconductor.org/packages/release/bioc/vignettes/Informeasure/inst/doc/Informeasure.html

###
N <- 1000
X <- rnorm(N)
X2 <- rnorm(N)
Y <- 3*X + rnorm(N, 0, 1)
Z1 <-X^2 + X^3 + X^4 + X^6 + rnorm(N, 0, 10)
Z2 <- X^2 + X^3 + X^4 + X^6 + Y^2 + Y^3 + Y^4 + Y^6 + rnorm(N, 0, 10)

probe <- tibble(X, X2, Y, Z1, Z2)
cor(probe)


res <- mutinformation(discretize(probe))
res
res %>% sweep(1, diag(res), "/")

mi.data(X = probe$X, Y = probe$Z1, discretization.method = "kmeans") 
mi.data(X = probe$X, Y = probe$Z2, discretization.method = "kmeans") 
