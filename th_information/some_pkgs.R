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