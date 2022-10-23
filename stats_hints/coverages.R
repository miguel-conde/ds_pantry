# https://content.wolfram.com/uploads/sites/19/2021/02/Cook.pdf

library(tidyverse)

# We use an example with n = 10. A plot will show how bad the approximation can 
# be and also displays the output of each step of the algorithm. We will compute 
# the coverage probability for p = 0.5. The input and output are presented in a 
# conversational style with some editorial comments along the way.
# We wish to determine the upper 2.5 percentage value from the standard normal 
# distribution. The variable zc is often called a critical value for the 
# standard normal distribution.

p <- 0.5
n <- 10
alpha <- .05

zc <- qnorm(alpha/2, lower.tail = FALSE)

se_binom <- function(x, n) {
  
  p_hat <- x / n
  
  var_hat <- p_hat * (1 - p_hat)
  
  out_se <- sqrt(var_hat / n)
  
  return(out_se)
}

xs <- 0:n


x_true <- xs[(xs/n - zc * se_binom(xs, n)) < p & (xs/n + zc * se_binom(xs, n) > p) ]

sum(dbinom(x_true, size = n, p))


coverage_binom <- function(n, alpha, ps) {
  
  xs <- 0:n
  
  zc <- qnorm(alpha/2, lower.tail = FALSE)
  
  p_hat <- xs/n
  ci_half <- zc * se_binom(xs, n)
  
  sapply(ps, function(p) {
    
    x_true <- xs[(p_hat - ci_half) < p & (p_hat + ci_half > p) ]
    
    sum(dbinom(x_true, size = n, p))
  })
}

coverage_binom(n, alpha, .5)
coverage_binom(n, alpha, seq(0, 1, by = .01))

ps <- seq(0, 1, by = .01)
coverages <- coverage_binom(n, alpha, ps)

plot(ps, coverages, type = "l")
abline(h = 1 - alpha, lty = 2)


plot_coverage_binom <- function(n, alpha, seq_by = 0.01) {
  
  ps <- seq(0, 1, by = seq_by)
  coverages <- coverage_binom(n, alpha, ps)
  
  plot(ps, coverages, type = "l")
  abline(h = 1 - alpha, lty = 2, col = "red")
  
}

plot_coverage_binom(n, alpha)

library(manipulate)
manipulate(plot_coverage_binom(n, alpha), n = slider(5, 100))
