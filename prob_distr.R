
# DISTRIBUCIONES DISCRETAS ------------------------------------------------
  
# Binomial ----------------------------------------------------------------

# Parameters
num_ber_exp <- 10 # n, size
p <- .25

# Probability mass function - Función de probabilidad
binom_pmf <- dbinom(x = 0:num_ber_exp, size = num_ber_exp, prob = p)
binom_pmf
plot(x = 0:num_ber_exp, y = binom_pmf, 
     type = "h", 
     main = "Binomial PMF (n = 10, p = .25)",
     xlab = "# Successes",
     ylab = "p")
 
# Función de distribución
binom_cdf <- pbinom(q = 0:num_ber_exp, size = num_ber_exp, prob = p)
binom_cdf
plot(x = 0:num_ber_exp, y = binom_cdf, 
     type = "h",
     main = "Binomial CDF (n = 10, p =.25)",
     xlab = "# Successes",
     ylab = "p")

# Probabilidad de que X >= 6:
pbinom(q = 5, num_ber_exp, p, lower.tail = F)

# Quantiles - Inversa de la función de distribución
qbinom(p = binom_cdf, size = num_ber_exp, prob = p)
qbinom(p = c(0.25, 0.8, 0.98), size = num_ber_exp, prob = p)


# Geometric ---------------------------------------------------------------

## Parameters
p <-  .25

# Geo PMF
geo_pmf <- dgeom(x = 0:25, prob = p)
geo_pmf
plot(x = 0:25, y = geo_pmf, 
     type = "h", 
     ylim = c(0, .3),
     main = "Geometric PMF (p = .25)",
     xlab = "First Successes",
     ylab = "p")

# Geo CDF
geo_cdf <- pgeom(q = 0:25, prob = p)
geo_cdf
plot(x = 0:25, y = geo_cdf, 
     type = "h",
     ylim = c(0, 1),
     main = "Geometric CDF (p =.25)",
     xlab = "First Successes",
     ylab = "p")

# Probabilidad de que X >= 6:
pgeom(q = 5, prob = p, lower.tail = F)

# Quantiles - Inversa de la función de distribución
qgeom(p = geo_cdf, prob = p)
qgeom(p = c(0.25, 0.8, 0.98), prob = p)
