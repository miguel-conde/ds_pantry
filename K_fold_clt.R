library(tidyverse)

# Vamos a simular un proceso de K-fold CV en el que para cada K medimos
# un MAE.
K <- 10

# Cada fold tiene n muestras:
n <- 1000

# Simulación del AE (Absolute Error)
get_aes <- function(n, RFUN, ..., POST_FUN = NULL, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  out <- RFUN(n, ...)
  
  if(!is.null(POST_FUN)) {
    out <- POST_FUN(out)
  }
  
  return(out)
}

get_aes(n, rnorm, mean = 0, sd = 1) %>% hist
get_aes(n, rnorm, mean = 0, sd = 1, seed = 123) %>% hist

get_aes(n, runif, min = -1, max = 1) %>% hist

# 1er fold.
# Debemos esperar una media y una desviación estándard parecidas a las que 
# especifiquemos

avg_1 <- get_aes(n, rnorm, mean = 0, sd = 1) %>% mean
sd_1 <- get_aes(n, rnorm, mean = 0, sd = 1) %>% sd

# También (TLC) podemos estimar:
#     - Media de la VA AVG_1: avg_1
#     - Dessviación estándar de la VA AVG_1: sd_1 / sqrt(n) = ERROR ESTÁNDARD

sd_avg_1 <- sd_1 / sqrt(n)

# Construyamos ahora todos los resultados de los K folds

out <- tibble(K_fold = 1:K, avg_K = NA, sd_K = NA, se_K = NA)
for(i in 1:K) {
  aux <- get_aes(n, rnorm, mean = 0, sd = 1)
  
  out[i, c("avg_K", "sd_K", "se_K")] <- c(mean(aux), sd(aux), sd(aux) / sqrt(n))
}

# Ahota podemos estimar:
#    - Media de las medias (VA AVG_AVG): avg_avg
#    - Desviación estándar de la VA AVG_AVG: sd_avg / sqrt(K)

avg_avg_res <- c(avg_avg = mean(out$avg_K),
                 sd_avg = sd(out$avg_K),
                 se_avg = sd(out$avg_K) / sqrt(K))
avg_avg_res

# Por tanto, la estimación de la desviación estándar original a partir del
# error estándar de esta avg_avg es:

avg_avg_res["se_avg"] * sqrt(K) * sqrt(n)

# No es muy buena porque K es pequeño

# Una distribución normal estandar positiva tiene
# - Media:
# - Desviación estándar:
