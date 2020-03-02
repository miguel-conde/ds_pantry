library(tidyverse)


# SETUP -------------------------------------------------------------------


set.seed(123)

N <- 10000

sol_sp_idx <- 1:N

# idx de las sol_sp que cumplen las distintas restricciones,
# tanto activas (checkbox marcada por el usuario) como no activas
r_sem_ok_idx <- sample(sol_sp_idx, 5000)
r_com_ok_idx <- sample(sol_sp_idx, 100)
r_3_ok_idx <- sample(sol_sp_idx, 700)
r_7_ok_idx <- sample(sol_sp_idx, 500)
# r_v_ok_idx <- sample(sol_sp_idx, 4500)
r_v_ok_idx <- sample(sol_sp_idx + 10, 4500)

# De MÁS a MENOS PRIORITARIA - De momento hard coded, decisión de negocio
v_r <- list(r_sem_ok_idx = r_sem_ok_idx,
            r_com_ok_idx = r_com_ok_idx,
            r_3_ok_idx = r_3_ok_idx,
            r_7_ok_idx = r_7_ok_idx,
            r_v_ok_idx = r_v_ok_idx
            )


# FUNCTIONS ---------------------------------------------------------------


showBits <- function(r) as.logical(rawToBits(as.raw(r)))

r_to_apply <- function(i, v_r) {
  # Identify which onstraints to apply
  
  N_r <- length(v_r)
  
  out_i <- rev(showBits(i)[1:N_r])
  names(out_i) <- names(v_r)
  # print(out_i)
  
  return(out_i)
}


# r_reduce <- function(x) {
#   
#   if(length(x) < 2) return(x)
#   
#   for(i in 1:length(x)) {
#     out <- intersect(x[[1]], x[[2]])
#   }
#   
#   return(out)
#   
# }


r_intersect <- function(v_r) {
  
  # Find the solutions compliant with the most valuable combination of
  # constraints
  
  N_r <- length(v_r)
  
  # Esta secuencia, junto al orden de los elementos de v_r, codifica cómo 
  # tratar las restricciones
  seq_pos <- (2^N_r - 1):(2^(N_r - 1))
  
  for(i in seq_pos) {
    
    r_applicable <- r_to_apply(i, v_r)
    
    res_i <- Reduce(intersect, v_r[r_applicable])
    # res_i <- r_reduce(v_r[r_active])
    # print(length(res_i))
    
    if(length(res_i) > 0) break
  }
  
  out <- list(r_compliant = r_applicable, res_i = res_i)
  
  return(out)
  
}

# 1st test
r_intersect(v_r)

# r_ok <- function(v_r) {
#   
#   out <- union(v_r[[1]], r_intersect(v_r)$res_i)
#   
#   return(out)
# }
# 
# r_ok(v_r)

# TESTS -------------------------------------------------------------------

## No hay intersección

r_sem_ok_idx <- 1:11
r_com_ok_idx <- 11:20
r_3_ok_idx <- 21:30
r_7_ok_idx <- 31:40
r_v_ok_idx <- 41:50


# De más a menos prioritaria
v_r <- list(r_sem_ok_idx = r_sem_ok_idx,
            r_com_ok_idx = r_com_ok_idx,
            r_3_ok_idx = r_3_ok_idx,
            r_7_ok_idx = r_7_ok_idx,
            r_v_ok_idx = r_v_ok_idx
)

r_intersect(v_r)

# r_ok(v_r)

# No se cumple la primera
r_sem_ok_idx <- c()
v_r <- list(r_sem_ok_idx = r_sem_ok_idx,
            r_com_ok_idx = r_com_ok_idx,
            r_3_ok_idx = r_3_ok_idx,
            r_7_ok_idx = r_7_ok_idx,
            r_v_ok_idx = r_v_ok_idx
)

r_intersect(v_r)

# r_ok(v_r)


# Uno cualquiera está vacío
r_sem_ok_idx <- 1:10
r_3_ok_idx <- c()
v_r <- list(r_sem_ok_idx = r_sem_ok_idx,
            r_com_ok_idx = r_com_ok_idx,
            r_3_ok_idx = r_3_ok_idx,
            r_7_ok_idx = r_7_ok_idx,
            r_v_ok_idx = r_v_ok_idx
)

# r_ok(v_r)


