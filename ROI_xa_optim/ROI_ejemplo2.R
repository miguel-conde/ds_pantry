library(ROI)
library(tidyverse)


# TUTORIAL ----------------------------------------------------------------


N_HOURS <- 1000 #52*7*24
P_PLANTA <- 130 # MW
P_MAX <- 0.2 * P_PLANTA
C <- 4 * P_PLANTA

set.seed(2021)
spot <- 40 + cumsum(rnorm(N_HOURS, 0, 2))


optim_bateria <- function(n_hours, p_max, c_max, spot, the_solver = "glpk", ...) {

  A <- sapply(1:n_hours, 
              function(x) {
                c(rep(1, x), rep(0, n_hours - x))
              }) %>% t()
  A <- rbind(A, A)
  
  dir <- c(rep("<=", n_hours), rep(">=", n_hours))
  rhs <- c(rep(c_max, n_hours), rep(0, n_hours))

  lp <- OP(objective   = L_objective(spot),
           constraints = L_constraint(A, dir = dir, rhs = rhs),
           types       = NULL, # Default
           bounds      = V_bound(li = 1:n_hours, ui = 1:n_hours, 
                                 lb = rep(-p_max, n_hours), ub = rep(p_max, n_hours), 
                                 nobj = n_hours),
           maximum     = TRUE)
  
  time_log <- system.time({
    (lp_sol <- ROI_solve(lp, solver = the_solver, ...))
  })
  
  out <- list(optim_sol = lp_sol,
              time_log = time_log)
  
  return(out)
  
}

res_optim <- optim_bateria(n_hours = N_HOURS,
                           p_max   = P_MAX, 
                           c_max   = C,
                           spot    = spot)

res_optim_alabama <- optim_bateria(n_hours = N_HOURS,
                           p_max   = P_MAX, 
                           c_max   = C,
                           spot    = spot,
                           the_solver = "alabama", 
                           control = list(start = rlnorm(N_HOURS)))

res_optim_lpsolve <- optim_bateria(n_hours = N_HOURS,
                                   p_max   = P_MAX, 
                                   c_max   = C,
                                   spot    = spot,
                                   the_solver = "lpsolve")

