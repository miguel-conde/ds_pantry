library(ROI)
library(tidyverse)


# TUTORIAL ----------------------------------------------------------------


N_HOURS <- 1000 #52*7*24
P_PLANTA <- 130 # MW
P_MAX <- 0.2 * P_PLANTA
C <- 4 * P_PLANTA

A <- sapply(1:N_HOURS, 
            function(x) {
              c(rep(1, x), rep(0, N_HOURS - x))
            }) %>% t()
A <- rbind(A, A)

dir <- c(rep("<=", N_HOURS), rep(">=", N_HOURS))
rhs <- c(rep(C, N_HOURS), rep(0, N_HOURS))

set.seed(2021)
spot <- 40 + cumsum(rnorm(N_HOURS, 0, 2))

lp <- OP(objective   = L_objective(spot),
         constraints = L_constraint(A, dir = dir, rhs = rhs),
         types       = NULL, # Default
         bounds      = V_bound(li = 1:N_HOURS, ui = 1:N_HOURS, 
                               lb = rep(-P_MAX, N_HOURS), ub = rep(P_MAX, N_HOURS), 
                               nobj = N_HOURS),
         maximum     = TRUE)

lp

# # Alternatively:
# 
# lp <- OP()
# 
# objective(lp)   <- L_objective(c(3, 7, -12)) # 3x_1 + 7x_2 -12x_3
# constraints(lp) <- L_constraint(A, dir = c("<=", "<=", "<="), rhs = rhs)
# bounds(lp)      <- V_bound(li = 3, ui = 3, lb = -10, ub = 10, nobj = 3)
# # types(lp)
# maximum(lp)     <- TRUE
# 
# lp
# 
# param <- rep.int(1, length(objective(lp))) # x_1 = x_2 = x_3 = 1
# objective(lp)(param) 
# 
# terms(objective(lp))

ROI_available_solvers(lp)[, c("Package", "Repository")]
ROI_installed_solvers()
ROI_registered_solvers()
library(ROI.plugin.glpk)
ROI_registered_solvers()
ROI_applicable_solvers(lp)

system.time({
  (lp_sol <- ROI_solve(lp, solver = "glpk"))
})

system.time({
  (lp_sol <- ROI_solve(lp, solver = "alabama", 
                       control = list(start = rlnorm(N_HOURS))))
})

optim_bateria <- function(n_hours, p_max, c_max, spot, the_solver = "glpk", ...) {
  
  n_hours <- 1000 #52*7*24
  P_PLANTA <- 130 # MW
  p_max <- 0.2 * P_PLANTA
  c_max <- 4 * P_PLANTA
  
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
