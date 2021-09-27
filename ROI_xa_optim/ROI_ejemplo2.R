library(ROI)
library(tidyverse)


# TUTORIAL ----------------------------------------------------------------


N_HOURS <- 1000 #52*7*24
P_PLANTA <- 130 # MW
P_MAX <- 0.2 * P_PLANTA
CAP_MAX <- 4 * P_PLANTA

set.seed(2021)
spot <- 40 + cumsum(rnorm(N_HOURS, 0, 2))


optim_bateria <- function(spot, p_max, c_max, the_solver = "glpk", ...) {
  
  n_hours <- length(spot)
  
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
           bounds      = V_bound(li = 1:n_hours, 
                                 ui = 1:n_hours, 
                                 lb = rep(-p_max, n_hours), 
                                 ub = rep(p_max, n_hours), 
                                 nobj = n_hours),
           maximum     = TRUE)
  
  time_log <- system.time({
    (lp_sol <- ROI_solve(lp, solver = the_solver, ...))
  })
  
  out <- list(optim_sol = lp_sol,
              time_log = time_log)
  
  return(out)
  
}

res_optim <- optim_bateria(spot    = spot,
                           p_max   = P_MAX, 
                           c_max   = CAP_MAX)

res_optim_alabama <- optim_bateria(spot    = spot,
                                   p_max   = P_MAX, 
                                   c_max   = CAP_MAX,
                                   the_solver = "alabama", 
                                   control = list(start = rlnorm(N_HOURS)))

res_optim_lpsolve <- optim_bateria(spot    = spot,
                                   n_hours = N_HOURS,
                                   p_max   = P_MAX, 
                                   c_max   = CAP_MAX,
                                   the_solver = "lpsolve")

set.seed(2021)
spot <- 40 + cumsum(rnorm(3000, 0, 2))

res_time_bench <- 
  res <- lapply(100*(1:30), function(N) {
    print(N)
    
    x <- optim_bateria(spot    = spot[1:N],
                       p_max   = P_MAX, 
                       c_max   = CAP_MAX,
                       the_solver = "glpk")
    print(x$time_log)
    x$time_log
  })