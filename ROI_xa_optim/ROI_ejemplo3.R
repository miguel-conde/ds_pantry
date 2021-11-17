library(ROI)
library(tidyverse)

# Reformulations ----------------------------------------------------------

Q <- rbind(c(0, 3, 0), c(0, 0, 1), c(0, 0, 0))

bqp <- OP(Q_objective(Q = Q + t(Q), 
                      L = c(-1, -4, -1)), 
          types = rep("B", 3))

glpk_signature <- ROI_solver_signature("glpk")

head(glpk_signature, 3)

milp <- ROI_reformulate(x = bqp, to = glpk_signature)

# ROI_solve(milp, solver = "glpk")


# Ejemplo 3 ---------------------------------------------------------------

N <- 168

# Price
e <- cumsum(rnorm(N, 0, 1)) + 40 

# Generation
p <- rnorm(N, 0, 5) + rnorm(N, 216, 20)*sin(7*2*pi/N * (1:N)) 
p[p < 0] <- 0
            
P_I <- 216 - 50
P_MAX <- .2 * 216
C <- 4 * P_MAX

Qm <- lapply(seq_along(p), 
             function(i) e[i]*rbind(c(0, 0, 0), c(p[i], 0, -1), c(0, 0, 0)))

Q <- matrix(0, ncol = 3*N, nrow = 3*N)

for (m in seq_along(Qm)) {
  r_c <- (3*(m-1)+1):(3*(m-1)+3)
  Q[r_c, r_c] <- Qm[[m]]
}

L <- matrix(c(rep(0, N), -p, rep(1, N)), byrow = FALSE,
            nrow = N)
  
L_cons
dir 
rh

qp <-  OP(Q_objective(Q = Q, L = L),
          # constraints = L_constraint()),
          types = rep(c("C", "B", "C"), N),
          bounds      = V_bound(li = 1:(3*N), 
                                ui = 1:(3*N), 
                                lb = rep(c(0, 0, 0), N), 
                                ub = rep(c(1, 1, P_MAX), N), 
                                nobj = 3*N),
          maximum = TRUE)

ROI_applicable_solvers(qp)

(blp_sol <- ROI_solve(qp, solver = "msbinlp", method = "glpk", nsol_max = 32))
(blp_sol <- ROI_solve(qp, solver = "glpk"))
          
          

          