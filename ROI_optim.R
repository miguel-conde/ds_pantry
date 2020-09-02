library(ROI)


#

A <- rbind(c(5, 7, 2), c(3, 2, -9), c(1, 3, 1))
dir <- c("<=", "<=", "<=")
rhs <- c(61, 35, 31)

lp <- OP(objective   = L_objective(c(3, 7, -12)),
         constraints = L_constraint(A, dir = dir, rhs = rhs),
         types       = NULL, # Default
         bounds      = V_bound(li = 3, ui = 3, lb = -10, ub = 10, nobj = 3),
         maximum     = TRUE)

lp

# Alternatively:

lp <- OP()

objective(lp)   <- L_objective(c(3, 7, -12)) # 3x_1 + 7x_2 -12x_3
constraints(lp) <- L_constraint(A, dir = c("<=", "<=", "<="), rhs = rhs)
bounds(lp)      <- V_bound(li = 3, ui = 3, lb = -10, ub = 10, nobj = 3)
# types(lp)
maximum(lp)     <- TRUE

lp

param <- rep.int(1, length(objective(lp))) # x_1 = x_2 = x_3 = 1
objective(lp)(param) 

terms(objective(lp))

ROI_available_solvers(lp)[, c("Package", "Repository")]
ROI_installed_solvers()
ROI_registered_solvers()
library(ROI.plugin.glpk)
ROI_registered_solvers()
ROI_applicable_solvers(lp)

(lp_sol <- ROI_solve(lp, solver = "glpk"))



# L1 Regression -----------------------------------------------------------


create_L1_problem <- function(x, j) {
  # browser()
  m <- ncol(x) + 1L
  n <- 2 * nrow(x)
  beta <- double(m + n)
  beta[j + 1] <- 1
  
  OP(objective = L_objective(c(rep(0, m), rep(1, n))),
     constraints = rbind(
       L_constraint(L   = cbind(1, x, diag(nrow(x)), -diag(nrow(x))),
                    dir = eq(nrow(x)), 
                    rhs = rep(0, nrow(x))),
       L_constraint(L   = beta, 
                    dir = "==", 
                    rhs = -1)),
     bounds = V_bound(li = seq_len(m), 
                      lb = rep(-Inf, m),
                      nobj = length(beta)))
}


data(stackloss)
l1p <- create_L1_problem(as.matrix(stackloss), 4)
L1_res <- ROI_solve(l1p, solver = "glpk")
solution(L1_res)[1:ncol(stackloss)]



# OLS ---------------------------------------------------------------------

create_ols_problem <- function(y, X, intcpt = TRUE) {
  
  X_design <- as.matrix(X)
  
  if (intcpt == TRUE) {
    
    X_design <- cbind(intcpt = rep(1, nrow(X_design)), X_design)
  } 
  
  n <- ncol(X_design)
  col_names <-  colnames(X_design)
  
  sse <- function(beta) {
    
    y_hat <- as.numeric(X_design %*% beta)
    
    out <- sum((y - y_hat)^2)
    
    return(out)
  }
  
  out <- OP()
  
  objective(out) <- F_objective(sse, n = n, names = col_names)
  bounds(out) <- V_bound(ld = -Inf, ud = Inf, nobj = n)
  
  return(out)
}

# NO INTERCPT
olsp <- create_ols_problem(y = stackloss$stack.loss, X = stackloss[, -4], intcpt = FALSE)

ROI_applicable_solvers(olsp)

(olsp_sol <- ROI_solve(olsp, solver = "nlminb", start = c(1,1,1)))

solution(olsp_sol)
objective(olsp)(solution(olsp_sol))

ref_lm <- lm(stack.loss ~ .-1, stackloss)
summary(ref_lm)

sum((fitted(ref_lm) - stackloss$stack.loss)^2)

# INTERCEPT
olsp <- create_ols_problem(y = stackloss$stack.loss, X = stackloss[, -4])

ROI_applicable_solvers(olsp)

(olsp_sol <- ROI_solve(olsp, solver = "nlminb", start = c(1,1,1,1)))

solution(olsp_sol)
objective(olsp)(solution(olsp_sol))

ref_lm <- lm(stack.loss ~ ., stackloss)
summary(ref_lm)
confint(ref_lm)

sum((fitted(ref_lm) - stackloss$stack.loss)^2)

# BOOT
library(boot)

sol <- function(d, i) {
  olsp <- create_ols_problem(y = d[i,]$stack.loss, X = d[i, -4])
  (olsp_sol <- ROI_solve(olsp, solver = "nlminb", start = c(1,1,1,1)))
  solution(olsp_sol)
}

# sol <- function(d, i) {
#   coef(lm(stack.loss ~ ., d[i,]))
# }

boot_res <- boot(stackloss, sol, R = 10000)

boot_res
boot.ci(boot_res, index = 1)
boot.ci(boot_res, index = 2)
boot.ci(boot_res, index = 3)
boot.ci(boot_res, index = 4)

# Si H_o (coeficiente = 0)
t_H_0 <- boot_res$t[,4] - mean(boot_res$t[,4])

density(t_H_0) %>% plot
ecdf(t_H_0) %>% plot

sum(boot_res$t0[4] > t_H_0) / 10000
ecdf(t_H_0)(boot_res$t0[4])