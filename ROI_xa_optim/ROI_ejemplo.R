library(tidyverse)

in_data_optim <- tribble( ~PME,  ~EHF,  ~Ap,   ~Pro, ~EHmin, ~EHmax,
                          83.00, 61.00, 1.10, 123.00,  10.00, 120.00, 
                          85.00, 72.00, 1.05, 124.00,  10.00, 160.00, 
                          87.00, 80.00, 0.97, 125.00,  10.00, 170.00, 
                          95.00, 75.00, 0.96, 110.00,  10.00, 170.00, 
                          90.00, 70.00, 1.01, 100.00,  10.00, 170.00, 
                          87.00, 63.00, 1.20,  90.00,  10.00, 170.00, 
                          82.00, 42.00, 1.30,  80.00,  10.00, 170.00, 
                          87.00, 33.00, 1.01,  70.00,  10.00, 170.00, 
                          88.00, 25.00, 0.95,  60.00,  10.00, 170.00, 
                          92.00, 65.00, 0.94,  90.00,  10.00, 170.00, 
                          93.00, 77.00, 0.99, 110.00,  10.00, 170.00, 
                          94.00, 65.00, 0.98, 130.00,  10.00, 180.00)

RVA_0 <- 45

aux <- c( 59.00, 0, 0,
          40.00 , 0, 
          107.00 , 128,
          27.00 ,0,0,   
          8.00 ,
          115.00 )

f_optim <- function(in_EHG, in_data_optim, RVA_0) {
  
  out <- in_data_optim %>% 
    mutate(EHG = in_EHG) %>% 
    mutate(EH = EHF + EHG,
           RVA = RVA_0 + cumsum(Pro - EH),
           ingresos = EHG * PME * Ap) 
  
  return(out)
}

f_optim(aux, in_data_optim, RVA_0)
f_optim(aux, in_data_optim, RVA_0) %>% pull(ingresos) %>% sum()

f_obj <- function(in_EHG) {
  
  probe <- f_optim(in_EHG = in_EHG, 
                   in_data_optim = in_data_optim, 
                   RVA_0 = RVA_0)
  
  # ## Constraints
  # # EHG(m) >= 0 por defecto, no necesario
  # # EH = PRO
  # if (abs(probe %>% summarise(EH = sum(EH) - sum(Pro))) <= 1e-16) return(-Inf)
  # 
  # # EH(m) <= EHmax(m)
  # if (any(probe$EH > probe$EHmax)) return(-Inf)
  # 
  # # EH(m) >= EHmax(m)
  # if (any(probe$EH < probe$EHmin)) return(-Inf)
  # 
  # # RVA >= 0
  # if (any(probe$RVA < 0)) return(-Inf)
  
  probe %>% 
    pull(ingresos) %>% 
    sum()
  
}

library(ROI)

# ROI_available_solvers()[,c("Package", "Repository")]
ROI_registered_solvers()

# Función objetivo
f_obj <- L_objective(L= in_data_optim$PME * in_data_optim$Ap, 
                     names = paste0("EHG_", 1:12))

# Bounds
opt_bounds <- V_bound(lb = pmax(0, in_data_optim$EHmin - in_data_optim$EHF), 
                      ub = pmax(0, in_data_optim$EHmax - in_data_optim$EHF),
                      names = c("EHG_min", "EHG_max"))

# Restricciones
A_constraints <- rbind(rep(1, 12),
                       lower.tri(matrix(1:144, 12, 12)) + diag(12))
dir_constraints <- c("==",
                     rep("<=", 12))
dir_constraints[10] <- "=="
rhs_constraints <- c(sum(in_data_optim$Pro) - sum(in_data_optim$EHF),
                     RVA_0 + cumsum(in_data_optim$Pro) - cumsum(in_data_optim$EHF))
opt_constraints <- L_constraint(L = A_constraints, 
                                dir = dir_constraints, 
                                rhs = rhs_constraints, 
                                names = paste0("EHG_", 1:12))

lp <- OP(objective = f_obj,
         constraints = opt_constraints,
         bounds = opt_bounds,
         maximum = TRUE)

terms(objective(lp))

ROI_applicable_solvers(lp)

(sol <- ROI_solve(lp))

objective(lp)(sol$solution)
solution(sol)

res_probe <- f_optim(sol$solution, in_data_optim, RVA_0) 

res_probe %>% pull(ingresos) %>% sum


res_probe %>% mutate(cumsum_EHG = cumsum(EHG), 
                     cumsum_Pro = RVA_0 + cumsum(Pro), 
                     check_RVA = cumsum_Pro - cumsum_EHG, 
                     max_EHG = RVA_0 + cumsum_Pro - EHF)


# Función optimización  ---------------------------------------------------


check_in_data_optim <- function(in_data_optim) {
  
  if (any(in_data_optim$EHF < 0)) {
    stop("La Energía Hidráulica Fluyente mensual EHF(m) no puede ser negativa.")
  }
  
  if (any(in_data_optim$Pro < 0)) {
    stop("La Energía Hidráulica Producible (recibida) mensual Pro(m) no puede ser negativa.")
  }
  
  if (any(in_data_optim$EHmin < 0)) {
    stop("La mínima Energía Hidráulica Fluyente mensual permitida EHmin(m) no puede ser negativa.")
  }
  
  if (any(in_data_optim$EHmax < 0)) {
    stop("La máxima Energía Hidráulica Fluyente mensual permitida EHmax(m) no puede ser negativa.")
  }
}

optim_hydro_gestionable <- function(in_data_optim, RVA_0) {
  
  check_in_data_optim(in_data_optim)
  
  ## FUNCIÓN OBJETIVO
  f_obj <- L_objective(L= in_data_optim$PME * in_data_optim$Ap, 
                       names = paste0("EHG_", 1:12))
  
  ## BOUNDS
  # La variable objetivo Energía Hidráulica Gestionable no puede ser negativa 
  # ningún mes
  # La Energía Hidráulica Total tiene límites mensuales inferior y superior 
  opt_bounds <- V_bound(lb = pmax(0, in_data_optim$EHmin - in_data_optim$EHF), 
                        ub = pmax(0, in_data_optim$EHmax - in_data_optim$EHF),
                        names = c("EHG_min", "EHG_max"))
  
  ## RESTRICCIONES
  # Anual: la Energía Hidráulica Total producida en el año tiene que ser igual a 
  #        la Producible (recibida) anual
  # Reservas: 1. Las Reservas a final de mes no pueden ser negativas
  #           2. La reserva a final de septiembre tiene que ser cero
  A_constraints <- rbind(# Anual
    rep(1, 12), 
    # Reservas
    lower.tri(matrix(1:144, 12, 12)) + diag(12)) 
  
  dir_constraints <- c(# Anual
    "==", 
    # Reservas
    rep("<=", 12)) 
  # Reservas septiembre
  dir_constraints[10] <- "=="
  
  rhs_constraints <- c(# Anual
    sum(in_data_optim$Pro) - sum(in_data_optim$EHF), 
    # Reservas
    RVA_0 + cumsum(in_data_optim$Pro) - cumsum(in_data_optim$EHF)) 
  
  opt_constraints <- L_constraint(L = A_constraints, 
                                  dir = dir_constraints, 
                                  rhs = rhs_constraints, 
                                  names = paste0("EHG_", 1:12))
  
  ## PPROBLEMA DE OPTIMIZACIÓN
  lp <- OP(objective = f_obj,
           constraints = opt_constraints,
           bounds = opt_bounds,
           maximum = TRUE)
  
  ## SOLUCIÓN
  sol <- ROI_solve(lp)
  
  return(sol)
}

sol_optim <- optim_hydro_gestionable(in_data_optim, RVA_0)

res_probe <- f_optim(sol_optim$solution, in_data_optim, RVA_0) 

res_probe
