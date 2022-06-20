
best_pdf <- function(in_series) {
  
  require(fitdistrplus)
  
  probe <- as.numeric(in_series)
  
  if (sd(probe) != 0) { # Para el caso de valores iguales en toda la serie
    
    # Parámetros iniciales para gamma y gev
    med_v <- mean(probe)
    var_v <- var(probe)
    
    alpha_g <- med_v^2 / var_v
    beta_g <- var_v / med_v
    
    xi_ge <- .2
    sigma_ge <- sqrt(var_v * xi_ge^2 / (gamma(1 -2 * xi_ge) - gamma(1 - xi_ge)^2))
    mu_ge    <- med_v - sigma_ge * (gamma(1 - xi_ge) - 1) / xi_ge
    
    options(warn = -1)
    
    distris <- c("norm", "lnorm", "weibull", "gev", "gamma")
    lst_starts <- list(NULL, NULL, NULL, 
                       list(loc = mu_ge, scale = sigma_ge, shape = xi_ge), 
                       list(shape = alpha_g, rate = beta_g))
    lst_fit_dist <- vector(mode = "list", length = length(distris))
    names(lst_fit_dist) <- names(lst_starts) <- distris
    for (i in seq_along(distris)) {
      
      lst_fit_dist[[i]] <- tryCatch(gofstat(fitdist(probe, distris[i], 
                                                    start = lst_starts[[distris[i]]])),
                                    error = function(e) { return(NULL) })
    }
    
    wm <- lst_fit_dist %>% lapply(function(x) if(!is.null(x)) x$bic else Inf) %>% 
      which.min()
    
    marg <- distris[wm]
    param <- fitdist(probe, marg, start = lst_starts[[distris[wm]]])$estimate
    
    options(warn = 0)
    
  } else {
    marg = "norm"
    param <- c(0,1)
    names(param) <- c("mean","sd")
    desp <- 0 
  }
  
  marginal <- list(marg  = marg,
                   param = param)
  
  return(marginal)
  
  
}