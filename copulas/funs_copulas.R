library(copula)


best_pdf_orig <- function(annual_series) {
  
  require(fitdistrplus)
  
  annual_series <- as.numeric(annual_series)
  
  if(sd(annual_series) != 0){ # Para el caso de valores iguales en toda la serie
    
    # Parámetros iniciales para gamma y gev
    med.v <- mean(annual_series)
    var.v <- var(annual_series)
    
    alpha.g <- med.v^2 / var.v
    beta.g <- var.v / med.v
    
    xi.ge <- .2
    sigma.ge <- sqrt(var.v * xi.ge^2 / (gamma(1 -2 * xi.ge) - gamma(1 - xi.ge)^2))
    mu.ge    <- med.v - sigma.ge * (gamma(1 - xi.ge) - 1) / xi.ge
    
    options(warn = -1)
    
    bic.vari <- c(tryCatch(gofstat(fitdist(annual_series, "norm"))$bic,
                           error = function(e) { return(Inf) }),
                  tryCatch(gofstat(fitdist(annual_series, "lnorm"))$bic,
                           error = function(e) { return(Inf) }),
                  tryCatch(gofstat(fitdist(annual_series, "weibull"))$bic,
                            error = function(e) { return(Inf) }),
                  tryCatch(gofstat(fitdist(annual_series, "gev",
                                           start = list(loc = mu.ge,
                                                        scale = sigma.ge,
                                                        shape = xi.ge)))$bic,
                           error = function(e) { return(Inf) }),
                  tryCatch(gofstat(fitdist(annual_series, "gamma",
                                           start = list(shape = alpha.g,
                                                        rate = beta.g)))$bic,
                           error = function(e) { return(Inf) }))
    
    wm <- which.min(bic.vari)
    
    if (wm == 1) {
      marg  <- "norm"
      param <- fitdist(annual_series, marg)$estimate
    }
    if (wm == 2) {
      marg <- "lnorm"
      param <- fitdist(annual_series, marg)$estimate
    }
    if (wm == 3) {
      marg <- "weibull"
      param <- fitdist(annual_series, marg)$estimate
    }
    if (wm == 4) {
      marg = "gev"
      param <- fitdist(annual_series, marg, 
                       start = list(loc = mu.ge, scale = sigma.ge,
                                    shape = xi.ge))$estimate
    }
    if (wm == 5) {
      marg = "gamma"
      param <- fitdist(annual_series, marg, 
                       start = list(shape = alpha.g, rate = beta.g))$estimate
    }
  } else {
    marg = "norm"
    param <- c(0,1)
    names(param) <- c("mean","sd")
    desp <- 0 
  }
  
  marginal <- list(marg  = marg,
                   param = param)
  
  options(warn = 0)
  
  return(marginal)
  
  
}

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

calc_stats <- function(in_data, consum_stats = NULL) {
  
  # in_data, <tibble>: cada columna es una serie, cada fila 1 observación
  
  # 1. Estadísticos
  
  stats_in_data <- in_data %>% 
    summarise_all(list(avg = ~ mean(.), sd = ~ sd(.))) %>% 
    gather(stat, value) %>% 
    mutate(stat = str_replace(stat, "_avg", "-avg")) %>% 
    mutate(stat = str_replace(stat, "_sd", "-sd")) %>% 
    separate(stat, c("instal", "stat"), "-") %>% 
    spread(stat, value)
  
  # 2. Matriz de correlaciones
  
  corr_matrix <- in_data %>% 
    cor()
  
  # 3. Añadir estadísticos de consumo
  if (! is.null(consum_stats)) {
    stats_in_data <- stats_in_data %>% 
      bind_rows(consum_stats$avg_sd)
    
    aux <- consum_stats$correlations %>% select(-correlations) %>% as.matrix()
    rownames(aux) <- consum_stats$correlations$correlations
    c_names <- c(colnames(aux)[colnames(aux) %in% 
                                 (consum_stats$avg_sd$instal %>% 
                                    janitor::make_clean_names())],
                 colnames(corr_matrix))
    r_names <- rownames(aux)[(rownames(aux) %>% 
                                janitor::make_clean_names()) %in% c_names]
    aux <- aux[r_names, c_names, drop = FALSE] 
    
    n_cons <- ncol(aux) - ncol(corr_matrix)
    new_corr_matrix <- matrix(NA, nrow = n_cons + nrow(corr_matrix), ncol = n_cons + ncol(corr_matrix))
    
    new_corr_matrix[1:nrow(aux), 1:ncol(aux)] <- aux
    colnames(new_corr_matrix) <- rownames(new_corr_matrix) <- colnames(aux)
    new_corr_matrix[(n_cons+1):nrow(new_corr_matrix), 
                    (n_cons+1):ncol(new_corr_matrix)] <- corr_matrix
    new_corr_matrix[(n_cons+1):nrow(new_corr_matrix), 
                    1:n_cons] <- new_corr_matrix[1, -n_cons]
    
    corr_matrix <- new_corr_matrix
  }
  
  
  out <- list(stats = stats_in_data,
              corr_matrix = corr_matrix)
  
  return(out)
}

wide_stats <- function(stats) {
  stats %>% gather(stat, value, -instal) %>% spread(instal, value)
}

get_copula_sim <- function(in_data, n_sim = 1000, seed = NULL) {
  #
  #
  # Ejemplo: 
  #           probe <- readRDS("data/xa_copulas.Rds")
  #             res <- get_copula_sim(probe, n_sim = 1000)
  #
  
  if (!is.null(seed)) set.seed(seed)
  
  best_pdfs <- in_data %>% lapply(best_pdf)
  
  stats <- calc_stats(in_data, consum_stats = NULL) 
  
  ## Cópulas
  
  # Definición
  def_copula <- normalCopula(param   = P2p(stats$corr_matrix), 
                             dim     = nrow(stats$corr_matrix), 
                             dispstr = "un")
  # Check: getSigma(def_copula)
  
  # Multivariate Distribution Constructed from Copula
  copula_sim <- mvdc(copula = def_copula, 
                     margins = best_pdfs %>% lapply(function(x) x$marg) %>% unlist(),
                     paramMargins = best_pdfs %>% lapply(function(x) as.list(x$param)))
  
  # Simulación -  random generator for the multivariate distribution via copula 
  #               and parametric margins
  ful_simul <- rMvdc(2000, copula_sim)
  
  dimnames(ful_simul) <- list(n_sim = NULL, instal = colnames(stats$corr_matrix))
  
  stats_orig <- stats
  stats_orig$stats <- stats_orig$stats %>% wide_stats()
  stats_sim <- ful_simul %>% as.data.frame() %>% calc_stats()
  stats_sim$stats <- stats_sim$stats %>% wide_stats()
  
  out <- list(sim = ful_simul,
              in_data = in_data,
              stats_orig = stats_orig,
              best_pdfs = best_pdfs,
              def_copula = def_copula,
              unif_sim = copula_sim,
              stats_sim = stats_sim)
  
  return(out)
}

