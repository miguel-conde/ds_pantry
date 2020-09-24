

dbind <- function(B1, B2) {
  #
  # Make B matrix from B1 (e.g., LLT + Season) and B2 (e.g., time variant 
  # covariates) matrices.
  #
  out1 <- cbind(B1, matrix(0, nrow = nrow(B1), ncol = ncol(B2)))
  out2 <- cbind(matrix(0, nrow = nrow(B2), ncol = ncol(B1)), B2)
  
  rbind(out1, out2)
}

make_LLT_B <- function() {
  #
  # Make B matrix for Local Linear Trend (LLT)
  # 
  
  matrix(c(1, 0, 1, 1), nrow = 2, ncol = 2)
}

make_season_B <- function(nf) {
  #
  # Make B matrix for a seasonal component
  # nf = Seasonal frequency
  # 
  
  B <- matrix(0, nf - 1L, nf -1L)
  
  B[1L, ] <- rep(-1, nf - 1L)
  
  if (nf >= 3L) {
    ind <- (3:nf) - 2L
    B[cbind(ind + 1L, ind)] <- 1
  }
  return(B)
}

make_LLT_season_B <- function(nf) {
  #
  # Make B matrix  to model Local Level Trend (LLT) + seasonal components 
  # nf = Seasonal frequency
  # 
  # Ch 19 Structural Time Series Models, p. 264 - MARSS User's Guide
  # 
  B <- matrix(0, nf + 1L, nf + 1L)
  B[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
  B[3L, ] <- c(0, 0, rep(-1, nf - 1L))
  if (nf >= 3L) {
    ind <- 3:nf
    B[cbind(ind + 1L, ind)] <- 1
  }
  return(B)
}

make_dynamic_covariates_B <- function(n_covariates) {
  diag(1, n_covariates)
}

make_LLT_Q <- function() {
  ldiag(c("s_mu", "s_beta"))
}

make_season_Q <- function(nf, suffix = "") {
  
  ldiag(c(list(paste0("s_w", suffix)), 
          as.list(rep(0, nf - 2))))
}

make_covariates_Q <- function(n_covariates) {
  
  ldiag(paste0("q_", 1:n_covariates))
}

make_LLT_Z <- function() {
  matrix(c(1,0), nrow = 1, ncol = 2)
}

make_season_Z <- function(nf) {
  matrix(c(1,rep(0, nf-2)), nrow = 1, ncol = nf-1)
}

make_covariates_Z <- function(n_covariates) {
  
}

make_R <- function(n_targets) {
  R <- matrix(list(0), N_TARGETS, N_TARGETS)
  diag(R) <-  "r"
  
  R
}

make_LLT_x0 <- function(y) {
  matrix(c(y[1], 0), nrow = 2, ncol = 1)
}

make_LLT_V0 <- function(y) {
  diag(1e+06*var(y)/100 + 1e-10, 2)
}

make_season_x0 <- function(nf) {
  matrix(0, nrow = nf-1, ncol = 1)
}

make_season_V0 <- function(y, nf) {
  diag(1e+06*var(y)/100 + 1e-10,  nf-1)
}

make_covariates_x0 <- function(covariates) {
  matrix(0, ncol = 1, nrow = nrow(covariates))
}

make_covariates_V0 <- function(covariates) {
  # diag(apply(covariates, 2, var, na.rm = TRUE) * 1e+06 / 100, nrow(covariates))
  diag(0, nrow(covariates))
}
