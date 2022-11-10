## LM
res_lm <- pdp_1_contrib(m_lm, Boston, "lstat", pdp_pred_lm, grid_resolution = 20)
head(res_lm)
sum(res_lm$yhat)

res_lm_ice <- partial(m_lm, pred.var = "lstat", train = Boston, grid.resolution = 20,
                      ice = TRUE, center = TRUE) %>% 
  as_tibble() %>% 
  group_by(lstat) %>% 
  summarise(yhat = mean(yhat))
head(res_lm_ice)
sum(res_lm_ice$yhat)

kk_lm <- res_lm %>% as_tibble() %>% mutate(yhat_ice = res_lm_ice$yhat)

## RF
res_rf <- pdp_1_contrib(m_rf, Boston, "lstat", pdp_pred_rf, grid_resolution = 20)
head(res_rf)
sum(res_rf$yhat)

res_rf_ice <- partial(m_rf, pred.var = "lstat", train = Boston, grid.resolution = 20,
                      ice = TRUE, center = TRUE) %>% 
  as_tibble() %>% 
  group_by(lstat) %>% 
  summarise(yhat = mean(yhat))
head(res_rf_ice)
sum(res_rf_ice$yhat)

kk_rf <- res_rf %>% as_tibble() %>% mutate(yhat_ice = res_rf_ice$yhat)

##
pdp_lm <- partial(object = m_lm, pred.var = "lstat", grid.resolution = 20)
head(pdp_lm)
sum(pdp_lm$yhat)

ice_lm <- partial(object = m_lm, pred.var = "lstat", grid.resolution = 20,
                  ice = TRUE, center = FALSE) %>% 
  as_tibble() %>% 
  group_by(lstat) %>% 
  summarise(yhat = mean(yhat))
head(ice_lm)
sum(ice_lm$yhat)

##
res <- pdp_1_contrib(m_lm, Boston, "lstat", pdp_pred_lm)

res_old <- pdp_1_contrib_old(m_lm, Boston, "lstat", pdp_pred_lm_old)

##
res <- pdp_1_contrib(m_rf, Boston, "lstat", pdp_pred_rf)

res_old <- pdp_1_contrib_old(m_rf, Boston, "lstat", pdp_pred_rf_old)

###

x_aux <- Boston %>% mutate_all(~ 0)

contribs_lm$contribs %>% 
  rename(y_avg = baseline) %>% 
  mutate(baseline = pdp_pred_lm(m_lm, x_aux), .before = 1) %>% 
  mutate(a_repartir = y_avg - baseline, .after = 1) %>% 
  rowwise() %>% 
  mutate(s = sum(c_across(4:ncol(.)))) %>% 
  ungroup() %>% 
  mutate_at(vars(4:ncol(.)), ~ . * a_repartir / s) %>% 
  select(-a_repartir, -y_avg, -s)

contribs_rf$contribs %>% 
  rename(y_avg = baseline) %>% 
  mutate(baseline = pdp_pred_rf(m_rf, x_aux), .before = 1) %>% 
  mutate(a_repartir = y_avg - baseline, .after = 1) %>% 
  rowwise() %>% 
  mutate(s = sum(c_across(4:ncol(.)))) %>% 
  ungroup() %>% 
  mutate_at(vars(4:ncol(.)), ~ . * a_repartir / s) %>% 
  select(-a_repartir, -y_avg, -s)

###
contribs_xgboost$contrib_grid$lstat


hill_fun <- function(x, K, S) {
  
  1 / (1 + (x / K)^-S)
}

sigmoid_fun <- function(x, A, B, C, D) {
  D + A / (1 + exp(-B*(x-C)))
}

gr_sigmoid_fun <- function(x, A, B, C, D) {
  A*B*exp(-B*(x-C)) / (1 + exp(-B*(x-C)))^2
}

gr_sigmoid_fn <- function(A_B_C_D, contribs) {
  gr_sigmoid_fun(contribs[[1]], A_B_C_D[1], A_B_C_D[2], A_B_C_D[3], A_B_C_D[4])
}

loss_fun_hill <- function(K_S, contribs) {
  out <- contribs %>% 
    as_tibble() %>% 
    mutate(hill = hill_fun(contribs[[1]], K_S[1], K_S[2])) %>% 
    mutate(d = .[[2]] - hill) %>% 
    summarise(sse = sum(d^2)) %>% 
    as.numeric()
  
  return(out)
}

loss_fun_sigmoid <- function(A_B_C_D, contribs) {
  out <- contribs %>% 
    as_tibble() %>% 
    mutate(sigmoid = sigmoid_fun(contribs[[1]], A_B_C_D[1], A_B_C_D[2], A_B_C_D[3], A_B_C_D[4])) %>% 
    mutate(d = .[[2]] - sigmoid) %>% 
    summarise(sse = sum(d^2)) %>% 
    as.numeric()
  
  return(out)
}


loss_fun_hill(c(1,2), contribs_xgboost$contrib_grid$lstat)
loss_fun_sigmoid(c(1,2, 100, 5), contribs_xgboost$contrib_grid$lstat)
gr_sigmoid_fn(c(1,2, 100, 5), contribs_xgboost$contrib_grid$lstat)

estimate_0_sigmoid <- function(contribs, tgt_var) {
  y_inf <- max(contribs$contrib_grid[[tgt_var]]$yhat)
  y_C <- (max(contribs$contrib_grid[[tgt_var]]$yhat) - 
            min(contribs$contrib_grid[[tgt_var]]$yhat)) / 2
  C_0 <- (max(contribs$contrib_grid[[tgt_var]][[1]]) - 
            min(contribs$contrib_grid[[tgt_var]][[1]])) / 2
  A_0 <- 2*(y_inf - y_C)
  D_0 <- 2*y_C - y_inf
  
  out <- c(A_0, 0, C_0, D_0)
  
  return(out)
}

y_inf <- max(contribs_xgboost$contrib_grid$zn$yhat)
y_C <- (max(contribs_xgboost$contrib_grid$zn$yhat) - 
          min(contribs_xgboost$contrib_grid$zn$yhat)) / 2
C_0 <- (max(contribs_xgboost$contrib_grid$zn[[1]]) - 
          min(contribs_xgboost$contrib_grid$zn[[1]])) / 2
A_0 <- 2*(y_inf - y_C)
D_0 <- 2*y_C - y_inf

estimate_0_sigmoid(contribs_xgboost, "zn")
res_optim <- optim(par = estimate_0_sigmoid(contribs_xgboost, "zn"), 
                   fn = loss_fun_sigmoid, 
                   # gr = gr_sigmoid_fn,
                   contribs = contribs_xgboost$contrib_grid$zn,
                   method = "L-BFGS-B")

contribs_xgboost$contrib_grid$zn %>% plot(type = "l")
curve(sigmoid_fun(x, res_optim$par[1], res_optim$par[2], res_optim$par[3], res_optim$par[4]), 0, 100, col = "blue", add = TRUE)

res_optim <- optim(par = estimate_0_sigmoid(contribs_xgboost, "lstat"), 
                   fn = loss_fun_sigmoid, 
                   # gr = gr_sigmoid_fn,
                   contribs = contribs_xgboost$contrib_grid$lstat,
                   method = "L-BFGS-B")

contribs_xgboost$contrib_grid$lstat %>% plot(type = "l")
curve(sigmoid_fun(x, res_optim$par[1], res_optim$par[2], res_optim$par[3], res_optim$par[4]), 0, 100, col = "blue", add = TRUE)

res_optim <- optim(par = estimate_0_sigmoid(contribs_xgboost, "rm"), 
                   fn = loss_fun_sigmoid, 
                   # gr = gr_sigmoid_fn,
                   contribs = contribs_xgboost$contrib_grid$rm,
                   method = "L-BFGS-B")

contribs_xgboost$contrib_grid$rm %>% plot(type = "l")
curve(sigmoid_fun(x, res_optim$par[1], res_optim$par[2], res_optim$par[3], res_optim$par[4]), 0, 100, col = "blue", add = TRUE)
