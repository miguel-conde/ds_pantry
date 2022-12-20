library(tidyverse)
library(archivist)


titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_rf      <- archivist::aread("pbiecek/models/4e0fc")
(henry          <- archivist::aread("pbiecek/models/a6538"))

# 1 - Ordenamos las variables

pred_vars <- setdiff(names(attributes(titanic_rf$terms)$dataClasses), "survived")

var_order <- vector(mode = "numeric", length = length(pred_vars))
names(var_order) <- pred_vars
for (v in pred_vars) {
  # Fijo el valor de la variable
  probe <- titanic_imputed %>% mutate_at(all_of(v), ~ henry[[v]])
  
  # 
  preds_with_fixed_var <- predict(titanic_rf,  probe, type = "prob")[, "yes"] 
  preds_no_fixed_var   <- predict(titanic_rf,  titanic_imputed, type = "prob")[, "yes"] 
  delta_var <- abs(mean(preds_with_fixed_var) - mean(preds_no_fixed_var))
  
  var_order[v] <- delta_var
}

var_order <- names(var_order[order(var_order, decreasing = TRUE)])

# 2 - Calculamos BD

var_contrib <- vector(mode = "numeric", length = length(var_order))
names(var_contrib) <- var_order

probe_i_1 <- titanic_imputed
preds_probe_i_1 <- predict(titanic_rf,  probe_i_1, type = "prob")[, "yes"]
avg_preds_probe_i_1 <- mean(preds_probe_i_1)

for (idx_v in seq_along(var_order)) {
  probe_i <- probe_i_1 %>% mutate_at(all_of(var_order[idx_v]), ~ henry[[var_order[idx_v]]])
  preds_probe_i   <- predict(titanic_rf,  probe_i, type = "prob")[, "yes"]
  avg_preds_probe_i <- mean(preds_probe_i)
  
  delta_i <- avg_preds_probe_i - avg_preds_probe_i_1
  
  avg_preds_probe_i_1 <- avg_preds_probe_i
  probe_i_1 <- probe_i
  
  var_contrib[idx_v] <- delta_i
}

var_contrib

# Check
predict(titanic_rf, henry, type = "prob")[, "yes"] - sum(var_contrib)

predict(titanic_rf, type = "prob")[, "yes"] %>% mean()

predict_parts(explainer = explain_rf,
              new_observation = henry,
              type = "break_down")
