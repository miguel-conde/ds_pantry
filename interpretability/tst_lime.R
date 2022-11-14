library(tidyverse)
#### LIME PACKAGE
library(lime)

data("Boston", package = "MASS")

m_lm <- caret::train(medv ~ ., Boston, method = "lm")

predict_model.lm <- function(x, new_data, type, ...) {
  # browser()
  res <- predict(x, new_data = new_data, ...)
  
  switch(
    type,
    raw = data.frame(Response = res),
    prob = data.frame()
  )
}

model_type.lm <- function(x, ...) "regression"

explainer <- lime(Boston, m_lm)
explanation <- explain(Boston %>% select(-medv), explainer,
                       n_labels = 1, n_features = ncol(Boston))
explanation

res_ex <- explanation %>% select(case, feature, feature_weight) %>% 
  spread(feature, feature_weight) %>% 
  inner_join(explanation %>% select(case, model_intercept) %>% 
               distinct(), by = "case") %>% 
  inner_join(explanation %>% select(case, model_prediction) %>% 
               distinct(), by = "case") %>% 
  inner_join(explanation %>% select(case, prediction) %>% 
               distinct(), by = "case") %>% 
  mutate(case = as.numeric(case)) %>% 
  arrange(case)

avg_contribs <- res_ex %>% summarise_at(vars(-case), ~mean(., na.rm = TRUE)) %>% 
  rename(`(Intercept)` = model_intercept) %>% 
  bind_rows(model.matrix(medv ~ ., Boston) %>% sweep(2, coef(m_lm$finalModel), "*") %>% 
              apply(2, mean)) %>% 
  mutate(model = c("lime", "lm"), .before = 1) %>% 
  gather(key, value, -model) %>% 
  spread(model, value) %>% 
  drop_na()

avg_contribs


contribs_explanation <- res_ex %>% 
  mutate(chas = ifelse(is.na(chas), 0, chas)) %>% 
  select(-case) %>% 
  mutate_at(vars(-contains("prediction")), ~ . * prediction / model_prediction) %>% 
  select(-contains("prediction")) %>% 
  relocate(model_intercept, .before = 1) %>% 
  mutate(baseline = predict(m_lm, Boston %>% mutate_all(~ 0)),
         .before = 1) %>% 
  mutate(a_repartir = model_intercept - baseline, .after = baseline) %>%
  rowwise() %>%
  mutate(s = sum(c_across(4:ncol(.)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(4:ncol(.)), ~ . + . * a_repartir / s) %>%
  select(-a_repartir, -model_intercept, -s) %>% 
  mutate(yhat = rowSums(., na.rm = TRUE))

contribs_explanation

contribs_model <- model.matrix(medv ~ ., Boston) %>% sweep(2, coef(m_lm$finalModel), "*") %>% 
  as_tibble() %>% mutate(yhat = rowSums(.))
contribs_model[, c("(Intercept)", setdiff(names(contribs_explanation), "baseline"))]
