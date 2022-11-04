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
               distinct(), by = "case")

avg_contribs <- res_ex %>% summarise_at(vars(-case), ~mean(., na.rm = TRUE)) %>% 
  rename(`(Intercept)` = model_intercept) %>% 
  bind_rows(model.matrix(medv ~ ., Boston) %>% sweep(2, coef(m_lm$finalModel), "*") %>% 
              apply(2, mean)) %>% 
  mutate(model = c("lime", "lm"), .before = 1) %>% 
  gather(key, value, -model) %>% 
  spread(model, value) %>% 
  drop_na()

avg_contribs
