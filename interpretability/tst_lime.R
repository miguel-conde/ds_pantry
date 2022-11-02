library(tidyverse)
#### LIME PACKAGE
library(lime)

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

res_ex %>% summarise_at(vars(-case), ~mean(., na.rm = TRUE)) %>% 
  gather(key, value)
m_lm$finalModel %>% summary()

