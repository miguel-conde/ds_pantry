library(tidyverse)

library(iml)
library(randomForest)

data("Boston", package = "MASS")
head(Boston)

set.seed(42)

data("Boston", package = "MASS")

rf <- randomForest(medv ~ ., data = Boston, ntree = 50)
m_lm <- lm(medv ~ ., data = Boston)

X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)
predictor_lm <- Predictor$new(m_lm, data = X, y = Boston$medv)

shapley <- Shapley$new(predictor, x.interest = X[1, ])
shapley$plot()

shapley$explain(x.interest = X[2, ])
shapley$plot()

results <- shapley$results
head(results)

shapley$y.hat.average
shapley$y.hat.interest
results %>% arrange(desc(phi))
results %>% arrange(desc(phi.var))

###
shapley_lm <- Shapley$new(predictor_lm, x.interest = X[1, ])
shapley_lm$plot()

shapley_lm$explain(x.interest = X[2, ])
shapley_lm$plot()

results <- shapley_lm$results
head(results)

fitted(m_lm)[2]
shapley_lm$y.hat.interest
shapley_lm$y.hat.average
sum(results %>% 
      select(feature, phi) %>% 
      spread(feature, phi)) + shapley$y.hat.average

results %>% arrange(desc(phi))
results %>% arrange(desc(phi.var))


xxx <- function(in_model, in_data, in_y) {
  
  predictor <- Predictor$new(in_model, data = in_data, y = in_y)
  
  shapley <- Shapley$new(predictor)
  
  tbl_contributions <- tibble()
  for (i in 1:nrow(in_data)) {
    
    if (i%%10 == 0) cat(sprintf("Observation %d\n", i))
    
    shapley$explain(x.interest = X[i, ])
    
    results <- shapley$results %>% 
      mutate(perc_phi = phi / sum(.$phi)) %>% 
      ## Opcion 1 - 2 lineas
      # mutate(new_phi = (shapley$y.hat.interest - shapley$y.hat.average) *
      #          perc_phi) %>%
      # mutate(new_phi = new_phi * (shapley$y.hat.interest - coef(in_model)["(Intercept)"]) / sum(new_phi)) %>%
      ## Opcion 2 - 2 linea
      mutate(new_phi = perc_phi * (sum(.$phi) + (shapley$y.hat.average - coef(in_model)["(Intercept)"]))) %>%
      select(feature, new_phi) %>% 
      spread(feature, new_phi ) %>% 
      mutate(intercept = coef(m_lm)["(Intercept)"],
             y_hat_average = shapley_lm$y.hat.average,
             y_hat = shapley_lm$y.hat.interest)
    
    tbl_contributions <- tbl_contributions %>% 
      bind_rows(results)
  }
  
  return(tbl_contributions)
}


kk <- xxx(in_model = m_lm, in_data = X[1:10,], in_y = Boston$medv[1:10])
kk %>% summarise_all(~ sum(.))

contrib_m_lm <- model.matrix(medv ~ ., data = Boston) %>% 
  apply(1, function(x) x* coef(m_lm)) %>% 
  t()


yyy <- function(in_model, in_data, in_y) {
  
  intcpt <- as.numeric(coef(in_model)["(Intercept)"])
  
  predictor <- Predictor$new(in_model, data = in_data, y = in_y)
  
  shapley <- Shapley$new(predictor)
  
  tbl_contributions <- tibble()
  for (i in 1:nrow(in_data)) {
    
    if (i%%10 == 0) cat(sprintf("Observation %d\n", i))
    
    shapley$explain(x.interest = X[i, ])
    
    results <- shapley$results
    sum_shap <- sum(results$phi)
    prediccion_shap <- shapley$y.hat.average + sum_shap
    contrib_shap <- prediccion_shap - intcpt
    contrib_model <- shapley$y.hat.interest - intcpt
    # Para que contrib_shap = contrib_model
    ratio_contribs <- contrib_shap / sum_shap * contrib_model / contrib_shap
    
    results <- results %>% 
      mutate(phi = phi * ratio_contribs) %>%
      select(feature, phi) %>% 
      spread(feature, phi ) %>% 
      mutate(intercept     = intcpt,
             y_hat_average = shapley$y.hat.average,
             y_hat         = shapley$y.hat.interest)
    
    tbl_contributions <- tbl_contributions %>% 
      bind_rows(results)
  }
  
  return(tbl_contributions)
}

kk <- yyy(in_model = m_lm, in_data = X, in_y = Boston$medv)
kk %>% rowwise() %>% mutate(y_hat_shap = sum(c_across(age:intercept)))

m_lm_contrib <- model.matrix(medv ~ ., data = Boston) %>% 
  apply(1, function(x) x* coef(m_lm)) %>% 
  t()

kk %>% select(age:intercept) %>% apply(2, sum)
m_lm_contrib  %>% apply(2, sum)

kk %>% select(age:intercept) %>% apply(2, sum) %>% bind_rows(as_tibble(m_lm_contrib) %>% apply(2, sum))

### LIME
set.seed(42)

data("Boston", package = "MASS")
m_lm <- lm(medv ~ ., data = Boston)

predictor_lm <- Predictor$new(m_lm, data = Boston, y = Boston$medv)


lime.explain <- LocalModel$new(predictor_lm, x.interest = Boston[1, ], k = ncol(Boston))
lime.explain$results
lime.explain$explain(x.interest = Boston[2,])
lime.explain$results

plot(lime.explain)

