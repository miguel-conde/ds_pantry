library(iml)
library(tidyverse)

N <- 1000
set.seed(!23)
tb_data <- tibble(x1 = rnorm(N),
                  x2 = rnorm(N, .1)) %>% 
  mutate(y = 2*x1 + 3*x2 + rnorm(N, .01))

f <- as.formula("y ~ .")
lm_fit <- lm(f, tb_data)
summary(lm_fit)

sweep(model.matrix(f, data = tb_data), 2, coef(lm_fit), "*")

#####

mod = Predictor$new(lm_fit, data = tb_data)

effect = FeatureEffect$new(mod, "x1", method = "ale",
                           grid.size = 20, center.at = NULL)
plot(effect)
effect$results
print(effect)

tb_data$x1 %>% summary()
coef(lm_fit)["x1"] * tb_data$x1 %>% summary()
effect$results %>% summary()

effect$set.feature("x2")
plot(effect)
effect$results
print(effect)

tb_data$x2 %>% summary()
coef(lm_fit)["x2"] * tb_data$x1 %>% summary()
effect$results %>% summary()

####
library(randomForest)

rf_fit <- randomForest(f, tb_data)

####



ale_aportes_lm <- est_aportes(lm_fit, data = tb_data,
                              vars_2_est = c("x1", "x2"), 
                              method = "ale", grid.size = 20, center.at = NULL)
ale_aportes_rf <- est_aportes(rf_fit, data = tb_data,
                              vars_2_est = c("x1", "x2"), 
                              method = "ale", grid.size = 20, center.at = NULL)



x_aporte("x1", 2, ale_aportes_lm)
x_aporte("x1", tb_data$x1[391], ale_aportes_lm)

plot(sapply(tb_data$x1, function(x) {
  x_aporte("x1", x, ale_aportes_lm)
}),
sapply(tb_data$x1, function(x) {
  x_aporte("x1", x, ale_aportes_rf)
}))



plot(x_aportes(tb_data$x1, "x1", ale_aportes_lm),
     x_aportes(tb_data$x1, "x1", ale_aportes_rf))

plot(predict(rf_fit, tb_data),
     x_aportes(tb_data$x1, "x1", ale_aportes_rf) + 
       x_aportes(tb_data$x2, "x2", ale_aportes_rf))
abline(a = 0, b = 1)

### RF PCA
require(caret)

rf_pca_fit <- train(f, 
                    method = "rf",
                    preProcess = "pca",
                    data = tb_data)

ale_aportes_rf_pca <- est_aportes(rf_pca_fit, 
                                  data = tb_data,
                                  vars_2_est = c("x1", "x2"), 
                                  method = "ale", 
                                  grid.size = 20, 
                                  center.at = NULL)

plot(x_aportes(tb_data$x1, "x1", ale_aportes_lm),
     x_aportes(tb_data$x1, "x1", ale_aportes_rf_pca))

plot(predict(rf_pca_fit, tb_data),
     x_aportes(tb_data$x1, "x1", ale_aportes_rf_pca) + 
       x_aportes(tb_data$x2, "x2", ale_aportes_rf_pca))
abline(a = 0, b = 1)


####
x_aportes(tb_data$x1, "x1", ale_aportes_lm) %>% head
x_aportes(tb_data$x1, "x1", ale_aportes_rf_pca) %>% head()

Metrics::mape(tb_data$y, x_aportes(tb_data$x1, "x1", ale_aportes_rf_pca) + 
                x_aportes(tb_data$x2, "x2", ale_aportes_rf_pca))
Metrics::mape(tb_data$y, predict(rf_pca_fit, tb_data))

plot(tb_data$y, predict(rf_pca_fit, tb_data))
abline(a = 0, b = 1)

####
predict(ale_aportes_rf_pca$mod_predictor$model, tb_data) %>% head
predict(rf_pca_fit, tb_data) %>% head

ale_aportes_rf_pca$mod_predictor$predict(tb_data) %>% head
ale_aportes_rf_pca$mod_predictor$prediction.function(tb_data) %>% head

####


res <- est_model_contrib(rf_pca_fit, tb_data, c("x1", "x2"))
res %>% head
res %>% rowSums() %>% head
predict(rf_pca_fit, tb_data) %>% head
