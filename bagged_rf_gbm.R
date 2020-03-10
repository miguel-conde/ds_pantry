library(tidyverse)
library(caret)


# LM BENCHMARK ------------------------------------------------------------

lm_fit <- lm(mpg ~ ., mtcars)
summary(lm_fit)

lm_fit <- lm(mpg ~ cyl + wt, mtcars)
summary(lm_fit)

# Train RMSE = 2.444202
Metrics::rmse(actual = mtcars$mpg, predicted = fitted(lm_fit))

lm_fit <- lm(mpg ~ cyl + disp, mtcars)
summary(lm_fit)


# BAGGED TREES ------------------------------------------------------------
?ranger::ranger
modelLookup("ranger")
getModelInfo("ranger")$fit

hyper_param_grid <- expand.grid(mtry = 2,       # m = p means bagged trees
                                min.node.size = 1,
                                splitrule = "variance") 

tr_ctrl <- trainControl(method = "cv",
                        number = 10,
                        verboseIter = TRUE,
                        returnResamp = "final")

# En general, se observa una disminución exponencial de las mejoras en la 
# predicción al aumentar B: la mayor parte de la mejora se logra con unos pocos 
# árboles (B < 10). Si el rendimiento no mejora con 50 árboles, mejor usar otros 
# métodos como random forest o boosting.

# seq_length_trees <- seq(1, 150, length.out = 20) %>% as.integer
seq_length_trees <- seq(1, 150, length.out = 150) %>% as.integer
lst_bagged_trees_fit <- vector(mode = "list", length = length(seq_length_trees))

set.seed(123)
for(i in seq_along(seq_length_trees)) {
  lst_bagged_trees_fit[[i]] <- train(form = mpg ~ .,
                                     data =  mtcars, 
                                     method = "ranger",
                                     preProcess = c("center", "scale"),
                                     ##
                                     num.trees = seq_length_trees[i],
                                     importance = "impurity",
                                     ##
                                     trControl = tr_ctrl, 
                                     tuneGrid = hyper_param_grid)
}

res <- lst_bagged_trees_fit %>% lapply(function(x) {
  x$results %>% inner_join(x$bestTune)
}) %>% bind_rows() %>%
  mutate(n_trees = seq_length_trees)

res

plot(res %>% dplyr::select(n_trees, RMSE), type = "o")

set.seed(123)
bagged_caret_fit <- train(form = mpg ~ cyl + wt,
                          data =  mtcars, 
                          method = "ranger",
                          preProcess = c("center", "scale"),
                          ##
                          num.trees = 11,
                          importance = "impurity",
                          ##
                          trControl = tr_ctrl, 
                          tuneGrid = hyper_param_grid)

bagged_caret_fit

# CV Error - 2.769561
# 
#    - A bit greater than LM model (2.444202) => some added bias
bagged_caret_fit$results %>% inner_join(bagged_caret_fit$bestTune)
bagged_caret_fit$resample %>% summarise_if(is.numeric, mean)

# Train error - 1.467456 
# 
#    - Better than LM train error => no added BIAS
#    - Better than bagged CV error => overffitting (1.302105)
Metrics::rmse(actual = mtcars$mpg, predict(bagged_caret_fit, mtcars))

# RANDOM FOREST -----------------------------------------------------------



# GBM ---------------------------------------------------------------------

?gbm::gbm
modelLookup("gbm")
getModelInfo("gbm")$fit

# B      = n.trees = 100,
# d      = interaction.depth = 1, 
# lambda = shrinkage = 0.1,
# bag.fraction = 0.5
# n.minobsinnode = 10,

set.seed(123)
hyper_param_grid <- expand.grid(n.trees = c(100, 500, 1000),
                                interaction.depth = c(1),
                                shrinkage = exp(runif(5, log(0.001), log(0.01))),
                                n.minobsinnode = 1)

tr_ctrl <- trainControl(method = "cv",
                        number = 10,
                        verboseIter = TRUE,
                        returnResamp = "final")

gbm_caret_fit <- train(form = mpg ~ cyl + wt,
                       data =  mtcars, 
                       method = "gbm",
                       preProcess = c("center", "scale"),
                       ##
                       # Bagging fraction
                       # bag.fraction = 0.5,
                       ##
                       trControl = tr_ctrl, 
                       tuneGrid = hyper_param_grid)

gbm_caret_fit

# CV Error - 2.493957
# 
#    - Approx. equal to LM model (2.444202) => no added bias
gbm_caret_fit$results %>% inner_join(gbm_caret_fit$bestTune)
gbm_caret_fit$resample %>% summarise_if(is.numeric, mean)

# Train error - 1.90706 
# 
#    - Better than LM train error => no added BIAS
#    - Better than gbm CV error => overffitting (0.586897)
Metrics::rmse(actual = mtcars$mpg, predict(gbm_caret_fit, mtcars))

plot(gbm_caret_fit)

# The more n.trees, the better CV RMSE (until shrinkage = 0.006)
# Selecting more trees (n.trees = 1000) and slower learning rate (0.001531223) should 
# reduce variance
hyper_param_grid <- expand.grid(n.trees = c(1000),
                                interaction.depth = c(1),
                                shrinkage = c(0.00235039),
                                n.minobsinnode = 1)

set.seed(123)
new_gbm_caret_fit <- train(form = mpg ~ cyl + wt,
                           data =  mtcars, 
                           method = "gbm",
                           preProcess = c("center", "scale"),
                           ##
                           # Bagging fraction
                           # bag.fraction = 0.5,
                           ##
                           trControl = tr_ctrl, 
                           tuneGrid = hyper_param_grid)

new_gbm_caret_fit

# CV Error - 2.55897
# 
#    - Approx. equal to LM model (2.444202) => some bias added to 
#                                              previous fit (2.493957)
new_gbm_caret_fit$results %>% inner_join(new_gbm_caret_fit$bestTune)
new_gbm_caret_fit$resample %>% summarise_if(is.numeric, mean)

# Train error - 2.173939 (worst than previous 1.90706, added BIAS) 
# 
#    - Better than LM train error => no significative added BIAS
#    - Better than gbm CV error => overffitting, but less than before (0.385031,
#                                  down 34.4 %)
Metrics::rmse(actual = mtcars$mpg, predict(new_gbm_caret_fit, mtcars))


# XGBOOST -----------------------------------------------------------------


modelLookup("xgbLinear")