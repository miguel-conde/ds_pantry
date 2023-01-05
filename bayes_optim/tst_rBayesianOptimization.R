library(tidyverse)
library(rBayesianOptimization)





# Example 1: Optimization -------------------------------------------------


# Example 1: Optimization
## Set Pred = 0, as placeholder
Test_Fun <- function(x) {
  list(Score = exp(-(x - 2)^2) + exp(-(x - 6)^2/10) + 1/ (x^2 + 1),
       Pred = 0)
}
## Set larger init_points and n_iter for better optimization result
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x = c(1, 3)),
                                # init_points = 2, n_iter = 1,
                                init_points = 5, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)


# Example 2: Parameter Tuning ---------------------------------------------

# Example 2: Parameter Tuning
library(xgboost)

data(agaricus.train, package = "xgboost")

dtrain <- xgb.DMatrix(agaricus.train$data,
                      label = agaricus.train$label)
cv_folds <- KFold(agaricus.train$label, nfolds = 5,
                  stratified = TRUE, seed = 0)

xgb_cv_bayes <- function(max_depth, min_child_weight, subsample) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = 0.3,
                             lambda = 1, alpha = 0,
                             objective = "binary:logistic",
                             eval_metric = "auc"),
               data = dtrain, nround = 100,
               folds = cv_folds, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 5, maximize = TRUE, verbose = 0)
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred = cv$pred)
}

OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max_depth = c(2L, 6L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0.5, 0.8)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

OPT_Res$Best_Par
OPT_Res$Best_Value
OPT_Res$History
OPT_Res$Pred


# OPTIM -------------------------------------------------------------------

fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

grr <- function(x) { ## Gradient of 'fr'
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}

# Minimize
optim(c(-1.2,1), fr)
(res <- optim(c(-1.2,1), fr, grr, method = "BFGS"))

###

Test_Fun <- function(x1, x2) {
  list(Score = -(100 * (x2 - x1 * x1)^2 + (1 - x1)^2),
       Pred = 0)
}
## Set larger init_points and n_iter for better optimization result
OPT_Res <- BayesianOptimization(Test_Fun,
                                bounds = list(x1 = c(-1.2, 1.2), x2 = c(-1.2, 1.2)),
                                # init_points = 2, n_iter = 1,
                                init_points = 2, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)


# CARET -------------------------------------------------------------------

data("Boston", package = "MASS")

X <- Boston %>% select(-medv)
y <- Boston %>% pull(medv)

library(caret)

getModelInfo("ranger")$ranger$grid(X, y, len = 3)

len <- 3
srule <- if (is.factor(y)) "gini" else "variance"
out <- expand.grid(mtry = caret::var_seq(p = ncol(X), 
                                         classification = is.factor(y), 
                                         len = len), 
                   min.node.size = ifelse(is.factor(y),  1, 5), 
                   splitrule = c(srule, "extratrees"))
out 

tune_grid <- expand.grid(mtry = c(2L,7L,13L),
                         min.node.size = c(4L, 5L),
                         splitrule = c("variance"))

library(tictoc)

tic()
rf_boston <- train(x = X, y = y,
                   method = "ranger", 
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            p = 0.75, savePredictions = TRUE),
                   tuneGrid = tune_grid)
toc()

                                                                                                         
###

ranger_cv_bayes <- function(mtry, min.node.size) {
  # browser()
  cv <- train(x = X, y = y,
              method = "ranger", 
              trControl = trainControl(method = "cv",
                                       number = 10,
                                       p = 0.75),
              tuneGrid = data.frame(mtry = mtry, 
                                    min.node.size = min.node.size,
                                    splitrule = "variance"))
  
  list(Score = cv$results %>% 
         inner_join(cv$bestTune, 
                    by = c("mtry", "min.node.size", "splitrule")) %>% 
         pull(RMSE),
       Pred = 0)
}

tic()
OPT_Res <- BayesianOptimization(ranger_cv_bayes,
                                bounds = list(mtry = c(2L, 7L, 13L),
                                              min.node.size = c(4L, 5L)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)
toc()
