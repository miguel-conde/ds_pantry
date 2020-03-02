## http://uc-r.github.io/iml-pkg

## FOR THE PACKAGE

library(iml)
library(tidyverse)

est_aportes <- function(mod_predictor, ...) {
  
  vars_2_est <- mod_predictor$data$feature.names
  
  effect = FeatureEffect$new(mod_predictor, vars_2_est[1], ...)
  
  out <- lapply(vars_2_est, function(x) {
    effect$set.feature(x)
    effect$results
  }) 
  
  names(out) <- vars_2_est
  
  out
}

### 

x_aporte <- function(x_var, x_value, ale_aportes) {
  
  aux <- ale_aportes[[x_var]]
  
  idx <- which(aux[[x_var]] == x_value)
  
  if (length(idx == 1)) return(aux[[1]][idx])
  
  if(x_value > max(aux[[x_var]]) | x_value < min(aux[[x_var]])) {
    stop("Out of range")
  }
  
  idx_slot <- c(max(which(aux[[x_var]] <= x_value)), 
                min(which(aux[[x_var]] > x_value)))
  x_range <- aux[[x_var]][idx_slot]
  aporte_range <- aux[[1]][idx_slot]
  
  prop <- (x_value - x_range[1]) / (x_range[2] - x_range[1])
  
  out <- prop * (aporte_range[2] - aporte_range[1]) + aporte_range[1]
  
  out
}

x_aportes <- function(xs, x_var, ale_aportes) {
  sapply(xs, function(x) {
    x_aporte(x_var, x, ale_aportes)
  })
}

est_model_contrib <- function(mod_predictor, ...) {
  
  ale_aportes <- est_aportes(mod_predictor, ...)
  
  out <- sapply(names(ale_aportes), function(x_var) {
    x_aportes(mod_predictor$data$get.x() %>% pull(x_var), x_var, ale_aportes)
  })
  
  aux_sum_contrib <- rowSums(out)
  aux_mod_fit_pred <- mod_predictor$predict(mod_predictor$data$get.x()) %>% 
    pull(1)
  aux <- aux_mod_fit_pred / aux_sum_contrib
  
  out <- out %>% apply(2, function(x) {
    x * aux
  })
  
  out
}


# TEST --------------------------------------------------------------------

## DATA

N <- 1000
set.seed(!23)
# tb_data <- tibble(x1 = rnorm(N),
#                   x2 = rnorm(N, .1)) %>% 
#   mutate(y = 2*x1 + 3*x2 + rnorm(N, .01))
tb_data <- tibble(x1 = abs(rnorm(N)),
                  x2 = x1 + abs(rnorm(N, .01)),
                  x3 = rnorm(N)) %>% 
  mutate(y = 2*x1 + 3*x2 -5*x3 + rnorm(N, .01))

cor(tb_data)

## MODELS

# LM
f <- as.formula("y ~ .")
lm_fit <- lm(f, tb_data)
summary(lm_fit)

# RF
library(randomForest)

rf_fit <- randomForest(f, tb_data)

# RF PCA
require(caret)

rf_pca_fit <- train(f, 
                    method = "rf",
                    preProcess = "pca",
                    data = tb_data)

## 
mod_predictor_lm <-  Predictor$new(model = lm_fit, 
                                   data = tb_data # , 
                                   # y = y,
                                   # predict.fun = predict.fun, 
                                   # type = type)
)
est_model_contrib(mod_predictor_lm, method = "ale", grid.size = 100) %>% head
sweep(model.matrix(f, data = tb_data), 2, coef(lm_fit), "*") %>% head

FeatureEffects$new(mod_predictor_lm, method = "ale") %>% plot()

mod_predictor_rf <-  Predictor$new(model = rf_fit, 
                                   data = tb_data # , 
                                   # y = y,
                                   # predict.fun = predict.fun, 
                                   # type = type)
)
est_model_contrib(mod_predictor_rf, method = "ale", grid.size = 100) %>% head

FeatureEffects$new(mod_predictor_rf, method = "ale") %>% plot()

mod_predictor_rf_pca <-  Predictor$new(model = rf_pca_fit, 
                                       data = tb_data # , 
                                       # y = y,
                                       # predict.fun = predict.fun, 
                                       # type = type)
)

est_model_contrib(mod_predictor_rf_pca, method = "ale") %>% head

FeatureEffects$new(mod_predictor_rf_pca, method = "ale") %>% plot()


#### TRY PDP

calc_pdp <- function(m, tb_data, feature) {
  
  quo_feature <- sym(feature)
  data_feature <- tb_data %>% pull(feature)
  
  pdp_feature <- sapply(data_feature, function(x) {
    tb_aux <- tb_data %>% mutate(!!quo_feature := x)
    predict(m, tb_aux) %>% mean
  })
  
  out <- tibble(!!quo_feature := data_feature,
                pdp = pdp_feature) %>% 
    arrange(!!quo_feature)
  
  out
}

pdp_x1 <- sapply(tb_data$x1, function(x) {
  tb_aux <- tb_data %>% mutate(x1 = x)
  predict(lm_fit, tb_aux) %>% mean
})

tibble(x1 = tb_data$x1, pdp = pdp_x1) %>% arrange(x1)
FeatureEffect$new(mod_predictor_lm, method = "pdp", feature = "x1")$results

plot(tb_data$x1, pdp_x1, type = "l")

calc_pdp(lm_fit, tb_data, "x1")
