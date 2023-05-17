
# LIBRERÍAS y SOURCES -----------------------------------------------------


library(tidymodels)

get_mode <- function(v, method = c("first", "all")) {
  
  method <- match.arg(method)
  
  uniqv <- unique(v)
  tab <- tabulate(match(v, uniqv))
  
  if (method == "first") out <- uniqv[which.max(tab)]
  else out <- uniqv[tab == max(tab)]
  
  return(out)
}


# DATOS -------------------------------------------------------------------


# Cargar el conjunto de datos de iris
data(iris)


# MODELOS -----------------------------------------------------------------

# Creamos recipe
the_recipe <- 
  recipe(Sepal.Length ~ ., data = iris) %>%
  # step_normalize(all_numeric(), -all_outcomes()) %>%
  # step_pca(all_numeric(), -all_outcomes(), num_comp = 2) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Creamos el modelo lineal
the_model_lm <- linear_reg()

# Creamos el modelo random forest
the_model_rf <- rand_forest(engine = "ranger", mode = "regression")

# Crear un pipeline para preprocesar los datos y ajustar el modelo lineal
modelo_pipeline_lm <- workflow() %>%
  add_recipe(the_recipe) %>% 
  add_model(the_model_lm) %>% 
  fit(data = iris)

# Crear un pipeline para preprocesar los datos y ajustar el modelo random forest
modelo_pipeline_rf <- workflow() %>%
  add_recipe(the_recipe) %>% 
  add_model(the_model_rf) %>% 
  fit(data = iris)

# Predecir los valores de la variable de respuesta para los datos de prueba
predicciones_lm <- 
  predict(modelo_pipeline_lm, iris)

predicciones_rf <- 
  predict(modelo_pipeline_rf, iris)

results_lm <- iris %>% select(Sepal.Length) %>% 
  bind_cols(predicciones_lm)

results_rf <- iris %>% select(Sepal.Length) %>% 
  bind_cols(predicciones_rf)

# Evaluar el rendimiento del modelo en los datos de prueba

rmse(results_lm, truth = Sepal.Length, estimate = .pred)
mae(results_lm, truth = Sepal.Length, estimate = .pred)
mape(results_lm, truth = Sepal.Length, estimate = .pred)
rsq(results_lm, truth = Sepal.Length, estimate = .pred)

rmse(results_rf, truth = Sepal.Length, estimate = .pred)
mae(results_rf, truth = Sepal.Length, estimate = .pred)
mape(results_rf, truth = Sepal.Length, estimate = .pred)
rsq(results_rf, truth = Sepal.Length, estimate = .pred)


# EXPLICABILIDAD ----------------------------------------------------------


# Partial Dependencies - iris ---------------------------------------------



library(DALEX)

the_pred_function <- function(model, newdata) {
  predict(model, newdata) %>% pull(1)
}

the_explainer_lm <- DALEX::explain(model = modelo_pipeline_lm,
                                   data  = iris %>% select(-Sepal.Length),
                                   y     = iris %>% pull(Sepal.Length), 
                                   predict_function = the_pred_function,
                                   label = "lm")

model_profile(explainer = the_explainer_lm, variables = "Sepal.Width") %>% 
  plot()

model_profile(explainer = the_explainer_lm) %>% 
  plot()

model_profile(explainer = the_explainer_lm, variable_type = "categorical") %>% 
  plot()

model_profile(explainer = the_explainer_lm, variables = "Sepal.Width", k = 2) %>% 
  plot()

model_profile(explainer = the_explainer_lm, variables = "Sepal.Width", groups = "Species") %>% 
  plot()

model_profile(explainer = the_explainer_lm) %>% 
  plot(geom = "profiles")


the_explainer_rf <- DALEX::explain(model = modelo_pipeline_rf,
                                   data  = iris %>% select(-Sepal.Length),
                                   y     = iris %>% pull(Sepal.Length), 
                                   predict_function = the_pred_function,
                                   label = "rf")

model_profile(explainer = the_explainer_rf, variables = "Sepal.Width") %>% 
  plot()

model_profile(explainer = the_explainer_rf) %>% 
  plot()

model_profile(explainer = the_explainer_rf, variable_type = "categorical") %>% 
  plot()

model_profile(explainer = the_explainer_rf, variables = "Sepal.Width", k = 2) %>% 
  plot()

model_profile(explainer = the_explainer_rf, variables = "Sepal.Width", groups = "Species") %>% 
  plot()

model_profile(explainer = the_explainer_rf) %>% 
  plot(geom = "profiles")
model_profile(explainer = the_explainer_rf, k = 3) %>% 
  plot(geom = "profiles")
model_profile(explainer = the_explainer_rf, groups = "Species") %>% 
  plot(geom = "profiles")

mp_lm <- model_performance(the_explainer_lm)
pdp_lm <- model_profile(explainer = the_explainer_lm)
mp_rf <- model_performance(the_explainer_rf)
pdp_rf <- model_profile(explainer = the_explainer_rf)

plot(list(pdp_lm, pdp_rf))
plot(list(mp_lm, mp_rf))


##

kk_iris_0 <- iris %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, ~ 0) %>% 
  mutate_if(is.character, ~ factor(.)) %>% 
  mutate_if(is.factor, ~ levels(.)[1])

predict(modelo_pipeline_lm, kk_iris_0)
predict(modelo_pipeline_rf, kk_iris_0)

kk_iris_1 <- kk_iris_0 %>% mutate(Sepal.Width = iris$Sepal.Width)
# the_dist <- predict(modelo_pipeline_lm, kk_iris_1) %>% summarise_all(~ mean(.))
the_dist <- mean(the_pred_function(modelo_pipeline_lm, iris)) - 
  mean(the_pred_function(modelo_pipeline_lm, kk_iris_1)) + 
  mean(the_pred_function(modelo_pipeline_lm, kk_iris_0))

h_lm <- model_profile(explainer = the_explainer_lm, variables = "Sepal.Width", 
                   N = nrow(iris), type = "partial") # Parece que tiene que ser PARTIAL
alpha_lm <- h_lm$agr_profiles %>% mutate(`_yhat_` = `_yhat_` - the_dist)

the_dist_rf <- mean(the_pred_function(modelo_pipeline_rf, iris)) - 
  mean(the_pred_function(modelo_pipeline_rf, kk_iris_1)) + 
  mean(the_pred_function(modelo_pipeline_rf, kk_iris_0))

h_rf <- model_profile(explainer = the_explainer_rf, variables = "Sepal.Width", 
                      N = nrow(iris), type = "partial")
alpha_rf <- h_rf$agr_profiles %>% mutate(`_yhat_` = `_yhat_` - the_dist_rf)


pdp_2_contrib <- function(model, data, tgt_var, model_explainer, pred_function) {
  
  data_0 <- data %>% 
    as_tibble() %>% 
    mutate_if(is.numeric, ~ 0) %>% 
    mutate_if(is.character, ~ factor(.)) %>% 
    mutate_if(is.factor, ~ levels(.)[1])
  
  data_1 <- data_0 %>% mutate(!!sym(tgt_var) := pull(data, !!sym(tgt_var)))
  # browser()
  the_dist <- mean(pred_function(model, data)) - 
    mean(pred_function(model, data_1)) + 
    mean(pred_function(model, data_0))
  
  h <- model_profile(explainer = model_explainer, 
                     variables = tgt_var, 
                     N = nrow(data), 
                     type = "partial") # Parece que tiene que ser PARTIAL
  
  alpha <- h$agr_profiles %>% mutate(`_yhat_` = `_yhat_` - the_dist)
  
  return(alpha)
  
}

pdp_2_contrib(modelo_pipeline_lm, iris, "Sepal.Width", the_explainer_lm, the_pred_function)
pdp_2_contrib(modelo_pipeline_rf, iris, "Sepal.Width", the_explainer_rf, the_pred_function)

pdp_2_contrib(modelo_pipeline_lm, iris, "Species", the_explainer_lm, the_pred_function)
pdp_2_contrib(modelo_pipeline_rf, iris, "Species", the_explainer_lm, the_pred_function)


# Cómo interpolar / extrapolar

# Baseline / Intercept ...
iris_0 <- iris %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, ~ 0) %>% 
  mutate_if(is.character, ~ factor(.)) %>% 
  mutate_if(is.factor, ~ factor(levels(.)[1], levels(.)))

the_pred_function(modelo_pipeline_lm, iris_0)
extract_fit_engine(modelo_pipeline_lm) %>% coef()

the_pred_function(modelo_pipeline_rf, iris_0)



# Partial dependencies - Boston -------------------------------------------

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

# Creamos recipe
the_recipe <- 
  recipe(medv ~ lstat + rm + dis + indus, data = Boston) %>%
  # step_normalize(all_numeric(), -all_outcomes()) %>%
  # step_pca(all_numeric(), -all_outcomes(), num_comp = 2) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Creamos el modelo lineal
the_model_lm <- linear_reg()

# Creamos el modelo random forest
the_model_rf <- rand_forest(engine = "ranger", mode = "regression")

# Crear un pipeline para preprocesar los datos y ajustar el modelo lineal
modelo_pipeline_lm <- workflow() %>%
  add_recipe(the_recipe) %>% 
  add_model(the_model_lm) %>% 
  fit(data = Boston)

# Crear un pipeline para preprocesar los datos y ajustar el modelo random forest
modelo_pipeline_rf <- workflow() %>%
  add_recipe(the_recipe) %>% 
  add_model(the_model_rf) %>% 
  fit(data = Boston)

the_explainer_lm <- DALEX::explain(model = modelo_pipeline_lm,
                                   data  = Boston %>% select(-medv),
                                   y     = Boston %>% pull(medv), 
                                   predict_function = the_pred_function,
                                   label = "lm")

the_explainer_rf <- DALEX::explain(model = modelo_pipeline_rf,
                                   data  = Boston %>% select(-medv),
                                   y     = Boston %>% pull(medv), 
                                   predict_function = the_pred_function,
                                   label = "rf")

pdp_2_contrib(modelo_pipeline_lm, Boston, "lstat", the_explainer_lm, the_pred_function)
pdp_2_contrib(modelo_pipeline_rf, Boston, "lstat", the_explainer_rf, the_pred_function)

pdp_2_contrib(modelo_pipeline_lm, Boston, "rm", the_explainer_lm, the_pred_function)
pdp_2_contrib(modelo_pipeline_rf, Boston, "rm", the_explainer_rf, the_pred_function)

pdp_2_contrib(modelo_pipeline_lm, Boston, "dis", the_explainer_lm, the_pred_function)
pdp_2_contrib(modelo_pipeline_rf, Boston, "dis", the_explainer_rf, the_pred_function)

pdp_2_contrib(modelo_pipeline_lm, Boston, "indus", the_explainer_lm, the_pred_function)
pdp_2_contrib(modelo_pipeline_rf, Boston, "indus", the_explainer_rf, the_pred_function)


# Shap --------------------------------------------------------------------

the_pred_function(modelo_pipeline_lm, iris) %>% mean()

avg_iris <- iris %>% 
  mutate_if(is.numeric, ~ mean(.)) %>% 
  mutate_if(is.factor, ~ get_mode(.))

the_pred_function(modelo_pipeline_lm, avg_iris)



the_explainer_rf
predict(the_explainer_rf, iris[1,])

shap_1_rf <- predict_parts(explainer = the_explainer_rf, 
                            new_observation = iris[1,], 
                            type = "shap",
                            B = 25)
plot(shap_1_rf)

shap_1_rf

shap_1_rf %>% as_tibble() %>% 
  group_by(variable_name) %>% 
  summarise(shp = mean(contribution)) %>% 
  spread(variable_name, shp)

### 
shap_1_lm <- predict_parts(explainer = the_explainer_lm, 
                           new_observation = iris[1,], 
                           type = "shap",
                           B = 25)
plot(shap_1_lm)

shap_1_lm

shap_1_lm %>% as_tibble() %>% 
  group_by(variable_name) %>% 
  summarise(shp = mean(contribution)) %>% 
  spread(variable_name, shp)


###



get_shap_values <- function(in_model, X, nsim, pred_wrapper, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  shap_values <- fastshap::explain(in_model,
                                   X = X,
                                   nsim = nsim,
                                   pred_wrapper = pred_wrapper,
                                   adjust = TRUE)
  shap_values
}


get_shap_contribs <- function(in_data, shap_values, in_model, pred_wrapper, pred = FALSE) {
  
  delta_vector <- rep(NA, ncol(shap_values)+1)
  names(delta_vector) <- c("baseline", colnames(shap_values))
  
  delta_vector["baseline"] <-
    pred_wrapper(in_model, in_data %>% mutate_all(~ 0)) %>% .[1]
  
  for (v in colnames(shap_values)) {
    
    aux <- in_data %>%
      mutate_at(vars(-all_of(v)), ~ 0) %>%
      mutate_at(all_of(v), ~ mean(.))
    
    delta_vector[v] <- pred_wrapper(in_model, aux[1,]) - delta_vector["baseline"]
  }
  
  shap_contribs <- shap_values %>%
    as.data.frame() %>%
    mutate(baseline = 0, .before = 1) %>%
    sweep(2, delta_vector[c("baseline", colnames(shap_values))], "+") %>%
    as_tibble()
  
  if (pred == TRUE) shap_contribs <- shap_contribs %>%
    mutate(y_hat = rowSums(shap_contribs))
  
  return(shap_contribs)
}

shap_contribs <- function(in_model,
                          in_data,
                          pred_vars,
                          pred_fun,
                          nsim = 10,
                          grid_resolution = 20,
                          seed = NULL) {
  
  shap_values <- get_shap_values(in_model     = in_model,
                                 X            = in_data,
                                 nsim         = nsim,
                                 pred_wrapper = pred_fun,
                                 seed         = seed)
  
  shap_contribs <- get_shap_contribs(in_data      = in_data,
                                     shap_values  = shap_values,
                                     in_model     = in_model,
                                     pred_wrapper = pred_fun)
  
  out <- list(contribs        = shap_contribs,
              smooth_contribs = NULL,
              contrib_grid    = NULL,
              contrib_funs    = NULL)
  
  return(out)
  
}

### 

library(ranger)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

Boston_train <- Boston[-1:-6, c(x_var, y_var)]
Boston_test  <- Boston[1:6, x_var]

f <- as.formula(paste(y_var, "~", paste(x_var, collapse = "+")))
model_lm <- lm(f, data = Boston_train)
model_rf <- ranger(f, data = Boston_train)

## LM
contribs_lm <- model.matrix(f, Boston_train) %>%
  sweep(2, coef(model_lm), "*") %>%
  as_tibble() %>%
  mutate(pred = rowSums(.))
head(contribs_lm)

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap_lm <- fastshap::explain(model_lm,
                             X = subset(Boston_train, select = -medv),
                             nsim = 10,
                             pred_wrapper = predict,
                             adjust = TRUE)
shap_lm

shap_contribs_lm <- get_shap_contribs(Boston_train, shap_lm, model_lm, predict, pred = TRUE)
shap_contribs_lm
head(contribs_lm)

plot(Boston_train$lstat, shap_contribs_lm$lstat)

## RF

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap_rf <- fastshap::explain(model_rf,
                             X = subset(Boston_train, select = -medv),
                             nsim = 10,
                             pred_wrapper = function(object, newdata) predict(object, newdata)$predictions,
                             adjust = TRUE)
shap_rf

shap_contribs_rf <- get_shap_contribs(Boston_train, shap_rf,
                                      model_rf,
                                      function(object, newdata) predict(object, newdata)$predictions,
                                      pred = TRUE)
shap_contribs_rf

plot(Boston_train$lstat, shap_contribs_rf$lstat)
plot(Boston_train$rm, shap_contribs_rf$rm)
plot(Boston_train$dis, shap_contribs_rf$dis)
plot(Boston_train$indus, shap_contribs_rf$indus)


shap_contribs(in_model = model_rf,
              in_data = Boston_train,
              pred_vars = NULL,
              pred_fun = function(object, newdata) predict(object, newdata)$predictions,
              nsim = 10,
              grid_resolution = 20,
              seed = NULL)


###
library(rcontribs)

pdp_pred <- function(object, newdata)  {
  results <- as.vector(predict(object, newdata))
  return(results)
}

res <- pdp_1_contrib(model_lm, Boston, "lstat", pdp_pred)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["lstat"]), 
              color = "red", linetype = 2) +
  coord_cartesian(ylim = c(-20, 30))

res <- pdp_1_contrib(m_lm, Boston, "black", pdp_pred_lm)

autoplot(res) +
  geom_abline(intercept = 0, slope = as.numeric(coef(m_lm)["black"]), color = "red") +
  coord_cartesian(ylim = c(-20, 30))

res_all_lm <- pdp_contribs(model_lm, Boston, 
                           Boston %>% select(-medv) %>% names(), 
                           pdp_pred)

probe_lm <- res_all_lm$contribs %>% mutate(y_hat = rowSums(.))
probe_lm

plot(predictions_lm, probe_lm$y_hat)
abline(a= 0, b = 1)

probe_lm %>% summarise_all(~ sum(.))
sum(predictions_lm)

contribs_lm %>% summarise_all(~ sum(.))

curve(res_all_lm$contrib_funs$crim(x), from = min(Boston$crim), to = max(Boston$crim))
curve(res_all_lm$contrib_funs$zn(x), from = min(Boston$zn), to = max(Boston$zn))
curve(res_all_lm$contrib_funs$indus(x), from = min(Boston$indus), to = max(Boston$indus))
curve(res_all_lm$contrib_funs$chas(x), from = min(Boston$chas), to = max(Boston$chas))
curve(res_all_lm$contrib_funs$nox(x), from = min(Boston$nox), to = max(Boston$nox))
curve(res_all_lm$contrib_funs$rm(x), from = min(Boston$rm), to = max(Boston$rm))
curve(res_all_lm$contrib_funs$age(x), from = min(Boston$age), to = max(Boston$age))
curve(res_all_lm$contrib_funs$dis(x), from = min(Boston$dis), to = max(Boston$dis))
curve(res_all_lm$contrib_funs$rad(x), from = min(Boston$rad), to = max(Boston$rad))
curve(res_all_lm$contrib_funs$tax(x), from = min(Boston$tax), to = max(Boston$tax))
curve(res_all_lm$contrib_funs$ptratio(x), from = min(Boston$ptratio), to = max(Boston$ptratio))
curve(res_all_lm$contrib_funs$black(x), from = min(Boston$black), to = max(Boston$black))
curve(res_all_lm$contrib_funs$lstat(x), from = min(Boston$lstat), to = max(Boston$lstat))


### 
kk <- model_profile(explainer = the_explainer_lm, variables = "Sepal.Width",
                    type = "partial")

kk$agr_profiles %>% 
  as_tibble() %>% 
  mutate(contrib = coef(extract_fit_engine(modelo_pipeline_lm))["Sepal.Width"] * `_x_`) %>% 
  mutate(dist = contrib - `_yhat_`)

pdp_2_contrib <- function(model, data, tgt_var, model_explainer, pred_function) {
  
  data_0 <- data %>% 
    as_tibble() %>% 
    mutate_at(all_of(tgt_var), ~ ifelse(is.numeric(.), 0, .)) %>% 
    mutate_at(all_of(tgt_var), ~ ifelse(is.character(.), factor(.), .)) %>% 
    mutate_at(all_of(tgt_var), ~ ifelse(is.factor(.), levels(.)[1], .))
  
  the_dist <- mean(pred_function(model, data_0)) 
  
  h <- model_profile(explainer = model_explainer, 
                     variables = tgt_var, 
                     N = nrow(data), 
                     type = "partial") # Parece que tiene que ser PARTIAL
  
  alpha <- h$agr_profiles %>% mutate(`_yhat_` = `_yhat_` - the_dist)
  
  return(alpha)
  
}

