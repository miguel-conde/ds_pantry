
# LIBRERÍAS y SOURCES -----------------------------------------------------


library(tidymodels)


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
#  3 TEMAS POR RESOLVER:

# 1 - Pasar de los pdps a contribuciones
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

# 2 - Qué hacer con la intercept
# 3 - Cómo interpolar


the_pred_function2 <- function(object, newdata) {
  predict(object, newdata) %>% pull(1)
}

pd_values <- pdp::partial(
  object = modelo_pipeline_lm,
  train = iris,
  pred.var = "Sepal.Width",
  pred.fun = the_pred_function2, 
  grid_resolution = 20,
  ice = TRUE
)

pd_values <- pd_values %>%
  as_tibble() %>%
  group_by(Sepal.Width) %>%
  summarise(yhat = mean(yhat))

pdp_1_contrib <- function(in_model, in_data, pred_var, pred_fun, grid_resolution = 20) {
  
  pd_values <- pdp::partial(
    in_model,
    train = in_data,
    pred.var = pred_var,
    pred.fun = pred_fun,
    grid.resolution =  grid_resolution,
    ice = TRUE
  )
  
  aux_class <- class(pd_values)
  
  pd_values <- pd_values %>%
    as_tibble() %>%
    group_by(!!sym(pred_var)) %>%
    summarise(yhat = mean(yhat))
  
  class(pd_values) <- aux_class
  
  avg_y_hat <- pred_fun(in_model, in_data) %>% mean()
  avg_x <- in_data %>% pull(!!sym(pred_var)) %>% mean()
  
  X_aux_0 <- in_data %>% mutate_all(~ 0)
  # X_aux_0 <- in_data %>% mutate_if(is.numeric, ~ 0) %>% mutate_if(is.factor, ~ levels(.)[1])
  X_aux_1 <- X_aux_0  %>% mutate(!!sym(pred_var) := avg_x)
  
  # La distancia es la media de:
  #          fitted - fitted cuando todos valen 0 menos el de interés
  dist <- avg_y_hat - (mean(pred_fun(in_model, X_aux_1)) - mean(pred_fun(in_model, X_aux_0)))
  
  # aux_class <- class(pd_values)
  # out <- pd_values %>%
  #   as_tibble() %>%
  #   mutate(yhat = yhat - dist) %>%
  #   as.data.frame()
  # class(out) <- aux_class
  
  out <- pd_values
  out$yhat <- out$yhat - dist
  
  return(out)
}

pdp_1_contrib(in_model = modelo_pipeline_lm, 
              in_data = iris, 
              pred_var = "Sepal.Width", 
              pred_fun = the_pred_function2, grid_resolution = 20)
