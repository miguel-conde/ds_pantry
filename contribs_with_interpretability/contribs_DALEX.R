
# LIBRERÍAS y SOURCES -----------------------------------------------------


library(tidymodels)


# DATOS -------------------------------------------------------------------


# Cargar el conjunto de datos de iris
data(iris)


# MODELOS -----------------------------------------------------------------

# Creamos recipe
the_recipe <- 
  recipe(Sepal.Length ~ ., data = iris) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_pca(all_numeric(), -all_outcomes(), num_comp = 2) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Creamos el modelo lineal
the_model_lm <- linear_reg()

# Creamos el modelo random forest
the_model_rf <- rand_forest(engine = "ranger")

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
# 2 - Qué hacer con la intercept
# 3 - Cómo interpolar