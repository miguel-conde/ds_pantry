
# https://www.cienciadedatos.net/documentos/59_machine_learning_co 

library(tidymodels)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(ggpubr)
library(univariateML)
library(GGally)
library(doParallel)



# DATOS -------------------------------------------------------------------

data("SaratogaHouses", package = "mosaicData")
datos <- SaratogaHouses %>%  as_tibble()

# Se renombran las columnas para que sean más descriptivas
colnames(datos) <- c("precio", "metros_totales", "antiguedad", "precio_terreno",
                     "metros_habitables", "universitarios",
                     "dormitorios", "chimenea", "banyos", "habitaciones",
                     "calefaccion", "consumo_calefacion", "desague",
                     "vistas_lago","nueva_construccion", "aire_acondicionado")


# EDA ---------------------------------------------------------------------

datos


# Resumen General ---------------------------------------------------------

skim(datos)


# Número de observaciones y valores ausentes ------------------------------

# Número de datos ausentes por variable
datos %>% map_dbl(~ sum(is.na(.)))


plot_missing(
  data    = datos, 
  title   = "Porcentaje de valores ausentes",
  ggtheme = theme_bw(),
  theme_config = list(legend.position = "none")
)


# Variable respuesta ------------------------------------------------------

p1 <- ggplot(data = datos, aes(x = precio)) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  geom_rug(alpha = 0.1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribución original") +
  theme_bw() 

p2 <- ggplot(data = datos, aes(x = sqrt(precio))) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  geom_rug(alpha = 0.1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Transformación raíz cuadrada") +
  theme_bw() 

p3 <- ggplot(data = datos, aes(x = log(precio))) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  geom_rug(alpha = 0.1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Transformación logarítmica") +
  theme_bw() 

ggarrange(p1, p2, p3, ncol = 1, align = "v")

# Tabla de estadísticos principales 
summary(datos$precio)

# Se comparan únicamente las distribuciones con un dominio [0, +inf)
# Cuanto menor el valor AIC mejor el ajuste
comparacion_aic <- AIC(
  mlbetapr(datos$precio),
  mlexp(datos$precio),
  mlinvgamma(datos$precio),
  mlgamma(datos$precio),
  mllnorm(datos$precio),
  mlrayleigh(datos$precio),
  mlinvgauss(datos$precio),
  mlweibull(datos$precio),
  mlinvweibull(datos$precio),
  mllgamma(datos$precio)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)


# Variables continuas -----------------------------------------------------

plot_density(
  data    = datos %>% select(-precio),
  ncol    = 3,
  title   = "Distribución variables continuas",
  ggtheme = theme_bw(),
  theme_config = list(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(colour = "black", size = 12, face = 2)
  )
)


# Valores observados de chimenea.
table(datos$chimenea)

# Se convierte la variable chimenea a factor.
datos <- datos %>%
  mutate(chimenea = as.factor(chimenea))

datos <- datos %>%
  mutate(
    # Se le añade +0.1 a la antigüedad para que cuando la antigüedad es
    # 0 no de -Inf
    log_antiguedad     = log10(antiguedad + 0.1),
    log_metros_totales = log10(metros_totales + 0.1),
    log_precio_terreno = log10(precio_terreno)
  )

# La limitación de la función plot_scatterplot() es que no permite añadir una
# curva de tendencia.
datos %>%
  select_if(is.numeric) %>%
  select(-c(antiguedad, metros_totales, precio_terreno)) %>%
  plot_scatterplot(
    by   = "precio",
    ncol = 3,
    geom_point_args = list(alpha = 0.1),
    ggtheme = theme_bw(),
    theme_config = list(
      strip.text = element_text(colour = "black", size = 12, face = 2),
      legend.position = "none"
    )
  )


custom_corr_plot <- function(variable1, variable2, df, alpha=0.3){
  p <- df %>%
    mutate(
      # Truco para que se ponga el título estilo facet
      title = paste(toupper(variable2), "vs", toupper(variable1))
    ) %>%
    ggplot(aes(x = !!sym(variable1), y = !!sym(variable2))) + 
    geom_point(alpha = alpha) +
    # Tendencia no lineal
    geom_smooth(se = FALSE, method = "gam", formula =  y ~ splines::bs(x, 3)) +
    # Tendencia lineal
    geom_smooth(se = FALSE, method = "lm", color = "firebrick") +
    facet_grid(. ~ title) +
    theme_bw() +
    theme(strip.text = element_text(colour = "black", size = 10, face = 2),
          axis.title = element_blank())
  return(p)
}


variables_continuas <- c("metros_habitables", "universitarios", "dormitorios",
                         "banyos", "habitaciones", "log_antiguedad", "log_metros_totales",
                         "log_precio_terreno")

plots <- map(
  .x = variables_continuas,
  .f = custom_corr_plot,
  variable2 = "precio",
  df = datos
)

ggarrange(plotlist = plots, ncol = 3, nrow = 3) %>%
  annotate_figure(
    top = text_grob("Correlación con precio", face = "bold", size = 16,
                    x = 0.13)
  )


# Correlación variables continuas -----------------------------------------

plot_correlation(
  data = datos %>% select(-starts_with("log_")),
  type = "continuous",
  title = "Matriz de correlación variables continuas",
  theme_config = list(legend.position = "none",
                      plot.title = element_text(size = 16, face = "bold"),
                      axis.title = element_blank(),
                      axis.text.x = element_text(angle = -45, hjust = +0.1)
  )
)

GGally::ggscatmat(
  data = datos %>% select(-starts_with("log_")) %>% select_if(is.numeric),
  alpha = 0.1) +
  theme_bw() +
  labs(title = "Correlación por pares") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_blank(),
    strip.text = element_text(colour = "black", size = 6, face = 2)
  )


# Variables cualitativas --------------------------------------------------

plot_bar(
  datos,
  ncol    = 3,
  title   = "Número de observaciones por grupo",
  ggtheme = theme_bw(),
  theme_config = list(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(colour = "black", size = 12, face = 2),
    legend.position = "none"
  )
)

table(datos$chimenea)

datos <- datos %>%
  mutate(
    chimenea = recode_factor(
      chimenea,
      `2` = "2_mas",
      `3` = "2_mas",
      `4` = "2_mas"
    )
  )

table(datos$chimenea)

custom_box_plot <- function(variable1, variable2, df, alpha=0.3){
  p <- df %>%
    mutate(
      # Truco para que se ponga el título estilo facet
      title = paste(toupper(variable2), "vs", toupper(variable1))
    ) %>%
    ggplot(aes(x = !!sym(variable1), y = !!sym(variable2))) + 
    geom_violin(alpha = alpha) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    facet_grid(. ~ title) +
    theme_bw() +
    theme(strip.text = element_text(colour = "black", size = 10, face = 2),
          axis.title = element_blank())
  return(p)
}

variables_cualitativas <- c("chimenea", "calefaccion", "consumo_calefacion", "desague",
                            "vistas_lago", "nueva_construccion", "aire_acondicionado")

plots <- map(
  .x = variables_cualitativas,
  .f = custom_box_plot,
  variable2 = "precio",
  df = datos
)

ggarrange(plotlist = plots, ncol = 3, nrow = 3) %>%
  annotate_figure(
    top = text_grob("Correlación con precio", face = "bold", size = 16,
                    x = 0.13)
  )


# DIVISIÓN TRAIN y TEST ---------------------------------------------------

# Reparto de datos en train y test
set.seed(123)
split_inicial <- initial_split(
  data   = datos,
  prop   = 0.8,
  strata = precio
)
datos_train <- training(split_inicial)
datos_test  <- testing(split_inicial)

summary(datos_train$precio)
summary(datos_test$precio)


# PREPROCESADO ------------------------------------------------------------


# Imputación de valores ausentes ------------------------------------------


# Exclusión de variables con varianza próxima a cero ----------------------


# Estandarización y escalado de variables numéricas -----------------------


# Binarización de las variables cualitativas ------------------------------

# La binarización consiste en crear nuevas variables dummy con cada uno de los 
# niveles de las variables cualitativas. A este proceso también se le conoce 
# como one hot encoding.

# Se almacenan en un objeto `recipe` todos los pasos de preprocesado y, finalmente,
# se aplican a los datos.


# Recipe ------------------------------------------------------------------


transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

transformer

# Se entrena el objeto recipe
transformer_fit <- prep(transformer)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
datos_train_prep <- bake(transformer_fit, new_data = datos_train)
datos_test_prep  <- bake(transformer_fit, new_data = datos_test)

glimpse(datos_train_prep)


# MODELADO ----------------------------------------------------------------


# Entrenamiento -----------------------------------------------------------

modelo_tree <- decision_tree(mode = "regression") %>%
  set_engine(engine = "rpart")

modelo_tree

# Entrenamiento empleando fórmula
modelo_tree_fit <- modelo_tree %>%
  fit(
    formula = precio ~ .,
    data    = datos_train_prep
  )

# Entrenamiento empleando x e Y.
variable_respuesta <- "precio"
predicores <- setdiff(colnames(datos_train_prep), variable_respuesta)
modelo_tree_fit <- modelo_tree %>%
  fit_xy(
    x = datos_train_prep[, predicores],
    y = datos_train_prep[[variable_respuesta]]
  )

modelo_tree_fit$fit
modelo_tree_fit$fit %>% class()


# Validación del modelo ---------------------------------------------------

set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  repeats = 10,
  strata  = precio
)

head(cv_folds)

modelo_tree <- decision_tree(mode = "regression") %>%
  set_engine(engine = "rpart")

validacion_fit <- fit_resamples(
  object       = modelo_tree,
  # El objeto recipe no tiene que estar entrenado
  preprocessor = transformer,
  # Las resamples se tienen que haber creado con los datos sin 
  # prerocesar
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE)
)

head(validacion_fit)

# Métricas promedio de todas las particiones
validacion_fit %>% collect_metrics(summarize = TRUE)

# Métricas individuales de cada una de las particiones
validacion_fit %>% collect_metrics(summarize = FALSE) %>% head()

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = validacion_fit %>% collect_metrics(summarize = FALSE),
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() 

p2 <- ggplot(
  data = validacion_fit %>% collect_metrics(summarize = FALSE),
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggarrange(p1, p2, nrow = 2, common.legend = TRUE, align = "v") %>% 
  annotate_figure(
    top = text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
validacion_fit %>% collect_predictions(summarize = TRUE) %>% head()

# Gráficos validación regresión

p1 <- ggplot(
  data = validacion_fit %>% collect_predictions(summarize = TRUE),
  aes(x = precio, y = .pred)
) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick") +
  labs(title = "Valor predicho vs valor real") +
  theme_bw()


p2 <- ggplot(
  data = validacion_fit %>% collect_predictions(summarize = TRUE),
  aes(x = .row, y = precio - .pred)
) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept =  0, color = "firebrick") +
  labs(title = "Residuos del modelo") +
  theme_bw()

p3 <- ggplot(
  data = validacion_fit %>% collect_predictions(summarize = TRUE),
  aes(x = precio - .pred)
) +
  geom_density() + 
  labs(title = "Distribución residuos del modelo") +
  theme_bw()

p4 <- ggplot(
  data = validacion_fit %>% collect_predictions(summarize = TRUE),
  aes(sample = precio - .pred)
) +
  geom_qq() +
  geom_qq_line(color = "firebrick") +
  labs(title = "Q-Q residuos del modelo") +
  theme_bw()

ggarrange(plotlist = list(p1, p2, p3, p4)) %>%
  annotate_figure(
    top = text_grob("Distribución residuos", size = 15, face = "bold")
  )

# Paralelización
registerDoParallel(cores = detectCores() - 1)
set.seed(2020)
modelo_tree <- decision_tree(mode = "regression") %>%
  set_engine(engine = "rpart")

validacion_fit <- fit_resamples(
  object       = modelo_tree,
  preprocessor = transformer,
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE)
)

stopImplicitCluster()


# Hiperparámetros (tuning) ------------------------------------------------

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_tree <- decision_tree(
  mode       = "regression",
  tree_depth = tune(),
  min_n      = tune()
) %>%
  set_engine(engine = "rpart")

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)

grid_fit <- tune_grid(
  object       = modelo_tree,
  # El objeto recipe no tiene que estar entrenado
  preprocessor = transformer,
  # Las resamples se tienen que haber creado con los datos sin 
  # prerocesar
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_grid(save_pred = TRUE),
  # Número de combinaciones generadas automáticamente
  grid         = 70
)
stopImplicitCluster()

# grid_fit %>% unnest(.metrics) %>% head()
grid_fit %>% collect_metrics(summarize = TRUE) %>% head()

grid_fit %>% show_best(metric = "rmse", n = 5)

grid_fit %>%
  collect_metrics(summarize = TRUE) %>%
  filter(.metric == "rmse") %>%
  select(-c(.estimator, n)) %>%
  pivot_longer(
    cols = c(tree_depth, min_n),
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(x = value, y = mean, color = parameter)) +
  geom_point() +
  geom_line() + 
  labs(title = "Evolución del error en función de los hiperparámetros") +
  facet_wrap(facets = vars(parameter), nrow = 2, scales = "free") +
  theme_bw() + 
  theme(legend.position = "none")

grid_fit %>%
  collect_metrics(summarize = TRUE) %>%
  filter(.metric == "rmse") %>%
  select(-c(.estimator, n)) %>%
  ggplot(aes(x = tree_depth, y = min_n, color = mean, size = mean)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(title = "Evolución del error en función de los hiperparámetros") +
  theme_bw()

# Se repite la búsqueda de mejores hiperparámetros pero esta vez definiendo el espacio de búsqueda.

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_tree <- decision_tree(
  mode       = "regression",
  tree_depth = tune(),
  min_n      = tune()
) %>%
  set_engine(engine = "rpart")

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# GRID DE HIPERPARÁMETROS
# 
set.seed(1234)
hiperpar_grid <- grid_random(
  # Rango de búsqueda para cada hiperparámetro
  tree_depth(range = c(1, 10), trans = NULL),
  min_n(range      = c(2, 100), trans = NULL),
  # Número combinaciones aleatorias probadas
  size = 50
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)

grid_fit <- tune_grid(
  object       = modelo_tree,
  # El objeto recipe no tiene que estar entrenado
  preprocessor = transformer,
  # Las resamples se tienen que haber creado con los datos sin 
  # prerocesar
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE),
  # Hiperparámetros
  grid         = hiperpar_grid
)
stopImplicitCluster()

grid_fit %>% show_best(metric = "rmse", n = 5)


# Modelo final ------------------------------------------------------------

# Selección de los mejores hiperparámetros encontrados
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_tree_final <- finalize_model(x = modelo_tree, parameters = mejores_hiperpar)
modelo_tree_final

modelo_tree_final_fit  <- modelo_tree_final %>%
  fit(
    formula = precio ~ .,
    data    = datos_train_prep
    #data   = bake(transformer_fit, datos_train)
  )


# Predicción --------------------------------------------------------------

predicciones <- modelo_tree_final_fit %>%
  predict(
    new_data = datos_test_prep,
    #new_data = bake(transformer_fit, datos_test),
    type = "numeric"
  )

predicciones %>% head()

# Error de test -----------------------------------------------------------

predicciones <- predicciones %>% 
  bind_cols(datos_test_prep %>% select(precio))

rmse(predicciones, truth = precio, estimate = .pred, na_rm = TRUE)


# Diagnostico -------------------------------------------------------------

predicciones_train <- modelo_tree_final_fit %>%
  predict(
    new_data = datos_train_prep,
    type = "numeric"
    )

predicciones_train <- predicciones_train %>% 
  bind_cols(datos_train_prep %>% select(precio))

# Error de train
rmse(predicciones_train, truth = precio, estimate = .pred, na_rm = TRUE)

# Error de CV
grid_fit %>% show_best(metric = "rmse", n = 1)

# Error de test
rmse(predicciones, truth = precio, estimate = .pred, na_rm = TRUE)


# WORKFLOWS ---------------------------------------------------------------

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_tree <- decision_tree(
  mode       = "regression",
  tree_depth = tune(),
  min_n      = tune()
) %>%
  set_engine(engine = "rpart")

# DEFINICIÓN DEL PREPROCESADO
# 
transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# WORKFLOW
# 
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo_tree)

workflow_modelado

# GRID DE HIPERPARÁMETROS
# 
hiperpar_grid <- grid_regular(
  # Rango de búsqueda para cada hiperparámetro
  tree_depth(range = c(1, 10), trans = NULL),
  min_n(range = c(2, 100), trans = NULL),
  # Número valores por hiperparámetro
  levels = c(3, 3)
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)

grid_fit <- tune_grid(
  object       = workflow_modelado,
  resamples    = cv_folds,
  metrics      = metric_set(rmse, mae),
  control      = control_resamples(save_pred = TRUE),
  # Hiperparámetros
  grid         = hiperpar_grid
)
stopImplicitCluster()

# ENTRENAMIENTO FINAL
# 
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_final_fit <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
) %>%
  fit(
    data = datos_train
  ) %>%
  pull_workflow_fit()


# ESTRATEGIAS DE TUNING ---------------------------------------------------

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_tree <- decision_tree(
  mode       = "regression",
  tree_depth = tune(),
  min_n      = tune()
) %>%
  set_engine(engine = "rpart")

# DEFINICIÓN DEL PREPROCESADO
# 
transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# WORKFLOW
# 
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo_tree)

# GRID DE HIPERPARÁMETROS
# 
hiperpar_grid <- grid_regular(
  # Rango de búsqueda para cada hiperparámetro
  tree_depth(range = c(1, 10), trans = NULL),
  min_n(range = c(2, 100), trans = NULL),
  # Número valores por hiperparámetro
  levels = c(3, 3)
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)

grid_fit <- tune_bayes(
  workflow_modelado, 
  resamples = cv_folds,
  # Iniciación aleatoria con 20 candidatos
  initial = 20,
  # Numero de iteraciones de optimización
  iter    = 30,
  # Métrica optimizada
  metrics = metric_set(rmse),
  control = control_bayes(no_improve = 20, verbose = FALSE)
)
stopImplicitCluster()

# Se muestra la evolución del error
autoplot(grid_fit, type = "performance") +
  labs(title = "Evolución del error") +
  theme_bw()

# Se muestra los mejores hiperparámetros encontrados
show_best(x = grid_fit, metric = "rmse")

# ENTRENAMIENTO FINAL
# 
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_final_fit <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
) %>%
  fit(
    data = datos_train
  ) %>%
  pull_workflow_fit()


# MODELOS -----------------------------------------------------------------


# glm ---------------------------------------------------------------------


# 1. Busqueda hiperprarámetros --------------------------------------------


# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_glm <- linear_reg(
  mode    = "regression",
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine(engine = "glmnet", nlambda = 100)

# DEFINICIÓN DEL PREPROCESADO
# 
transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# WORKFLOW
# 
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo_glm)

# GRID DE HIPERPARÁMETROS
# 
hiperpar_grid <- grid_regular(
  # Rango de búsqueda para cada hiperparámetro
  penalty(range = c(0, 1), trans = NULL),
  mixture(range = c(0, 1), trans = NULL),
  # Número de combinaciones totales
  levels = c(10, 10)
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)
grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(rmse),
  control   = control_resamples(save_pred = TRUE),
  # Hiperparámetros
  grid      = hiperpar_grid
)
stopImplicitCluster()

show_best(grid_fit, metric = "rmse", n = 10)


# 2. Mejor modelo ---------------------------------------------------------


# ENTRENAMIENTO FINAL
# 
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_glm <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
)

modelo_glm_fit <-  modelo_glm %>%
  fit(
    data = datos_train
  )


# 3. Predicciones test ----------------------------------------------------


# PREDICCIÓN TEST
# 
predicciones <- modelo_glm_fit %>%
  predict(
    new_data = datos_test,
    type = "numeric"
  )


# 4. Métrica test ---------------------------------------------------------


# MÉTRICAS TEST
# 
predicciones <- predicciones %>% 
  bind_cols(datos_test_prep %>% select(precio))

error_test_glm  <- rmse(
  data     = predicciones,
  truth    = precio,
  estimate = .pred,
  na_rm    = TRUE
) %>%
  mutate(
    modelo = "GLM"
  )

error_test_glm


# Random Forest -----------------------------------------------------------

# 1. Busqueda hiperprarámetros --------------------------------------------

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_rf <- rand_forest(
  mode  = "regression",
  mtry  = tune(),
  trees = tune(),
  min_n = tune()
) %>%
  set_engine(engine = "ranger")

# DEFINICIÓN DEL PREPROCESADO
# 
transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# WORKFLOW
# 
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo_rf)

# GRID DE HIPERPARÁMETROS
# 
hiperpar_grid <- grid_max_entropy(
  # Rango de búsqueda para cada hiperparámetro
  mtry(range = c(1L, 10L), trans = NULL),
  trees(range = c(500L, 3000L), trans = NULL),
  min_n(range = c(2L, 100L), trans = NULL),
  # Número de combinaciones totales
  size = 100
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)
grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(rmse),
  control   = control_resamples(save_pred = TRUE),
  # Hiperparámetros
  grid      = hiperpar_grid
)
stopImplicitCluster()

show_best(grid_fit, metric = "rmse")

# 2. Mejor modelo ---------------------------------------------------------

# ENTRENAMIENTO FINAL
# 
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_rf <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
)

modelo_rf_fit <- modelo_rf %>%
  fit(
    data = datos_train
  )

# 3. Predicciones test ----------------------------------------------------

# PREDICCIÓN TEST
# 
predicciones <- modelo_rf_fit %>%
  predict(
    new_data = datos_test,
    type     = "numeric"
  )

# 4. Métrica test ---------------------------------------------------------

# MÉTRICAS DE TEST
# 
predicciones <- predicciones %>% 
  bind_cols(datos_test_prep %>% select(precio))

error_test_rf  <- rmse(
  data     = predicciones,
  truth    = precio,
  estimate = .pred,
  na_rm    = TRUE
) %>%
  mutate(
    modelo = "RF"
  )
error_test_rf

# svm ---------------------------------------------------------------------

# 1. Busqueda hiperprarámetros --------------------------------------------

# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_svm <- svm_rbf(
  mode      = "regression",
  cost      = tune(),
  rbf_sigma = tune(),
  margin    = tune()
) %>%
  set_engine(engine = "kernlab")

# DEFINICIÓN DEL PREPROCESADO
# 
transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# WORKFLOW
# 
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo_svm)

# GRID DE HIPERPARÁMETROS
# 
hiperpar_grid <- grid_random(
  # Rango de búsqueda para cada hiperparámetro
  cost(range = c(-10, -1), trans = log2_trans()),
  rbf_sigma(range = c(-10, 0), trans = log10_trans()),
  svm_margin(range = c(0, 0.2), trans = NULL), 
  # Número de combinaciones totales
  size = 100
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)
grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(rmse),
  control   = control_resamples(save_pred = TRUE),
  # Hiperparámetros
  grid      = hiperpar_grid
)
stopImplicitCluster()

show_best(grid_fit, metric = "rmse", n = 10)

# 2. Mejor modelo ---------------------------------------------------------

# ENTRENAMIENTO FINAL
# 
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_svm <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
)

modelo_svm_fit <- modelo_svm %>%
  fit(
    data = datos_train
  )

# 3. Predicciones test ----------------------------------------------------

# PREDICCIÓN TEST
# 
predicciones <- modelo_svm_fit %>%
  predict(
    new_data = datos_test,
    type     = "numeric"
  )

# 4. Métrica test ---------------------------------------------------------

# MÉTRICAS TEST
# 
predicciones <- predicciones %>% 
  bind_cols(datos_test_prep %>% select(precio))

error_test_svm <- rmse(
  data     = predicciones,
  truth    = precio,
  estimate = .pred,
  na_rm    = TRUE
) %>%
  mutate(
    modelo = "SVM"
  )

error_test_svm


# Mars --------------------------------------------------------------------

# 1. Busqueda hiperprarámetros --------------------------------------------


# DEFINICIÓN DEL MODELO Y DE LOS HIPERPARÁMETROS A OPTIMIZAR
# 
modelo_mars <- mars(
  mode  = "regression",
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "none"
) %>%
  set_engine(engine = "earth")

# DEFINICIÓN DEL PREPROCESADO
# 
transformer <- recipe(
  formula = precio ~ .,
  data =  datos_train
) %>%
  step_naomit(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes())

# DEFINICIÓN DE LA ESTRATEGIA DE VALIDACIÓN Y CREACIÓN DE PARTICIONES
# 
set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  strata  = precio
)

# WORKFLOW
# 
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo_mars)

# GRID DE HIPERPARÁMETROS
# 
hiperpar_grid <- grid_regular(
  # Rango de búsqueda para cada hiperparámetro
  num_terms(range = c(1, 20), trans = NULL),
  levels = 20
)

# EJECUCIÓN DE LA OPTIMIZACIÓN DE HIPERPARÁMETROS
# 
registerDoParallel(cores = parallel::detectCores() - 1)
grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(rmse),
  control   = control_resamples(save_pred = TRUE),
  # Hiperparámetros
  grid      = hiperpar_grid
)
stopImplicitCluster()

show_best(grid_fit, metric = "rmse", n = 10)

# 2. Mejor modelo ---------------------------------------------------------

# ENTRENAMIENTO FINAL
# 
mejores_hiperpar <- select_best(grid_fit, metric = "rmse")

modelo_mars <- finalize_workflow(
  x = workflow_modelado,
  parameters = mejores_hiperpar
)

mmodelo_mars_fit <- modelo_svm %>%
  fit(
    data = datos_train
  )

# 3. Predicciones test ----------------------------------------------------

# PREDICCIÓN TEST
# 
predicciones <- mmodelo_mars_fit %>%
  predict(
    new_data = datos_test,
    type     = "numeric"
  )

# 4. Métrica test ---------------------------------------------------------

# MÉTRICAS TEST
# 
predicciones <- predicciones %>% 
  bind_cols(datos_test_prep %>% select(precio))

error_test_mars <- rmse(
  data     = predicciones,
  truth    = precio,
  estimate = .pred,
  na_rm    = TRUE
) %>%
  mutate(
    modelo = "MARS"
  )
error_test_mars

# Comparación -------------------------------------------------------------


# 1. Error de validación cruzada ------------------------------------------

set.seed(1234)
cv_folds <- vfold_cv(
  data    = datos_train,
  v       = 5,
  repeats = 5,
  strata  = precio
)


registerDoParallel(cores = parallel::detectCores() - 1)

validacion_glm <- fit_resamples(
  object       = modelo_glm,
  # preprocessor = transformer,
  resamples    = cv_folds,
  metrics      = metric_set(rmse),
  control      = control_resamples(save_pred = FALSE)
) %>%
  collect_metrics(summarize = FALSE) %>%
  mutate(modelo = "GLM")

validacion_svm <- fit_resamples(
  object        = modelo_svm,
  #preprocessor = transformer,
  resamples     = cv_folds,
  metrics       = metric_set(rmse),
  control       = control_resamples(save_pred = FALSE)
) %>%
  collect_metrics(summarize = FALSE) %>%
  mutate(modelo = "SVM")

validacion_rf <- fit_resamples(
  object       = modelo_rf,
  # preprocessor = transformer,
  resamples    = cv_folds,
  metrics      = metric_set(rmse),
  control      = control_resamples(save_pred = FALSE)
) %>%
  collect_metrics(summarize = FALSE) %>%
  mutate(modelo = "RF")

validacion_mars <- fit_resamples(
  object       = modelo_mars,
  # preprocessor = transformer,
  resamples    = cv_folds,
  metrics      = metric_set(rmse),
  control      = control_resamples(save_pred = FALSE)
) %>%
  collect_metrics(summarize = FALSE) %>%
  mutate(modelo = "MARS")

stopImplicitCluster()

bind_rows(
  validacion_glm,
  validacion_rf,
  validacion_svm,
  validacion_mars
) %>%
  ggplot(aes(x = modelo, y = .estimate, color = modelo)) +
  geom_violin() +
  geom_boxplot(outlier.shape = NA, width = 0.2) +
  geom_point(alpHa = 0.1) +
  labs(title = "Error de validación cruzada", y = "RMSE") +
  theme_bw() +
  theme(legend.position = "none")


# 2. Error de test --------------------------------------------------------

errores_test <- bind_rows(
  error_test_glm,
  error_test_rf,
  error_test_svm,
  error_test_mars
)

errores_test %>% select(modelo, .metric, .estimate)

ggplot(data = errores_test) +
  geom_col(aes(x = modelo, y = .estimate), color = "gray") +
  coord_flip() +
  labs(title = "Error de test") +
  theme_bw()


