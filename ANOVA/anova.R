
library(tidyverse)
library(car)


# 0-Supuestos del ANOVA ---------------------------------------------------

# Para aplicar correctamente ANOVA, nuestros datos deben cumplir con ciertos supuestos:
  
# Independencia: Los grupos comparados deben ser independientes entre sí, es decir, 
#                que la selección de cualquier individuo en un grupo no influya 
#                en la selección de los individuos en otro grupo.
# Normalidad: Los residuos (diferencias entre los valores observados y los 
#             predichos por el modelo) para cada grupo deben distribuirse normalmente.
# Homogeneidad de Varianzas (Homocedasticidad): Las varianzas de los grupos deben 
#                                               ser aproximadamente iguales.

# Normalidad: Podemos usar el test de Shapiro-Wilk en R (shapiro.test()) o en 
#             Python (scipy.stats.shapiro) para evaluar la normalidad de los 
#             residuos en cada grupo.
# 
# Homogeneidad de varianzas: El test de Levene (disponible tanto en R como en Python) 
#                            se utiliza para evaluar si las varianzas son iguales 
#                            entre los grupos. En R, se puede realizar con 
#                            leveneTest() del paquete car, y en Python, con 
#                            scipy.stats.levene.

# 1-Way ANOVA -------------------------------------------------------------

# ANOVA de un factor: También conocido como ANOVA unidireccional, compara las 
# medias entre los grupos que están clasificados en base a una sola característica 
# (factor). Es útil cuando estamos interesados en examinar el efecto de un solo 
# factor en una variable dependiente.


# Generación de datos -----------------------------------------------------

# Generar datos de muestra
set.seed(123) # Para reproducibilidad
datos <- data.frame(
  tienda = factor(rep(c("Tienda A", "Tienda B", "Tienda C"), each = 30)),
  satisfaccion = c(rnorm(30, mean = 70, sd = 10),
                   rnorm(30, mean = 75, sd = 10),
                   rnorm(30, mean = 65, sd = 10))
)

# Comprobación de supuestos -----------------------------------------------

# Verificación de Normalidad con el Test de Shapiro-Wilk
# H0: La distribución es normal.
shapiro_test_tiendaA <- shapiro.test(datos$satisfaccion[datos$tienda == 'Tienda A'])
shapiro_test_tiendaB <- shapiro.test(datos$satisfaccion[datos$tienda == 'Tienda B'])
shapiro_test_tiendaC <- shapiro.test(datos$satisfaccion[datos$tienda == 'Tienda C'])

# Verificación de Homogeneidad de Varianzas con el Test de Levene
# H0: Todas las muestras tienen varianzas iguales.
levene_test <- leveneTest(satisfaccion ~ tienda, data = datos)

# Mostrar resultados de los tests
print(shapiro_test_tiendaA)
print(shapiro_test_tiendaB)
print(shapiro_test_tiendaC)

# Prueba de Shapiro-Wilk para Normalidad
# La prueba de Shapiro-Wilk se utiliza para evaluar la normalidad de la 
# distribución de los datos en cada grupo. Los resultados son:
#   
# Tienda A: Estadístico = 0.97894, p-valor = 0.7966
# Tienda B: Estadístico = 0.98662, p-valor = 0.9614
# Tienda C: Estadístico = 0.98085, p-valor = 0.8478

# En todos los casos, los p-valores son mayores que 0.05, lo que significa que 
# no rechazamos la hipótesis nula de normalidad. Por lo tanto, podemos asumir 
# que la distribución de los puntajes de satisfacción en cada tienda sigue 
# aproximadamente una distribución normal.


print(levene_test)

# La prueba de Levene evalúa si las varianzas de los grupos son iguales. El 
# resultado de la prueba es un estadístico de 0.6446 con un p-valor de 0.5274. 
# Dado que el p-valor es mayor que 0.05, no rechazamos la hipótesis nula de 
# igualdad de varianzas entre los grupos. Esto indica que las varianzas son 
# homogéneas entre las tiendas.

# Visualización con un diagrama de caja
ggplot(datos, aes(x = tienda, y = satisfaccion)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de Satisfacción por Tienda", 
       x = "Tienda", 
       y = "Puntaje de Satisfacción")

# Visualización de la Distribución de los Datos
# La visualización mediante un boxplot muestra la dispersión y la mediana de los 
# puntajes de satisfacción para cada tienda. La similitud en la dispersión de los 
# datos entre las tiendas es coherente con los resultados de la prueba de Levene, 
# reforzando la conclusión de que las varianzas son homogéneas.

# Conclusión
# Los datos cumplen con los supuestos de normalidad y homogeneidad de varianzas 
# necesarios para realizar un ANOVA de manera válida. Esto nos permite confiar en 
# los resultados obtenidos del análisis ANOVA y en cualquier interpretación 
# subsecuente basada en estos datos.


# Realizar ANOVA de un factor ---------------------------------------------
resultado_anova <- aov(satisfaccion ~ tienda, data = datos)
summary(resultado_anova)

# información valiosa sobre el análisis realizado en el conjunto de datos con la 
# variable dependiente satisfaccion y la variable independiente categórica tienda. 
# Aquí está la interpretación de cada componente del resumen:
#
#  Df (Grados de Libertad):
#  
#  Para tienda: 2, lo que indica que hay tres niveles o grupos en la variable 
#               independiente tienda (el número de niveles menos uno).
#  Para Residuals: 87, lo que refleja el número total de observaciones menos el 
#                  número de niveles de la variable independiente (en este caso, 90 - 3 = 87).
#
#  Sum Sq (Suma de Cuadrados):
#    
#  Para tienda: 2041, es la suma de cuadrados entre grupos, lo que indica la 
#               variabilidad debido a la diferencia entre las medias de los grupos.
#  Para Residuals: 7008, es la suma de cuadrados dentro de los grupos (o 
#                  residuales), que mide la variabilidad dentro de cada grupo.
#
#  Mean Sq (Media Cuadrática):
#    
#  Para tienda: 1020.7, que se obtiene dividiendo la suma de cuadrados entre 
#              grupos por sus grados de libertad (2041 / 2).
#  Para Residuals: 80.5, calculada dividiendo la suma de cuadrados residuales por 
#                  sus grados de libertad (7008 / 87).
# 
#  F value: 12.67, es el valor F calculado dividiendo la media cuadrática entre 
#           grupos por la media cuadrática dentro de grupos (1020.7 / 80.5). 
#           Un valor F elevado sugiere que las variaciones entre las medias de 
#           los grupos son más significativas que las variaciones dentro de los 
#           grupos.
#  
#  Pr(>F): 1.48e-05, es el p-valor asociado al valor F. Indica la probabilidad 
#          de observar un valor F tan extremo como el obtenido si la hipótesis 
#          nula fuera cierta (es decir, si no hubiera diferencias entre las 
#          medias de los grupos). Un p-valor muy pequeño (en este caso, menor 
#          que 0.05) nos lleva a rechazar la hipótesis nula, sugiriendo que hay 
#          diferencias estadísticamente significativas en los puntajes de 
#          satisfacción entre al menos dos de las tiendas.

# Cuando realizamos un ANOVA de un factor y encontramos un p-valor menor a un 
# umbral de significancia (comúnmente 0.05), concluimos que hay evidencia 
# suficiente para rechazar la hipótesis nula. La hipótesis nula (H0) en el 
# contexto de un ANOVA de un factor afirma que todas las medias de los grupos son 
# iguales, o dicho de otra manera, que no hay diferencias significativas entre ellas.
# 
# Al obtener un p-valor de aproximadamente 0.00052, significativamente menor que 
# 0.05, rechazamos la hipótesis nula y aceptamos la hipótesis alternativa, la cual 
# sugiere que al menos una de las medias de los grupos difiere significativamente 
# de las otras. Sin embargo, el ANOVA de un factor no nos dice cuál de los grupos 
# difiere de los demás, solo que al menos uno de ellos lo hace.
# 
# Para determinar específicamente cuáles grupos tienen diferencias significativas 
# entre sus medias, necesitaríamos realizar un análisis post hoc, como la prueba de 
# Tukey, que compara todas las parejas de grupos para identificar dónde radican las 
# diferencias significativas.


# Análisis post-hoc -------------------------------------------------------

# Una vez que ANOVA nos indica que hay diferencias significativas entre los grupos, 
# el análisis post-hoc se utiliza para identificar específicamente entre qué grupos 
# existen estas diferencias. Una de las pruebas post-hoc más comunes es la prueba 
# de Tukey, también conocida como el procedimiento de comparación honestamente 
# significativa (HSD) de Tukey. Esta prueba compara todas las posibles parejas de 
# medias de grupos y ajusta el p-valor para múltiples comparaciones, manteniendo 
# la tasa de error de Tipo I en el nivel deseado (comúnmente 0.05).

TukeyHSD(resultado_anova)

# Este resumen nos muestra las comparaciones entre cada par de tiendas. Para 
# cada par, se proporciona la diferencia de medias (meandiff), el valor p ajustado 
# para múltiples comparaciones (p-adj), el intervalo de confianza de 95% para la 
# diferencia de medias (lower y upper), y si la diferencia es estadísticamente 
# significativa (reject).
# 
# Basado en estos resultados, podemos concluir lo siguiente:
  
# Existe una diferencia significativa en los puntajes de satisfacción entre la 
# Tienda A y la Tienda B, siendo los puntajes en la Tienda B significativamente 
# más altos.
# La Tienda A y la Tienda C también muestran una diferencia significativa, con 
# puntajes más bajos en la Tienda C.
# Entre la Tienda B y la Tienda C, hay una diferencia significativa, con puntajes 
# notablemente más altos en la Tienda B.
# 
# En resumen, todas las comparaciones muestran diferencias significativas, lo que 
# sugiere que el nivel de satisfacción varía de manera importante entre las tiendas.


# Equivalencia con regresión lineal ---------------------------------------


resultado_lm <- lm(satisfaccion ~ tienda, data = datos)
summary(resultado_lm)

# Los resultados de la regresión lineal en R que has proporcionado pueden 
# interpretarse de manera similar a los resultados de un ANOVA, gracias a la 
# relación estrecha entre estas dos técnicas estadísticas. Aquí te guío a través 
# de la interpretación, enfocándonos en cómo leer los resultados de ANOVA desde 
# la salida de la regresión lineal:

# Coeficientes (Estimates)
#
# (Intercept): La estimación para el intercepto, 69.529, representa el puntaje 
#              medio de satisfacción para la tienda de referencia (en este caso, 
#              Tienda A, asumiendo que Tienda A se omitió en la codificación 
#              dummy debido al orden alfabético). Este valor es significativamente 
#              diferente de 0 (p < 2e-16), lo que era de esperarse.
# 
# tiendaTienda B: El coeficiente para Tienda B, 7.254, indica que la diferencia 
#                 en el puntaje medio de satisfacción entre Tienda B y la tienda 
#                 de referencia (Tienda A) es de aproximadamente 7.254 puntos. 
#                 Este coeficiente es estadísticamente significativo (p = 0.00238), 
#                 lo que sugiere que Tienda B tiene un puntaje de satisfacción 
#                 significativamente más alto en comparación con Tienda A.
# 
# tiendaTienda C: El coeficiente para Tienda C, -4.285, muestra la diferencia en 
#                 el puntaje medio de satisfacción entre Tienda C y Tienda A. 
#                 Aunque la dirección del efecto indica que Tienda C tiene un 
#                 puntaje de satisfacción más bajo, este resultado no es 
#                 estadísticamente significativo al nivel de 0.05 (p = 0.06785), 
#                 lo que sugiere una tendencia pero no una diferencia definitiva.
# 
# Estadísticas del Modelo
# 
# Residual Standard Error (Error estándar residual): 8.975 en 87 grados de libertad. 
#                                                    Esta métrica nos da una idea 
#                                                    de cuánto se desvían las 
#                                                    observaciones alrededor de 
#                                                    la línea de regresión ajustada.
# 
# Multiple R-squared (R cuadrado múltiple): 0.2256. Este valor indica que aproximadamente 
#                                           el 22.56% de la variabilidad en el 
#                                           puntaje de satisfacción puede ser 
#                                           explicada por la diferencia entre tiendas.
# 
# Adjusted R-squared (R cuadrado ajustado): 0.2078. Este valor ajusta el R cuadrado 
#                                           múltiple para el número de predictores 
#                                           en el modelo, proporcionando una medida más 
#                                           precisa de la bondad de ajuste para 
#                                           modelos con un número diferente de 
#                                           variables independientes.
# 
# F-statistic (Estadístico F): 12.67 sobre 2 y 87 grados de libertad. Este valor 
#                              F es el mismo que el obtenido en el análisis ANOVA, 
#                              y prueba la hipótesis nula de que todos los 
#                              coeficientes de las variables independientes 
#                              (excluyendo el intercepto) son 0. En otras palabras, 
#                              prueba si el modelo es globalmente significativo.
# 
# p-value (Valor p del F-estadístico): 1.48e-05. Este es el p-valor asociado al 
#                                      estadístico F, que nos indica la probabilidad 
#                                      de observar un valor tan extremo del 
#                                      estadístico F bajo la hipótesis nula de que 
#                                      el modelo no tiene efecto. Un p-valor tan 
#                                      pequeño nos lleva a rechazar la hipótesis 
#                                      nula, confirmando que hay diferencias 
#                                      significativas en el puntaje medio de 
#                                      satisfacción entre al menos dos de las tiendas.
# 
# En resumen, la regresión lineal confirma las conclusiones obtenidas a través 
# del ANOVA: existe una diferencia estadísticamente significativa en los puntajes 
# de satisfacción entre las tiendas, con Tienda B mostrando un aumento significativo 
# en la satisfacción en comparación con Tienda A, y Tienda C mostrando una tendencia 
# hacia una satisfacción más baja que Tienda A, aunque esta última diferencia no 
# es estadísticamente significativa al nivel del 0.05.
