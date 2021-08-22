library(tidyverse)
library(dagitty)

# Cerrar la puerta de atrás: bloquear los caminos confusores entre un predictor X
# y una respuesta Y

N <- 1000

# 1 - Fork ----------------------------------------------------------------

# El clásico confusor. Z es la causa común a X e Y, generando una correlación
# entre ellas. 
dag_fork <- dagitty("dag {X <- Z -> Y}")

Z <- rnorm(N)
X <-  3 * Z + rnorm(N, 0, 0.1)
Y <- -2 * Z + rnorm(N, 0.01)

df <- tibble(X, Y, Z)

cor(df)

# Si condicionamos en Z, añadir X no aporta nada. X e Y son independientes, 
# condicionalmente en Z. Hay una asociación entre X e Y inducida por Z, pero
# no una relación causal de X sobre Y (intervenir en X no cambiará Y)

coordinates(dag_fork) <- list(x=c(X = 0, Z = 1, Y = 2),
                              y=c(X = 1, Z = 0, Y = 1))
drawdag(dag_fork)

Z_lm <- lm(Y ~ Z, df)

summary(Z_lm)

Z_y_X_lm <- lm(Y ~ X + Z, df)

summary(Z_y_X_lm)

cor(predict(Z_lm, df), predict(Z_y_X_lm, df))
plot(predict(Z_lm, df), predict(Z_y_X_lm, df))

# Para cerrar un FORK, no condicionamos (no incluimos) Z

adjustmentSets(dag_fork ,exposure = "X", outcome = "Y")
impliedConditionalIndependencies(dag_fork)

X_lm <- lm(Y ~ X, df)

summary(X_lm)

cor(predict(X_lm, df), predict(Z_lm, df))
plot(predict(X_lm, df), predict(Z_y_X_lm, df))
cor(predict(X_lm, df), predict(Z_lm, df))
plot(predict(X_lm, df), predict(Z_y_X_lm, df))

