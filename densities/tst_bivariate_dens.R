library(tidyverse)
library(ggside)
library(tidyquant)

# DATA --------------------------------------------------------------------

N <- 1000

set.seed(2022)

beta_0 <- 1
beta_1 <- 2.3
x <-  runif(N, -10, 10)
mu <- x * beta_1 + beta_0
sigma <- 1.85
y <- rnorm(N, mu, sigma)

dataset <- tibble(x = x, mu = mu, y = y) %>% arrange(x)

plot(dataset$x, dataset$y)

lm_fit <- lm(y ~ x, dataset)
summary(lm_fit)

psych::pairs.panels(dataset)

plot(density(x))
plot(density(y))

dataset %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se = TRUE) +
  geom_xsidedensity(
    aes(y = after_stat(density)),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(x = after_stat(density)),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) +
  scale_color_tq() + 
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Fuel Economy by Vehicle Type",
       subtitle = "ggside density",
       x = "Highwaay MPG", y = "City MPG") +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)

# kdevine -----------------------------------------------------------------


library(kdevine)

# estimate density (use xmin to indicate positive support)
fit <- kdevine(dataset[, c("x","y")])
# evaluate density estimate
dkdevine(c(0, 0.1), fit)
# plot simulated data
pairs(rkdevine(nrow(dataset), fit))

contour(fit$vine)

old_par = par(mfrow = c(1,2))
fit$marg.dens[[1]] %>% plot()
plot(density(x))
fit$marg.dens[[2]] %>% plot()
plot(density(y))
par(mfrow = c(1,1))

fit$vine$T1[[1]]$c %>% plot()
# fit$vine$T1[[2]]$c %>% plot()

###
library(fields)
# library(MASS)
set.seed(144)
x <- rnorm(1000)
y <- 5*x + rnorm(1000)
k <- MASS::kde2d(x, y)


points <- data.frame(x=0:2, y=c(0, 5, 5))
interp.surface(k, points)

image(k)
persp(k, phi = 30, theta = 20, d = 5)
contour(k)


### 
library(ks)
fhat <- kde(x=iris[,2:3])
plot(fhat, display="filled.contour", cont=seq(10,90,by=10), lwd=1, alpha=0.5)
plot(fhat, display="persp", border=1, alpha=0.5)

p <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Length)) + 
  geom_point() +
  xlim(0, 5) +
  ylim(0, 8)

p + geom_density_2d()
p + geom_density2d_filled(alpha = 0.5)
p + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.25, colour = "black")


p + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.25, colour = "black") + facet_wrap(vars(Species))

# 
library(ks)
probe <- tibble(x,y)
fhat <- kde(x=probe)
plot(fhat, display="filled.contour", cont=seq(10,90,by=10), lwd=1, alpha=0.5)
plot(fhat, display="persp", border=1, alpha=0.5)


points <- data.frame(x=0:2, y=c(0, 5, 5))
obj_2_int <- list(x = fhat$eval.points[[1]],
                  y = fhat$eval.points[[2]],
                  z = fhat$estimate)
interp.surface(obj_2_int, points)


p <- ggplot(probe, aes(x = x, y = y)) + 
  geom_point() +
  xlim(-7, 7) +
  ylim(-17, 17)

p + geom_density_2d()
p + geom_density2d_filled(alpha = 0.5)
p + geom_density_2d_filled(alpha = 0.5) +
  geom_density_2d(size = 0.25, colour = "black")


par(mfrow = c(1,2))

persp(k, phi = 30, theta = 20, d = 5)
plot(fhat, display="persp", border=1, alpha=0.5)

contour(k)
plot(fhat, display="filled.contour", cont=seq(10,90,by=10), lwd=1, alpha=0.5)
par(mfrow = c(1,1))



# INTENTO SERIO -----------------------------------------------------------


# Dsitribuciones marginales -----------------------------------------------

p_x <- density(dataset$x)
plot(p_x)

p_y <- density(dataset$y)
plot(p_y)

p_marg_x <- tibble(x = p_x$x, p_x = p_x$y)
p_marg_y <- tibble(y = p_y$x, p_y = p_y$y)


# Distribución conjunta ---------------------------------------------------


library(fields)
# library(MASS)

k <- MASS::kde2d(dataset$x, dataset$y)

image(k)
persp(k, phi = 30, theta = 20, d = 5)
contour(k)

points <- data.frame(x=p_x$x, y=p_y$x) %>% expand_grid() %>% as.data.frame()
p_conj_x_y <- points %>% 
  mutate(p_x_y = interp.surface(k, points)) %>% 
  as_tibble()

# Distribuciones condicionales --------------------------------------------


p_y_cond_x <- p_x_y / p_x$y
