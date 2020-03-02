library(tidyverse)
library(UsingR)
# library(reshape)

# INTRO -------------------------------------------------------------------


data(galton)
# long <- melt(galton)
long <- gather(galton, variable, value)

g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g + facet_grid(. ~ variable)
g

library(manipulate)

myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + 
    geom_histogram(fill = "salmon", colour = "black", binwidth=1)
  g <- g + geom_vline(xintercept = mu, size = 3)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

g <- ggplot(galton, aes(x = child)) + 
  geom_histogram(fill = "salmon", colour = "black", binwidth=1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

ggplot(galton, aes(x = parent, y = child)) + geom_point()


freqData <- galton %>% 
  group_by(parent, child) %>% 
  summarise(freq = n())

g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

## Regression through the origin
freqData <- galton %>% 
  mutate(child = child - mean(galton$child),
         parent = parent - mean(galton$parent)) %>% 
  group_by(parent, child) %>% 
  summarise(freq = n())

myPlot <- function(beta){
  y <- galton$child
  x <- galton$parent
  
  g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
  g <- g + scale_size(range = c(2, 20), guide = "none" )
  g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
  g <- g + geom_point(aes(colour=freq, size = freq))
  g <- g + scale_colour_gradient(low = "lightblue", high="white")
  g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
  mse <- mean( (y - beta * x) ^2 )
  g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
  g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

solution <- lm(formula = I(child - mean(child)) ~ I(parent - mean(parent)) -
                 1, data = galton)
summary(solution)

g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

g + geom_abline(aes(slope = coefficients(solution)["parent"],
                    intercept = 0),
                size = 2)

# NOTATION ----------------------------------------------------------------


parent_child_stats <- galton %>% 
  summarise(parent_hat = mean(parent),
            child_hat = mean(child),
            parent_s2 = sd(parent)^2,
            child_s2 = sd(child)^2,
            cov_parent_child = cov(parent, child),
            cor_parent_child = cor(parent, child))
parent_child_stats

n <- count(galton) %>% as.numeric

parent_hat <- sum(galton %>% pull(parent)) / n
parent_hat
mean(galton$parent)

child_hat  <- sum(galton %>% pull(child)) / n
child_hat
mean(galton$child)

parent_s2 <- sum((galton$parent - parent_hat)^2) / (n - 1)
parent_s2
sd(galton$parent)^2

child_s2  <- sum((galton$child - child_hat)^2) / (n - 1)
child_s2
sd(galton$child)^2

cov_parent_child <- sum((galton$parent - parent_hat) *
                          (galton$child - child_hat)) / (n - 1)
cov_parent_child
cov(galton)

cor_parent_child <- cov_parent_child / sqrt(parent_s2 * child_s2)
cor_parent_child
cor(galton)

mean(galton$parent - parent_hat)
sd(galton$parent / sqrt(parent_s2))^2

mean((galton$parent - parent_hat) / sqrt(parent_s2))
sd((galton$parent - parent_hat) / sqrt(parent_s2))^2


# ORDINARY LEAST SQUARES --------------------------------------------------

lm_galton <- lm(child ~ parent, galton)
coef(lm_galton)

beta_parent <- cor_parent_child * sqrt(child_s2 / parent_s2)
beta_parent
beta_0 <- child_hat - beta_parent * parent_hat
beta_0


lm_galton_c <- lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, 
                  galton)
coef(lm_galton_c)

beta_parent_c <- cor(galton$child, galton$parent) *
  sd(galton$child) / sd(galton$parent)
beta_parent_c

lm_galton_n <- lm(I((child - mean(child))/sd(child)) ~ 
                    I((parent - mean(parent)) / sd(parent)) - 1, 
                  galton)
coef(lm_galton_n)

beta_parent_c <- cor(galton$child, galton$parent) 
beta_parent_c

cor(galton$parent, galton$child)


# Regression to the mean --------------------------------------------------

y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)

rho <- cor(x, y)

g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g

# In paired measurements, if there’s randomness then the extreme values of 
# one element of the pair will be likely less extreme in the other element.
y_hat <- rho * x
x_hat <- rho * y


# Statistical linear regression models ------------------------------------

# Up to this point, we’ve only considered estimation. Estimation is useful, 
# but we also need to know how to extend our estimates to a population. This is
# the process of statistical inference. Our approach to statistical inference 
# will be through a statistical model. At the bare minimum, we need a few
# distributional assumptions on the errors. However, we’ll focus on full model 
# assumptions under Gaussianity.

# MODEL ADDITIVE COMPONENTS
#   - Systematic component: a straight line beta_0 + beta_1 * X_i
#   - Independent and identically distributed (iid) Gausiian errors
#     epsilon_i ~ N(0, sigma^2)
#
#                      Y_i = beta_0 + beta_1 * X_i + epsilon_i
# Under this model:
#                   E[Y_i | X_i = x_i] = mu_i = beta_0 + beta_1 * x_i 
# and:
#                 Var[Y_i | X_i = x_i] = sigma^2
#
# With this model specification (linear with gaussian additive errors), we can
# hypothesize and discuss the nature of the errors. In fact, we’ll even cover 
# ways to estimate them to investigate our model assumption.

# Let’s analyze the diamond data set from the UsingR package. The data is 
# diamond prices (in Singapore dollars) and diamond weight in carats. Carats 
# are a standard measure of diamond mass, 0.2 grams.
data(diamond)

head(diamond)

g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

diamond_fit <- lm(formula = price ~ carat, data = diamond)
coef(diamond_fit)

# Let's fit the model with a more interpretable intercept by centering our X 
# variable.
diamond_fit_2 <- lm(formula = price ~ I(carat - mean(carat)), data = diamond)
coef(diamond_fit_2)

# Thus the new intercept, 500.1, is the expected price for the average sized 
# diamond of the data (0.2042 carats). Notice the estimated slope didn’t change 
# at all.

# Now let’s try changing the scale. This is useful since a one carat increase 
# in a diamond is pretty big. What about changing units to 1/10th of a carat? 
# We can just do this by just dividing the coefficient by 10, no need to refit 
# the model.
# Thus, we expect a 372.102 (SIN) dollar change in price for every 1/10th of a 
# carat increase in mass of diamond.
diamond_fit_3 <- lm(formula = price ~ I(10*(carat - mean(carat))), 
                    data = diamond)
coef(diamond_fit_3)


# Residuals ---------------------------------------------------------------

# Residuals represent variation left unexplained by our model. We emphasize the 
# difference between residuals and errors. The errors unobservable true errors 
# from the known coefficients, while residuals are the observable errors from 
# the estimated coefficients. In a sense, the residuals are estimates of
# the errors.
# 
#                  Y_i = beta_0 + beta_1 * X_i + epsilon_i
#                  Y_hat_i = beta_hat_0 + beta_hat_1 * X_i 
#
#                  e_i = Y_i - Y_hat_i
#
# Least squares minimizes the sum of the squared residuals. 
# Note that the ei are observable, while the errors, ϵi are not. 
# The residuals can be thought of as estimates of the errors.

# Properties of the residuals
#
# 1 - E[e_i] = 0
#   
#     - If just an intercept is included, sum_i(e_i) = 0
#       This tells us that the residuals are not independent. If we know n-1,
#       then we know the n-th.
#     - In fact we will only have n - p free residuals, where p is the number 
#       of coefficients in our model.
#     - If a regressor variable X is included in the model, then 
#       sum_i(e_i * X_i) = 0
#
# (Check this here: https://math.stackexchange.com/questions/494181/why-the-sum-of-residuals-equals-0-when-we-do-a-sample-regression-by-ols)
# 
# Uses of residuals:
# 
# - For investigating poor model fit. Residual plots highlight poor model fit.
# - To create covariate adjusted variables. Specifically, residuals can be
#   thought of as the outcome (Y) with the linear association of the predictor 
#   (X) removed. So, for example, if you wanted to create a weight variable 
#   with the linear effect of height removed, you would fit a linear regression 
#   with weight as the outcome and height as the predictor and take the
#   residuals. (Note this only works if the relationship is linear)
#
# Finally, we should note the different sorts of variation one encounters in 
# regression:
#            - Residual variation (variation after removing the predictor)
#            - Systematic variation (variation explained by the regression model)
#            - Total variation (the ttal variation of our response)  


y <- diamond$price
x <- diamond$carat
n <- length(y)

fit <- lm(y ~ x)

## The easiest way to get the residuals
e <- resid(fit)

## Obtain the residuals manually, get the predicted Ys first
yhat <- predict(fit)

## The residuals are y - yhat. Let's check by comparing this
## with R's build in resid function
max(abs(e -(y - yhat)))

## Let's do it again hard coding the calculation of Yhat
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

# Residuals versus X
# A useful plot is the residuals versus the X values. This allows us to zoom 
# in on instances of poor model fit. Whenever we look at a residual plot, we 
# are searching for systematic patterns of any sort.
plot(x, e, type = "h")
abline(h = 0)
lines(x, e, type = "p")

## Non-linear data
x = runif(100, -3, 3); y = x + sin(x) + rnorm(100, sd = .2); 
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

## Residual plot
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2); 
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g

# Another common feature where our model fails is when the variance around 
# the regression line is not constant. Remember our errors are assumed to be 
# Gaussian with a constant error. Here’s an example where heteroskedasticity 
# is not at all apparent in the scatterplot.

## Heteroskedasticity
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

## Getting rid of the blank space can be helpful
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))), 
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2); 
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g

# If we look at the residual plot for the diamond data, things don’t look 
# so bad.
diamond$e <- resid(lm(price ~ carat, data = diamond))
g = ggplot(diamond, aes(x = carat, y = e))
g = g + xlab("Mass (carats)")
g = g + ylab("Residual price (SIN $)")
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g


# INTERLUDE - RESIDUALS GENERATION MATRIX ---------------------------------

y <- diamond$price
x <- diamond$carat
n <- length(y)

fit <- lm(y ~ x)

# Y = X %*% beta
# t(X) %*% Y = t(X) %*% X %*% beta
# (t(X) %*% X)^(-1) %*% t(x) %*% Y = beta

calc_betas <- function(X, y) {
  # browser()
  X_aux <- cbind(matrix(1, ncol = 1, nrow = nrow(X)),
                     X)
  betas <- solve(t(X_aux) %*% X_aux) %*% t(X_aux) %*% y
  
  return(betas)
}

calc_betas(X = matrix(x, ncol = 1), y = y)
coef(fit)

# https://en.wikipedia.org/wiki/Projection_matrix
# https://en.wikipedia.org/wiki/Leverage_(statistics)
# https://stats.stackexchange.com/questions/208242/hat-matrix-and-leverages-in-classical-multiple-regression

#
# E = Y - Y_hat = Y - X %*% beta = Y - X %*% (t(X) %*% X)^(-1) %*% t(x) %*% Y =
#                 (I - X %*% (t(X) %*% X)^(-1) %*% t(x)) %*% Y =
#                 RGM %*% Y
#
#                 RGM %*% X = 0

calc_RGM <- function(X, y) {
  # browser()
  X_aux <- cbind(matrix(1, ncol = 1, nrow = nrow(X)),
                 X)
  identity_M <- diag(nrow(X_aux))
  
  RGM <- (identity_M - X_aux %*% solve(t(X_aux) %*% X_aux) %*% t(X_aux)) 
  
  return(RGM)
}

RGM <- calc_RGM(X = matrix(x, ncol = 1), y = y)

RGM %*% y
residuals(fit)

# RGM %*% X = 0
X_aux <- cbind(matrix(1, ncol = 1, nrow = length(x)),
               matrix(x, ncol = 1))
RGM %*% X_aux

RGM %*% X_aux %>% abs %>% max

# H = I - RGM = hatvalues
H = 1 - RGM
diag(H)
hatvalues(fit)


# LM in MATRIX FORM -------------------------------------------------------

# https://en.wikipedia.org/wiki/Projection_matrix
# https://en.wikipedia.org/wiki/Leverage_(statistics)
# https://stats.stackexchange.com/questions/208242/hat-matrix-and-leverages-in-classical-multiple-regression

y <- diamond$price
x <- diamond$carat
n <- length(y)

fit <- lm(y ~ x)

## DESIGN MATRIX
X <- model.matrix(y ~ x)
Y <- matrix(y, nrow = length(y), ncol = 1)

N <- nrow(X)
K <- ncol(X) - 1

##
M_est_betas <- solve(t(X) %*% X) %*% t(X)

est_betas <- M_est_betas %*% Y

Y_hat <- X %*% est_betas

all.equal(as.numeric(Y_hat), as.numeric(fitted(fit)))

## PROJECTION MATRIX = HAT MATRIX
P <- X %*% M_est_betas

Y_hat <- P %*% Y

all.equal(as.numeric(Y_hat), as.numeric(fitted(fit)))

# RESIDUAL MAKER MATRIX
M <- diag(1, nrow(P)) - P

# Residuals
e <- M %*% Y

all.equal(as.numeric(e), as.numeric(residuals(fit)))

## LEVERAGE MATRIX = PROJECTION MATRIX
H <- P

# Hat values
hat_values <- diag(H)

all.equal(as.numeric(hat_values), as.numeric(hatvalues(fit)))

# Standardised residuals
sd_e <- sqrt(sum(residuals(fit)^2) / (N - (K+1)))
std_resid <- e / sd_e / sqrt(1 - hat_values)

all.equal(as.numeric(std_resid), as.numeric(rstandard(fit)))

## Studentized residuals

est_sigma_i <- sd_e * sqrt((N - K - 1 - std_resid) / (N - K - 2))

student_resid <- e / est_sigma_i / sqrt(1 - hat_values)

all.equal(as.numeric(student_resid), as.numeric(rstudent(fit)))

##  COOK'S DISTANCE
cooks_dist <- student_resid^2 / (K + 1) * hat_values / (1 - hat_values)

all.equal(as.numeric(cooks_dist), as.numeric(cooks.distance(fit)))

## END OF INTERLUDE @@

## Estimating residual variation

# It seems natural to use our residual variation to estimate population 
# error variation
y <- diamond$price
x <- diamond$carat

N <- nrow(diamond)
K <- 1
resid_d_f <- N - (K+1)

sd_e <- sqrt(sum(residuals(fit)^2) / (resid_d_f))

identical(sd_e, summary(fit)$sigma)

## Summarizing variation

# Total Variability
total_var <- sum((y - mean(y))^2)

# Regression variability
regression_var <- sum((fitted(fit) - mean(y))^2)

# Residuals variability
residuals_var <- sum((y - fitted(fit))^2)

identical(regression_var + residuals_var, total_var)

# Consider our diamond example again. The plot below shows the variation 
# explained by a model with an intercept only (representing total variation) 
# and then the mass is included as a linear predictor.
# Notice how much the variation decreases when including the diamond mass.

## Diamond data residual plot
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g

# R squared
# R squared is the percentage of the total variability that is explained by 
# the linear relationship with the predictor
r2 <- regression_var / total_var
identical(r2, summary(fit)$r.squared)

# Anscombe’s residuals (named after their inventor) are a famous example of 
# how R squared doesn’t tell the whole story about model fit. In this example,
# four data sets have equivalent R squared values and beta values, but 
# dramatically different model fits. The result is to suggest that reducing 
# data to a single number, be it R squared, a test statistic or a P-value, 
# often masks important aspects of the data.
data(anscombe)
example(anscombe)


# MULTIVARIABLE REGRESSION ANALYSIS ---------------------------------------

# The least squares estimate for the coefficient of a multivariate regression 
# model is exactly regression through the origin with the linear relationships 
# with the other regressors removed from both the regressor and outcome by 
# taking residuals. In this sense, multivariate regression “adjusts” a
# coefficient for the linear impact of the other variables.
n = 100
x = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)

## Generate the data
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)

## Get the residuals having removed X2 and X3 from X1 and Y
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))

## Fit regression through the origin with the residuals
sum(ey * ex) / sum(ex ^ 2)

## Double check with lm
coef(lm(ey ~ ex - 1))

## Fit the full linear model to show that it agrees
coef(lm(y ~ x + x2 + x3))


# MULTIVARIABLE EXAMPLES AND TRICKS ---------------------------------------

require(datasets)
data(swiss)
?swiss

pairs.panels(swiss)

swiss_lm <- lm(Fertility ~ . , data = swiss)
summary(swiss_lm)
