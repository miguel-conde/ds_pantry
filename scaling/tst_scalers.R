# https://www.analyticsvidhya.com/blog/2020/04/feature-scaling-machine-learning-normalization-standardization/
# https://www.atoti.io/articles/when-to-perform-a-feature-scaling/
# https://pianalytix.com/scaling-techniques-for-ml-data/
# https://www.analyticsvidhya.com/blog/2021/05/feature-scaling-techniques-in-python-a-complete-guide/
# 
#

# max-min vs. standardization is an eternal question among machine learning 
# newcomers. Let me elaborate on the answer in this section.
# 
# max-min is good to use when you know that the distribution of your data does not 
# follow a Gaussian distribution. This can be useful in algorithms that do not 
# assume any distribution of the data like K-Nearest Neighbors and Neural Networks.
#
# Standardization, on the other hand, can be helpful in cases where the data 
# follows a Gaussian distribution. However, this does not have to be necessarily 
# true. Also, unlike normalization, standardization does not have a bounding 
# range. So, even if you have outliers in your data, they will not be affected 
# by standardization.
#
# However, at the end of the day, the choice of using normalization or 
# standardization will depend on your problem and the machine learning algorithm 
# you are using. There is no hard and fast rule to tell you when to normalize or 
# standardize your data. You can always start by fitting your model to raw, 
# normalized and standardized data and compare the performance for best results.

# LIBRARIES & SOURCES -----------------------------------------------------


library(tidyverse)
library(ggside)
library(tidyquant)

# When predictor variables have very large values in them, there are sometimes 
# numerical glitches. Even well-known statistical software can suffer from these 
# glitches, leading to mistaken estimates. These problems are very com- mon for 
# polynomial regression, because the square or cube of a large number can be 
# truly massive. Standardizing largely resolves this issue. It should be your 
# default behavior.


# SCALERS  ----------------------------------------------------------------


# Absolute Maximum Scaling ------------------------------------------------

abs_max <- function(x, ...) {
  
  # Changing x range to: -1 - 1
  # Sensible to ouliers
  
  x_scaled <- x / max(abs(x), ...)
  
  return(x_scaled)
}

# Min-Max Scaling ---------------------------------------------------------

min_max <- function(x, a = 0, b = 1, ...) {
  
  # Changing x range to: 0 - 1
  # Sensible to outliers
  
  # Is Mostly Used When We Know That The Data Does Not Follow A Gaussian Distribution.
  
  # is good to use when the distribution of data does not follow a Gaussian distribution. 
  # It can be useful in algorithms that do not assume any distribution of the data 
  # like K-Nearest Neighbors.
  # In Neural Networks algorithm that require data on a 0–1 scale, normalization 
  # is an essential pre-processing step. Another popular example of data 
  # normalization is image processing, where pixel intensities have to be 
  # normalized to fit within a certain range (i.e., 0 to 255 for the RGB color range).
  
  x_scaled <- a + (x - min(x, ...)) / (max(x, ...) - min(x, ...)) * (b - a)
  
  return(x_scaled)
}


# Normalization -----------------------------------------------------------

normalization <- function(x, FUN = mean, ...) {
  
  # Changing the shape of the distribution of x
  # Sensible to outliers
  
  x_scaled <- (x - FUN(x, ...)) / (max(x, ...) - min(x, ...))
  
  return(x_scaled)
}


# Standardization ---------------------------------------------------------

standarization <- function(x, ...) {
  
  # This will make sure that all the features are centred around the mean value 
  # with a standard deviation value of 1. This is the best to use if your feature 
  # is normally distributed like salary or age.
  # 
  # Standardization Can Be In Use When The Data Follows Gaussian Distribution. 
  # It Is More Robust For Handling Outliers As It Does Not Rescale Attributes 
  # In A Fixed Range. Thus, When The Dataset Contains Outliers Values, 
  # Standardization Is Prefer Over Min-Max Normalization. Also, It Facilitates 
  # The Convergence Of Computational Algorithms Like Gradient Descent.
  #
  # It can be helpful in cases where the data follows a Gaussian distribution. 
  # Though this does not have to be necessarily true. Since standardization does 
  # not have a bounding range, so, even if there are outliers in the data, they 
  # will not be affected by standardization.
  # In clustering analyses, standardization comes in handy to compare similarities 
  # between features based on certain distance measures. Another prominent example 
  # is the Principal Component Analysis, where we usually prefer standardization 
  # over Min-Max scaling since we are interested in the components that maximize 
  # the variance.
  
  x_scaled <- (x - mean(x, ...)) / sd(x, ...)
  
  return(x_scaled)
}


# Robust Scaling ----------------------------------------------------------

robust <- function(x, ...) {
  
  # This method centres the median value at zero and this method is robust to outliers.
  # This Method Is Of Most Use When Data Contains Outliers And Is Prefer Over 
  # Standardization And Normalization As It Produces More Robust Estimates For 
  # The Center And Value Range Of The Variable.
  
  IQR1 <- IQR(x)
  
  x_scaled <- (x - median(x, ...)) / IQR1
  
  return(x_scaled)
}


# LX_norm -----------------------------------------------------------------

norma_lx <- function(x, p = 2) {
  sum(abs(x)^p)^(1/p)
}

lx_norm <- function(dataset, ..., p = 2) {
  # browser()
  out <- dataset %>% 
    select(...) %>% 
    # mutate_at(vars(...), ~ .^2) %>% 
    # mutate(norma = sqrt(rowSums(select(., ...)))) %>% 
    mutate(norma = (rowSums(abs(select(., ...))^p))^(1/p)) %>% 
    # mutate_at(vars(...), ~ sqrt(.) / norma) %>% 
    mutate_at(vars(...), ~ . / norma) %>% 
    select(-norma)
  
  return(out)
}


# 2D DENSITY PLOT FUNCTION ------------------------------------------------

my_2d_dens_plot <- function(dataset, x, y, 
                            title = "", 
                            x_label = quo_name(x), y_label = quo_name(y), 
                            fix_axis = FALSE,
                            use_density = TRUE) {
  x <- enquo(x)
  y <- enquo(y)
  
  p <- dataset %>% 
    ggplot(aes(x = !!x, y = !!y)) 
  
  if(use_density == TRUE) {
    p <- p +
      geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
      geom_density_2d(n = 200, size = 0.25, colour = "black", show.legend = FALSE) 
  } else {
    p <- p +
      geom_point(size = 2, alpha = 0.3) +
      geom_smooth(aes(color = NULL), se = TRUE) 
  }
  
  if(fix_axis == TRUE) {
    # browser()
    x_min <- min(dataset %>% select(!!x, !!y), na.rm = TRUE)
    x_max <- max(dataset %>% select(!!x, !!y), na.rm = TRUE)
    y_min <- x_min
    y_max <- x_max
    
    p <- p +
      scale_x_continuous(limits = c(x_min, x_max)) +
      scale_y_continuous(limits = c(y_min, y_max))
  }
  
  p <- p +
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
    labs(title = title, x = x_label, y = y_label)
  
  return(p)
}

my_2d_dens_plot <- function(dataset, x, y, show_points = TRUE, 
                            title = "", 
                            x_label = quo_name(x), y_label = quo_name(y)) {
  
  x <- enquo(x)
  y <- enquo(y)
  
  p <- dataset %>% 
    ggplot(aes(x = !!x, y = !!y))  +
    geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
    geom_density_2d(n = 200, size = 0.25, colour = "black", show.legend = FALSE)
  
  if (show_points == TRUE) {
    p <- p +
      geom_point(size = 2, alpha = .1) +
      geom_smooth(aes(color = NULL), se = TRUE)
  }
  
  p <- p +
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
    labs(title = title, x = x_label, y = y_label) 
  
  return(p)
}

# MOCK DATA ---------------------------------------------------------------


N <- 1000

set.seed(2022)

# A skwed sample
skew_sample <- rexp(N, .000001)
skew_sample <- rgamma(N, .5, rate = 1/2000)

# A normal sample
norm_sample <- rnorm(N, 1000, 1)

dataset <- tibble(norm_samp = norm_sample, skew_samp = skew_sample)



# TEST --------------------------------------------------------------------

dataset %>% 
  mutate(x = norm_samp, y = skew_samp) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "No Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  mutate(x = abs_max(norm_samp), y = abs_max(skew_samp)) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "Absolute Maximum Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  mutate(x = min_max(norm_samp), y = min_max(skew_samp)) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "Min - Max Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  mutate(x = normalization(norm_samp), y = normalization(skew_samp)) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "Normalization Scaling, using mean", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  mutate(x = normalization(norm_samp, FUN = median), 
         y = normalization(skew_samp, FUN = median)) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "Normalization Scaling, using median", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  mutate(x = standarization(norm_samp), y = standarization(skew_samp)) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "Standarization (z-scores) Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  mutate(x = robust(norm_samp), y = robust(skew_samp)) %>% 
  my_2d_dens_plot(x, y, 
                  title   = "Robust Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  lx_norm(norm_samp, skew_samp) %>% 
  my_2d_dens_plot(norm_samp, skew_samp, 
                  title   = "L2 Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")

dataset %>% 
  lx_norm(norm_samp, skew_samp, p = 1) %>% 
  my_2d_dens_plot(norm_samp, skew_samp, 
                  title   = "L1 Scaling", 
                  x_label = "Normal Distribution",
                  y_label = "Skewed Distribution")


# SCALERS FOR SCORING -----------------------------------------------------

# MOCK DATA ---------------------------------------------------------------


set.seed(2022)

N_days <- 100
N_results_day <- rpois(100, lambda = 10)


sapply(1:N_days,
       function(d) {
         data.frame(mentions = rpois(N_results_day[d], lambda = 5)) %>% 
           mutate(day = d, .before = 1)
       }) %>% head() 

l_res <- vector(mode = "list", length = N_days)
for (d in 1:N_days) {
  
  l_res[[d]] <- tibble(mentions = rpois(N_results_day[d], lambda = 1)) %>% 
    mutate(day = d, .before = 1)
}

res <- bind_rows(l_res) %>% 
  mutate(id = 1:nrow(.), .before = 1)


# TF-IDF  -----------------------------------------------------------------

tf_idf <- tibble()
for (w in 1:N_days) {
  aux <- res %>% 
    filter(day <= w) %>% 
    mutate(window = paste0("1_", w),
           n = n(),
           with_mentions = sum(mentions > 0),
           tf = mentions,
           idf = log(n / (1 + with_mentions)),
           tf_idf = tf * idf,
           max_min = (mentions - min(mentions)) / ((max(mentions) - min(mentions))),
           log_max_min = log(1+max_min),
           z_score = (mentions - mean(mentions)) / sd(mentions),
           max_min_log = (log(mentions+1) - min(log(mentions+1))) / (max(log(mentions+1)) - min(log(mentions+1)))) 
  
  tf_idf <- tf_idf %>% bind_rows(aux %>% filter(day == w))
}

tf_idf %>% arrange(desc(tf_idf))
tf_idf$tf_idf %>% summary()
tf_idf$tf_idf %>% hist()

tf_idf %>% select(tf_idf:max_min_log) %>% cor()

ggplot(tf_idf, aes(x = mentions, y = log_max_min)) + 
  geom_point() + 
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))

tf_idf %>% 
  group_by(mentions) %>% 
  summarise(avg_score = mean(log_max_min)) %>% 
  pull(avg_score) %>% diff()

## 
ggplot(tf_idf, aes(x = mentions, y = max_min_log)) + 
  geom_point() + 
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))

tf_idf %>% 
  group_by(mentions) %>% 
  summarise(avg_score = mean(max_min_log)) %>% 
  pull(avg_score) %>% diff()

