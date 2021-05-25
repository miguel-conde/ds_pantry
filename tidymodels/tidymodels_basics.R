library(tidyverse)
library(tidymodels)


# Load the data -----------------------------------------------------------


library(modeldata) # This is also loaded by the tidymodels package
data(ames)

# or, in one line:
data(ames, package = "modeldata")

dim(ames)

# Exploring the data ------------------------------------------------------

library(tidymodels)

# For convenience, tidymodels contains a function that captures most of the 
# common naming conflicts that we might encounter
tidymodels_prefer(quiet = FALSE)

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50)

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50) +
  scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


# Splitting the data ------------------------------------------------------

## SIMPLE RANDOM SAMPLING

# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(123)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80) # From rsample package
ames_split

# The object ames_split is an rsplit object and only contains the partitioning 
# information; to get the resulting data sets, we apply two more functions:

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

## STRATIFIED SAMPLING
set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

# The rsample package contains a function called initial_time_split() that is 
# very similar to initial_split(). Instead of using random sampling, the prop 
# argument denotes what proportion of the first part of the data should be used 
# as the training set; the function assumes that the data have been pre-sorted 
# in an appropriate order.


# Feature engineering with recipes ----------------------------------------

## Write RECIPE - define the preprocessing
simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())

simple_ames

# Other selectors specific to the recipes package are: all_numeric_predictors(),
# all_numeric(), all_predictors(), and all_outcomes(). As with dplyr, one or 
# more unquoted expressions, separated by commas, can be used to select which 
# columns are affected by each step.

# PREP recipe - estimate (from the training set) any quantities required by the
# steps
simple_ames <- prep(simple_ames, training = ames_train)
simple_ames

# One important argument to prep() is retain. When retain = TRUE (the default), 
# the prepared version of the training set is kept within the recipe. This data 
# set has been pre-processed using all of the steps listed in the recipe. Since 
# prep() has to execute the recipe as it proceeds, it may be advantageous to 
# keep this version of the training set so that, if that data set is to be used 
# later, redundant calculations can be avoided. However, if the training set is 
# big, it may be problematic to keep such a large amount of data in memory. Use 
# retain = FALSE to avoid this.

## BAKE the recipe - apply the preprocessing operations to a data set 
test_ex <- bake(simple_ames, new_data = ames_test)
names(test_ex) %>% head()

# Trick:
bake(simple_ames, ames_test, starts_with("Neighborhood_"))

# To get the processed version of the training set,:
bake(simple_ames, new_data = NULL) %>% nrow()

ames_train %>% nrow()

# Example
complex_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20) %>% 
  prep(training = ames_train) %>% 
  bake(ames_test, starts_with("Neighborhood_"))
