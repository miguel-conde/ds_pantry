# https://dalex.drwhy.ai/
# https://ema.drwhy.ai/
# https://github.com/ModelOriented/DrWhy
# https://modeloriented.github.io/DALEX/articles/vignette_titanic.html


library(tidyverse)
library("DALEX")
library("ranger")


head(titanic_imputed)

# Random Forest -----------------------------------------------------------


# prepare model
model_titanic_rf <- ranger(survived ~ gender + age + class + embarked +
                             fare + sibsp + parch,  data = titanic_imputed,
                           classification = TRUE)
model_titanic_rf

# Explainer
explain_titanic_rf <- explain(model_titanic_rf, 
                              data = titanic_imputed,
                              y = titanic_imputed$survived, 
                              label = "Random Forest v7",
                              colorize = FALSE)

# Variable importance plots
vi_rf <- model_parts(explain_titanic_rf)
head(vi_rf)
plot(vi_rf)

# Variable effects
vr_age  <- model_profile(explain_titanic_rf, variables =  "age")
head(vr_age)
plot(vr_age)

vr_class  <- model_profile(explain_titanic_rf, variables =  "class")
plot(vr_class)

vr_fare  <- variable_profile(explain_titanic_rf, variables =  "fare")
plot(vr_fare)

vr_embarked  <- model_profile(explain_titanic_rf, variables =  "embarked")
plot(vr_embarked)

# Instance level explanations
new_passanger <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew", "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8,
  sibsp = 0,
  parch = 0,
  fare = 72,
  embarked = factor("Southampton", levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton"))
)

sp_rf <- predict_parts(explain_titanic_rf, new_passanger)
plot(sp_rf)


# More models -------------------------------------------------------------

# Logistic Regression
library("rms")
model_titanic_lmr <- lrm(survived ~ class + gender + rcs(age) + sibsp +
                           parch + fare + embarked, titanic_imputed)
explain_titanic_lmr <- explain(model_titanic_lmr, data = titanic_imputed, 
                               y = titanic_imputed$survived, 
                               predict_function = function(m,x) 
                                 predict(m, x, type = "fitted"),
                               label = "Logistic regression")

# GBM
library("gbm")
model_titanic_gbm <- gbm(survived  ~ class + gender + age + sibsp +
                           parch + fare + embarked, data = titanic_imputed, 
                         n.trees = 15000)
explain_titanic_gbm <- explain(model_titanic_gbm, data = titanic_imputed, 
                               y = titanic_imputed$survived, 
                               predict_function = function(m,x) 
                                 predict(m, x, n.trees = 15000, type = "response"),
                               label = "Generalized Boosted Models",
                               colorize = FALSE)
# SVM
library("e1071")
model_titanic_svm <- svm(survived ~ class + gender + age + sibsp +
                           parch + fare + embarked, data = titanic_imputed, 
                         type = "C-classification", probability = TRUE)
explain_titanic_svm <- explain(model_titanic_svm, data = titanic_imputed, 
                               y = titanic_imputed$survived, 
                               label = "Support Vector Machines",
                               colorize = FALSE)

# kNN
library("caret")
model_titanic_knn <- knn3(survived ~ class + gender + age + sibsp +
                            parch + fare + embarked, data = titanic_imputed, k = 5)
explain_titanic_knn <- explain(model_titanic_knn, data = titanic_imputed, 
                               y = titanic_imputed$survived, 
                               predict_function = function(m,x) predict(m, x)[,2],
                               label = "k-Nearest Neighbors",
                               colorize = FALSE)

# Variable performance
vi_rf <- model_parts(explain_titanic_rf)
vi_lmr <- model_parts(explain_titanic_lmr)
vi_gbm <- model_parts(explain_titanic_gbm)
vi_svm <- model_parts(explain_titanic_svm)
vi_knn <- model_parts(explain_titanic_knn)

plot(vi_rf, vi_lmr, vi_gbm, vi_svm, vi_knn, bar_width = 4)

# Single variable
vr_age_rf   <- model_profile(explain_titanic_rf, variables = "age")
vr_age_lmr  <- model_profile(explain_titanic_lmr, variables = "age")
vr_age_gbm  <- model_profile(explain_titanic_gbm, variables = "age")
vr_age_svm  <- model_profile(explain_titanic_svm, variables = "age")
vr_age_knn  <- model_profile(explain_titanic_knn, variables = "age")
plot(vr_age_rf$agr_profiles, 
     vr_age_lmr$agr_profiles, 
     vr_age_gbm$agr_profiles, 
     vr_age_svm$agr_profiles, 
     vr_age_knn$agr_profiles)

# Instance level explanations
sp_rf <- predict_parts(explain_titanic_rf, new_passanger)
plot(sp_rf)

sp_lmr <- predict_parts(explain_titanic_lmr, new_passanger)
plot(sp_lmr)

sp_gbm <- predict_parts(explain_titanic_gbm, new_passanger)
plot(sp_gbm)

sp_svm <- predict_parts(explain_titanic_svm, new_passanger)
plot(sp_svm)

sp_knn <- predict_parts(explain_titanic_knn, new_passanger)
plot(sp_knn)
