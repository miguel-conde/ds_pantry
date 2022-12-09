library(tidyverse)
library(readr)
library(xgboost)
library(ranger)
library(caret)
library(fastshap)

churn_data <- readr::read_csv("./data/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  janitor::clean_names() %>% 
  mutate_at(vars(gender, partner, dependents, phone_service, multiple_lines,
                 internet_service, online_security, online_backup, 
                 device_protection, tech_support, streaming_tv,
                 streaming_movies, contract, paperless_billing, 
                 payment_method),
            ~ factor(.)) %>% 
  mutate(senior_citizen = factor(senior_citizen, labels = c("No", "Yes"))) %>% 
  mutate(churn = factor(churn, labels = c("No", "Yes")))


x_var <- setdiff(colnames(churn_data), c("customer_id", "churn"))
y_var <- "churn"

mm <- model.matrix(~ .-1, churn_data[, c(x_var, y_var)])
x_train <- mm[, setdiff(colnames(mm), "churnYes")]
y_train <- mm[, "churnYes"]

model_xg <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

Metrics::accuracy(y_train, predict(model_xg, x_train) > .5)

set.seed(101)  # for reproducibility
shap_xg <- fastshap::explain(model_xg, 
                             X = x_train, 
                             nsim = 10, 
                             pred_wrapper = predict, 
                             adjust = TRUE)
shap_xg

####

## 1 - Mantenemos los factors
rf_churn_data <- churn_data[, c(x_var, y_var)] %>% drop_na()
model_rf <- ranger(churn ~ ., rf_churn_data)

Metrics::accuracy(rf_churn_data$churn, predict(model_rf, rf_churn_data)$predictions)

set.seed(101)  # for reproducibility
shap_rf <- fastshap::explain(model_rf, 
                             X = rf_churn_data %>% select(-churn), 
                             nsim = 10, 
                             pred_wrapper = function(object, new_data) predict(object, new_data)$predictions, 
                             adjust = TRUE)
shap_rf

## 2 - Onehot coding
dummy <- dummyVars(" ~ .-churn", data = rf_churn_data, fullRank = TRUE)
rf_churn_data_2 <- predict(dummy, rf_churn_data) %>% 
  as_tibble() %>% 
  bind_cols(rf_churn_data %>% select(churn)) %>% 
  janitor::clean_names()

model_rf_2 <- ranger(churn ~ ., rf_churn_data_2, probability = TRUE)

Metrics::accuracy(rf_churn_data_2$churn == "Yes", predict(model_rf_2, rf_churn_data_2)$predictions[,"Yes"] > .5 )

set.seed(101)  # for reproducibility
shap_rf_2 <- fastshap::explain(model_rf_2, 
                             X = rf_churn_data_2 %>% select(-churn) %>% as.data.frame(), 
                             nsim = 10, 
                             pred_wrapper = function(object, newdata) predict(object, newdata)$predictions[, "Yes"])
shap_rf_2

