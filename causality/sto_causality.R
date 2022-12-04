source("./causality/utils_sto_causal.R")

# DATA --------------------------------------------------------------------

library(readr)
co2_data <- readr::read_csv("./causality/data/co2-mm-mlo_csv.csv")

ggplot(data = co2_data, aes(x = Date)) +
  geom_line(aes(y = Interpolated)) +
  geom_line(aes(y = Trend))

temp_data <- readr::read_csv("./causality/data/monthly_csv.csv") %>% 
  filter(Source == "GCAG") 
lubridate::day(temp_data$Date) <-  1
ggplot(data = temp_data, aes(x = Date)) +
  geom_line(aes(y = Mean))

temp_co2_data <- co2_data %>% 
  janitor::clean_names() %>% 
  select(date, co2 = interpolated) %>% 
  inner_join(temp_data %>% 
               janitor::clean_names() %>% 
               select(date, temp = mean),
             by = "date") %>% 
  # mutate(co2 = log(co2)) %>% 
  mutate(temp = c(rep(NA,12), diff(temp, 12)),
         co2 = c(rep(NA,12), diff(log(co2), 12))) %>% 
  drop_na()


# PAPER -------------------------------------------------------------------

####
obj_causal_temp_co2 <- est_causal(temp_co2_data, "temp", "co2", J = 20, lambda = 10)
autoplot(obj_causal_temp_co2)
evr(obj_causal_temp_co2)

####
obj_causal_co2_temp <- est_causal(temp_co2_data, "co2", "temp", J = 20, lambda = 10)
autoplot(obj_causal_co2_temp)
evr(obj_causal_co2_temp)
