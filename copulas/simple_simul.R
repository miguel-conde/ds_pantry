library(tidyverse)
library(lubridate)
# library(clock)
library(units)
select <- dplyr::select
# source("copulas/funs_copulas.R", encoding = "utf8")
# 
# n_sim <- 1000
# 

TZ <- "UTC"

# DATA --------------------------------------------------------------------


# probe <- readRDS("data/xa_copulas.Rds")
all_data <- readRDS("data/probe_copulas.Rds")

tz(all_data$hist_h_eq_pv$datetime) <- TZ
tz(all_data$hist_h_eq_eo$datetime) <- TZ
tz(all_data$omip_raw$datetime)     <- TZ
tz(all_data$input_prices$datetime) <- TZ

# Históricos
historical_data <- all_data$hist_h_eq_pv %>% 
  full_join(all_data$hist_h_eq_eo, by = "datetime") %>% 
  left_join(all_data$omip_raw, by = "datetime") %>% 
  arrange(datetime) %>% 
  filter(! (month(datetime) == 2 & day(datetime) == 29)) %>% 
  drop_units() %>% 
  mutate(hour = as.integer((0:(nrow(.)-1)) %% 8760), .after = 1) %>%
  mutate(yr = as.integer(year(datetime)), .after = hour) %>% 
  mutate(mth = as.integer(month(datetime)), .after = yr) %>% 
  mutate(wk_in_yr = as.integer(isoweek(datetime)), .after = mth) %>% 
  mutate(dy_in_yr = as.integer(yday(datetime)), .after = wk_in_yr) %>% 
  mutate(wdy = wday(datetime,label = TRUE, abbr = FALSE) %>% 
           as.character() %>% factor(), 
         .after = dy_in_yr) %>% 
  mutate(hr_in_dy = hour(datetime), .after = wdy) %>% 
  select(-pv2, -pv3, -pv4, -pv5, -eo1) 

### Checks
historical_data %>% group_by(yr = year(datetime)) %>% summarise(n = n()) %>% summary()
historical_data %>% group_by(date = as_date(datetime)) %>% summarise(n = n()) %>% filter(n != 24)

all_data$hist_h_eq_pv %>% group_by(date = as_date(datetime)) %>% summarise(n = n()) %>% filter(n != 24)
all_data$hist_h_eq_eo %>% group_by(date = as_date(datetime)) %>% summarise(n = n()) %>% filter(n != 24)
all_data$omip_raw %>% group_by(date = as_date(datetime)) %>% summarise(n = n()) %>% filter(n != 24)

all_data$hist_h_eq_pv %>% 
  full_join(all_data$hist_h_eq_eo, by = "datetime") %>% 
  left_join(all_data$omip_raw, by = "datetime") %>% 
  arrange(datetime) %>% group_by(date = as_date(datetime)) %>% summarise(n = n()) %>% filter(n != 24)
###

log_diff_historical_data <- historical_data %>% 
  mutate_if(is.numeric, ~ c(NA, diff(log(.))))

# Baselines
baselines_data <- 
  # PV
  all_data$annual_hourly_profile_h_eq_pv %>% 
  # Ponemos fechas correctas
  mutate(datetime = seq(as.POSIXct("2022-01-01 00:00:00", tz = TZ), 
                        as.POSIXct("2022-12-31 23:00:00", tz = TZ), by="hour"),
         .before = 1) %>%
  # EO
  full_join(all_data$annual_hourly_profile_h_eq_eo %>% 
              select(-hour) %>% 
              # Quitamos 29 de febrero
              mutate(datetime = seq(as.POSIXct("2020-01-01 00:00:00", tz = TZ), 
                                    as.POSIXct("2020-12-31 23:00:00", tz = TZ), by="hour"),
                     .before = 1) %>% 
              filter(! (month(datetime) == 2 & day(datetime) == 29)) %>% 
              # Ponemos fechas correctas
              mutate(datetime = seq(as.POSIXct("2022-01-01 00:00:00", tz = TZ), 
                                    as.POSIXct("2022-12-31 23:00:00", tz = TZ), by="hour")),
            by = "datetime") %>% 
  # Price
  full_join(all_data$input_prices %>% drop_na(), by = "datetime") %>% 
  arrange(datetime) %>% 
  drop_units()%>% 
  mutate(hour = as.integer((0:(nrow(.)-1)) %% 8760), .after = 1) %>%
  mutate(yr = as.integer(year(datetime)), .after = hour) %>% 
  mutate(mth = as.integer(month(datetime)), .after = yr) %>% 
  mutate(wk_in_yr = as.integer(isoweek(datetime)), .after = mth) %>% 
  mutate(dy_in_yr = as.integer(yday(datetime)), .after = wk_in_yr) %>% 
  mutate(wdy = wday(datetime,label = TRUE, abbr = FALSE) %>% 
           as.character() %>% factor(), 
         .after = dy_in_yr) %>% 
  mutate(hr_in_dy = hour(datetime), .after = wdy) %>% 
  select(-pv2, -pv3, -pv4, -pv5, -eo1)

### Checks
baselines_data %>% group_by(yr = year(datetime)) %>% summarise(n = n()) %>% summary()
### 


# GENERACIÓN / CONSUMO ----------------------------------------------------

res <- historical_data %>% 
  inner_join(baselines_data %>% 
               select(hour, pv1, eo2), 
             by = "hour", suffix = c("_hist", "_base")) %>% 
  # mutate(pv1_diff = pv1_hist - pv1_base,
  #        eo2_diff = eo2_hist - eo2_base) %>%  
  mutate(perc_diff_pv1 =log(ifelse(pv1_base != 0 & pv1_hist != 0, pv1_hist / pv1_base, 1))) %>% 
  mutate(perc_diff_eo2 =log(ifelse(eo2_base != 0 & eo2_hist != 0, eo2_hist / eo2_base, 1)))

# PRICE -------------------------------------------------------------------

# DESESTACIONALIZACIÓN ----------------------------------------------------

tb_ts <- historical_data %>% select(-pv1, -eo2)

desest <- function(tb_ts) {
  
  aux_lm <- lm(price ~ hour:factor(yr) + # Year Trend
                 factor(yr) + # Intercept year
                 factor(mth) + # Yearly seasonality (12 months)
                 factor(wk_in_yr) + # Yearly seasonality (52 weeks)
                 factor(dy_in_yr) + # Yearly seasonality (365 days)
                 wdy + # Weekly seasonality (7 days of week)
                 factor(hr_in_dy), # Hourly seasonality (24 hrs)
               tb_ts)
  
  out <- tb_ts %>% mutate(trnd_est  = fitted(aux_lm),
                   net_price = price - trnd_est,
                   perc_net_price = net_price / trnd_est)
  
  return(out)
}

out %>% 
  ggplot(aes(x = datetime)) + 
  geom_line(aes(y = price, col = "Price", alpha = .25)) + 
  geom_line(aes(y = trnd_est, col = "Trend+Season", alpha = .25))

out %>% ggplot(aes(x = datetime, y = trnd_est)) + geom_line()
out %>% ggplot(aes(x = datetime, y = net_price)) + geom_line()
out %>% ggplot(aes(x = datetime, y = perc_net_price)) + geom_line()

out %>% 
  ggplot(aes(x = datetime)) + 
  geom_line(aes(y = trnd_est, alpha = .25))

out %>% 
  ggplot(aes(x = datetime)) + 
  geom_line(aes(y = trnd_est)) + 
  scale_x_datetime(limits = as.POSIXct(c("2020-01-02", "2020-12-31"))) + 
  scale_y_continuous(limits = c(4, 52))
baselines_data %>% ggplot(aes(x = datetime, y = price)) + geom_line()

library(forecast)
decomp <- out$price %>% msts(c(24,7*24,8760)) %>% mstl(s.window = "periodic")
plot(decomp)
decomp[,"Remainder"] %>% density() %>% plot

cor(decomp[,"Remainder"], out$net_price)
plot(decomp[,"Remainder"], out$net_price)

(decomp[,"Remainder"] / (decomp[,"Data"] - decomp[,"Remainder"])) %>% plot()

cor(decomp[,"Remainder"] / (decomp[,"Data"] - decomp[,"Remainder"]), out$perc_net_price)
plot(decomp[,"Remainder"] / (decomp[,"Data"] - decomp[,"Remainder"]), out$perc_net_price)

density(decomp[,"Remainder"] / (decomp[,"Data"] - decomp[,"Remainder"])) %>% plot()

# STATS -------------------------------------------------------------------

avgs <- historical_data %>% summarise_if(is.numeric, ~ mean(.))

cov_matrix <- historical_data %>% 
  select_if(is.numeric) %>% 
  cov()
