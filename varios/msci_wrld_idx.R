library(tidyverse)

# https://www.msci.com/end-of-day-history?chart=regional&priceLevel=0&scope=R&style=C&asOf=Nov%2019,%202021&currency=15&size=36&indexId=106

library(readxl)

msci_wrld_idx <- read_excel("C:/Users/migue/Downloads/historyIndex.xls",
                            skip = 6) %>% 
  head(-19) %>% 
  janitor::clean_names() %>% 
  rename(close = world_standard_large_mid_cap) %>% 
  mutate(close = as.numeric(str_remove(close, ",")),
         date = lubridate::mdy(date))

library(tibbletime)




make_roll_fun <- function(FUN, window) {
  
  out <- rollify(FUN, window = window)
}

roll_mean_60 <- make_roll_fun(mean, window = 60)

msci_wrld_idx %>% mutate(roll_mean_60 = roll_mean_60(close))

# Rentabilidad total del periodo
profit <- function(x) {
  
  out <- (x[length(x)] - x[1]) / x[1]
  
  return(out)
}

# Rentabilidad anual media
avg_yrly_profit <- function(x) {
  
  out <- x[length(x)] / x[1]
  
  out <- out^(1/length(x)*12) - 1
  
  return(out)
}

profit_10yr <- make_roll_fun(profit, window = 12*10)
profit_15yr <- make_roll_fun(profit, window = 12*15)
profit_20yr <- make_roll_fun(profit, window = 12*20)
profit_25yr <- make_roll_fun(profit, window = 12*25)
profit_30yr <- make_roll_fun(profit, window = 12*30)

avg_yrly_profit_10yr <- make_roll_fun(avg_yrly_profit, window = 12*10)
avg_yrly_profit_15yr <- make_roll_fun(avg_yrly_profit, window = 12*15)
avg_yrly_profit_20yr <- make_roll_fun(avg_yrly_profit, window = 12*20)
avg_yrly_profit_25yr <- make_roll_fun(avg_yrly_profit, window = 12*25)
avg_yrly_profit_30yr <- make_roll_fun(avg_yrly_profit, window = 12*30)

msci_wrld_idx <- msci_wrld_idx %>% 
  mutate(profit_10yr = profit_10yr(close),
         profit_15yr = profit_15yr(close),
         profit_20yr = profit_20yr(close),
         profit_25yr = profit_25yr(close),
         profit_30yr = profit_30yr(close),
         avg_yrly_profit_10yr = avg_yrly_profit_10yr(close),
         avg_yrly_profit_15yr = avg_yrly_profit_15yr(close),
         avg_yrly_profit_20yr = avg_yrly_profit_20yr(close),
         avg_yrly_profit_25yr = avg_yrly_profit_25yr(close),
         avg_yrly_profit_30yr = avg_yrly_profit_30yr(close))

msci_wrld_idx %>% select(date, profit_10yr) %>% 
  plot(type = "l", ylim = c(-1, 16),
       main = "Rentabilidades Totales",
       ylab = "%")
legend("topleft", c("10 años", "15 años", "20 años", "25 años", "30 años"),
       lty = 1, col = 1:5, bty = "n")
abline(h = 0, lty = 2)
msci_wrld_idx %>% select(date, profit_15yr) %>% lines(col = 2)
msci_wrld_idx %>% select(date, profit_20yr) %>% lines(col = 3)
msci_wrld_idx %>% select(date, profit_25yr) %>% lines(col = 4)
msci_wrld_idx %>% select(date, profit_30yr) %>% lines(col = 5)

msci_wrld_idx %>% select(date, avg_yrly_profit_10yr) %>% 
  plot(type = "l", ylim = c(-.05, .17),
       main = "Rentabilidades Anuales Medias",
       ylab = "%")
legend("topleft", c("10 años", "15 años", "20 años", "25 años", "30 años"),
       lty = 1, col = 1:5, bty = "n")
abline(h = 0, lty = 2)
msci_wrld_idx %>% select(date, avg_yrly_profit_15yr) %>% lines(col = 2)
msci_wrld_idx %>% select(date, avg_yrly_profit_20yr) %>% lines(col = 3)
msci_wrld_idx %>% select(date, avg_yrly_profit_25yr) %>% lines(col = 4)
msci_wrld_idx %>% select(date, avg_yrly_profit_30yr) %>% lines(col = 5)

##
msci_wrld_idx$profit_10yr %>% hist()
msci_wrld_idx$profit_10yr %>% mean(na.rm = TRUE)
msci_wrld_idx$profit_10yr %>% median(na.rm = TRUE)

msci_wrld_idx$avg_yrly_profit_10yr %>% hist()
msci_wrld_idx$avg_yrly_profit_10yr %>% mean(na.rm = TRUE)
msci_wrld_idx$avg_yrly_profit_10yr %>% median(na.rm = TRUE)

##
msci_wrld_idx$profit_15yr %>% hist()
msci_wrld_idx$profit_15yr %>% mean(na.rm = TRUE)
msci_wrld_idx$profit_15yr %>% median(na.rm = TRUE)

msci_wrld_idx$avg_yrly_profit_15yr %>% hist()
msci_wrld_idx$avg_yrly_profit_15yr %>% mean(na.rm = TRUE)
msci_wrld_idx$avg_yrly_profit_15yr %>% median(na.rm = TRUE)

##
msci_wrld_idx$profit_20yr %>% hist()
msci_wrld_idx$profit_20yr %>% mean(na.rm = TRUE)
msci_wrld_idx$profit_20yr %>% median(na.rm = TRUE)

msci_wrld_idx$avg_yrly_profit_20yr %>% hist()
msci_wrld_idx$avg_yrly_profit_20yr %>% mean(na.rm = TRUE)
msci_wrld_idx$avg_yrly_profit_20yr %>% median(na.rm = TRUE)

##
msci_wrld_idx$profit_25yr %>% hist()
msci_wrld_idx$profit_25yr %>% mean(na.rm = TRUE)
msci_wrld_idx$profit_25yr %>% median(na.rm = TRUE)

msci_wrld_idx$avg_yrly_profit_25yr %>% hist()
msci_wrld_idx$avg_yrly_profit_25yr %>% mean(na.rm = TRUE)
msci_wrld_idx$avg_yrly_profit_25yr %>% median(na.rm = TRUE)

##
msci_wrld_idx$profit_30yr %>% hist()
msci_wrld_idx$profit_30yr %>% mean(na.rm = TRUE)
msci_wrld_idx$profit_30yr %>% median(na.rm = TRUE)

msci_wrld_idx$avg_yrly_profit_30yr %>% hist()
msci_wrld_idx$avg_yrly_profit_30yr %>% mean(na.rm = TRUE)
msci_wrld_idx$avg_yrly_profit_30yr %>% median(na.rm = TRUE)

##
msci_wrld_idx %>% 
  select(date, profit_10yr) %>% 
  drop_na() %>% 
  mutate(ok = profit_10yr > 0) %>% 
  pull(ok) %>% mean()

##
msci_wrld_idx %>% 
  select(date, avg_yrly_profit_10yr) %>% 
  drop_na() %>% 
  mutate(ok = avg_yrly_profit_10yr > 0) %>% 
  pull(ok) %>% mean()



