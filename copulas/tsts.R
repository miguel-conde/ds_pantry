library(tidyverse)
library(lubridate)
library(clock)
library(units)
select <- dplyr::select
source("copulas/funs_copulas.R", encoding = "utf8")

n_sim <- 1000

probe <- readRDS("data/xa_copulas.Rds")
all_data <- readRDS("C:/Users/mcondedesimon/OneDrive - Deloitte (O365D)/PROYECTOS/PLAYGROUND/ds_pantry/data/all_data_copulas.Rds")


res <- get_copula_sim(probe, n_sim = 1000)

###
sunny <- all_data$app_data$annual_hourly_profile_h_eq_pv %>% 
  select(1) %>% 
  bind_cols(all_data$app_data$annual_hourly_profile_h_eq_pv %>% 
              select(-1) %>% 
              sapply(function(x) ifelse(x == as_units(0.0, "h"), FALSE, TRUE)) %>% 
              as_tibble()) %>% 
  mutate(wk = hour %/% (24*7) + 1) %>% 
  filter(wk <= 52)



probe_2 <- all_data$app_data$hist_pv %>% 
  full_join(all_data$app_data$hist_eo, by = "datetime") %>% 
  full_join(all_data$omip_data$omip, by = "datetime") %>% 
  arrange(datetime) %>% 
  drop_na() %>% 
  mutate(wk = week(datetime)) %>% 
  mutate(hr = hour(datetime) + (yday(datetime)-1)*24) %>% 
  filter(wk != 53) %>%
  full_join(sunny, by = c("wk", "hr" = "hour"), suffix = c("", "_sunny")) %>% 
  mutate(fundao    = fundao    * as_units(as.integer(fundao_sunny)),
         almagro   = almagro   * as_units(as.integer(almagro_sunny)),
         zalea     = zalea     * as_units(as.integer(zalea_sunny)),
         aldehuela = aldehuela * as_units(as.integer(aldehuela_sunny)),
         la_vega   = la_vega   * as_units(as.integer(la_vega_sunny))) %>% 
  # mutate_at(vars(fundao, almagro, zalea, aldehuela, la_vega), 
  #           ~ ifelse(. <= as_units(0, "h*megawatt"), 
  #                    as_units(1e-16, "h*megawatt"), .)) %>% 
  mutate_at(vars(fundao, almagro, zalea, aldehuela, la_vega, sierra_de_aguas, sierra_de_banos, price), 
            ~ as.numeric(.))
         
res_2 <- get_copula_sim(probe_2 %>% 
                          dplyr::select(-datetime, -hr, -wk, -ends_with("_sunny")),
                        n_sim = 1000)

#####
probe_pv <- all_data$app_data$hist_pv %>% 
  mutate(date = as_date(date_group(datetime, "day"))) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, ~ sum(as.numeric(.)))
probe_eo <- all_data$app_data$hist_eo %>% 
  mutate(date = as_date(date_group(datetime, "day"))) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, ~ sum(as.numeric(.)))
probe_price <- all_data$omip_data$omip  %>% 
  mutate(date = as_date(date_group(datetime, "day"))) %>% 
  group_by(date) %>% 
  summarise_if(is.numeric, ~ mean(as.numeric(.)))

probe_3 <- probe_pv %>% 
  full_join(probe_eo, by = "date") %>% 
  full_join(probe_price, by = "date") %>% 
  arrange(date) %>% 
  drop_na()

res_3 <- get_copula_sim(probe_3 %>% select(-date), n_sim = 1000)

#### num_anyos x 52
#### Calculamos el valor medio horario de cada semana de cada anyo y lo dividimos
#### entre el valor medio horario de cada año
perfil_peso_semanal


#### 1 x 52
#### Media por columna de perfil_peso_semanal
perfil_peso_medio_semanal

#### num_anyos x 52
#### perfil_peso_semanal - perfil_peso_medio_semanal (por columna)
dif_peso_precio_semanal

#### 1 x 52
#### Desviación estándar por columna de dif_peso_precio_semanal
sd_dif_peso_precio_semanal


#### num_anyos x num_autocorr
#### Autocorrelación de las diferencias
#### Aplicamos acf a cada serie anual (fila) en dif_peso_precio_semanal
autoc_dif_semana



# ACF ---------------------------------------------------------------------

probe_acf <- all_data$app_data$hist_pv %>% 
  select(fundao) 

for (i in 1:24) {
  probe_acf <- probe_acf %>% mutate(!!sym(paste0("lag_", i)) := lag(fundao, i))
}

probe_acf <- probe_acf %>% drop_na()

res_acf <- get_copula_sim(probe_acf, n_sim = 1000)
