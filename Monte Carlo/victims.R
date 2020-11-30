library(tidyverse)


library(readr)
victimas <- read_delim("Monte Carlo/data/victimas_mortales_de_acci.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  drop_na()
write_csv2(victimas, "Monte Carlo/data/victimas_trafico.csv")

probe <- victimas %>% unite(fecha, ano, periodo, sep = " ") %>% 
  mutate(fecha = paste(fecha, "01")) %>% 
  mutate(fecha = lubridate::ymd(fecha)) 
plot(probe, type = "l")

probe <- probe %>% 
  filter(fecha >= as.Date("2013-01-01")) %>% 
  group_by(anio = lubridate::year(fecha)) %>% 
  summarise(muertos_en_accidentes = sum(muertos_en_accidentes)) %>% 
  ungroup()

plot(probe, type = "l")
hist(probe$muertos_en_accidentes, freq = FALSE)

estimate_lambda <- mean(probe$muertos_en_accidentes)
lines(density(probe$muertos_en_accidentes))
lines(dpois(seq(30, 140), lambda = estimate_lambda), col = "blue")

(rpois(100000, estimate_lambda) > 1000) %>% mean()
(rpois(100000, estimate_lambda) > 1200) %>% mean()
(rpois(100000, estimate_lambda) <  800) %>% mean()


# VG ----------------------------------------------------------------------

victimas <- read_delim("Monte Carlo/data/mujeres_asesinadas_por_vi.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  janitor::clean_names() %>% 
  drop_na()
write_csv2(victimas,  "Monte Carlo/data/victimas_genero.csv")

probe <- victimas %>% unite(fecha, ano, periodo, sep = " ") %>% 
  mutate(fecha = paste(fecha, "01")) %>% 
  mutate(fecha = lubridate::ymd(fecha)) %>% 
  slice(1:18)

plot(probe, type = "l")

hist(probe$mujeres_asesinadas_por_violencia_de_genero, freq = FALSE)

estimate_lambda <- mean(probe$mujeres_asesinadas_por_violencia_de_genero)
lines(density(probe$mujeres_asesinadas_por_violencia_de_genero))
lines(dpois(seq(40, 70), lambda = estimate_lambda), col = "blue")

(rpois(100000, estimate_lambda) > 60) %>% mean()
(rpois(100000, estimate_lambda) > 70) %>% mean()
(rpois(100000, estimate_lambda) <  40) %>% mean()

