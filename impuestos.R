
library(tidyverse)


# IRPF --------------------------------------------------------------------



# https://www.businessinsider.es/tramos-irpf-2019-asi-quedan-tablas-renta-348097
tb_tramos_IRPF <- tribble(~desde, ~hasta, ~tipo,
                          0, 12450, .19,
                          12450, 20200, .24,
                          20200, 35200, .3,
                          35200, 60000, .37,
                          60000, Inf, .45)

calc_ret <- function(sba, tb_tramos) {
  aux <- tb_tramos %>% 
    mutate(aux = hasta,
           cum_sba = cumsum(hasta)) %>% 
    mutate(new_sba = ifelse(cum_sba <= sba, 
                            cum_sba,
                            pmin(cum_sba, sba)))

  aux$new_sba[-1] <- (aux$new_sba %>% diff)
  
  aux <- aux %>% transmute(desde = desde,
                           hasta = hasta,
                           tipo = tipo,
                           tramo_sba = new_sba,
                           irpf = tipo * tramo_sba)
           
  out <- list(tabla = aux,
              resumen = aux %>% summarise(irpf_total = sum(irpf)) %>% 
                mutate(tipo_efectivo = irpf_total / sba,
                       sba = sba))
  
  return(out)
}

calc_ret(45000, tb_tramos = tb_tramos_IRPF)
calc_ret(48000, tb_tramos = tb_tramos_IRPF)

para_plot <- sapply((12:450)*1000, function(x) {
  calc_ret(x, tb_tramos = tb_tramos_IRPF)$resumen 
}) %>% t %>% as.data.frame %>% sapply(unlist) %>% as_tibble()

plot(para_plot %>% select(sba, tipo_efectivo), type = "l",
     ylim= c(0, .4))

para_plot_2 <- sapply((12:150)*1000, function(x) {
  calc_ret(x, tb_tramos = tb_tramos_IRPF)$resumen 
}) %>% t %>% as.data.frame %>% sapply(unlist) %>% as_tibble()

plot(para_plot_2 %>% select(sba, tipo_efectivo), type = "l",
     ylim= c(0, .4))


# SS ----------------------------------------------------------------------

# http://www.seg-social.es/wps/portal/wss/internet/Trabajadores/CotizacionRecaudacionTrabajadores/36537

bases <- c(minima = 1466.40, maxima = 4070.1) * 12

tb_cotizacion <- tribble(~tipo_cotiz, ~empresa, ~trabajador,
                         "Comunes", .236, .047,
                         "Desempleo", .055, .0155,
                         "Fogasa", .002, 0,
                         "Formación Profesional", 0.006, 0.001) %>% 
  mutate(total_SS = rowSums(tb_cotizacion %>% 
                           select_if(is.numeric)))

total_cotizacion <- tb_cotizacion %>% 
  summarise_if(is_numeric, sum)

calc_cot <- function(sba, bases, total_cotizacion) {
  aux <- max(min(bases["maxima"], sba), bases["minima"])
  
  out <- tibble(sba = sba) %>% 
    bind_cols(total_cotizacion %>% mutate_all(funs(. * aux)))
  
  return(out)
}

calc_cot(45000, bases, total_cotizacion)
calc_cot(48000, bases, total_cotizacion)

para_plot_3 <- 
  sapply((12:450)*1000, calc_cot, bases, total_cotizacion) %>% 
  t %>% as.data.frame() %>% sapply(unlist) %>% as_tibble()


# TOTAL -------------------------------------------------------------------

calc_total <- function(sba, tb_tramos_irpf, bases_ss, total_tipos_cotizacion) {
  
  irpf <- calc_ret(sba, tb_tramos_irpf)
  ss <- calc_cot(sba, bases_ss, total_tipos_cotizacion)
  
  out <- inner_join(irpf$resumen, ss, by = "sba") %>% 
    mutate(coste_laboral = sba + empresa,
           sobrecoste = coste_laboral / sba)
  
  return(out)
}

calc_total(40000, tb_tramos_IRPF, bases, total_cotizacion)
calc_total(43000, tb_tramos_IRPF, bases, total_cotizacion)
calc_total(45000, tb_tramos_IRPF, bases, total_cotizacion)
calc_total(48750, tb_tramos_IRPF, bases, total_cotizacion)

tb_total %>% filter(sba == 40000 | sba == 43000 | sba == 45000 | sba == 48750)

para_plot_4 <- 
  sapply((12:450)*1000, calc_total, tb_tramos_irpf, bases_ss, total_tipos_cotizacion) %>% 
  t %>% as.data.frame() %>% sapply(unlist) %>% as_tibble()

plot(para_plot_4 %>% select(sba, sobrecoste), type = "l")

plot(para_plot_4 %>% select(sba, coste_laboral), type = "l")

abline(a = 0, b = 1, col = "blue")
plot(para_plot_4 %>% select(sba, coste_laboral) %>% 
       filter(sba <= 100000), type = "l")
abline(a = 0, b = 1, col = "blue")


# NOMINA ------------------------------------------------------------------

sba <- 45000

sbm_dinerario <- sba / 14 # 1649.95 + 110.08 + 1454.26 = 3214.29 => IRPF
sbm_especie <- 27.77

seg_acc_m <- 3.81

base_cotiz_m <- sbm_dinerario + sbm_especie + seg_acc_m # 3245.87

prorrata <- sba/14/12*2 # 535.72

base_cotizacion <- base_cotiz_m + prorrata # 3781.59 => SS

cotiz_anual <- calc_cot(base_cotizacion * 12, bases, total_cotizacion)
cotiz_anual

cotiz_mensual <- cotiz_anual %>% mutate_all(funs(. / 12))
cotiz_mensual # trabajador: 177.73 + 62.39 = 240.12
              # empresa: 852.3 + 56.72 + 207.99 + 22.69 + 7.56 = 1147.26


