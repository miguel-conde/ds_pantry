library(readxl)



# CORES -------------------------------------------------------------------


get_cores_data <- function(sheet = "Gasolinas") {
  #
  # Esta función se baja de la web de CORES el archivo excel con los consumos 
  # nacionales mensuales de productos petrolíferos
  #
  # PARAMETERS
  #   - sheet: char("Gasolinas", "Gasoleos") - hoja del libro excel cuyos datos 
  #            se desea obtener
  #
  # RETURN
  #   - Tibble con <fecha> (ajustada al dia fina de mes) y una columna para el 
  #     consumo mensual (en Tn) por producto 
  
  destfile <- paste0(tempfile(), ".xlsx")
  
  curl::curl_download(URL_CORES, destfile)
  
  consumos_pp <- read_excel(destfile, sheet = sheet, skip = 5) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(mes)) %>% 
    filter(mes != "total") %>% 
    mutate(dia = 1) %>% 
    unite(fecha, c("ano", "mes", "dia"), sep = "-") %>% 
    mutate(fecha = ymd(fecha))
  
  unlink(destfile)
  
  day(consumos_pp$fecha)  <-  days_in_month(consumos_pp$fecha)
  
  consumos_pp
}

make_cores_data <- function() {
  #
  # Esta función utiliza la anterior - get_cores_data() - para obtener los consumos
  # totales mensuales de gasolinas y gasoleos
  #
  # RETURN
  #   - Tibble con <fecha> (ajustada al dia fina de mes), <gna>, <goa> y <total>
  #
  
  gna_data <- get_cores_data() %>% 
    select(fecha, total) %>% 
    rename(gna = total)
  
  goa_data <- get_cores_data("Gasoleos") %>% 
    select(fecha, total) %>% 
    rename(goa = total)
  
  cores_data <- gna_data %>% 
    inner_join(goa_data, by = "fecha") %>% 
    mutate(total = gna + goa)
  
  cores_data
}


# INE ---------------------------------------------------------------------

# ♂devtools::install_github("oddworldng/INEbaseR")
library(INEbaseR)

get_ine_data <- function(code, date_start = "2010-01-01") {
  #
  # Esta función se baja de la web del INE la serie temporal trimestral que se
  # le indique
  #
  # PARAMETERS
  #   - code: char, código de la serie deseada
  #   - date_start: fecha de inicio deseada
  #
  # RETURN
  #   - Tibble con <fecha> (ajustada al dia fina de trimestre) y <valor>. Los 
  #     atributos de esre tibble continen detalles de la serie
  #
  
  # ALGUNOS CÓDIGOS ÚTILES:
  #
  # "CNTR4412" = RENTA NACIONAL DISPONIBLE NETA - DATO BASE
  # "CNTR3179" = Total Nacional. Base 2010. Datos ajustados de estacionalidad y 
  #              calendario. Producto interior bruto a precios de mercado. 
  #              Variación anual Índices de volumen encadenados.
  # "CNTR3180" = Total Nacional. Base 2010. Datos ajustados de estacionalidad y 
  #              calendario. Producto interior bruto a precios de mercado. 
  #              Variación trimestral. Índices de volumen encadenados.
  # "CNTR3883" = Total Nacional. Ocupados. Puestos de trabajo equivalentes a 
  #              tiempo completo. Datos ajustados de estacionalidad y 
  #              calendario. Variación anual Total CNAE. Base 2010.
  # "CNTR3896" = Total Nacional. Ocupados. Puestos de trabajo equivalentes a 
  #              tiempo completo. Datos ajustados de estacionalidad y 
  #              calendario. Variación trimestral. Total CNAE. Base 2010.
  # "EPA815"   = Total Nacional. Tasa de paro de la población. Ambos sexos. 
  #              16 y más años.
  
  # aux <- get_data_serie(code, date_start)
  aux <- INEbaseR::get_series(code = code, 
                              resource = "data",
                              date_start = date_start,
                              date_end = "2100-12-31",
                              nlast = 100000)
  
  
  out <- aux$Data %>% 
    as.tibble %>% 
    mutate(Fecha = as.POSIXct(Fecha/1000, origin = "1970-01-01"),
           fecha = as.Date(Fecha + months(3))) %>%
    rename(valor = Valor) %>% 
    select(fecha, valor)  
  
  aux$Data <- NULL
  attributes(out) <- c(attributes(out), aux)
  
  out
}


# Eurostat ----------------------------------------------------------------


library(eurostat)

library(rvest)

# Get Eurostat data listing
toc <- get_eurostat_toc()
toc


# info about passengers
head(search_eurostat("GDP and main components"))
head(search_eurostat("Population by age group", type = "all", fixed = FALSE))

# For the original data, see
# http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=tsdtr210
id <- search_eurostat("Modal split of passenger transport", 
                      type = "table")$code[1]
print(id)

dat <- get_eurostat("nama_10_fcs", time_format = "num")
