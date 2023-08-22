library(tidyverse)

results = tribble(
  ~partido, ~votos,
  "pp", 8091840,
  "psoe", 7760970,
  "vox", 3033744,
  "sumar", 3014006,
  "erc", 462883,
  "junts", 392634,
  "bildu", 333362,
  "pnv", 275782,
  "bng", 152327,
  "cc", 114718,
  "upn", 51764
)


compute_dhont <- function(results, n, thr = 0.03) {
  
  probe <- results %>% 
    mutate(perc_votos = votos / sum(votos)) %>% 
    filter(perc_votos > thr) %>% 
    select(-perc_votos)
  
  for (i in 1:n) {
    probe[as.character(i)] <- probe$votos / i
  }
  
  out <- probe %>% 
    gather(resto, valor, -partido, -votos) %>% 
    arrange(desc(valor)) %>%
    slice(1:n) %>% 
    group_by(partido) %>% 
    summarise(n_esc = n()) %>% 
    arrange(desc(n_esc))
  
  out <- out %>% 
    right_join(results, by = "partido") %>% 
    mutate(n_esc = ifelse(is.na(n_esc), 0, n_esc)) %>% 
    mutate(perc_esc = 100.0 * n_esc / n,
           perc_votos = 100.0 * votos / sum(votos),
           ratio_esc_votos = perc_esc / perc_votos)
  
  return(out)
  
}

compute_dhont(results, 350)
compute_dhont(results, 350, 0)

# https://elecciones.comunidad.madrid/es/informacion-util/simulacion-metodo-dhondt

results2 <- tribble(
  ~partido, ~votos,
  "a", 168000,
  "b", 104000,
  "c", 72000,
  "d", 64000,
  "e", 40000,
  "f", 32000
)

compute_dhont(results2, 8, 0.05)