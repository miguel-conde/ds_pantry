library(tidyverse)

ins_pkgs_4 <- installed.packages() %>% as_tibble()
ins_pkgs_3_6_2 <- 
  installed.packages(lib.loc = "C:/Users/Miguel/Documents/R/win-library/3.6") %>% 
  as_tibble()

lacking_4_pkgs <- 
  ins_pkgs_3_6_2 %>% 
  slice(which( ! ins_pkgs_3_6_2$Package %in% ins_pkgs_4$Package)) %>% 
  pull(Package)

install.packages(lacking_4_pkgs[! lacking_4_pkgs %in% c("bayesm3", "Conento")])

pkgs_2_update <- old.packages() %>% as_tibble()

update.packages(oldPkgs = pkgs_2_update$Package)


