library(tidyverse)

ins_pkgs_3_6 <- installed.packages() %>% as_tibble()
ins_pkgs_3_4 <- 
  installed.packages(lib.loc = "C:/Users/Miguel/Documents/R/win-library/3.4") %>% 
  as_tibble()

lacking_3_6_pkgs <- 
  ins_pkgs_3_4 %>% 
  slice(which( ! ins_pkgs_3_4$Package %in% ins_pkgs_3_6$Package)) %>% 
  pull(Package)

install.packages(lacking_3_6_pkgs)

pkgs_2_update <- old.packages() %>% as_tibble()

update.packages(oldPkgs = pkgs_2_update$Package)


