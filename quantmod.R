library(tidyverse)
library(quantmod)
library(timetk)

# getSymbols("YHOO",src="google") # from google finance
getSymbols("YHOO",src="yahoo") # Google Finance stopped providing data in March, 2018.
getSymbols("GOOG",src="yahoo") # from yahoo finance
getSymbols("DEXJPUS",src="FRED") # FX rates from FRED
getSymbols("XPT/USD",src="Oanda") # Platinum from Oanda

setSymbolLookup(YHOO='yahoo',GOOG='yahoo')
setSymbolLookup(DEXJPUS='yahoo')
setSymbolLookup(XPTUSD=list(name="XPT/USD",src="yahoo"))
# saveSymbolLookup(file="mysymbols.rda")

getSymbols(c("YHOO","GOOG","DEXJPUS","XPTUSD"))


getSymbols("GOOG",src="yahoo")
getSymbols("^DJI",src="yahoo")
getSymbols("^IBEX",src="yahoo")

# Lags negativos: el pasado de DJI
ccf(Cl(DJI) %>% as.numeric %>% log %>% diff, 
    Cl(IBEX) %>% as.numeric() %>% log %>% diff(),
    na.action = na.pass)

aux <- tk_tbl(Cl(DJI), rename_index = "date") %>% 
  full_join(tk_tbl(Cl(IBEX), rename_index = "date"),
            by = "date") 

aux <- aux %>% 
  mutate(p_ret_dji = c(NA, diff(log(aux$DJI.Close))),
         p_ret_ibex = c(NA, diff(log(aux$IBEX.Close))))

aux <- aux %>% 
  mutate(Lag_6_DGI = lag(p_ret_dji, 6),
         Lag_7_DGI = lag(p_ret_dji, 7)) %>% 
  mutate(res_6 = (p_ret_ibex > 0 & Lag_6_DGI > 0) | 
           (p_ret_ibex < 0 & Lag_6_DGI < 0),
         res_7 = (p_ret_ibex > 0 & Lag_7_DGI > 0)  | 
           (p_ret_ibex < 0 & Lag_7_DGI < 0))

aux %>% pull(res_6) %>% table %>% prop.table()
aux %>% pull(res_7) %>% table %>% prop.table()
