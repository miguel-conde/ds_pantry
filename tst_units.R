#### Package units

library(tidyverse)
library(units)

valid_udunits()
valid_udunits_prefixes()

# define a fortnight
install_unit(symbol = "fn", def = "2 week", name = "fortnight")
yr <- as_units("year")
set_units(yr, fn) # by symbol
set_units(yr, fortnight) # by name
# clean up
remove_unit("fn", "fortnight")

install_unit(symbol = "MW", def = "megawatt", name = "megawatt")
W <- as_units("watt")
set_units(W, "megawatt")
remove_unit("MW", "megawatt")

# working with currencies
install_unit("dollar")
install_unit("euro", "1.22 dollar")
install_unit("yen", "0.0079 euro")
set_units(as_units("dollar"), yen)
# clean up
remove_unit(c("dollar", "euro", "yen"))


## MONETARIAS
install_unit(symbol = "EUR", name = "euro")
install_unit(symbol = "USD", name = "US dolar")

as_units(1.1, "dollar") / as_units("euro")

## 
as_units(1.5, "megawatts")
as_units(1.5, "megawatthour")
