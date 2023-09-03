library(tidyverse)

# https://violenciagenero.igualdad.gob.es/violenciaEnCifras/victimasMortales/fichaMujeres/2023/VMortales_2023_09_01.pdf

the_data <-  tribble(~yr, ~n,
                     2003, 71,
                     2004, 72,
                     2005, 57,
                     2006, 69,
                     2007, 71,
                     2008, 76,
                     2009, 57,
                     2010, 73,
                     2011, 62,
                     2012, 51,
                     2013, 54,
                     2014, 55,
                     2015, 60,
                     2016, 49,
                     2017, 50,
                     2018, 53,
                     2019, 56,
                     2020, 50,
                     2021, 49,
                     2022, 49) %>% 
  mutate(t = 1:nrow(.), .before = 2)


poiss_glm <- glm(n ~ t, data = the_data, family = "poisson")

summary(poiss_glm)

log_lambda_fit <- coef(poiss_glm)["(Intercept)"] + coef(poiss_glm)["t"] * the_data$t
res_data <- the_data %>% 
  mutate(n_fit = exp(log_lambda_fit))

res_data %>% 
  gather(key, value, -yr, -t) %>% 
  ggplot(aes(x = yr, y = value, colour = key)) +
  geom_line() +
  geom_point() +
  ylim(0, NA)

# % disminución media anual del numero de asesinatos
(exp(coef(poiss_glm)["t"]) - 1) * 100

### https://www.ine.es/jaxiT3/Datos.htm?t=56937#!tabs-tabla
total_m <- tribble(~yr, ~n,
                   2002, 24121834,	
                   2003, 23986952,	
                   2004, 23794728,	
                   2005, 23678767,	
                   2006, 23622448,	
                   2007, 23586945,	
                   2008, 23597473,	
                   2009, 23648876,	
                   2010, 23700373,	
                   2011, 23662983,	
                   2012, 23553902,	
                   2013, 23433048,	
                   2014, 23226305,	
                   2015, 22873931,	
                   2016, 22459813,	
                   2017, 22120648,	
                   2018, 21766121,	
                   2019, 21458232,	
                   2020, 21096666
                   )

total_m_0_4 <- tribble(~yr, ~n,
                       2002, 950027	,
                       2003, 980664	,
                       2004, 1002765	,
                       2005, 1023234	,
                       2006, 1046948	,
                       2007, 1073363	,
                       2008, 1103044	,
                       2009, 1143743	,
                       2010, 1183455	,
                       2011, 1201103	,
                       2012, 1204803	,
                       2013, 1203024	,
                       2014, 1182868	,
                       2015, 1146609	,
                       2016, 1111140	,
                       2017, 1075664	,
                       2018, 1040022	,
                       2019, 1003688	,
                       2020, 960865)

total_m_5_9 <- tribble(~yr, ~n,
                       2002, 1113557	,
                       2003, 1129113	,
                       2004, 1152163	,
                       2005, 1176310	,
                       2006, 1190018	,
                       2007, 1197563	,
                       2008, 1199130	,
                       2009, 1190426	,
                       2010, 1173368	,
                       2011, 1152604	,
                       2012, 1131023	,
                       2013, 1109150	,
                       2014, 1079480	,
                       2015, 1044265	,
                       2016, 1009154	,
                       2017, 976972	,
                       2018, 952718	,
                       2019, 938704	,
                       2020, 930961)

total_m_10_14 <- tribble(~yr, ~n,
                         2002, 1223325	,
                         2003, 1213807	,
                         2004, 1193642	,
                         2005, 1168930	,
                         2006, 1146330	,
                         2007, 1126465	,
                         2008, 1108660	,
                         2009, 1089736	,
                         2010, 1074430	,
                         2011, 1061044	,
                         2012, 1044204	,
                         2013, 1029663	,
                         2014, 1020453	,
                         2015, 1014719	,
                         2016, 1012356	,
                         2017, 1015764	,
                         2018, 1023357	,
                         2019, 1029163	,
                         2020, 1025104)


the_data_m <- total_m %>% 
  full_join(total_m_0_4, by = "yr", suffix = c("_total", "_0_4")) %>% 
  full_join(total_m_5_9, by = "yr") %>% 
  full_join(total_m_10_14, by = "yr", suffix = c("_5_9", "_10_14")) %>% 
  mutate(n_adultas = n_total - n_0_4 - n_5_9 - n_10_14) %>% 
  select(yr, n_adultas) %>% 
  mutate(yr = rev(yr)) %>% 
  arrange(yr)

probe <- the_data_m %>% 
  inner_join(the_data) %>% 
  mutate(n_100K = n / n_adultas * 100000)

poiss_glm <- glm(n ~ t, data = probe, family = "poisson", offset = log(n_adultas/100000))

summary(poiss_glm)

# log_lambda_fit <- coef(poiss_glm)["(Intercept)"] + coef(poiss_glm)["t"] * probe$t
# log_lambda_ci_low <- coef(poiss_glm)["(Intercept)"] + confint(poiss_glm)["t", "2.5 %"] * probe$t
# log_lambda_ci_high <- coef(poiss_glm)["(Intercept)"] + confint(poiss_glm)["t", "97.5 %"] * probe$t
# 
# res_data <- probe %>% 
#   mutate(n_fit = exp(log_lambda_fit),
#          n_ci_low = exp(log_lambda_ci_low),
#          n_ci_high = exp(log_lambda_ci_high))

pred <- predict(poiss_glm, type = "response", se.fit = TRUE)

res_data <- probe %>%
  mutate(n_fit     = pred$fit,
         n_ci_low  = n_fit - 2*pred$se.fit,
         n_ci_high = n_fit + 2*pred$se.fit)
  
res_data %>% 
  gather(key, value, -yr, -t, -n_adultas, -n_100K) %>% 
  ggplot(aes(x = yr, y = value, colour = key)) +
  geom_line() +
  geom_point() +
  ylim(0, NA)

# % disminución media anual del numero de asesinatos por cada 10M de mujeres adultas
(exp(coef(poiss_glm)["t"]) - 1) * 100

