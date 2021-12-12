library(tidyverse)

library(dagitty)

verisure_dag <- dagitty('dag {
bb="0,0,1,1"
audience [pos="0.058,0.426"]
channel [pos="0.346,0.236"]
daypart [exposure,pos="0.459,0.238"]
sales [outcome,pos="0.839,0.406"]
weekday [pos="0.561,0.234"]
audience -> sales
channel -> audience
channel -> sales
daypart -> audience
daypart -> sales
weekday -> audience
weekday -> sales
}

')

plot(verisure_dag)

dagitty::impliedConditionalIndependencies(verisure_dag, type = "missing.edge")
dagitty::impliedConditionalIndependencies(verisure_dag, type = "basis")
dagitty::impliedConditionalIndependencies(verisure_dag, type = "all.pairs")

simple_verisure_dag <- dagitty('dag {
bb="0,0,1,1"
audience [pos="0.058,0.426"]
channel [pos="0.346,0.236"]
daypart [exposure,pos="0.459,0.238"]
sales [outcome,pos="0.839,0.406"]
weekday [pos="0.561,0.234"]
audience -> sales
channel -> audience
channel -> sales
daypart -> audience
daypart -> sales
weekday -> audience
weekday -> sales
}
')

plot(simple_verisure_dag)

dagitty::impliedConditionalIndependencies(simple_verisure_dag, type = "missing.edge")
dagitty::impliedConditionalIndependencies(simple_verisure_dag, type = "basis")
dagitty::impliedConditionalIndependencies(simple_verisure_dag, type = "all.pairs")

adjustmentSets(simple_verisure_dag, effect = "total")
adjustmentSets(simple_verisure_dag, effect = "direct")

adjustmentSets(simple_verisure_dag, "audience", "sales", effect = "total")
adjustmentSets(simple_verisure_dag, "audience", "sales", effect = "direct")

adjustmentSets(simple_verisure_dag, "daypart", "sales", effect = "total")
adjustmentSets(simple_verisure_dag, "daypart", "sales", effect = "direct")

adjustmentSets(simple_verisure_dag, "weekday", "sales", effect = "total")
adjustmentSets(simple_verisure_dag, "weekday", "sales", effect = "direct")

adjustmentSets(simple_verisure_dag, "channel", "sales", effect = "total")
adjustmentSets(simple_verisure_dag, "channel", "sales", effect = "direct")

adjustmentSets(simple_verisure_dag, 
               c("channel", "weekday", "daypart", "audience"), 
               "sales", 
               effect = "total")
adjustmentSets(simple_verisure_dag, 
               c("channel", "weekday", "daypart", "audience"), 
               "sales", 
               effect = "direct")

####
N_ch <- 5
N_wd <- 7
N_dp <- 3

m_m <- expand.grid(channel = 1:N_ch, weekday = 1:N_wd, daypart = 1:N_dp) %>% 
  apply(2, factor) %>% 
  as.data.frame()

N <- nrow(m_m)

U_a <- 1e5*rgamma(N, 2, 1)
U_s <- rgamma(N, 2, 1)

d_a <- 5e5*runif(N_dp-1, 0, 1)
w_a <- 4e5*runif(N_wd-1, 0, 1)
c_a <- 1e6*runif(N_ch-1, 0, 1)
audience <- model.matrix(~ ., m_m)[,-1] %*% c(c_a, w_a, d_a) + U_a
df <- m_m %>% cbind(audience)

d_s <- 1*runif(N_dp-1, 0, 1)
a <- 3e-5*runif(1, 0, 1)
w_s <- 6*runif(N_wd-1, 0, 1)
c_s <- 2*runif(N_ch-1, 0, 1)
sales <- model.matrix(~ ., df)[,-1] %*% c(c_s, w_s, d_s, a) + U_s

df <- df %>% cbind(sales)

w_t_s <- w_s + w_a*a

## Validación del modelo

library(lavaan)
corr <- lavCor( df )

localTests( simple_verisure_dag, sample.cov=corr, sample.nobs=nrow( df ) )
# Modelo validado. An estimate of around 0 with a p-value higher than 0.05 would 
# mean that the data do not provide evidence against the implied conditional 
# independence being tested.

## Channel
# Total
effects_channel <- coef(lm(sales ~ channel, df)) %>%
  stack() %>% 
  rename(total = values) %>% 
  left_join(
    # Direct
    coef(lm(sales ~ channel + audience + daypart + weekday, df)) %>% 
      stack() %>% 
      rename(direct = values),
    by = "ind") %>% 
  mutate(indirect = total - direct) %>% 
  select(ind, direct, indirect, total) %>% 
  filter(ind != "(Intercept)") %>% 
  as_tibble()

## Weekday
# Total
effects_weekday <- coef(lm(sales ~ weekday, df)) %>%
  stack() %>% 
  rename(total = values) %>% 
  left_join(
    # Direct
    coef(lm(sales ~ weekday + audience + channel + daypart, df)) %>% 
      stack() %>% 
      rename(direct = values),
    by = "ind") %>% 
  mutate(indirect = total - direct) %>% 
  select(ind, direct, indirect, total) %>% 
  filter(ind != "(Intercept)") %>% 
  as_tibble()

## daypart
# Total
effects_daypart <- coef(lm(sales ~ daypart , df)) %>%
  stack() %>% 
  rename(total = values) %>% 
  left_join(
    # Direct
    coef(lm(sales ~ daypart + audience + channel + weekday, df)) %>% 
      stack() %>% 
      rename(direct = values),
    by = "ind") %>% 
  mutate(indirect = total - direct) %>% 
  select(ind, direct, indirect, total) %>% 
  filter(ind != "(Intercept)") %>% 
  as_tibble() %>% 
  filter(str_detect(ind, "daypart"))

## audience
# Total
effects_audience <- coef(lm(sales ~ audience + channel + daypart + weekday, df)) %>%
  stack() %>% 
  rename(total = values) %>% 
  left_join(
    # Direct
    coef(lm(sales ~ audience + channel + daypart + weekday, df)) %>% 
      stack() %>% 
      rename(direct = values),
    by = "ind") %>% 
  mutate(indirect = total - direct) %>% 
  select(ind, direct, indirect, total) %>% 
  filter(ind == "audience") %>% 
  as_tibble()

effects <- effects_channel %>% 
  rbind(effects_weekday) %>% 
  rbind(effects_daypart) %>% 
  rbind(effects_audience)
effects %>% as.data.frame()

coef(lm(sales ~ ., df)) %>%
  stack() %>% 
  rename(total = values) %>% 
  filter(ind != "(Intercept)")


testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("channel","daypart"),
                       c("channel","weekday"),
                       c("daypart","weekday"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
}

testImplications(corr, nrow(df))
