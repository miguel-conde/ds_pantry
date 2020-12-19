library(tidyverse)

library(lme4)

N <- 8 * 100
dataset <- tibble(country = factor(c(rep("IT", N/2), rep("GE", N/2))),
                  customer_segment = factor(rep(c("total", "existing", "new", "vip"), N/4))) %>% 
  mutate(price = rnorm(N)^2,
         promo = rbinom(N, 3, c(.50, .35, .15)) %>% as.factor,
         media = rnorm(N, sd = 10)^2,
         # sales = rnorm(N, sd = .1)^2,
         sales = -5*as.numeric(customer_segment)*as.numeric(country)*price +
           3*as.numeric(country)*media +
           7*as.numeric(country)*as.numeric(promo))

M1 <- lmer(sales ~ media + promo + price +
             (media + promo + price | customer_segment) + 
             (media + promo + price | country),
           data = dataset)
summary(M1)
coef(M1)
ranef(M1)

Metrics::mape(dataset$sales, fitted(M1))
plot(dataset$sales, fitted(M1))
abline(a=0,b=1)

plot(fitted(M1), residuals(M1))

coef(M1)$customer_segment["total", ]    + coef(M1)$country["IT", ]
coef(M1)$customer_segment["existing", ] + coef(M1)$country["IT", ]
coef(M1)$customer_segment["new", ]      + coef(M1)$country["IT", ]
coef(M1)$customer_segment["vip", ]      + coef(M1)$country["IT", ]
coef(M1)$customer_segment["total", ]    + coef(M1)$country["GE", ]
coef(M1)$customer_segment["existing", ] + coef(M1)$country["GE", ]
coef(M1)$customer_segment["new", ]      + coef(M1)$country["GE", ]
coef(M1)$customer_segment["vip", ]      + coef(M1)$country["GE", ]


M2 <- lmer(sales ~ price  -1 + customer_segment + country +
             (1 | customer_segment)  + (media + promo + 0 | country),
           data = dataset)
summary(M2)
coef(M2)
ranef(M2)

coef(M2)$customer_segment["total", ]    + coef(M2)$country["IT", ]
coef(M2)$customer_segment["existing", ] + coef(M2)$country["IT", ]
coef(M2)$customer_segment["new", ]      + coef(M2)$country["IT", ]
coef(M2)$customer_segment["vip", ]      + coef(M2)$country["IT", ]
coef(M2)$customer_segment["total", ]    + coef(M2)$country["GE", ]
coef(M2)$customer_segment["existing", ] + coef(M2)$country["GE", ]
coef(M2)$customer_segment["new", ]      + coef(M2)$country["GE", ]
coef(M2)$customer_segment["vip", ]      + coef(M2)$country["GE", ]

M3 <- lmer(sales ~ price + media + promo  +
             (price + 1 | customer_segment)  + 
             (price + media + promo + 0 | country),
           data = dataset)
summary(M3)
coef(M3)
ranef(M3)

Metrics::mape(dataset$sales, fitted(M3))
plot(dataset$sales, fitted(M3))
abline(a=0,b=1)

plot(fitted(M3), residuals(M3))

coef(M3)$customer_segment["total", ]    + coef(M3)$country["IT", ]
coef(M3)$customer_segment["existing", ] + coef(M3)$country["IT", ]
coef(M3)$customer_segment["new", ]      + coef(M3)$country["IT", ]
coef(M3)$customer_segment["vip", ]      + coef(M3)$country["IT", ]
coef(M3)$customer_segment["total", ]    + coef(M3)$country["GE", ]
coef(M3)$customer_segment["existing", ] + coef(M3)$country["GE", ]
coef(M3)$customer_segment["new", ]      + coef(M3)$country["GE", ]
coef(M3)$customer_segment["vip", ]      + coef(M3)$country["GE", ]

M4 <- lmer(sales ~ price + media + promo  +
             (price + media + promo | customer_segment)  + 
             (price + media + promo | country),
           data = dataset)
summary(M4)
coef(M4)
fixef(M4)
ranef(M4)

Metrics::mape(dataset$sales, fitted(M4))
plot(dataset$sales, fitted(M4))
abline(a=0,b=1)

plot(fitted(M4), residuals(M4))

coef(M4)$customer_segment["total", ]    + coef(M4)$country["IT", ]
coef(M4)$customer_segment["existing", ] + coef(M4)$country["IT", ]
coef(M4)$customer_segment["new", ]      + coef(M4)$country["IT", ]
coef(M4)$customer_segment["vip", ]      + coef(M4)$country["IT", ]
coef(M4)$customer_segment["total", ]    + coef(M4)$country["GE", ]
coef(M4)$customer_segment["existing", ] + coef(M4)$country["GE", ]
coef(M4)$customer_segment["new", ]      + coef(M4)$country["GE", ]
coef(M4)$customer_segment["vip", ]      + coef(M4)$country["GE", ]

M5 <- lmer(sales ~ country:customer_segment +
             customer_segment:media + customer_segment:country:media +
             customer_segment:promo + customer_segment:country:promo - 1 +
             (price + media + promo + 0| customer_segment)  + 
             (price + media + promo + 0| country),
           data = dataset)
summary(M5)
coef(M5)
fixef(M5)
ranef(M5)

Metrics::mape(dataset$sales, fitted(M5))
plot(dataset$sales, fitted(M5))
abline(a=0,b=1)

plot(fitted(M5), residuals(M5))

coef(M5)$customer_segment["total", ]    + coef(M5)$country["IT", ]
coef(M5)$customer_segment["existing", ] + coef(M5)$country["IT", ]
coef(M5)$customer_segment["new", ]      + coef(M5)$country["IT", ]
coef(M5)$customer_segment["vip", ]      + coef(M5)$country["IT", ]
coef(M5)$customer_segment["total", ]    + coef(M5)$country["GE", ]
coef(M5)$customer_segment["existing", ] + coef(M5)$country["GE", ]
coef(M5)$customer_segment["new", ]      + coef(M5)$country["GE", ]
coef(M5)$customer_segment["vip", ]      + coef(M5)$country["GE", ]
