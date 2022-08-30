# https://psyteachr.github.io/stat-models-v1/index.html


library("lme4")
library("tidyverse")

?sleepstudy

sleepstudy

just_308 <- sleepstudy %>%
  filter(Subject == "308")

ggplot(just_308, aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9)


sleepstudy %>% ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  facet_wrap(~ Subject) +
  scale_x_continuous(breaks = 0:9)

sleep2 <- sleepstudy %>% 
  mutate(days_deprived = Days - 2) %>% 
  filter(days_deprived >= 0)

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

## Complete Pooling
cp_model <- lm(Reaction ~ days_deprived, sleep2)

summary(cp_model)

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_abline(intercept = coef(cp_model)[1],
              slope = coef(cp_model)[2],
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

## No pooling
np_model <- lm(Reaction ~ days_deprived + Subject + days_deprived:Subject,
               data = sleep2)

summary(np_model)

all_intercepts <- c(coef(np_model)["(Intercept)"],
                    coef(np_model)[3:19] + coef(np_model)["(Intercept)"])

all_slopes  <- c(coef(np_model)["days_deprived"],
                 coef(np_model)[20:36] + coef(np_model)["days_deprived"])

ids <- sleep2 %>% pull(Subject) %>% levels() %>% factor()

# make a tibble with the data extracted above
np_coef <- tibble(Subject = ids,
                  intercept = all_intercepts,
                  slope = all_slopes)

np_coef

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_abline(data = np_coef,
              mapping = aes(intercept = intercept,
                            slope = slope),
              color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

# If we want to test the null hypothesis that the fixed slope is zero, we could do using a one-sample test.

np_coef %>% pull(slope) %>% t.test()

## Partial pooling
pp_mod <- lmer(Reaction ~ days_deprived + (days_deprived | Subject), sleep2)

summary(pp_mod)

newdata <- crossing(
  Subject = sleep2 %>% pull(Subject) %>% levels() %>% factor(),
  days_deprived = 0:7)

head(newdata, 17)

newdata2 <- newdata %>%
  mutate(Reaction = predict(pp_mod, newdata))

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_line(data = newdata2,
            color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:7) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")

## FIXED EFFECTS

fixef(pp_mod)

# Std Errors
sqrt(diag(vcov(pp_mod)))
# vcov(pp_mod) %>% diag() %>% sqrt()

# OR, equivalently using pipes:

# t-values
tvals <- fixef(pp_mod) / sqrt(diag(vcov(pp_mod)))

tvals

# Associated p-values
2 * (1 - pnorm(abs(tvals)))

# Confidence intervals
confint(pp_mod)

## RANDOM EFFECTS
sigma(pp_mod) # residual

# variance-covariance matrix for random factor Subject
VarCorr(pp_mod)[["Subject"]] # equivalently: VarCorr(pp_mod)[[1]]

diag(VarCorr(pp_mod)[["Subject"]]) # just the variances

attr(VarCorr(pp_mod)[["Subject"]], "correlation")[1, 2] # the correlation

# directly compute correlation from variance-covariance matrix
mx <- VarCorr(pp_mod)[["Subject"]]

## if cov = rho * t00 * t11, then
## rho = cov / (t00 * t11).
mx[1, 2] / (sqrt(mx[1, 1]) * sqrt(mx[2, 2]))

ranef(pp_mod)[["Subject"]]

?`merMod-class`

mutate(sleep2,
       fit = fitted(pp_mod),
       resid = residuals(pp_mod)) %>%
  group_by(Subject) %>%
  slice(c(1,10)) %>%
  print(n = +Inf)

## create the table with new predictor values
ndat <- crossing(Subject = sleep2 %>% pull(Subject) %>% levels() %>% factor(),
                 days_deprived = 8:10) %>%
  mutate(Reaction = predict(pp_mod, newdata = .))

ggplot(sleep2, aes(x = days_deprived, y = Reaction)) +
  geom_line(data = bind_rows(newdata2, ndat),
            color = 'blue') +
  geom_point() +
  scale_x_continuous(breaks = 0:10) +
  facet_wrap(~Subject) +
  labs(y = "Reaction Time", x = "Days deprived of sleep (0 = baseline)")
