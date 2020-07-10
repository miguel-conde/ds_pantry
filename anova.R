library(tidyverse)
library(lsr)

load(here::here("data", "clinicaltrial.Rdata"))

clin.trial

contrasts(clin.trial$drug)
options()

aggregate( mood.gain ~ drug + therapy, clin.trial, mean )
xtabs( ~ drug + therapy, clin.trial )

# H0s: ROW (drug) MEANS are the SAME _AND_ COL MEANS (therapy) are the same
drug.anova <- aov(mood.gain ~ drug + therapy, clin.trial)
summary(drug.anova)

# Estimated group means
effects::Effect(c("drug", "therapy"), drug.anova)
effects::effect(c("drug*therapy"), drug.anova)

# (Look like: 
aggregate( mood.gain ~ drug + therapy, clin.trial, mean ) %>% 
  as_tibble() %>% 
  group_by(drug, therapy) %>% 
  summarise(mood.gain = mean(mood.gain)) %>% 
  spread(therapy, mood.gain)
# )

# Marginals means:
effects::effect(c("drug"), drug.anova)
effects::effect(c("therapy"), drug.anova)

# Contrasts
contrasts(clin.trial$therapy)
contrasts(clin.trial$drug)

clin.trial.2 <- model.matrix( ~ ., data = clin.trial) %>% 
  as.data.frame()

summary(lm(mood.gain ~ ., clin.trial))

## LM like ANOVA
# 1 - Drug
drug.regression <- lm( mood.gain ~ druganxifree + drugjoyzepam + therapyCBT, clin.trial.2 )
summary( drug.regression )

summary(lm(mood.gain ~ therapy, clin.trial))
no.drug.regresion <- lm(mood.gain ~ therapyCBT, clin.trial.2)
summary(no.drug.regresion)

# Compare (for drug):
anova(no.drug.regresion, drug.regression)

# To:
summary(aov(mood.gain ~ drug + therapy, clin.trial))
# (F-satistic, p-value, dfs)

# 2 - Therapy
therapy.regression <- lm( mood.gain ~ therapyCBT, clin.trial.2 )
summary( therapy.regression )

summary(lm(mood.gain ~ therapy, clin.trial))
no.therapy.regresion <- lm(mood.gain ~ druganxifree + drugjoyzepam + therapyCBT, clin.trial.2)
summary(no.drug.regresion)

# Compare (for Therapy):
anova(therapy.regression, no.therapy.regresion)

# To:
summary(aov(mood.gain ~ drug + therapy, clin.trial))
