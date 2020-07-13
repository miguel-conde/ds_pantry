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


# COOKBOOK ----------------------------------------------------------------


# 1-Way -------------------------------------------------------------------

boxplot(mpg ~ factor(gear), data = mtcars,
        xlab = "gear", ylab = "mpg")

# Applies a Welch correction to address the nonhomogeneity of a 
# variance:
oneway.test(mpg ~ factor(gear), data = mtcars)

mtcars_aov <- aov(mpg ~ factor(gear), data = mtcars)
summary(mtcars_aov)

model.tables(mtcars_aov, "means")
model.tables(mtcars_aov, "effects")

mtcars_posthoc <- TukeyHSD(mtcars_aov)
mtcars_posthoc
plot(mtcars_posthoc)


# 2-Way -------------------------------------------------------------------

old_par <- par(mfrow = c(1,2))
boxplot(mpg ~ factor(gear), data = subset(mtcars, am == 0),
        xlab = "gear", ylab = "mpg",
        main = "automatic")
boxplot(mpg ~ factor(gear), data = subset(mtcars, am == 1),
        xlab = "gear", ylab = "mpg",
        main = "manual")
par(old_par)

boxplot(mpg ~ factor(gear) * factor(am), data = mtcars,
        xlab = "gear * transmission", ylab = "mpg",
        main = "mpg by gear * transmission")

# Interaction plot
interaction.plot(mtcars$gear, mtcars$am,
                 response = mtcars$mpg,
                 type = "b",
                 col = 1:3,
                 leg.bty = "o",
                 leg.bg = "beige",
                 lwd = 2, pch = c(18, 24, 22),
                 xlab = "Number of Gears",
                 ylab = "Mean Miles per Gallon",
                 main = "Interaction Plot")

mtcars_2_aov <- aov(mpg ~ factor(gear)*factor(am), 
                    data = mtcars)
summary(mtcars_2_aov)

model.tables(mtcars_2_aov, "means")
model.tables(mtcars_2_aov, "effects")

mtcars_posthoc_2 <- TukeyHSD(mtcars_2_aov)
mtcars_posthoc_2

old_par <- par(mfrow = c(1,2))
plot(mtcars_posthoc_2)
par(old_par)

# ANOVA and LM ------------------------------------------------------------

mtcars_lm <- lm(mpg ~ factor(gear)*factor(am), 
                data = mtcars)
summary(mtcars_lm)

summary(aov(mtcars_lm))
