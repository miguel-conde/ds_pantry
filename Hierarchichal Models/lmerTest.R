library(tidyverse)
library(lmerTest)

mydata <- read.csv("https://web.archive.org/web/20160624172041if_/http://www-personal.umich.edu/~bwest/classroom.csv")
# (the data is no longer at `http://www-personal.umich.edu/~bwest/classroom.csv`
# hence the link to web.archive.org)


# Crossed version:
crossed_model <- lmer(mathgain ~ (1 | schoolid) + (1 | classid), mydata)
summary(crossed_model)

# Nested version:
nested_model <- lmer(mathgain ~ (1 | schoolid/classid), mydata)
summary(nested_model)

