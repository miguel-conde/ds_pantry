# https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified
# https://www.muscardinus.be/2017/07/lme4-random-effects/
# https://stats.stackexchange.com/questions/487039/multiple-membership-vs-crossed-random-effects/487047#487047
# https://errickson.net/stats-notes/vizrandomeffects.html


library(tidyverse)
library(lme4)

rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)  


coef(fm1)
fixef(fm1)
ranef(fm1)


fixef(fm1)["(Intercept)"] + ranef(fm1)$Subject$`(Intercept)`
fixef(fm1)["Days"] + ranef(fm1)$Subject$Days

confint(fm1)

rc1 <- rescov(fm1, sleepstudy)
image(rc1)

###### https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified

mydata <- read.csv("https://web.archive.org/web/20160624172041if_/http://www-personal.umich.edu/~bwest/classroom.csv")
# (the data is no longer at `http://www-personal.umich.edu/~bwest/classroom.csv`
# hence the link to web.archive.org)


# Crossed version:
crossed_model <- lmer(mathgain ~ (1 | schoolid) + (1 | classid), mydata)
summary(crossed_model)

# Linear mixed model fit by REML ['lmerMod']
# Formula: mathgain ~ (1 | schoolid) + (1 | classid)
# Data: mydata
# 
# REML criterion at convergence: 11768.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.6441 -0.5984 -0.0336  0.5334  5.6335 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# classid  (Intercept)   99.23   9.961  
# schoolid (Intercept)   77.49   8.803  
# Residual             1028.23  32.066  
# Number of obs: 1190, groups:  classid, 312; schoolid, 107


# Nested version:
nested_model <- lmer(mathgain ~ (1 | schoolid/classid), mydata)
summary(nested_model)

# Formula: mathgain ~ (1 | schoolid/classid)
# 
# REML criterion at convergence: 11768.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.6441 -0.5984 -0.0336  0.5334  5.6335 
# 
# Random effects:
#   Groups           Name        Variance Std.Dev.
# classid:schoolid (Intercept)   99.23   9.961  
# schoolid         (Intercept)   77.49   8.803  
# Residual                     1028.23  32.066  
# Number of obs: 1190, groups:  classid:schoolid, 3

######
dt <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
                 header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
# data was previously publicly available from
# http://researchsupport.unt.edu/class/Jon/R_SC/Module9/lmm.data.txt
# but the link is now broken
xtabs(~ school + class, dt)

m0 <- lmer(extro ~ open + agree + social + (1 | school/class), data = dt)
summary(m0)

m1 <- lmer(extro ~ open + agree + social + (1 | school) + (1 |class), data = dt)
summary(m1)

###
dt$classID <- paste(dt$school, dt$class, sep=".")
xtabs(~ school + classID, dt)

m2 <- lmer(extro ~ open + agree + social + (1 | school/classID), data = dt)
summary(m2)

m3 <- lmer(extro ~ open + agree + social + (1 | school) + (1 |classID), data = dt)
summary(m3)

########
fe_model_matrix <- getME(m2, "X")
re_model_matrix <- getME(m2, "mmList")

preds <- fe_model_matrix  %*% as.matrix(fixef(m2)) +
  rowSums(re_model_matrix$`1 | classID:school` %*% t(ranef(m2)$`classID:school`)) +
  rowSums(re_model_matrix$`1 | school` %*% t(as.matrix(ranef(m2)$school)))

head(preds)
fitted(m2) %>% head()
