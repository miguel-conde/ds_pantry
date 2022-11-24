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
re2_model_matrix <- getME(m2, "Z")
re_model_matrix <- getME(m2, "mmList")

preds <- 
  # Fixed effects
  fe_model_matrix  %*% as.matrix(fixef(m2)) +
  # Random effects, nivel superior (school)
  rowSums(re_model_matrix$`1 | school` %*% t(as.matrix(ranef(m2)$school))) +
  # Random effects, nivel inferior anidado, classID dentro de school
  rowSums(re_model_matrix$`1 | classID:school` %*% t(ranef(m2)$`classID:school`))

head(preds)
fitted(m2) %>% head()

### ESTA ES LA BUENA
the_re_model_matrix <- as.matrix(getME(m2, "Z"))
dim(the_re_model_matrix)
colnames(the_re_model_matrix)

the_fe_coef_vector <- as.matrix(fixef(m2)) 
dim(the_fe_coef_vector)

# Con esto puedo coger random effects nivel por nivel
# the_re_coef_vector <- rbind(as.matrix(ranef(m2)$school), 
#                          as.matrix(ranef(m2)$`classID:school`))
the_re_coef_vector <- as.matrix(bind_rows(lapply(ranef(m2), as.data.frame)))
the_re_coef_vector <- the_re_coef_vector[colnames(the_model_matrix),,drop = FALSE]
dim(the_re_coef_vector)
colnames(the_re_coef_vector)

preds2 <- 
  # Fixed effects
  # [1200 x 4] * [4 x 1] = [1200 x 1]
  fe_model_matrix %*% the_fe_coef_vector +
  # Random effects, nivel inferior anidado, classID dentro de school
  # [1200 x 30] * [30 x 1] = [1200 x 1]
  the_re_model_matrix %*% the_re_coef_vector

head(preds2)
fitted(m2) %>% head()


# INTERCEPT + SLOPE -------------------------------------------------------

m_i_s <- lmer(extro ~ open + agree + social + (open + 1 | school/classID), data = dt)
summary(m_i_s)
confint(m_i_s)


# Intento 1 ---------------------------------------------------------------

## FIXED

# FE COEF VECTOR
the_fe_coef_vector <- as.matrix(fixef(m_i_s))
dim(the_fe_coef_vector)

# FE MODEL MATRIX
fe_model_matrix <- getME(m_i_s, "X")
dim(fe_model_matrix) # [1200 obs x 4 variables]
colnames(fe_model_matrix)
head(fe_model_matrix)

## RANDOM

# RE COEF VECTOR
the_re_coef_vector <- as.matrix(bind_rows(lapply(ranef(m_i_s), as.data.frame)))
dim(the_re_coef_vector) # [30 grupos x 2 variables]
colnames(the_re_coef_vector)
rownames(the_re_coef_vector)
head(the_re_coef_vector)

# RE MODEL MATRIX
the_re_model_matrix <- as.matrix(getME(m_i_s, "Z"))
dim(the_re_model_matrix) # [1200 obs x 60]
colnames(the_re_model_matrix)
head(the_re_model_matrix)


# Intento 2 ---------------------------------------------------------------

## Nivel superior school
the_re_coef_vector_school <- as.matrix(ranef(m_i_s)[["school"]])
dim(the_re_coef_vector_school) # [6 grupos x 2 variables]
colnames(the_re_coef_vector_school)
rownames(the_re_coef_vector_school)
head(the_re_coef_vector_school)

the_re_model_matrix_school <- as.matrix(getME(m_i_s, "mmList")[["open + 1 | school"]])
dim(the_re_model_matrix_school) # [1200 obs x 2 variables]
colnames(the_re_model_matrix_school)
head(the_re_model_matrix_school)

# 1200 predicciones x 6 grupos
the_re_model_matrix_school %*% t(the_re_coef_vector_school)

dt %>% 
  select(open, school) %>% 
  mutate(beta_intcpt = the_re_coef_vector_school[school, "(Intercept)"]) %>% 
  mutate(beta_open = the_re_coef_vector_school[school, "open"]) %>% 
  mutate(contrib_intcpt = beta_intcpt, contrib_open = beta_open * open) %>% 
  mutate(pred = contrib_intcpt + contrib_open) %>% 
  head()


## Nivel inferior ClassID anidado en school
the_re_coef_vector_classID <- as.matrix(ranef(m_i_s)[["classID:school"]])
dim(the_re_coef_vector_classID) # [24 grupos x 2 variables]
colnames(the_re_coef_vector_classID)
rownames(the_re_coef_vector_classID)
head(the_re_coef_vector_classID)

the_re_model_matrix_classID <- as.matrix(getME(m_i_s, "mmList")[["open + 1 | classID:school"]])
dim(the_re_model_matrix_classID) # [1200 obs x 2 variables]
colnames(the_re_model_matrix_classID)
head(the_re_model_matrix_classID)

# 1200 predicciones x 24 grupos
the_re_model_matrix_classID %*% t(the_re_coef_vector_classID)

dt %>% 
  select(open, school, classID) %>% 
  mutate(beta_intcpt = the_re_coef_vector_classID[paste0(classID, ":", school), "(Intercept)"]) %>% 
  mutate(beta_open = the_re_coef_vector_classID[paste0(classID, ":", school), "open"]) %>% 
  mutate(contrib_intcpt = beta_intcpt, contrib_open = beta_open * open) %>% 
  mutate(pred = contrib_intcpt + contrib_open) %>% 
  head()


# Intento 3 ---------------------------------------------------------------

fe_coefs <- as.matrix(fixef(m_i_s))
re_coefs <- lapply(ranef(m_i_s), as.data.frame) %>% bind_rows() %>% t()
 
f <- extro ~ open + agree + social + (open + 1 | school/classID)
extro ~ open + agree + social + (open + 1 | school/classID)
mm <- model.matrix(extro ~ open + agree + social + school + classID:school, dt)

idx_no <- which(apply(mm, 2, sum) == 0)
mm <- mm[, -idx_no]
colnames(mm) %>% str_remove("school") %>% str_remove("classID")


terms(f)
attributes(terms(f))$term.labels

all.vars(terms(f))
all.vars(delete.response(terms(f)))
lme4:::findbars(f)



# Xb 
fix <- getME(m_i_s,'X') %*% fixef(m_i_s)
# Zu
ran <- t(as.matrix(getME(m_i_s,'Zt'))) %*% unlist(ranef(m_i_s))
# Xb + Zu
fixran <- fix + ran

head(cbind(fix, ran, fixran, fitted(fm1)))


# Intento 4 ---------------------------------------------------------------

re_mm <- lme4:::model.matrix.merMod(m_i_s, "randomListRaw") 
re <- ranef(m_i_s)

re$`classID:school` %>% dim()
re$school %>% dim()

re_mm$`open + 1 | classID:school` %>% dim()
re_mm$`open + 1 | school` %>% dim()


# Intento 5 ---------------------------------------------------------------

X <- getME(m_i_s,'X')
beta <- fixef(m_i_s)

contribs_fe <- X %>% sweep(2, beta, "*")
dim(contribs_fe)
head(contribs_fe)
colnames(contribs_fe)

Z <- as.matrix(getME(m_i_s, "Z"))
colnames(Z)
b <- ranef(m_i_s) %>% lapply(function(x) apply(x, 1, function(y) y)) %>% unlist()
names(b)

contribs_re <- Z %>% sweep(2, b, "*")
dim(contribs_re)
head(contribs_re)
colnames(contribs_re)

preds <- rowSums(contribs_fe) + rowSums(contribs_re)
head(preds)

head(fitted(m_i_s))

###
contribs_re <- contribs_re %>% apply(1, function(x) x[x!=0]) %>% t()
colnames(contribs_re) <- ranef(m_i_s) %>% 
  lapply(names) %>% 
  unlist %>% 
  as.data.frame() %>% 
  rename(estimate = 1) %>% 
  rownames_to_column("var") %>% 
  unite(name_var, 1,2) %>% 
  unlist() %>%
  as.vector()

(rowSums(contribs_fe) + rowSums(contribs_re)) %>% head()

###
colnames(contribs_fe)
colnames(contribs_re)

cbind(contribs_fe, contribs_re) %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(pred = sum(c_across(1:8))) %>% 
  ungroup()

# Function ----------------------------------------------------------------

xxx <- function(lmer_mod) {
  
  X <- getME(lmer_mod,'X')
  beta <- fixef(lmer_mod)
  
  contribs_fe <- X %>% sweep(2, beta, "*")
  
  Z <- as.matrix(getME(lmer_mod, "Z"))
  b <- ranef(lmer_mod) %>% lapply(function(x) apply(x, 1, function(y) y)) %>% unlist()
  
  ### 
  contribs_re_raw <- Z %>% sweep(2, b, "*")

  contribs_re <- contribs_re_raw %>% apply(1, function(x) x[x!=0]) %>% t()
  colnames(contribs_re) <- ranef(lmer_mod) %>% 
    lapply(names) %>% 
    unlist %>% 
    as.data.frame() %>% 
    rename(estimate = 1) %>% 
    rownames_to_column("var") %>% 
    unite(name_var, var, estimate) %>% 
    unlist() %>%
    as.vector()
  
}

# merTools ----------------------------------------------------------------

library(merTools)

predictInterval(m_i_s)   # for various model predictions, possibly with new data

REsim(m_i_s)             # mean, median and sd of the random effect estimates

plotREsim(REsim(m_i_s))  # plot the interval estimates


# Predict RE --------------------------------------------------------------

# Predicción del modelo completo, classID anidado en school
predict(m_i_s) %>% head()
predict(m_i_s, re.form = ~ (open + 1 | school/classID)) %>% head()

# Predicción del modelo de nivel más alto, school
predict(m_i_s, re.form = ~ (open + 1 | school)) %>% head()

# Predicción del modelo de nivel más bajo, classID anidado en school
predict(m_i_s, re.form = ~ (open + 1 |classID:school)) %>% head()

# También podemos quitar los fixed effects
predict(m_i_s, random.only = TRUE) %>% head()
predict(m_i_s, random.only = TRUE, re.form = ~ (open + 1 | school/classID)) %>% head()
predict(m_i_s, random.only = TRUE, re.form = ~ (open + 1 | school)) %>% head()
predict(m_i_s, random.only = TRUE, re.form = ~ (open + 1 |classID:school)) %>% head()


get_mlmer_contribs <- function(in_model, new_data = NULL, ...) {
  
  if (is.null(new_data)) new_data <- in_model@frame
  
  model_vars <- coef(in_model) %>% 
    lapply(names) %>% 
    unlist() %>% 
    str_split(":") %>% 
    unlist() %>% 
    unique()
  
  out <- vector(mode = "list", length = length(model_vars) - 1)
  names(out) <- setdiff(model_vars, "(Intercept)")
  orig_pred <- predict(in_model, newdata = new_data, ...)
  for ( v in names(out)) {
    
    aux <- new_data %>% mutate(!!sym(v) := 0)
    out[[v]] = orig_pred - predict(in_model, newdata = aux, ...)
    
  }
  
  out <- bind_cols(out)
  
  if ("(Intercept)" %in% model_vars) {
    aux <- new_data %>% 
      mutate_if(is.numeric, ~ 0) # %>% 
      # mutate_if(is.factor, ~ levels(.)[1])
    out <- out %>% mutate(`(Intercept)` = predict(in_model, newdata = aux, ...),
                          .before = 1)
  }
  
  return(out)
}

get_mlmer_contribs(m_i_s) %>% 
  rowwise() %>% 
  mutate(pred = sum(c_across(1:4))) %>% 
  ungroup() %>% 
  head()

predict(m_i_s) %>% head