source("Hierarchichal Models/contribs_lmer.R", encoding = "utf8")


# MODELS -----------------------------------------------------------------
dt <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
                 header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) %>% 
  as_tibble()
# data was previously publicly available from
# http://researchsupport.unt.edu/class/Jon/R_SC/Module9/lmm.data.txt
# but the link is now broken

###
dt$classID <- paste(dt$school, dt$class, sep=".")


# Intercept only ----------------------------------------------------------

m0 <- lmer(extro ~ 1 + (1 | school/classID), data = dt)
summary(m0)

# Varying Intercept only --------------------------------------------------
m2 <- lmer(extro ~ open + agree + social + (1 | school/classID), data = dt)
summary(m2)

m3 <- lmer(extro ~ open + agree + social + (1 | school) + (1 |classID), data = dt)
summary(m3)

# Varying intercept & slope -----------------------------------------------

m_i_s <- lmer(extro ~ open + agree + social + (open + 1 | school/classID), data = dt)
summary(m_i_s)
confint(m_i_s)

# Con interacción
m_int <- lmer(extro ~ open + open:school + agree + social + (open + 1 | school/classID), data = dt)


# TESTS -------------------------------------------------------------------


# Intercept only ----------------------------------------------------------
get_mlmer_contribs2(m0, pred = TRUE)

predict(m0) %>% head()

####

# Con FE, sin RE
get_mlmer_contribs(m0, re.form = NA)

# Con FE, solo RE del modelo a mayor nivel, school
get_mlmer_contribs(m0, re.form = ~ (1 | school))

# Con FE, solo RE del modelo del nivel inferior anidado, classID dentro de school
get_mlmer_contribs(m0, re.form = ~ (1 | classID:school))

# Con FE, todos los RE
get_mlmer_contribs(m0)
get_mlmer_contribs(m0, re.form = NULL)
get_mlmer_contribs(m0, re.form = ~ (1 | school/classID))

####

# Sin FE, sin RE
get_mlmer_contribs(m0, re.form = NA, random.only = TRUE)

# Sin FE, solo RE del modelo a mayor nivel, school
get_mlmer_contribs(m0, re.form = ~ (1 | school), random.only = TRUE)

# Sin FE, solo RE del modelo del nivel inferior anidado, classID dentro de school
get_mlmer_contribs(m0, re.form = ~ (1 | classID:school), random.only = TRUE)

# Sin FE, todos los RE
get_mlmer_contribs(m0, random.only = TRUE)
get_mlmer_contribs(m0, re.form = NULL, random.only = TRUE)
get_mlmer_contribs(m0, re.form = ~ (1 | school/classID), random.only = TRUE)

# Intercept & slope -------------------------------------------------------

get_mlmer_contribs(m_i_s, pred = TRUE)

predict(m_i_s) %>% head()

####

# Con FE, sin RE
get_mlmer_contribs(m_i_s, re.form = NA)

# Con FE, solo RE del modelo a mayor nivel, school
get_mlmer_contribs(m_i_s, re.form = ~ (open + 1 | school))

# Con FE, solo RE del modelo del nivel inferior anidado, classID dentro de school
get_mlmer_contribs(m_i_s, re.form = ~ (open + 1 | classID:school))

# Con FE, todos los RE
get_mlmer_contribs(m_i_s)
get_mlmer_contribs(m_i_s, re.form = NULL)
get_mlmer_contribs(m_i_s, re.form = ~ (open + 1 | school/classID))

####

# Sin FE, sin RE
get_mlmer_contribs(m_i_s, re.form = NA, random.only = TRUE)

# Sin FE, solo RE del modelo a mayor nivel, school
get_mlmer_contribs(m_i_s, re.form = ~ (open + 1 | school), random.only = TRUE)

# Sin FE, solo RE del modelo del nivel inferior anidado, classID dentro de school
get_mlmer_contribs(m_i_s, re.form = ~ (open + 1 | classID:school), random.only = TRUE)

# Sin FE, todos los RE
get_mlmer_contribs(m_i_s, random.only = TRUE)
get_mlmer_contribs(m_i_s, re.form = NULL, random.only = TRUE)
get_mlmer_contribs(m_i_s, re.form = ~ (open + 1 | school/classID), random.only = TRUE)


# Interacciones -----------------------------------------------------------

get_mlmer_contribs(m_int, pred = TRUE)

predict(m_int) %>% head()

####

# Con FE, sin RE
get_mlmer_contribs(m_int, re.form = NA)

# Con FE, solo RE del modelo a mayor nivel, school
get_mlmer_contribs(m_int, re.form = ~ (open + 1 | school))

# Con FE, solo RE del modelo del nivel inferior anidado, classID dentro de school
get_mlmer_contribs(m_int, re.form = ~ (open + 1 | classID:school))

# Con FE, todos los RE
get_mlmer_contribs(m_int)
get_mlmer_contribs(m_int, re.form = NULL)
get_mlmer_contribs(m_int, re.form = ~ (open + 1 | school/classID))

####

# Sin FE, sin RE
get_mlmer_contribs(m_int, re.form = NA, random.only = TRUE)

# Sin FE, solo RE del modelo a mayor nivel, school
get_mlmer_contribs(m_int, re.form = ~ (open + 1 | school), random.only = TRUE)

# Sin FE, solo RE del modelo del nivel inferior anidado, classID dentro de school
get_mlmer_contribs(m_int, re.form = ~ (open + 1 | classID:school), random.only = TRUE)

# Sin FE, todos los RE
get_mlmer_contribs(m_int, random.only = TRUE)
get_mlmer_contribs(m_int, re.form = NULL, random.only = TRUE)
get_mlmer_contribs(m_int, re.form = ~ (open + 1 | school/classID), random.only = TRUE)

