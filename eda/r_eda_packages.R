library(tidyverse)

library(readr)

# read csv file
df <- 
  read_delim(
    file = "data/cardio_train.csv",
    col_types = "iifidiiffffff",
    delim=";")


# pre-processing
df <- 
  # remove the id
  select(df, -id) %>%
  # age: days -> years
  mutate(age = round(age / 365))


# observe first rows
head(df)

summary(df)


# DataExplorer ------------------------------------------------------------

# install.packages("DataExplorer")
library(DataExplorer)


df %>%
  create_report(
    output_file = paste("eda/Report",
                        format(Sys.time(), "%Y-%m-%d %H%M%S %Z"),
                        sep=" - "),
    report_title = "EDA Report - Cardiovascular Disease Dataset",
    y = "cardio"
  )


# GGally ------------------------------------------------------------------

#install.packages("GGally")
library(GGally)

# change plot size (optional)
options(repr.plot.width = 20, repr.plot.height = 10)

df %>% 
  select("age", "cholesterol", "height", "weight") %>%
  ggpairs(mapping = aes(color = df$cardio, alpha = 0.5))


# SmartEDA ----------------------------------------------------------------

# install.packages("SmartEDA")
library(SmartEDA)

# similarly, with dplyr syntax: df %>% ExpReport(...)
ExpReport(
  df,
  Target="cardio",
  label=NULL,
  op_file="eda/dReport.html",
  op_dir=getwd())


# tableone ----------------------------------------------------------------

df2 <- 
  df %>%
  # modify factor levels
  mutate_at(c("cholesterol", "gluc"), ~ recode(.,
                                               "1" = "normal",
                                               "2" = "above normal", 
                                               "3" = "well above normal")
  ) %>%
  mutate_at(c("smoke", "alco", "active", "cardio"), ~ recode(.,
                                                             "0" = "no",
                                                             "1" = "yes")
  ) %>%
  mutate_at(c("gender"), ~ recode(.,
                                  "1" = "woman",
                                  "2" = "male")
  ) %>%
  # rename columns
  rename(
    "systolic blood pressure" = "ap_hi",
    "diastolic blood pressure" = "ap_lo",
    "glucose" = "gluc",
    "smoking" = "smoke",
    "alcohol" = "alco",
    "physical activity" = "active",
    "cardiovascular disease" = "cardio"
  )


head(df2)

# install.packages("tableone")
library(tableone)

# we perform a stratification based on the presence of cardiovascular disease
tableOne <- CreateTableOne(vars = colnames(select(df2, -"cardiovascular disease")), 
                           strata = c("cardiovascular disease"), 
                           data = df2)


# we pass a list of continuous variables not normally distributed in the "nonnormal" argument
print(
  tableOne,
  nonnormal = c("age", "weight", "height", "systolic blood pressure", "diastolic blood pressure"),
  showAllLevels = TRUE)

# categorical part only
tableOne$CatTable


# Curiosity ---------------------------------------------------------------

# install.packages("dlstats")
library("dlstats")

stats <- cran_stats(c("SmartEDA", "DataExplorer", "tableone", "GGally", "Hmisc", 
                      "exploreR", "dlookr", "desctable", "summarytools"))

stats %>%
  filter(start >= "2021-01-01" & end < "2022-01-01") %>%
  select(package, downloads) %>%
  group_by(package) %>% 
  summarize(downloads = sum(downloads)) %>%
  arrange(desc(downloads))

# log10 scale is used as the difference between downloads is comparatively large
ggplot(stats, aes(end, log10(downloads), group=package, color=package)) +
  geom_line()


# dlookr ------------------------------------------------------------------

# install.packages("dlookr")
library("dlookr")

df %>%
  eda_web_report(target = "cardio", subtitle = "heartfailure", 
                 output_dir = ".", output_file = "EDA.html", theme = "blue")
