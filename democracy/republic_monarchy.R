library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Democracy_Index"

library(rvest)
temp <- url %>% 
  html %>%
  html_nodes("table")

DI_2019 <- html_table(temp[3])[[1]] %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  head(-1) %>% 
  mutate(rank = as.integer(rank)) %>% 
  mutate_at(vars(3:8), ~ as.numeric(.))
names(DI_2019) <- c("rank", 
                    "country", 
                    "score",                         
                    "electoral_process_and_pluralism",
                    "functioning_of_government",     
                    "political_participation",       
                    "political_culture", 
                    "civil_liberties", 
                    "regime_type",                    
                     "region", 
                     "changes_from_last_year")
DI_2019 <-  DI_2019 %>% 
  mutate(regime_type = factor(regime_type, levels = c("Authoritarian",
                                                      "Hybrid regime",
                                                      "Flawed democracy",
                                                      "Full democracy")),
         region = as.factor(region))

boxplot(score ~ region, DI_2019)

###
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

PPA_FMI_2019 <- html_table(temp[3], fill = TRUE)[[1]] %>% 
  janitor::clean_names()
names(PPA_FMI_2019) <- c("rank_ppa", "country", "int_dollars")
PPA_FMI_2019 <- PPA_FMI_2019 %>% 
  mutate(int_dollars = str_remove(int_dollars, ",")) %>% 
  mutate(int_dollars = as.numeric(int_dollars))
PPA_BM_2016  <- html_table(temp[4], fill = TRUE)[[1]]
PPA_CIA_2017 <- html_table(temp[5], fill = TRUE)[[1]]


### 
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_system_of_government"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

rep_mon <- html_table(temp[6], fill = TRUE)[[1]] %>% 
  janitor::clean_names()
names(rep_mon)[1:2] <- c("country", "const_form")

##

all_data <- DI_2019 %>% 
  full_join(PPA_FMI_2019, by = "country") %>% 
  full_join(rep_mon, by = "country") %>% 
  select(country, score, regime_type, const_form, int_dollars, region) %>% 
  drop_na() %>% 
  mutate(const_form = ifelse(str_detect(const_form, "monarchy"), 
                             "monarchy", const_form)) %>% 
  mutate(const_form = ifelse(const_form == "Republic", 
                             "republic", const_form)) %>% 
  filter(const_form != "n/a") %>% 
  mutate(const_form = factor(const_form))

aov_all <- aov(int_dollars ~ regime_type*const_form, data = all_data)

summary(aov_all)

tukey_aov <- TukeyHSD(aov_all)
tukey_aov

par(mfrow = c(1, 2))
plot(tukey_aov)
par(mfrow = c(1, 1))

plot(int_dollars ~ score, all_data, type = "p", col = const_form)
legend("topleft",
       legend = levels(all_data$const_form),
       col = levels(all_data$const_form) %>% as.numeric,
       bty = "n")

library(ggplot2)

p <- ggplot(aes(x = score, y = int_dollars, color = const_form),
            data = all_data) +
  geom_point() + geom_text(aes(label = country ), hjust = 0, vjust = 0) +
  labs(x = "Democracy Index 2018", 
       y = "Per Capita GDP (PPA International Dollars)")
p

## 
library(highcharter)
hchart(all_data, "scatter", hcaes(x = score, y = int_dollars, color = const_form))

colors <- c(
  "#FB1108", "#FD150B", "#FA7806", "#FBE426", "#FCFB8F",
  "#F3F5E7", "#C7E4EA", "#ABD6E6", "#9AD2E1"
)

all_data$color <- highcharter::colorize(log(as.numeric(all_data$const_form)+1), colors)

x <- c("Country:", "Democracy Index 2018:", "Regime Type:", "Const. Form:", "Per Capita PPA (int$):")
y <- c("{point.country}", 
       "{point.score:.2f}",
       "{point.regime_type}",
       "{point.const_form}",
       "{point.int_dollars:.2f}")

tltip <- tooltip_table(x, y)

hchart(
  all_data,
  "scatter",
  hcaes(
    x = score, 
    y = int_dollars, 
    color = color
  )
  # minSize = 2,
  # maxSize = 20
) %>%
  hc_chart(
    # backgroundColor = "black",
    zoomType = "xy",
    backgroundColor = hex_to_rgba("black", 0.5),
    divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
  ) %>% 
  hc_plotOptions(
    series = list(stickyTracking = FALSE)
    ) %>%
  hc_xAxis(
    title = list(text = "Democracy Index 2018"),
    type = "logarithmic",
    gridLineWidth = 0# ,
    # reversed = TRUE
  ) %>%
  hc_yAxis(
    title = list(text = "Per Capita PPA (international $)"),
    type = "logarithmic", 
    gridLineWidth = 0
  ) %>%
  hc_title(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "Democracy Level, Constitutional Form and Economic Development"
  ) %>%
  hc_subtitle(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "(Logarithmic Axis)"
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  ) %>%
  hc_size(
    height = 700
  )

## 
ftable <- table(all_data$regime_type, all_data$const_form)
mosaicplot(ftable)
chisq.test(ftable)

##
boxplot(score ~ const_form, all_data)
oneway.test(score ~ const_form, all_data)
all_data_aov <- aov(score ~ const_form, all_data)
summary(all_data_aov)
model.tables(all_data_aov, "means")
all_data_post_hoc <- TukeyHSD(all_data_aov)
all_data_post_hoc
plot(all_data_post_hoc)

##
interaction.plot(all_data$regime_type, all_data$const_form, all_data$int_dollars, type = "b")
