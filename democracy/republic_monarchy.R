library(tidyverse)
library(rvest)
library(countrycode)

# DATA --------------------------------------------------------------------


# Country ISO Codes -------------------------------------------------------

url <- "https://www.iban.com/country-codes"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

ctry_codes <- html_table(temp[1], fill = TRUE)[[1]] %>% 
  janitor::clean_names()

# Democracy Index ---------------------------------------------------------


url <- "https://en.wikipedia.org/wiki/Democracy_Index"

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
         region = as.factor(region),
         country_code = countryname(country, destination = 'iso3c'))


# Per Capita PPA ----------------------------------------------------------

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
  mutate(country_code = countryname(country, destination = 'iso3c'))
PPA_FMI_2019 <- PPA_FMI_2019 %>% 
  mutate(int_dollars = str_remove(int_dollars, ",")) %>% 
  mutate(int_dollars = as.numeric(int_dollars))
PPA_BM_2016  <- html_table(temp[4], fill = TRUE)[[1]]
PPA_CIA_2017 <- html_table(temp[5], fill = TRUE)[[1]]



# System of Government ----------------------------------------------------


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
rep_mon <- rep_mon  %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))

# Gini Index --------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_income_equality"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

gini <- html_table(temp[4], fill = TRUE)[[1]] %>% 
  janitor::clean_names() %>% 
  select(country, world_bank_gini_4) %>% 
  tail(-1) %>% 
  as_tibble()
names(gini)[2] <- c("gini_index")
gini <- gini %>% 
  mutate(gini_index = as.numeric(gini_index)) %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))

# Freedom Index -----------------------------------------------------------

# Population --------------------------------------------------------------

pop <- world_bank_pop %>% 
  gather(year, pop, -(1:2)) %>% 
  spread(indicator, pop) %>% 
  filter(year == "2017") 


url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

temp <- url %>% 
  html %>%
  html_nodes("table")

spans <- temp %>% 
  html_nodes(xpath = "//*/tr/td/span")

xml_remove(spans)

pop <- html_table(temp[1], fill = TRUE)[[1]] %>% 
  janitor::clean_names() %>% 
  select(country = country_or_dependent_territory, 
         pop = population,
         per_pop = percent_of_world ) %>% 
  as_tibble() %>% 
  mutate(pop = as.numeric(str_remove_all(pop, ",")),
         per_pop = as.numeric(str_remove_all(per_pop, "%")),
         country = str_remove_all(country, "\\[.*\\]")) %>% 
  mutate(country_code = countryname(country, destination = 'iso3c'))

# Final Dataset -----------------------------------------------------------


all_data <- DI_2019 %>% 
  full_join(PPA_FMI_2019, by = "country_code") %>% 
  full_join(rep_mon, by = "country_code") %>% 
  full_join(gini, by = "country_code") %>%
  full_join(pop, by = "country_code") %>%
  select(country_code, score, regime_type, const_form, int_dollars, 
         gini_index,
         pop,
         region) %>% 
  drop_na() %>% 
  left_join(ctry_codes %>% select(country, alpha_3_code),
            by = c("country_code" = "alpha_3_code")) %>% 
  mutate(const_form = ifelse(str_detect(const_form, "monarchy"), 
                             "monarchy", const_form)) %>% 
  mutate(const_form = ifelse(const_form == "Republic", 
                             "republic", const_form)) %>% 
  filter(const_form != "n/a") %>% 
  mutate(const_form = factor(const_form))


# EDA ---------------------------------------------------------------------




aov_all <- aov(int_dollars ~ regime_type*const_form, data = all_data)

summary(aov_all)

tukey_aov <- TukeyHSD(aov_all)
tukey_aov


# PPA / Democracy Index Plot ----------------------------------------------

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

colors <- c(
  "#FB1108", "#FD150B", "#FA7806", "#FBE426", "#FCFB8F",
  "#F3F5E7", "#C7E4EA", "#ABD6E6", "#9AD2E1"
)

all_data$color <- highcharter::colorize(log(as.numeric(all_data$const_form)+1), colors)
all_data <- all_data %>% 
  mutate(shape = ifelse(const_form == "monarchy", "circle", "square"))
x <- c("Country:", 
       "Democracy Index 2018:", 
       "Regime Type:", 
       "Const. Form:", 
       "Per Capita PPA (int$):",
       "Population: ")
y <- c("{point.country}", 
       "{point.score:.2f}",
       "{point.regime_type}",
       "{point.const_form}",
       "{point.int_dollars:.0f}",
       "{point.pop:.0f}")

tltip <- tooltip_table(x, y)

hchart(
  all_data,
  "scatter",
  hcaes(x = score, 
        y = int_dollars, 
        color = color,
        size = pop)#,
  # marker = list(symbol = "triangle")
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
  ) # %>%
  # hc_size(
  #   height = 700
  # )

# PPA / Gimo Index Plot ---------------------------------------------------
hchart(
  all_data,
  "scatter",
  hcaes(
    x = gini_index, 
    y = int_dollars, 
    color = color,
    size = pop
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
    title = list(text = "Gini Index"),
    type = "logarithmic",
    gridLineWidth = 0,
    reversed = TRUE
  ) %>%
  hc_yAxis(
    title = list(text = "Per Capita PPA (international $)"),
    type = "logarithmic", 
    gridLineWidth = 0
  ) %>%
  hc_title(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "Income Equality, Constitutional Form and Economic Development"
  ) %>%
  hc_subtitle(
    style = list(color = hex_to_rgba("white", 0.5)),
    text = "(Logarithmic Axis)"
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  ) # %>%
# hc_size(
#   height = 700
# )


###

library(highcharter)

colors <- c(
  "#FB1108", "#FD150B", "#FA7806", "#FBE426", "#FCFB8F",
  "#F3F5E7", "#C7E4EA", "#ABD6E6", "#9AD2E1"
)

all_data$color <- highcharter::colorize(log(all_data$int_dollars), colors)
# all_data <- all_data %>% 
#   mutate(shape = ifelse(const_form == "monarchy", "circle", "square"))

n <- 4

stops <- data.frame(
  q = 0:n/n,
  # c = colors[c(1,3,5,7,9)],
  c = colors,
  stringsAsFactors = FALSE
)

stops <- list_parse2(stops)
stops <- color_stops(5, colors)

x <- c("Country:", 
       "Democracy Index 2018:", 
       "Regime Type:", 
       "Const. Form:", 
       "Per Capita GDP (PPA int$):",
       "Gini Index:",
       "Population: ")
y <- c("{point.country}", 
       "{point.score:.2f}",
       "{point.regime_type}",
       "{point.const_form}",
       "{point.int_dollars:.0f}",
       "{point.gini_index:.2f}",
       "{point.pop:.0f}")

tltip <- tooltip_table(x, y)

highchart() %>% 
  hc_add_series(all_data %>% filter(const_form == "monarchy"), 
                hcaes(x = score, # Democracy Index
                      y = gini_index, 
                      color = color, 
                      # value = all_data %>% 
                      #   filter(const_form == "monarchy") %>% 
                      #   pull(int_dollars) %>% 
                      #   log(),
                      size = pop),
                marker = list(symbol = "square"),
                type = "scatter",
                name = "Monarchies") %>% 
  hc_add_series(all_data %>% filter(const_form == "republic"), 
                hcaes(x = score, 
                      y = gini_index, 
                      color = color, 
                      # value = all_data %>% 
                      #   filter(const_form == "republic") %>% 
                      #   pull(int_dollars) %>% 
                      #   log(),
                      size = pop),
                marker = list(symbol = "circle"),
                type = "scatter",
                name = "Republics") %>%
  hc_chart(
    backgroundColor = "black",
    zoomType = "xy"# ,
    # backgroundColor = hex_to_rgba("black", 0.5),
    # divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
  ) %>% 
  hc_plotOptions(
    series = list(stickyTracking = FALSE)
  ) %>%
  hc_xAxis(
    title = list(text = "Democracy Index 2018", style = list(color = "white")),
    # type = "logarithmic",
    gridLineWidth = 0
  ) %>%
  hc_yAxis(
    title = list(text = "Gini Index", style = list(color = "white")),
    # type = "logarithmic", 
    gridLineWidth = 0,
    reversed = TRUE
  ) %>%
  hc_title(
    # style = list(color = hex_to_rgba("white", 0.5)),
    style = list(color = "white"),
    text = "Comparing Republics vs. Monarchies"
  ) %>%
  hc_subtitle(
    # style = list(color = hex_to_rgba("white", 0.5)),
    style = list(color = "white"),
    text = "Democracy Level, Income Equality and Economic Development"
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  ) %>% 
  hc_colorAxis(type = "logarithmic",
               stops = stops) %>%
  hc_legend(enabled = TRUE,
            # title = "Per Capita GDP (PPA international dollars)"#,
            title = list(text = "Per Capita GDP (PPA international $)"),
            # color = hex_to_rgba("white", 0.5),
            color = "white",
            # layout = "vertical", 
            # align = "right", 
            verticalAlign = "top"#,
            # floating = TRUE
            # bubbleLegend = list(enabled = TRUE, color = '#e4d354')
            ) %>% 
  hc_add_theme(hc_theme_monokai())
  # hc_add_theme(hc_theme_chalk())
  # hc_add_theme(hc_theme_flatdark())
  # hc_add_theme(hc_theme_darkunica())
# ANALYSIS ----------------------------------------------------------------

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
