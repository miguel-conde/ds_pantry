library(tidyverse)

# https://www.rplumber.io/

# Plumber allows you to create a web API by merely decorating your existing 
# R source code with roxygen2-like comments. 

# https://towardsdatascience.com/how-to-make-rest-apis-with-r-a-beginners-guide-to-plumber-9be4cd8c8015

# https://www.restapitutorial.com/

library(gapminder)

#* @apiTitle Gapminder API
#* @apiDescription API for exploring Gapminder dataset


# Endpoint 1 — /countries -------------------------------------------------


# The idea behind this endpoint is that it should return countries and their 
# respective data after a couple of filters are applied. To be more precise, 
# this endpoint accepts parameter values for continent, life expectancy, and 
# population. Value for continent must be exact, and values for the other two 
# parameters filter data so that only rows with greater values are returned.


# These comments allow plumber to make your R functions available as API 
# endpoints. You can use either #* as the prefix or #', but we recommend the 
# former since #' will collide with roxygen2.

#* Returns countries that satisfy condition
#* @param in_continent
#* @param in_lifeExpGT Life expectancy greater than
#* @param in_popGT Population greater than
#* @get /countries
function(in_continent, in_lifeExpGT, in_popGT) {
  gapminder %>%
    filter(
      year == 2007,
      continent == in_continent,
      lifeExp > in_lifeExpGT,
      pop > in_popGT
    )
}


# Endpoint 2 — /plot ------------------------------------------------------

# The goal now is to return an image instead of raw data. The image will contain 
# a line plot made with ggplot2, showing life expectancy over time.
# Two parameters are required — country and chart title — both are 
# self-explanatory.
# If you want to return an image from an API with R, you’ll have to put the 
# following comment: 
#   #* @serializer contentType list(type='image/png'). 
# Everything else is more or less the same.
 
#* #* Returns a line plot of life expectancy for country
#* @param in_country
#* @param in_title Chart title
#* @get /plot
#* @serializer contentType list(type='image/png')
function(in_country, in_title) {
  subset <- gapminder %>%
    filter(country == in_country)
  plot <- ggplot(subset, aes(x = year, y = lifeExp)) +
    geom_line(color = "#0099f9", size = 2) +
    geom_point(color = "#0099f9", size = 5) +
    ggtitle(in_title) +
    theme_classic() +
    theme(aspect.ratio = 9 / 16)
  file <- "plot.png"
  ggsave(file, plot)
  readBin(file, "raw", n = file.info(file)$size)
}


# Endpoint 3 — /calculate_gdp ---------------------------------------------

# You’ll now learn how to work with POST methods (or any other sends data in 
# the request body). The goal is to create another endpoint that calculates 
# the total GDP for a specified country in the latest year (2007).

#* Returns most recent GDP for a country
#* @param in_country
#* @post /calculate_gdp
function(in_country) {
  gapminder %>%
    filter(
      year == 2007,
      country == in_country
    ) %>%
    summarize(gdp = pop * gdpPercap)
}



# library(plumber)
# # 'plumber.R' is the location of the file shown above
# pr("plumber.R") %>%
#   pr_run(port=8000)