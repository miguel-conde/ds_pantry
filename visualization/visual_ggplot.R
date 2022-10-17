library(tidyverse)

# REFERENCES --------------------------------------------------------------

# https://exts.ggplot2.tidyverse.org/gallery/
# https://github.com/jrnold/ggthemes
# vignette("ggplot2-specs") = https://cran.r-project.org/web/packages/ggplot2/vignettes/ggplot2-specs.html
# http://rstudio.com/resources/cheatsheets


# 1. DATA VISUALISATION ---------------------------------------------------

# https://r4ds.had.co.nz/data-visualisation.html


# First steps -------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# With ggplot2, you begin a plot with the function ggplot(). 
# ggplot() creates a coordinate system that you can add layers to. 
# The first argument of ggplot() is the dataset to use in the graph. 
# So ggplot(data = mpg) creates an empty graph, but it’s not very interesting 
# so I’m not going to show it here.

# You complete your graph by adding one or more layers to ggplot(). 
# The function geom_point() adds a layer of points to your plot, which creates a 
# scatterplot. ggplot2 comes with many geom functions that each add a different 
# type of layer to a plot. 

# Each geom function in ggplot2 takes a mapping argument. 
# This defines how variables in your dataset are mapped to visual properties. 
# The mapping argument is always paired with aes(), and the x and y arguments of 
# aes() specify which variables to map to the x and y axes. 
# ggplot2 looks for the mapped variables in the data argument, in this case, mpg.

# GRAPHING TEMPLATE
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))


# Aesthetic mappings ------------------------------------------------------

# An aesthetic is a visual property of the objects in your plot. Aesthetics 
# include things like the size, the shape, or the color of your points. 
# Since we already use the word “value” to describe data, let’s use the word 
# “level” to describe aesthetic properties.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# To map an aesthetic to a variable, associate the name of the aesthetic to the 
# name of the variable inside aes(). ggplot2 will automatically assign a unique 
# level of the aesthetic (here a unique color) to each unique value of the 
# variable, a process known as scaling. ggplot2 will also add a legend that 
# explains which levels correspond to which values.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# What happened to the SUVs? ggplot2 will only use six shapes at a time. By 
# default, additional groups will go unplotted when you use the shape aesthetic.

# For each aesthetic, you use aes() to associate the name of the aesthetic with 
# a variable to display. The aes() function gathers together each of the 
# aesthetic mappings used by a layer and passes them to the layer’s mapping 
# argument.
aes(x = displ, y = hwy, alpha = class)

# Once you map an aesthetic, ggplot2 takes care of the rest. It selects a 
# reasonable scale to use with the aesthetic, and it constructs a legend that 
# explains the mapping between levels and values. For x and y aesthetics, 
# ggplot2 does not create a legend, but it creates an axis line with tick marks 
# and a label. The axis line acts as a legend; it explains the mapping between 
# locations and values.

# Once you map an aesthetic, ggplot2 takes care of the rest. It selects a 
# reasonable scale to use with the aesthetic, and it constructs a legend that 
# explains the mapping between levels and values. For x and y aesthetics, 
# ggplot2 does not create a legend, but it creates an axis line with tick marks 
# and a label. The axis line acts as a legend; it explains the mapping between 
# locations and values.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# Here, the color doesn’t convey information about a variable, but only changes 
# the appearance of the plot. To set an aesthetic manually, set the aesthetic by 
# name as an argument of your geom function; i.e. it goes outside of aes(). 
# You’ll need to pick a level that makes sense for that aesthetic:

# The name of a color as a character string.
# 
# The size of a point in mm.
# 
# The shape of a point as a number, as shown in Figure 3.1.

# Exercises ---------------------------------------------------------------

# What’s gone wrong with this code? Why are the points not blue?
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

# Which variables in mpg are categorical? Which variables are continuous? 
# (Hint: type ?mpg to read the documentation for the dataset). How can you see 
# this information when you run mpg?
  
# Map a continuous variable to color, size, and shape. How do these aesthetics 
# behave differently for categorical vs. continuous variables?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = displ))
  
# What happens if you map the same variable to multiple aesthetics?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv, shape = drv))
  
# What does the stroke aesthetic do? What shapes does it work with? 
# (Hint: use ?geom_point)

vignette("ggplot2-specs")

sizes <- expand.grid(size = (0:3) * 2, stroke = (0:3) * 2)
ggplot(sizes, aes(size, stroke, size = size, stroke = stroke)) + 
  geom_abline(slope = -1, intercept = 6, colour = "white", size = 6) + 
  geom_point(shape = 21, fill = "red") +
  scale_size_identity()

ggplot(sizes, aes(size, stroke, size = size)) + 
  geom_abline(slope = -1, intercept = 6, colour = "white", size = 6) + 
  geom_point(shape = 21, fill = "red") +
  scale_size_identity()


# What happens if you map an aesthetic to something other than a variable name, 
# like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))


# Facets ------------------------------------------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ class)

# Exercises ---------------------------------------------------------------

# What happens if you facet on a continuous variable?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ hwy)
  
# What do the empty cells in plot with facet_grid(drv ~ cyl) mean? How do they 
# relate to this plot?
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# What plots does the following code make? What does . do?
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# Take the first faceted plot in this section:
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# What are the advantages to using faceting instead of the colour aesthetic? 
# What are the disadvantages? How might the balance change if you had a larger 
# dataset?
  
# Read ?facet_wrap. What does nrow do? What does ncol do? What other options 
# control the layout of the individual panels? Why doesn’t facet_grid() have 
# nrow and ncol arguments?
  
# When using facet_grid() you should usually put the variable with more unique 
# levels in the columns. Why?


# Geometric Objects -------------------------------------------------------

# A geom is the geometrical object that a plot uses to represent data. People 
# often describe plots by the type of geom that the plot uses. 

# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Every geom function in ggplot2 takes a mapping argument. However, not every 
# aesthetic works with every geom. 
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv))

# ggplot2 provides over 40 geoms, and extension packages provide even more 
# (see https://exts.ggplot2.tidyverse.org/gallery/ for a sampling).
#
# The best way to get a comprehensive overview is the ggplot2 cheatsheet, which 
# you can find at http://rstudio.com/resources/cheatsheets. 

# Grouping
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# Multiple geoms
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# This, however, introduces some duplication in our code. Imagine if you wanted 
# to change the y-axis to display cty instead of hwy. You’d need to change the 
# variable in two places, and you might forget to update one. You can avoid this 
# type of repetition by passing a set of mappings to ggplot(). ggplot2 will treat 
# these mappings as global mappings that apply to each geom in the graph. In other
# words, this code will produce the same plot as the previous code:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# If you place mappings in a geom function, ggplot2 will treat them as local 
# mappings for the layer. It will use these mappings to extend or overwrite the 
# global mappings for that layer only. This makes it possible to display different 
# aesthetics in different layers.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# You can use the same idea to specify different data for each layer. Here, our 
# smooth line displays just a subset of the mpg dataset, the subcompact cars. 
# The local data argument in geom_smooth() overrides the global data argument in 
# ggplot() for that layer only.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)


# Exercises ---------------------------------------------------------------

# What geom would you use to draw a line chart? 

ggplot(data = mpg) +
  geom_line(mapping = aes(x = displ, y = hwy, color = fl))

#A boxplot? 
ggplot(data = mpg) +
  geom_boxplot(aes(x = fl, y = hwy))

# A histogram? 
ggplot(data = mpg) +
  geom_histogram(mapping = aes(x = hwy))

ggplot(data = mpg) +
  geom_histogram(mapping = aes(y = hwy))

# An area chart?
ggplot(data = mpg) +
  geom_area(aes(x = displ, y = hwy))
  
# Run this code in your head and predict what the output will look like. Then, 
# run the code in R and check your predictions.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# What does show.legend = FALSE do? What happens if you remove it?
# Why do you think I used it earlier in the chapter?

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(se = FALSE, show.legend = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# What does the se argument to geom_smooth() do?
  
# Will these two graphs look different? Why/why not?
  
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))


# Statistical transformations ---------------------------------------------

diamonds %>% 
  group_by(cut) %>% 
  summarise(n = n())

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# On the y-axis, it displays count, but count is not a variable in diamonds! 
# Where does count come from? Many graphs, like scatterplots, plot the raw values 
# of your dataset. Other graphs, like bar charts, calculate new values to plot:
#   
# bar charts, histograms, and frequency polygons bin your data and then plot bin 
# counts, the number of points that fall in each bin.
# 
# smoothers fit a model to your data and then plot predictions from the model.
# 
# boxplots compute a robust summary of the distribution and then display a 
# specially formatted box.
# 
# The algorithm used to calculate new values for a graph is called a stat, short 
# for statistical transformation. 

?geom_bar

# You can generally use geoms and stats interchangeably. For example, you can 
# recreate the previous plot using stat_count() instead of geom_bar():
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# Override the default stat
demo <- diamonds %>% 
  group_by(cut) %>% 
  summarise(freq = n())

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# override the default mapping from transformed variables to aesthetics
# Proportion
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(count)))

# Draw greater attention to the statistical transformation in your code
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# ggplot2 provides over 20 stats for you to use. Each stat is a function, so you 
# can get help in the usual way, e.g. ?stat_bin. To see a complete list of stats, 
# try the ggplot2 cheatsheet.


# Exercises ---------------------------------------------------------------

# What is the default geom associated with stat_summary()? How could you rewrite 
# the previous plot to use that geom function instead of the stat function?
  
# What does geom_col() do? How is it different to geom_bar()?
  
# Most geoms and stats come in pairs that are almost always used in concert. 
# Read through the documentation and make a list of all the pairs. What do they 
# have in common?
  
# What variables does stat_smooth() compute? What parameters control its behaviour?
  
# In our proportion bar chart, we need to set group = 1. Why? In other words 
# what is the problem with these two graphs?
  
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop)))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))


# Position adjustments ----------------------------------------------------

# x = cut = colour
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
# x = cut = fill
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

# x = cut, fill = clarity
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# The stacking is performed automatically by the position adjustment specified 
# by the position argument. If you don’t want a stacked bar chart, you can use 
# one of three other options: "identity", "dodge" or "fill".

# identity will place each object exactly where it falls in the context of the 
# graph. This is not very useful for bars, because it overlaps them.
# The identity position adjustment is more useful for 2d geoms, like points, 
# where it is the default.
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# position = "fill" works like stacking, but makes each set of stacked bars the 
# same height. This makes it easier to compare proportions across groups.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# position = "dodge" places overlapping objects directly beside one another. 
# This makes it easier to compare individual values.
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")


# There’s one other type of adjustment that’s not useful for bar charts, but it 
# can be very useful for scatterplots.You can avoid OVERPLOTTING gridding by 
# setting the position adjustment to “jitter”. position = "jitter" adds a small 
# amount of random noise to each point. 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# To learn more about a position adjustment, look up the help page associated 
# with each adjustment: 
# ?position_dodge, ?position_fill, ?position_identity, ?position_jitter, and 
# ?position_stack.

# Exercises ---------------------------------------------------------------

# What is the problem with this plot? How could you improve it?
  
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

# What parameters to geom_jitter() control the amount of jittering?

# Compare and contrast geom_jitter() with geom_count().
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()

# What’s the default position adjustment for geom_boxplot()? Create a 
# visualisation of the mpg dataset that demonstrates it.
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot(position = "dodge")
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot(position = "fill")
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot(position = "identity")
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot(position = "stack")
ggplot(data = mpg, mapping = aes(x = manufacturer, y = hwy)) + 
  geom_boxplot(position = "jitter")

# Coordinate systems ------------------------------------------------------

# The default coordinate system is the Cartesian coordinate system where the x 
# and y positions act independently to determine the location of each point. 
# There are a number of other coordinate systems that are occasionally helpful.

# coord_flip() switches the x and y axes. 

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# coord_quickmap() sets the aspect ratio correctly for maps. 
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar() uses polar coordinates.
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# Exercises ---------------------------------------------------------------

# Turn a stacked bar chart into a pie chart using coord_polar().
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = factor(1), fill = clarity), 
    show.legend = TRUE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_polar(theta = "y")
# 
# What does labs() do? Read the documentation.
# 
# What’s the difference between coord_quickmap() and coord_map()?
#   
# What does the plot below tell you about the relationship between city and 
# highway mpg? Why is coord_fixed() important? What does geom_abline() do?
  
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

# If you don't give any arguments, geom_abline() uses default values, 
# intercept = 0 and slope = 1.
# A fixed scale coordinate system forces a specified ratio between the physical 
# representation of data units on the axes.


# The layered grammar of graphics -----------------------------------------

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

# TODO

# 2. GRAPHICS FOR COMMUNICATION -------------------------------------------

# https://r4ds.had.co.nz/graphics-for-communication.html

# LABEL -------------------------------------------------------------------

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size")

# The purpose of a plot title is to summarise the main finding. Avoid titles 
# that just describe what the plot is, e.g. “A scatterplot of engine displacement 
# vs. fuel economy”.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

# You can also use labs() to replace the axis and legend titles. It’s usually a 
# good idea to replace short variable names with more detailed descriptions, and 
# to include the units.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

# Mathematical equations. Read about the available options in ?plotmath
df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )


# Exercises ---------------------------------------------------------------

# Create one plot on the fuel economy data with customised title, subtitle, 
# caption, x, y, and colour labels.

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  labs(title = "Fuel efficiency generally decreases with engine size",
       subtitle = "Two seaters (sports cars) are an exception because of their light weight",
       caption = "Source: https://fueleconomy.gov/",
       color = "Car class",
       x = "Engine Displacement (litres)",
       y = "Highway miles per gallon")

# The geom_smooth() is somewhat misleading because the hwy for large engines is 
# skewed upwards due to the inclusion of lightweight sports cars with big engines. 
# Use your modelling tools to fit and display a better model.

ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size",
       subtitle = "Two seaters (sports cars) are an exception because of their light weight",
       caption = "Source: https://fueleconomy.gov/",
       color = "Car class",
       x = "Engine Displacement (litres)",
       y = "Highway miles per gallon")

# Take an exploratory graphic that you’ve created in the last month, and add 
# informative titles to make it easier for others to understand.


# ANNOTATIONS -------------------------------------------------------------

# geom_text() is similar to geom_point(), but it has an additional aesthetic: label. 

# There are two possible sources of labels. First, you might have a tibble that 
# provides labels. 

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)

# We can make things a little better by switching to geom_label()
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)

# The ggrepel package by Kamil Slowikowski will automatically adjust labels so 
# that they don’t overlap
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)

# You can sometimes use the same idea to replace the legend with labels placed 
# directly on the plot. 
class_avg <- mpg %>%
  group_by(class) %>%
  summarise(
    displ = median(displ),
    hwy = median(hwy)
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(mpg, aes(displ, hwy, colour = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA
  ) +
  geom_point() +
  theme(legend.position = "none")

# Alternatively, you might just want to add a single label to the plot, but 
# you’ll still need to create a data frame.
label <- mpg %>%
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

# Alternative:
label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

"Increasing engine size is related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()

# geom_hline() and geom_vline()
# geom_rect()
# geom_segment()

# Exercises ---------------------------------------------------------------

# Use geom_text() with infinite positions to place text at the four corners of 
# the plot.

# Read the documentation for annotate(). How can you use it to add a text label 
# to a plot without having to create a tibble?
  
# How do labels with geom_text() interact with faceting? How can you add a label 
# to a single facet? How can you put a different label in each facet? 
# (Hint: think about the underlying data.)

# What arguments to geom_label() control the appearance of the background box?
  
# What are the four arguments to arrow()? How do they work? Create a series of 
# plots that demonstrate the most important options.


# SCALES ------------------------------------------------------------------

# Scales control the mapping from data values to things that you can perceive. 
# Normally, ggplot2 automatically adds scales for you. 
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()

# The default scales are named according to the type of variable they align 
# with: continuous, discrete, datetime, or date.


# Axis ticks and legend keys ----------------------------------------------

# Breaks
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

# Labels
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

# You can also use breaks and labels to control the appearance of legends. 
# Collectively axes and legends are called guides. 
# Axes are used for x and y aesthetics; legends are used for everything else.

# Another use of breaks is when you have relatively few data points and want to 
# highlight exactly where the observations occur. 
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y")

# Note that the specification of breaks and labels for date and datetime scales 
# is a little different:
# date_labels takes a format specification, in the same form as parse_datetime().
# date_breaks (not shown here), takes a string like “2 days” or “1 month”.


# Legend layout -----------------------------------------------------------

# You will most often use breaks and labels to tweak the axes. While they both 
# also work for legends, there are a few other techniques you are more likely to 
# use.
#
# To control the overall position of the legend, you need to use a theme() 
# setting. In brief, they control the non-data parts of the plot. 
# The theme setting legend.position controls where the legend is drawn.
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default

# You can also use legend.position = "none" to suppress the display of the 
# legend altogether.

# To control the display of individual legends, use guides() along with 
# guide_legend() or guide_colourbar()
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(colour = "Car class") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'


# Replacing a scale -------------------------------------------------------

# Instead of just tweaking the details a little, you can instead replace the 
# scale altogether. 
# There are two types of scales you’re mostly likely to want to switch out: 
# continuous position scales and colour scales. 
# Fortunately, the same principles apply to all the other aesthetics, so once 
# you’ve mastered position and colour, you’ll be able to quickly pick up other 
# scale replacements.

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

# However, the disadvantage of this transformation is that the axes are now 
# labelled with the transformed values, making it hard to interpret the plot. 
# Instead of doing the transformation in the aesthetic mapping, we can instead 
# do it with the scale. This is visually identical, except the axes are labelled 
# on the original data scale.
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()

# Another scale that is frequently customised is colour. The default categorical 
# scale picks colours that are evenly spaced around the colour wheel. Useful 
# alternatives are the ColorBrewer scales which have been hand tuned to work 
# better for people with common types of colour blindness. The two plots below 
# look similar, but there is enough difference in the shades of red and green 
# that the dots on the right can be distinguished even by people with red-green 
# colour blindness.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")

# Don’t forget simpler techniques. If there are just a few colours, you can add 
# a redundant shape mapping. This will also help ensure your plot is 
# interpretable in black and white.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")

# The ColorBrewer scales are documented online at http://colorbrewer2.org/ and 
# made available in R via the RColorBrewer package, by Erich Neuwirth. 
# Figure 28.2 shows the complete list of all palettes. 
# The sequential (top) and diverging (bottom) palettes are particularly useful 
# if your categorical values are ordered, or have a “middle”. 
# This often arises if you’ve used cut() to make a continuous variable into a 
# categorical variable.
RColorBrewer::display.brewer.all()

RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)

# When you have a predefined mapping between values and colours, use 
# scale_colour_manual().
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))

RColorBrewer::brewer.pal(3, "RdYlBu")

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "#FC8D59", Democratic = "#91BFDB"))

# For continuous colour, you can use the built-in scale_colour_gradient() or 
# scale_fill_gradient(). 
# If you have a diverging scale, you can use scale_colour_gradient2(). That 
# allows you to give, for example, positive and negative values different 
# colours. That’s sometimes also useful if you want to distinguish points above 
# or below the mean.

# Another option is scale_colour_viridis() provided by the viridis package. 
# It’s a continuous analog of the categorical ColorBrewer scales. The designers, 
# Nathaniel Smith and Stéfan van der Walt, carefully tailored a continuous 
# colour scheme that has good perceptual properties. Here’s an example from the 
# viridis vignette.
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis(option = "magma") +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis(option = "turbo") +
  coord_fixed()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  viridis::scale_color_viridis(option = "turbo", discrete = TRUE)


# Exercises ---------------------------------------------------------------

# Why doesn’t the following code override the default scale?
  
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()

# Hay que usar scale_fill_gradient()

# What is the first argument to every scale? How does it compare to labs()?
  
# Change the display of the presidential terms by:
#  
# Combining the two variants shown above.
# Improving the display of the y axis.
# Labelling each term with the name of the president.
# Adding informative plot labels.
# Placing breaks every 4 years (this is trickier than it seems!).

# Use override.aes to make the legend on the following plot easier to see.

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20)


ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20) +
  guides(colour = guide_legend(nrow = 4, override.aes = list(alpha = 1, size = 4))) +
  scale_x_log10() +
  scale_y_log10()

# ZOOMING -----------------------------------------------------------------

# There are three ways to control the plot limits:
#
# Adjusting what data are plotted
# Setting the limits in each scale
# Setting xlim and ylim in coord_cartesian()

# To zoom in on a region of the plot, it’s generally best to use coord_cartesian().

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

# You can also set the limits on individual scales. Reducing the limits is 
# basically equivalent to subsetting the data. It is generally more useful if 
# you want expand the limits, for example, to match scales across different plots.

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point()

# One way to overcome this problem is to share scales across multiple plots, 
# training the scales with the limits of the full data.

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_colour_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# THEMES ------------------------------------------------------------------

# Finally, you can customise the non-data elements of your plot with a theme:
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

# ggplot2 includes eight themes by default, as shown in Figure 28.3. 
# Many more are included in add-on packages like ggthemes 
# (https://github.com/jrnold/ggthemes)


# SAVING YOUR PLOTS -------------------------------------------------------

# There are two main ways to get your plots out of R and into your final 
# write-up: ggsave() and knitr. 
# 
# ggsave() will save the most recent plot to disk:
ggplot(mpg, aes(displ, hwy)) + geom_point()
# ggplot(mpg, aes(displ, hwy)) + geom_point()

# If you don’t specify the width and height they will be taken from the 
# dimensions of the current plotting device. For reproducible code, you’ll want 
# to specify them.


# Figure sizing -----------------------------------------------------------

# Generally, however, I think you should be assembling your final reports using 
# R Markdown, so I want to focus on the important code chunk options that you 
# should know about for graphics. 

# There are five main options that control figure sizing: 
# fig.width, fig.height, fig.asp, out.width and out.height. 
# Image sizing is challenging because there are two sizes (the size of the 
# figure created by R and the size at which it is inserted in the output 
# document), and multiple ways of specifying the size (i.e., height, width, and 
# aspect ratio: pick two of three).
# 
# I only ever use three of the five options:
#
# I find it most aesthetically pleasing for plots to have a consistent width. To enforce this, I set fig.width = 6 (6") and fig.asp = 0.618 (the golden ratio) in the defaults. Then in individual chunks, I only adjust fig.asp.
# 
# - I control the output size with out.width and set it to a percentage of the 
# line width. I default to out.width = "70%" and fig.align = "center". That give 
# plots room to breathe, without taking up too much space.
# 
# - To put multiple plots in a single row I set the out.width to 50% for two plots, 
# 33% for 3 plots, or 25% to 4 plots, and set fig.align = "default". Depending 
# on what I’m trying to illustrate (e.g. show data or show plot variations), I’ll 
# also tweak fig.width, as discussed below.
# 
# If you find that you’re having to squint to read the text in your plot, you 
# need to tweak fig.width. If fig.width is larger than the size the figure is 
# rendered in the final doc, the text will be too small; if fig.width is smaller, 
# the text will be too big. You’ll often need to do a little experimentation to 
# figure out the right ratio between the fig.width and the eventual width in your
# document. To illustrate the principle, the following three plots have fig.width 
# of 4, 6, and 8 respectively:
#
# If you want to make sure the font size is consistent across all your figures, 
# whenever you set out.width, you’ll also need to adjust fig.width to maintain 
# the same ratio with your default out.width. For example, if your default 
# fig.width is 6 and out.width is 0.7, when you set out.width = "50%" you’ll 
# need to set fig.width to 4.3 (6 * 0.5 / 0.7).


# Other important options -------------------------------------------------

# When mingling code and text, like I do in this book, I recommend setting 
# fig.show = "hold" so that plots are shown after the code. This has the pleasant 
# side effect of forcing you to break up large blocks of code with their 
# explanations.
# 
# To add a caption to the plot, use fig.cap. In R Markdown this will change the 
# figure from inline to “floating”.
# 
# If you’re producing PDF output, the default graphics type is PDF. This is a 
# good default because PDFs are high quality vector graphics. However, they can 
# produce very large and slow plots if you are displaying thousands of points. 
# In that case, set dev = "png" to force the use of PNGs. They are slightly lower 
# quality, but will be much more compact.
# 
# It’s a good idea to name code chunks that produce figures, even if you don’t 
# routinely label other chunks. The chunk label is used to generate the file 
# name of the graphic on disk, so naming your chunks makes it much easier to pick 
# out plots and reuse in other circumstances (i.e. if you want to quickly drop a 
# single plot into an email or a tweet).
