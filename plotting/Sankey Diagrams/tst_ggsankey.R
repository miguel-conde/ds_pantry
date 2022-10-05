# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# 1 - Sample data set -----------------------------------------------------

df <- mtcars %>%
  make_long(cyl, vs, am, gear, carb) 


# 2 - Sankey plot with ggsankey -------------------------------------------


# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr) # Also needed

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16) 

# 2.1 - Adding labels -----------------------------------------------------
# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr) # Also needed

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey() +
  geom_sankey_label() +
  theme_sankey(base_size = 16) 

# 3 - Color customization -------------------------------------------------

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr) # Also needed

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey(flow.alpha = 0.75, node.color = 1) +
  scale_fill_viridis_d(option = "A", alpha = 0.95) +
  theme_sankey(base_size = 16) 

# 3.1 - Changing the color of the labels ----------------------------------

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr) # Also needed

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d(option = "A", alpha = 0.95) +
  theme_sankey(base_size = 16) 

# 4 - Legend customization ------------------------------------------------

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr) # Also needed

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  guides(fill = guide_legend(title = "Title")) 


# 4.1 - Removing the legend -----------------------------------------------

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr) # Also needed

ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = 0.5, node.color = 1) +
  geom_sankey_label(size = 3.5, color = 1, fill = "white") +
  scale_fill_viridis_d() +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none") 











