library(tidyverse)


seq(-3, 3, by = 0.01) %>% 
  expand.grid(x = ., y = .) %>% 
  ggplot(aes(x = (1 - x - sin(y^2)),
             y = (1 + y - cos(x^2)))) +
  geom_point(alpha = 0.05, shape = 20, size = 0) +
  theme_void() + 
  coord_polar()
