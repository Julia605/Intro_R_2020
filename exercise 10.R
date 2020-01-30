# ggplot
# Mapping on day 3
# 30th January 2020
# Nthabiseng Thibeli

# Load libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)

# Load Africa map
load("data/africa_map.RData")

ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(-20, 51), ylim = c(-35, 40), expand = 0) # Force lon/lat extent
sa_1


sa_2 <- sa_1 +
     annotate("text", label = "Atlantic\nOcean", 
                           x = 4, y = -25.5, 
                           size = 5.0, 
                           angle = -36, 
                           colour = "grey37") +
     annotate("text", label = "Indian\nOcean", 
                           x = 39, y = -27, 
                           size = 4.5, 
                           angle = 60, 
                           colour = "black")
 sa_2
 
 sa_3 <- sa_2 +
   # scalebar(x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
   #          dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
   #          transform = TRUE, model = "WGS84") + # Set appearance
   north(x.min = -10, x.max = -8, y.min = -21, y.max = -19, # Set location of symbol
         scale = 5, symbol = 16)
 sa_3
 
 
 
 
 
 
 

