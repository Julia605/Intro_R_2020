# Assignment 1
# 04 February 2020
# Nthabiseng Thibeli

#load libraries
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(maps)
library(lubricate)

# loading data
load("data/gebco_sa_2.RData")


# converting the data to numeric
bathy_wide_1 <- as.data.frame(apply(bathy_wide_tidy,2,as.numeric))


# mapping
final_map <- ggplot(data = bathy_wide_1, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = "elevation")) +
  scale_fill_gradient("elevation/\nDepth (m)", values=scales::rescale(c(-7000,-3000,0,2000,3500)) +
                        colors= c("darkblue", "turquoise1","forestgreen","burlywood3","wheat1"))
  labs(x="",y="")
  scale_x_continuous(breaks = seq(15,20,25,30, 35,),# creating breaks on the x axis
                     labels=c("15","20", "25","30","35"),# put labels on the x axis
                     position = "bottom", expand = c(0,0))+ # put x axis labels below the figure
  scale_y_continuous(breaks=seq(-20, -30,-35,-40,-45), # create breaks on the y axis
                     labels=c("-20","-30","-35","-40","-45"), # put labels on the y axis
                     expand =c(0,0)) +
  theme(axis.title = element_blank(), # Remove the axis labels
          legend.text = element_text(size = 7), # Change text size in legend
          legend.title = element_text(size = 7), # Change legend title text size
          legend.key.height = unit(0.3, "cm"), # Change size of legend
          legend.background = element_rect(colour = "white"), # Add legend background
          legend.justification = c(1, 0), # Change position of legend
          legend.position = c(0.55, 0.4) # Fine tune position of legend
    )
  final_map

# To convert gebco_sa.asc to gebco_sa_2.Rdata
# In the console, enter the command; read.table("data/gebco_sa.asc",header=TRUE)
# Save workspace
# Click "save as", choose a folder then click "save"
  
  
  
  
  
  
  
  
  
  
  
  
  


