# Day 1
# Data laminaria data analyses
# 28 January 2020

# loading packages
library(tidyverse)

laminaria <- read_csv("data/laminaria.csv")

mutate(total_length_half= total_length/2)%>%

  select(site,total_length_half(na.rm=TRUE))  

group_by(site,blade_length)%>%
  summarise (min_blade_length=min(blade_length),
            max_blade_length=max(blade_length),
            mean_blade_length=mean(blade_length))
 laminaria%>%
  group_by(site)%>% 
  summarise (max_stipe_mass=max(stipe_mass))

laminaria%>%
  select(site,region,stipe_length)
  

## Neat script
# Always good to add comments next to each line. this helps you and the collaborator to understand what you are doing
# Overall mark for day 1: 6/10





