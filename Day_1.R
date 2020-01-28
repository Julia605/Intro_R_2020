# Day 1
# Data, laminaria, statistical analyses
# 28 January 2020

# loading packages
library(tidyverse)

laminaria <- read_csv("data/laminaria.csv")

## viewing the data

head(laminaria)#first 6 rows
tail(laminaria)# last 6 rows
glimpse(laminaria)
view(laminaria)#overview opens the data
names(laminaria)#column names

# tidyverse

lam_sub<-laminaria %>% # Tell R which dataframe we are using
  select(site, total_length) # Select only specific columns

lam_slice<-laminaria %>% 
  select(site, total_length) %>% # Select specific columns first
  slice(56:78)

lam_kom<-laminaria %>%
  filter(site == "Kommetjie")


laminaria %>% # Tell R which dataset to use
  filter(site == "Kommetjie") %>% # Filter out only records from Kommetjie
  nrow() # Count the number of remaining rows
