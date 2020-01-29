# summary stats
# 29th January 2020
# Nthabiseng Thibeli
#############

 # loading packages
library(tidyverse)

# loading in the data

laminaria %>% # Chose the dataframe
  summarise(avg_bld_weight = mean(thallus_mass)) # Calculate mean thallus mass

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_bld_weight = mean(thallus_mass), # Create a summary of the mean of the thallus mass
            sd_bld_weight = sd(thallus_mass))# Create a summary of the sd of the thallus mass

laminaria %>% #Tell R that we want to use the laminaria dataframe
group_by(site) %>% 
    summarise(avg_thallus_mass = mean(thallus_mass))

laminaria %>% # Select 'laminaria'
group_by(site) %>% # Group the dataframe by site
  summarise(var_thallus_mass = var(thallus_mass),
            med_thallus_mass=median(thallus_mass))

ggplot(data = laminaria, aes(x = thallus_mass, y = blade_weight)) +
  geom_point(shape = 7,colour = "green", fill = "white") +
  labs(x = "thallus mass (kg)", y = "blade_weight (g)")

# Load libraries
library(tidyverse)

# Load data
ChickWeight <- datasets::ChickWeight

# Create a basic figure
ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point() +
  geom_line(aes(group = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_line() +
  geom_point(aes(group = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_line() +
  geom_smooth(method = "lm")

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) +
  geom_smooth(method = "lm", cex.lab = 5)

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Time (days)", y = "Mass (g)", colour = "diet type") + # Change the labels
  theme(legend.position = "top") # Change the legend position

plot(iris$Petal.Length, iris$Petal.Length.Width, main = "Chickweight",
     xlab = "time", ylab = "mass")


histogram_1 <- ggplot(data = ChickWeight, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 80) +
  labs(x = "Mass (g)", y = "Count")

histogram_1

box_1 <- ggplot(data = ChickWeight, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Mass (g)")

box_1

# Load data
ChickWeight <- datasets::ChickWeight

# Create faceted figure
lam_d <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 1) + # This is the line that creates the facets
  labs(x = "Count", y = "Mass (g)")





