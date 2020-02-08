# Honours_assignment
# 06 February 2020
# Nthabiseng Thibeli

# Section 1

# load libraries
library(tidyverse)

# load the data
BOD <- datasets::BOD 

head(BOD)
tail(BOD)
glimpse(BOD)
View(BOD)
names(BOD)

# Examine the built-in dataset BOD. Which of the following is true:
# c.BOD is tidy: each row is an observation with two values (time and demand)

# load data
BJsales <- datasets::BJsales

EuStockMarkets <- datasets::EuStockMarkets

DNase <- datasets::DNase

Formaldehyde <- datasets::Formaldehyde

Orange <- datasets::Orange

UCBAdmissions <- datasets::UCBAdmissions

# Which of the following built-in datasets is tidy (you can pick more than one):
# DNase, Formaldehyde,Orange

# Section 2

#load libraries

library(dplyr)
library(dslabs)
data(murders)


head(murders)
tail(murders)
glimpse(murders)
View(murders)
names(murders)

# The murders data set consists of 5 columns namely; state, abb, region, population and total. It has 51 rows. 
# This dataset has all the states of the United States of America, their abbreviations, regions in which the staes are in, 
# total populations and total murders in each state.

murders <- mutate(murders,population_in_millions=population/10^6)

# tidyverse

murders_1<-murders %>% # Tell R which dataframe we are using
  select(state, population) # Select only specific columns

# removing Florida from the dataset
murders_1 <- murders_1 %>% 
  filter(state!="Florida")

# Create new data frame called no_south
no_south <- murders %>% 
  filter(region!="South")

# There are 34 states in this category.

# Create new data frame for New york and Texas
NY_Texas <- murders %>%
  filter(state%in%c("New York", "Texas"))

# Calculate population size of the South and West regionally
# Create data frame South_West
South_West <- murders %>% 
  filter(region==c("South","West"))

# Calculate population size           
South_West_pop <- sum(South_West$population)
South_West_pop <- as.data.frame(South_West_pop)

# Create new data frame for Northeast region population size
North_East <- murders %>% 
  filter(region==c("Northeast")) %>% 
  select(population)

# Create two plots of your choice and explain visible trends
library(tidyverse)
library(ggplot2)

plot1 <- ggplot(data = murders, aes(x = region, y = total, colour = region)) +
  geom_point(aes(size=population)) +
  geom_smooth(method = "lm", size=1.0)
plot1

# Trend: The total number of murders is highest in the West region,then the South region
# followed by the Northeast region and the North Central region has the least murders. 

plot2 <- ggplot(data = murders, aes(x = state, y = population, colour = region)) +
  geom_point(aes(size=population)) +
  geom_smooth(method = "lm", size=1.0)
plot2

# Trend: The states in the West region have the highest population,followed by the South region
# then the Northeast region, and lastly the North Central region.

# Compare the population size of the South with the population size of the West
# The population size of the West is higher than that of the South.

#Create a new data frame where the total>20 but<100 and to exclude the value 120
Above_20 <- murders %>% 
  filter(total>20)
between_20_and_100 <- Above_20 %>% 
  filter(total<100)

#Create an object, containing from 10th to 24th and 26th row. Consider using the slice function
btw_10_and_24 <- murders %>% 
  slice(10:24,26)

# Use as_tibble to convert the murders data table into a tribble and save it in an object called murders_tibble. 
# Use the group_by function to convert murders into a tibble that is grouped by region.
murders_tibble <- as.tibble(murders) %>% 
  group_by(region)

#Write tidyverse code that is equivalent to this code: 
murders.tibble <- as.tibble(murders)



# Section 3

#load libraries

library(dplyr)
library(dslabs)
data("heights")

head(heights)
tail(heights)
glimpse(heights)
View(heights)
names(heights)

# The dataset heights consists of 2 columns namely; sex and height. It has 1050 rows. This dataset has heights of both males an females in centimeters.
# The maximum height 


# Determine the average and standard deviation for males and females. Then calculate the median, minimum and maximum values.

heights %>% 
  group_by(sex)%>% 
  summarise(avg_height=mean(height), # Create a summary of the mean of the males
            sd_height=sd(height),  # Create a summary of the sd of the males
            min_height=min(height), # create summary for the minimum value
            max_height=max(height), # create summary for the maximum value
            med_height=median(height)) # create summary for the median value


# Section 4
# a) count the number of elements missing in both x and y
DATA <- data.frame(x=c(1,6,21,19,NA,73,NA), y=c(NA,NA,3,NA,13,24,NA))

missing_x <- DATA %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

# b) Transform the code used in above a) , into a function
new_code <- function(dataset){dataset %>% select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))
}
# c)Create three new vectors and test the function created in b)
new_DATA <- data.frame(x=c(1,2,7,NA,12,NA,NA,15), y=c(4,8,12,16,20,NA,NA,NA), z=c(3,6,9,NA,12,15,18,NA))

missing_values <- new_code(new_DATA)


# Section 5

Seasonal_data <- data.frame(year=c(2015,2016,2017,2018),
                            winter=c(41,39,47,40),
                            spring=c(41,46,57,45),
                            summer=c(75,52,85,66),
                            Autumn=c(57,66,52,66))
# Create a hypothesis
# The dataset consists of the 4 seasons of the year and the annual average temperatures from 2015 to 2018.

S_data1 <- Seasonal_data %>%
  gather(winter, spring,summer,Autumn,key = "season",value = "temp")

## plotting-function ggplot
library(tidyverse)
library(ggplot2)
library(ggpubr)


Seasonal_data_1 <- ggplot(data = S_data1, aes(x = year, y = temp, colour = season)) +
    geom_point() +
    geom_smooth(method = "gam") +
    labs(x = "Years", y = "Temperature")
Seasonal_data_1

# Histogram

histogram_1 <- ggplot(data = S_data1, aes(x = temp)) +
    geom_histogram(aes(fill = season), position = "dodge", binwidth = 80) +
    labs(x = "Years", y = "Temperature")
histogram_1   

# Write a paragraph discussings your findings
# Summer has the highest annual temperatures from 2015 to 2018, followed by Autumn
# then spring, and winter has the lowest annual temperatures throughout the years.


cats_data <- tibble(cats=c("A", "B", "C"),
      position=c("1-2-3", "3-1-2", "2-3-1"),
      minutes=c(3,3,3),
      seconds=c(12,44,15))
cats_data

# Use the separate function to split the position column into new three columns.
# The new column names will be first_place, second_place, third_place.
cats_data_1 <- cats_data %>% 
  separate(col = position, into = c("first_place", "second_place","third_place"), sep = "-")
  

# Unite the minutes and seconds columns into its own column . The new column name will be total_time
cats_data_2 <- cats_data_1 %>% 
  unite(minutes, seconds, col = "total_time", sep = "-")


# Section 6

# Find, select and use a datasets of your choice.

# loading packages
library(tidyverse)

Titanic<- read_csv("data/Titanic.csv")

## viewing the data

head(Titanic)
tail(Titanic)
glimpse(Titanic)
view(Titanic)#overview opens the data
names(Titanic)#column names

# Apply functions to the dataset

# arrange

Titanic %>% 
  arrange(desc(Sex))

# select

Titanic %>% 
  select(-(Age:Sex))

# mutate

Titanic %>% 
  mutate(Half_Freq=Freq/2)

# group_by
# Group by Class

Titanic_Class <- Titanic %>% 
  group_by(Freq)

# Gather 

Titanic_1 <- Titanic %>%
  gather(Sex, Age, key = "Identification",value = "Description")

# Joining

Titanic_tidy <- left_join(Titanic, Titanic_1, by = c("Freq", "Class"))
                                                     

# loading packages
library(tidyverse)

WorldPhones <- read_csv("data/WorldPhones.csv")

## viewing the data

head(WorldPhones)
tail(WorldPhones)
glimpse(WorldPhones)
view(WorldPhones)#overview opens the data
names(WorldPhones)#column names


# Apply functions to the dataset

# Gather 

WorldPhones_1 <- WorldPhones %>%
  gather(N.Amer, S.Amer,Mid.Amer, key = "America",value = "Phones")

# Spread

WorldPhones_2 <- WorldPhones_1 %>% 
  spread(key = America, value = Phones)


#separate

WorldPhones_3 <- WorldPhones_1 %>% 
  separate(col = America, into = c("region", "America"))


















