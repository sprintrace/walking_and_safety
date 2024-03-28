#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

library(tidyr)
library(tidyverse)
library(here)
library(ggplot2)
library(readr) 


#1    GGPlot for GunLaw (the total of all Americans in survey)
Gunlaw <- read_csv("data/analysis_data/Gunlaw.csv")
View(Gunlaw)

transform(Gunlaw, 'Total Americans in Favour' = as.numeric('Total Americans in Favour')) 

names(Gunlaw) <- c("Year", "Total")

ggplot(data=Gunlaw, aes(x = Year, y = Total, group = 1)) +
  geom_line()+
  labs(title = "Percentage of Americans in favour of police backed gun permits", y = "Total % of Americans in favour")




#2    GGPlot for Gender
## load in CSV ##
Gender <- read_csv("data/analysis_data/Gender.csv")
View(Gender)

## create ggplot ##

### turn strings into numbers
transform(Gender, 'Female' = as.numeric('Female')) 
transform(Gender, 'Male' = as.numeric('Male')) 

####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Gender <- Gender |> 
  pivot_longer(cols=c('Female', 'Male'), names_to='Gender', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Gender, aes(x = Year, y=Total, group = Gender)) +
  geom_line(aes(colour=Gender))+
  labs(title = "Percentage of American Women vs Men \n in favour of police backed gun permits", y = "Total % in favour")





#3    GGPlot for Health

## load in CSV ##
Health <- read_csv("data/analysis_data/Health.csv")
View(Health)

## create ggplot ##

### turn strings into numbers
transform(Health, 'Degree or Higher' = as.numeric('Degree or Higher')) 
transform(Health, 'Highschool' = as.numeric('Highschool')) 
transform(Health, 'No Highschool' = as.numeric('No Highschool')) 
transform(Health, 'No Highschool' = as.numeric('No Highschool')) 


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Health <- Health |> 
  pivot_longer(cols=c('Degree or Higher', 'Highschool', 'No Highschool'), names_to='Education', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Health, aes(x = Year, y=Total, group = Education)) +
  geom_line(aes(colour=Education))+
  labs(title = "Percentage of Americans in favour of police \n backed gun permits based on higest level of education", y = "Total % in favour")




#4    GGPlot for Age


## load in CSV ##
GunlawRepVsDem.csv <- read_csv("data/analysis_data/GunlawRepVsDem.csv")
View(GunlawRepVsDem.csv)

## create ggplot ##

### turn strings into numbers
transform(GunlawRepVsDem.csv, 'Democrats' = as.numeric('Democrats')) 
transform(GunlawRepVsDem.csv, 'Republicans' = as.numeric('Republicans')) 
transform(GunlawRepVsDem.csv, 'Other/Non-affiliated' = as.numeric('Other/Non-affiliated')) 


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
GunlawRepVsDem.csv <- GunlawRepVsDem.csv |> 
  pivot_longer(cols=c('Democrats', 'Republicans', 'Other/Non-affiliated'), names_to='Party', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=GunlawRepVsDem.csv, aes(x = Year, y=Total, group = Party)) +
  geom_line(aes(colour=Party))+
  labs(title = "Americans in favour of police backed gun permits \n based on political affiliation", y = "Total % in favour")





#5    GGPlot for Race

## load in CSV ##
Race <- read_csv("data/analysis_data/Race.csv")
View(Race)

## create ggplot ## 

### turn strings into numbers
transform(Race, 'White' = as.numeric('White')) 
transform(Race, 'Black' = as.numeric('Black')) 
transform(Race, 'Other' = as.numeric('Other')) 


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Race <- Race |> 
  pivot_longer(cols=c('White', 'Black', 'Other'), names_to='Race', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Race, aes(x = Year, y=Total, group = Race)) +
  geom_line(aes(colour=Race))+
  labs(title = "Americans in favour of police backed \n gun permits based on racial identity", y = "Total % in favour")