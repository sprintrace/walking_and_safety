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




####################################      THIS IS THE FILE WE WILL USE FOR GRAPHS     ####################


#1    GGPlot for GunLaw (the total of all Americans in survey)
fear <- read_parquet("data/analysis_data/fear.parquet")
View(fear)

transform(fear, 'Total Americans in Favour' = as.numeric('Total Americans in Favour')) 

is.numeric(fear$Total.Americans.in.Favour) ## test



names(fear) <- c("Year", "Total")

ggplot(data=fear, aes(x = Year, y = Total, group = 1)) +
  geom_line()+
  labs(title = "Percentage of feeling unsafe walking alone at night", y = "Total % of Americans in fear walking alone at night")




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
  labs(title = "Percentage of American Women vs Men \n who feel unsafe outside at night", y = "Total percent of those in fear")





#3    GGPlot for Health ////////////////////// Not Currently Working

## load in CSV ##
Health <- read_csv("data/analysis_data/Health.csv")
View(Health)

## create ggplot ##

### turn strings into numbers
transform(Health, 'Excellent' = as.numeric('Excellent')) 
transform(Health, 'Good' = as.numeric('Good')) 
transform(Health, 'Fair' = as.numeric('Fair')) 
transform(Health, 'Poor' = as.numeric('Poor')) 



####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Health <- Health |> 
  pivot_longer(cols=c('Excellent', 'Good', 'Fair','Poor'), names_to='Healthy', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Health, aes(x = Year, y=Total, group = Healthy)) +
  geom_line(aes(colour=Healthy))+
  labs(title = "Percentage of Americans who fear walking alone at night \ based on their physical health", y = "% of population")




#4    GGPlot for Age


## load in CSV ##
Age <- read_csv("data/analysis_data/Age.csv")
View(Age)

## create ggplot ##

### turn strings into numbers
transform(Age, '18-34' = as.numeric('18-34')) 
transform(Age, '35-49' = as.numeric('35-49')) 
transform(Age, '50-64' = as.numeric('50-64')) 
transform(Age, '50-64' = as.numeric('65+')) 


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Age <- Age |> 
  pivot_longer(cols=c('18-34', '35-49', '50-64', '65+'), names_to='Party', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Age, aes(x = Year, y=Total, group = Party)) +
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
  labs(title = "Americans in feeling of safety walking at night \n based on racial identity", y = "Total % in favour")





####

