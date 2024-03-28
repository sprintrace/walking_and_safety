#### Preamble ####
# Purpose: collect and analyze the data from the US General Survey
# Author: 
# Date: 
# Contact: 
# License: 
# Pre-requisites: 


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####

## random number generating code from class

#   tibble(
#   certianty = runif(n = 1000, min = 0, max = 11)|> floor(),
#   guess_correct = runif(n=1000, min = 0, max = 2)|> floor(),
#   )

#   expect that guessed_correct is only ever 0 or 1.

#   expect that certianty is only ever an integer of 0 to 10, inclusive
#   max(simulated_data$certianty) == 10
#   min(simulated_data$certianty) == 0
#   any(unique(simulated_data$certianty)== c(0:10)) 

###################################################################################################
###################################################################################################


library(tidyr)
library(tidyverse)
library(here)
library(ggplot2)
library(readr) 


#1    GGPlot for GunLaw (the total of all Americans in survey)
fear <- read_csv("data/analysis_data/fear.csv")
View(fear)

transform(fear, 'Total Americans in Favour' = as.numeric('Total Americans in Favour')) 

names(fear) <- c("Year", "Total")

ggplot(data=fear, aes(x = Year, y = Total, group = 1)) +
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


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Health <- Health |> 
  pivot_longer(cols=c(' Excellent ', ' Good ', ' Fine '), names_to='Fear', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Health, aes(x = Year, y=Total, group = Fear)) +
  geom_line(aes(colour=Fear))+
  labs(title = "Percentage of Americans in favour of police \n backed gun permits based on higest level of education", y = "Total % in favour")




#4    GGPlot for Age


## load in CSV ##
Age <- read_csv("data/analysis_data/Age.csv")
View(Age)

## create ggplot ##

### turn strings into numbers
transform(Age, 'Democrats' = as.numeric('Democrats')) 
transform(Age, 'Republicans' = as.numeric('Republicans')) 
transform(Age, 'Other/Non-affiliated' = as.numeric('Other/Non-affiliated')) 


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Age <- Age |> 
  pivot_longer(cols=c('Democrats', 'Republicans', 'Other/Non-affiliated'), names_to='Party', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Age, aes(x = Year, y=Total, group = Party)) +
  geom_line(aes(colour=Party))+
  labs(title = "Americans in favour of police backed gun permits \n based on political affiliation", y = "Total % in favour")





#5    GGPlot for RaceAndGunlaw

## load in CSV ##
RaceAndGunlaw <- read_csv("data/analysis_data/RaceAndGunlaw.csv")
View(GunlawRepVsDem.csv)

## create ggplot ## 

### turn strings into numbers
transform(RaceAndGunlaw, 'White' = as.numeric('White')) 
transform(RaceAndGunlaw, 'Black' = as.numeric('Black')) 
transform(RaceAndGunlaw, 'Other' = as.numeric('Other')) 


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
RaceAndGunlaw <- RaceAndGunlaw |> 
  pivot_longer(cols=c('White', 'Black', 'Other'), names_to='Race', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=RaceAndGunlaw, aes(x = Year, y=Total, group = Race)) +
  geom_line(aes(colour=Race))+
  labs(title = "Americans in favour of police backed \n gun permits based on racial identity", y = "Total % in favour")