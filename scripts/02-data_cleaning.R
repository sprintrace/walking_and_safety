#### Preamble ####
# Purpose: collect and analyze the data from the US General Survey
# Author: 
# Date: 
# Contact: 
# License: 
# Pre-requisites: 

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
library(readxl)
library(arrow)


#### Clean data ####
#### get from excel

#1
fear.xlsx <- read_excel("data/raw_data/fear.xlsx")
View(fear.xlsx)

#2
Gender <- read_excel("data/raw_data/Gender.xlsx")
View(Gender)

#3
Health <- read_excel("data/raw_data/Health.xlsx")
View(Health)  

#4
Age <- read_excel("data/raw_data/Age.xlsx")
View(Age)  

#5
Race <- read_excel("data/raw_data/Race.xlsx")
View(Race)  


 
################################################ Repeat 32 - 42 for the other excel files
#### Tidy up Data ####

##  1

fear.xlsx <- fear.xlsx |> 
  slice(7:8)
fear.xlsx[1,1]<-"Year"
names(fear.xlsx)[1]<-"C1"

#### PIVOT LONGER ####

fear.xlsx <- fear.xlsx[-1] |> t() |> as.data.frame()


####continue 
names(fear.xlsx) <- c("Year", "Total Americans in Favour")

##### new lines to fix the num in brackets #####

fear.xlsx$'Total Americans in Favour' <- sapply(fear.xlsx$'Total Americans in Favour', function(x) { gsub("[\r\n]", "", x) })

fear.xlsx[c('Total','Y')] <- str_split_fixed(fear.xlsx$'Total Americans in Favour', ' ', 2)

fear.xlsx <- fear.xlsx |>
  select(c("Year", "Total"))

names(fear.xlsx) <- c("Year", "Total Americans in Favour")






##  2

Gender <- Gender |> 
  slice(7:9)



#### PIVOT LONGER #### 

Gender <- Gender[-1] |> t() |> as.data.frame()

names(Gender)[1]<-"Year"
names(Gender)[2]<-"Female"
names(Gender)[3]<-"Male"

##### new lines to fix the num in brackets #####

Gender$'Female' <- sapply(Gender$'Female', function(x) { gsub("[\r\n]", "", x) })

Gender[c('F','Y')] <- str_split_fixed(Gender$'Female', ' ', 2)

#### Male Data () removal

Gender$'Male' <- sapply(Gender$'Male', function(x) { gsub("[\r\n]", "", x) })

Gender[c('M','YY')] <- str_split_fixed(Gender$'Male', ' ', 2)

#### remove the unwanted collumbs ####
Gender <- Gender |>
  select(c("Year", "F", "M"))

names(Gender) <- c("Year", "Female", "Male")  




##  3

Health <- Health |> 
  slice(7:11)
Health[1,1]<-"Year"
names(Health)[1]<-"C1"

#### PIVOT LONGER ####

## new line

names(Health)[1]<-"Year"
names(Health)[2]<-"Excellent"
names(Health)[3]<-"Good"
names(Health)[4]<-"Fair"
names(Health)[5]<-"Poor"
#### old lines

Health <- Health[-1] |> t() |> as.data.frame()

names(Health) <- c("Year", "Excellent", "Good","Fair", "Poor")



##### new lines to fix the num in brackets #####

Health$'Excellent' <- sapply(Health$'Excellent', function(x) { gsub("[\r\n]", "", x) })

Health[c('D','extra')] <- str_split_fixed(Health$'Excellent', ' ', 2)

## Good health

Health$'Good' <- sapply(Health$'Good', function(x) { gsub("[\r\n]", "", x) })

Health[c('High','extra2')] <- str_split_fixed(Health$'Good', ' ', 3)

## Fair health 


Health$'Fair' <- sapply(Health$'Fair', function(x) { gsub("[\r\n]", "", x) })

Health[c('NoH','extra3')] <- str_split_fixed(Health$'Fair', ' ', 4)


## Poor health


Health$'Poor' <- sapply(Health$'Poor', function(x) { gsub("[\r\n]", "", x) })

Health[c('NoH3','extra32')] <- str_split_fixed(Health$'Poor', ' ', 5)


#### remove the unwanted collumbs ####
Health <- Health |>
  select(c("Year","D","High","NoH","NoH3"))

names(Health) <- c("Year", "Excellent", "Good","Fair","Poor")  
















##  4

Age <- Age |>
  slice(7:11)
Age[1,1]<-"Year"
names(Age)[1]<-"C1"

## New line

#### PIVOT LONGER ####

Age <- Age[-1] |> t() |> as.data.frame()
names(Age) <- c("Year", "18-34", "35-49", "50-64", "65+")

#CoPilot code
###   colnames(Age) <- c("Year", "18-34", "35-49", "50-64", "65+")

##### New lines to fix the num in brackets #####

Age$'18-34' <- sapply(Age$'18-34', function(x) { gsub("[\r\n]", "", x) })
Age[c('18-34',' ')] <- str_split_fixed(Age$'18-34', ' ', 2)  # Assuming space separates values

# 35-49

Age$'35-49' <- sapply(Age$'35-49', function(x) { gsub("[\r\n]", "", x) })
Age[c('35-49',' ')] <- str_split_fixed(Age$'35-49', ' ', 2)  # Assuming space separates values

# 50-64

Age$'50-64' <- sapply(Age$'50-64', function(x) { gsub("[\r\n]", "", x) })
Age[c('50-64',' ')] <- str_split_fixed(Age$'50-64', ' ', 2)  # Assuming space separates values


# 65+

Age$'65+' <- sapply(Age$'65+', function(x) { gsub("[\r\n]", "", x) })
Age[c('65+',' ')] <- str_split_fixed(Age$'65+', ' ', 2)  # Assuming space separates values


#### remove the unwanted collumbs ####
Age <- Age |>
  select(c("Year","18-34","35-49","50-64", "65+"))

### LINE IS REDUNDANT
#names(RaceAndGunlaw) <- c("Year", "White", "Black", "Other")







##  5

Race <- Race |>
  slice(7:10)
Race[1,1]<-"Year"
names(Race)[1]<-"C1"

## New line
#names(RaceAndGunlaw)[1]<-"Year"
#names(RaceAndGunlaw)[2]<-"White"
#names(RaceAndGunlaw)[3]<-"Black"
#names(RaceAndGunlaw)[4]<-"Other"

#### PIVOT LONGER ####

Race <- Race[-1] |> t() |> as.data.frame()
names(Race) <- c("Year", "White", "Black", "Other")


##### New lines to fix the num in brackets #####

Race$'White' <- sapply(Race$'White', function(x) { gsub("[\r\n]", "", x) })
Race[c('White',' ')] <- str_split_fixed(Race$'White', ' ', 2)  # Assuming space separates values

# BLACK

Race$'Black' <- sapply(Race$'Black', function(x) { gsub("[\r\n]", "", x) })
Race[c('Black',' ')] <- str_split_fixed(Race$'Black', ' ', 2)  # Assuming space separates values

# Others

Race$'Other' <- sapply(Race$'Other', function(x) { gsub("[\r\n]", "", x) })
Race[c('Other',' ')] <- str_split_fixed(Race$'Other', ' ', 2)  # Assuming space separates values


#### remove the unwanted collumbs ####
Race <- Race |>
  select(c("Year","White","Black","Other"))

### LINE IS REDUNDANT
#names(RaceAndGunlaw) <- c("Year", "White", "Black", "Other")








#### Save data #### 

####################### MUST BE PARQUET FILE NOW ##

# Write to Parquet format
write_parquet(fear, "data/analysis_data/fear.parquet")
write_parquet(Gender, "data/analysis_data/Gender.parquet")
write_parquet(Health, "data/analysis_data/Health.parquet")
write_parquet(Age, "data/analysis_data/Age.parquet")
write_parquet(Race, "data/analysis_data/Race.parquet")
