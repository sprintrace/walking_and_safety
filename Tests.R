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


#1    GGPlot for fear (the total of all Americans in survey)
fear <- read_parquet("data/analysis_data/fear.parquet")
View(fear)

transform(fear, 'Total Americans in Favour' = as.numeric('Total Americans in Favour')) 

is.numeric(fear$Total.Americans.in.Favour) ## test



names(fear) <- c("Year", "Total")

ggplot(data=fear, aes(x = Year, y = Total, group = 1)) +
  geom_line()+
  labs(title = "Percentage of Americans feeling unsafe walking alone at night", y = "Total % of Americans in fear walking alone at night")




#2    GGPlot for Gender
## load in parquet ##
Gender <- read_parquet("data/analysis_data/Gender.parquet")
View(Gender)
# View the first few rows of the dataset
head(Gender)

# Convert the 'Female' and 'Male' columns to numeric using dplyr for better readability and efficiency
Gender <- Gender %>%
  mutate(Female = as.numeric(Female),
         Male = as.numeric(Male))

# Check if the columns are numeric
is.numeric(Gender$Female) # Should return TRUE
is.numeric(Gender$Male)   # Should return TRUE


####Make 2 columns side by side into two values in one column one above the other (turn data into long format)
Gender <- Gender |> 
  pivot_longer(cols=c('Female', 'Male'), names_to='Gender', values_to='Total')
###### MAKE THE PLOT #######
ggplot(data=Gender, aes(x = Year, y=Total, group = Gender)) +
  geom_line(aes(colour=Gender))+
  labs(title = "Percentage of American Women vs Men \n who responded 'yes' to feeling unsafe outside at night", y = "Total percent of those who responded 'yes' for fear")





#3    GGPlot for Health 


# Read the data
Health <- read_parquet("data/analysis_data/Health.parquet")

# View the first few rows of the dataset
head(Health)

# Convert the health category columns to numeric and include tests to check the conversion
Health <- Health %>%
  mutate(
    Excellent = as.numeric(Excellent),
    Good = as.numeric(Good),
    Fair = as.numeric(Fair),
    Poor = as.numeric(Poor)
  ) %>%
  # Include tests after the transformation
  mutate(
    Excellent_is_numeric = is.numeric(Excellent),
    Good_is_numeric = is.numeric(Good),
    Fair_is_numeric = is.numeric(Fair),
    Poor_is_numeric = is.numeric(Poor)
  )

# Check the results of the tests
print(Health$Excellent_is_numeric) # test
print(Health$Good_is_numeric)      # test
print(Health$Fair_is_numeric)      # test
print(Health$Poor_is_numeric)      # test

# Remove the test columns before plotting
Health <- Health %>%
  select(-c(Excellent_is_numeric, Good_is_numeric, Fair_is_numeric, Poor_is_numeric))

# Pivot the data to long format
Health_long <- Health %>%
  pivot_longer(
    cols = c(Excellent, Good, Fair, Poor),
    names_to = "Healthy",
    values_to = "Total"
  )

# Create the ggplot
ggplot(data = Health_long, aes(x = Year, y = Total, group = Healthy)) +
  geom_line(aes(colour = Healthy)) +
  labs(title = "Percentage of Americans who responded yes to being afraid to walk alone at night based on their physical health",
       y = "% of population") +
  theme_minimal()


#4    GGPlot for Age

# Read the data
Age <- read_parquet("data/analysis_data/Age.parquet")

# View the first few rows of the dataset
head(Age)

# Convert the age group columns to numeric and include tests to check the conversion
Age <- Age %>%
  mutate(
    `18-34` = as.numeric(`18-34`),
    `35-49` = as.numeric(`35-49`),
    `50-64` = as.numeric(`50-64`),
    `65+` = as.numeric(`65+`)
  ) %>%
  # Include tests after the transformation
  mutate(
    `18-34_is_numeric` = is.numeric(`18-34`),
    `35-49_is_numeric` = is.numeric(`35-49`),
    `50-64_is_numeric` = is.numeric(`50-64`),
    `65+_is_numeric` = is.numeric(`65+`)
  )

# Check the results of the tests
print(Age$`18-34_is_numeric`) # Test
print(Age$`35-49_is_numeric`) # Test
print(Age$`50-64_is_numeric`) # Test
print(Age$`65+_is_numeric`)   # Test

# Remove the test columns before plotting
Age <- Age %>%
  select(-c(`18-34_is_numeric`, `35-49_is_numeric`, `50-64_is_numeric`, `65+_is_numeric`))

# Pivot the data to long format
Age_long <- Age %>%
  pivot_longer(
    cols = c(`18-34`, `35-49`, `50-64`, `65+`),
    names_to = "Age_Group",
    values_to = "Total"
  )

# Create the ggplot
ggplot(data = Age_long, aes(x = Year, y = Total, group = Age_Group)) +
  geom_line(aes(colour = Age_Group)) +
  labs(title = "Percentage of Americans who fear walking alone at night based on age group",
       y = "Total % in favour") +
  theme_minimal()




#5    GGPlot for Race

# Read the data
Race <- read_parquet("data/analysis_data/Race.parquet")

# View the first few rows of the dataset
head(Race)

# Convert the race category columns to numeric and include tests to check the conversion
Race <- Race %>%
  mutate(
    White = as.numeric(White),
    Black = as.numeric(Black),
    Other = as.numeric(Other)
  ) %>%
  # Include tests after the transformation
  mutate(
    White_is_numeric = is.numeric(White),
    Black_is_numeric = is.numeric(Black),
    Other_is_numeric = is.numeric(Other)
  )

# Check the results of the tests
print(Race$White_is_numeric) # Test
print(Race$Black_is_numeric) # Test
print(Race$Other_is_numeric) # Test

# Remove the test columns before plotting
Race <- Race %>%
  select(-c(White_is_numeric, Black_is_numeric, Other_is_numeric))

# Pivot the data to long format
Race_long <- Race %>%
  pivot_longer(
    cols = c(White, Black, Other),
    names_to = "Race",
    values_to = "Total"
  )

# Create the ggplot
ggplot(data = Race_long, aes(x = Year, y = Total, group = Race)) +
  geom_line(aes(colour = Race)) +
  labs(title = "Americans' who responded 'yes' to feeling fear walking at night based on racial identity",
       y = "Total % feeling fear") +
  theme_minimal()

