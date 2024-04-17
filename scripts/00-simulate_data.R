##1 simulate fear data

set.seed(853)

simulated_fear_data <-
  tibble(
    year = c(1973:2022), 

    nmr =
      runif(n = 50, min = 20, max = 80)
  )

head(simulated_fear_data)

##2 simulate gender data
set.seed(853)

simulated_gender_data <-
  tibble(
    year =
      rep(c(1973:2022), 2),
    
    gender = c(rep("Male", 50), rep("Female", 50)),
    
    nmr =
      runif(n = 100, min = 20, max = 80)
  )

head(simulated_gender_data)

##3 simulate race data
set.seed(853)

simulated_race_data <-
  tibble(
    year =
      rep(c(1973:2022), 3),
    
    Race = c(rep("Black", 50), rep("White", 50), rep ("Other", 50)),
    
    nmr =
      runif(n = 150, min = 20, max = 80)
  )

head(simulated_race_data) #### add 50 for every variable i.e. 2 variables = 100, 3 variables = 150




##4 simulate Age data

set.seed(853)

simulated_age_data <-
  tibble(
    year = rep(c(1973:2022), 4), 
    Age = c(rep("18-34", 50), rep("35-49", 50), rep("50-64", 50), rep("65+", 50)),
    nmr = runif(n = 200, min = 20, max = 85) 
  )

head(simulated_age_data)



##5 simulate Health data

set.seed(853)

simulated_Health_data <-
  tibble(
    year = rep(c(1973:2022), 4), 
    Health = c(rep("Excellent", 50), rep("Good", 50), rep("Fair", 50), rep("Poor", 50)),
    nmr = runif(n = 200, min = 20, max = 80) 
  )

# View the first few rows of the simulated age data
head(simulated_Health_data)
