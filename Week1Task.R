

# Load libraries
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(viridis)
library(lubridate)


setwd("/Users/wendyyy/Documents/CHAMPS_Epi Work/Original Code")
Dataset1 <- read.csv("vw_lk_Analytics_Dataset_2024-09-18_12-12-47.csv", stringsAsFactors=FALSE, na.strings=c("",".","NA"))

# To split the infant age group, here is the steps:
# Filter the data for the CH00718(infant) age group.
# Calculate the age of each infant in days from calc_dob to calc_dod.
# Split the filtered data into two subgroups: infants aged 28 days to less than 6 months and those aged 6 months to less than 12 months.


class(Dataset1$calc_dob)
class(Dataset1$calc_dod)
summary(Dataset1$calc_dob)
head(Dataset1$calc_dob)
head(Dataset1$calc_dod)




# Convert calc_dob and calc_dod to Date format
Dataset1$calc_dob <- as.Date(Dataset1$calc_dob, format="%m/%d/%y")
Dataset1$calc_dod <- as.Date(Dataset1$calc_dod, format="%m/%d/%y")

head(Dataset1$calc_dob)
head(Dataset1$calc_dod) 


# Filter for age group CH00718 (Infants 28 days to less than 12 months)
infant_data <- Dataset1 %>%
  filter(age_group == "CH00718")


head(infant_data$calc_dob)

# Calculate age in days
infant_data <- infant_data %>%
  mutate(age_in_days = as.numeric(difftime(calc_dod, calc_dob, units = "days")))

  
head(infant_data$age_in_days)



# Infants aged 28 days to less than 6 months (28 to 182 days)
infants_28days_to_6months <- infant_data %>%
  filter(age_in_days >= 28 & age_in_days < 182)

# Infants aged 6 months to less than 12 months (182 to 365 days)
infants_6months_to_12months <- infant_data %>%
  filter(age_in_days >= 182 & age_in_days < 365)


head(infants_28days_to_6months)
head(infants_6months_to_12months)



# Filter for age group CH01405 (Early Neonate (1 to 6 days))
early_neonate_data <- Dataset1 %>%
  filter(age_group == "CH01405")

early_neonate_data <- early_neonate_data %>%
  mutate(age_in_hours = as.numeric(difftime(calc_dod, calc_dob, units = "hours")))

# Early neonates aged 24 hours to less than 72 hours (1 day to <3 days)
early_neonates_24_to_72hours <- early_neonate_data %>%
  filter(age_in_hours >= 24 & age_in_hours < 72) %>%
  mutate(subgroup = "24 to <72 hours")

# Early neonates aged 72 hours to 6 days (3 days to <6 days, 72 to 144 hours)
early_neonates_72hours_to_6days <- early_neonate_data %>%
  filter(age_in_hours >= 72 & age_in_hours < 144) %>%
  mutate(subgroup = "72 hours to <6 days")

infant_combined <- bind_rows(infants_28days_to_6months, infants_6months_to_12months)
early_neonate_combined <- bind_rows(early_neonates_24_to_72hours, early_neonates_72hours_to_6days)
Dataset1_final <- bind_rows(Dataset1, infant_combined, early_neonate_combined)

head(Dataset1_final)


table(Dataset1_final)





##### Play around with the Code provided earler #####
vars_date_1<-c("calc_dob","calc_dod")

Dataset1 <- Dataset1 %>%
  mutate(across(all_of(c(vars_date_1)), as.character)) %>%
  mutate(across(all_of(vars_date_1), ~as_date(.x, format="%m/%d/%Y")))

Dataset1 %>%
  select(all_of(vars_date_1)) %>%
  slice(1:5)

### . ###




##### Task 2 #####


###### Then look at Staphylococcus aureus in different age group ######

vars_eti<-c("Underlying_Cause_Factor_etiol1","Underlying_Cause_Factor_etiol1_othr","Underlying_Cause_Factor_etiol2","Underlying_Cause_Factor_etiol2_othr",
            "Underlying_Cause_Factor_etiol3","Underlying_Cause_Factor_etiol3_othr","Immediate_Cause_of_Death_etiol1","Immediate_Cause_of_Death_etiol1_othr",
            "Immediate_Cause_of_Death_etiol2","Immediate_Cause_of_Death_etiol2_othr","Immediate_Cause_of_Death_etiol3","Immediate_Cause_of_Death_etiol3_othr",
            "Morbid_Condition_01_etiol1","Morbid_Condition_01_etiol1_othr","Morbid_Condition_01_etiol2","Morbid_Condition_01_etiol2_othr",
            "Morbid_Condition_01_etiol3","Morbid_Condition_01_etiol3_othr","Morbid_Condition_02_etiol1","Morbid_Condition_02_etiol1_othr",     
            "Morbid_Condition_02_etiol2","Morbid_Condition_02_etiol2_othr","Morbid_Condition_02_etiol3","Morbid_Condition_02_etiol3_othr",
            "Morbid_Condition_03_etiol1","Morbid_Condition_03_etiol1_othr","Morbid_Condition_03_etiol2","Morbid_Condition_03_etiol2_othr",
            "Morbid_Condition_03_etiol3","Morbid_Condition_03_etiol3_othr","Morbid_Condition_04_etiol1","Morbid_Condition_04_etiol1_othr",
            "Morbid_Condition_04_etiol2","Morbid_Condition_04_etiol2_othr","Morbid_Condition_04_etiol3","Morbid_Condition_04_etiol3_othr",
            "Morbid_Condition_05_etiol1","Morbid_Condition_05_etiol1_othr","Morbid_Condition_05_etiol2","Morbid_Condition_05_etiol2_othr",
            "Morbid_Condition_05_etiol3","Morbid_Condition_05_etiol3_othr","Morbid_Condition_06_etiol1","Morbid_Condition_06_etiol1_othr",
            "Morbid_Condition_06_etiol2","Morbid_Condition_06_etiol2_othr","Morbid_Condition_06_etiol3","Morbid_Condition_06_etiol3_othr",
            "Morbid_Condition_07_etiol1","Morbid_Condition_07_etiol1_othr","Morbid_Condition_07_etiol2","Morbid_Condition_07_etiol2_othr",
            "Morbid_Condition_07_etiol3","Morbid_Condition_07_etiol3_othr","Morbid_Condition_08_etiol1","Morbid_Condition_08_etiol1_othr",
            "Morbid_Condition_08_etiol2","Morbid_Condition_08_etiol2_othr","Morbid_Condition_08_etiol3","Morbid_Condition_08_etiol3_othr") 

staph_cases <- Dataset1_final %>%
  filter(apply(Dataset1_final[vars_eti], 1, function(row) any(grepl("Staphylococcus aureus", row, ignore.case = TRUE))))


# Calculate the total number of deaths for each age group from the original dataset
total_deaths_by_age <- Dataset1_final %>%
  group_by(age_group) %>%
  summarise(Total_Deaths = n())  # Total number of deaths in each age group

print(total_deaths_by_age)

# Calculate the number of Staphylococcus aureus cases for each age group from the filtered staph_cases
staph_cases_by_age <- staph_cases %>%
  group_by(age_group) %>%
  summarise(Staph_Cases = n())  # Count of Staphylococcus aureus cases in each age group

print(staph_cases_by_age)

# Merge both summaries to calculate the percentage of Staphylococcus aureus deaths
age_group_summary <- left_join(total_deaths_by_age, staph_cases_by_age, by = "age_group") %>%
  mutate(
    Staph_Cases = replace_na(Staph_Cases, 0),  # Replace NA with 0
    Percentage_Staph = (Staph_Cases / Total_Deaths) * 100  # Calculate the percentage
  )

# Print the final summary table
print(age_group_summary) 


# CH00716 Stillbirth
# CH00718 Infant (28 days to less than 12 months)
# CH00719 Child (12 months to less than 60 Months)
# CH01404	Death in the first 24 hours
# CH01405	Early Neonate (1 to 6 days)
# CH01406	Late Neonate (7 to 27 days) 

summary(Dataset1_final)

save(Dataset1_final, file="/Users/wendyyy/Documents/CHAMPS_Epi Work/Champs_R/AnalyticsData.rda")





