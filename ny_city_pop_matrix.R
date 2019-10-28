# New York City population matrix
# Author: Andrew Flowers <andrew.w.flowers@gmail.com>

library(tidyverse)

setwd("~/github/new-york-msa-pop-matrix/") # This changes depending on your machine

options(scipen=999)

## STEP ONE: Load raw data

# What is this data and how was it created? 
# What: IPUMS microdata from American Community Survey (ACS) 2017 5-year (2013-2017)
# IPUMS homepage for ACS data: https://usa.ipums.org/usa/index.shtml
# Key variables: CITY, SEX, AGE, RACE, and various DIFF* (disability) variables
# To recreate start here: https://usa.ipums.org/usa-action/variables/group

# Download the zipped data file (either MSA or City) here: https://drive.google.com/open?id=1NZrNHa3A2U8arzmtZvALPNZvFsxjLaMO
# Once unzipped (click on it), you should moved the file "acs_2017_5yr_race_age_sex_disability_city.csv" to your working directory

getwd()

raw_ipums_data <- read_csv("acs_2017_5yr_race_age_sex_disability_city.csv") %>% 
  filter(CITY == 4610) # Filter for NY City: https://usa.ipums.org/usa-action/variables/CITY#codes_section

clean_ipums_data <- raw_ipums_data %>% 
  mutate(
    # Sex categories: https://usa.ipums.org/usa-action/variables/SEX#codes_section
    sex_clean = case_when(
      (SEX == 1) ~ 'Male',
      (SEX == 2) ~ 'Female',
    ),
    # Age categories: https://usa.ipums.org/usa-action/variables/AGE#codes_section
    age_clean = case_when(
      (AGE <= 5) ~ '00_05',
      (AGE >= 6) & (AGE <= 18) ~ '06_18',
      (AGE >= 19) & (AGE <= 64) ~ '19_64',
      (AGE >=65) ~ '65_+',
    ),
    # Racial categories: (1) https://usa.ipums.org/usa-action/variables/RACE#codes_section and (2) https://usa.ipums.org/usa-action/variables/HISPAN#codes_section
    race_clean = case_when(
      (RACE == 1) & (HISPAN == 0) ~ 'White',
      (RACE %in% c(4, 5, 6)) & (HISPAN == 0) ~ 'Asian',
      (RACE %in% c(8, 9)) & (HISPAN == 0) ~ 'Mixed_race',
      (RACE != 2) & (HISPAN %in% c(1, 2, 3, 4)) ~ 'Hispanic',
      (RACE == 2) & (HISPAN %in% c(1, 2, 3, 4)) ~ 'Black_hispanic',
      (RACE == 2) & (HISPAN == 0) ~ 'Black',
      (RACE == 3) & (HISPAN == 0) ~ 'Native_american',
      (RACE == 7) & (HISPAN == 0) ~ 'Other'
    ),
    # Disability boolean (TRUE/FALSE): https://usa.ipums.org/usa-action/variables/group?id=disab
    disability_clean = case_when(
      (
        (DIFFREM == 2) |
        (DIFFPHYS == 2) |
        (DIFFMOB == 2) |
        (DIFFCARE == 2) |
        (DIFFSENS == 2) |
        (DIFFEYE == 2) |
        (DIFFHEAR == 2)
      ) ~ TRUE
    ),
    disability_clean = ifelse(is.na(disability_clean), FALSE, disability_clean)
  )

# Total NY City pop est
total_pop_est <- clean_ipums_data %>% summarize(total_pop_est = sum(PERWT, na.rm = T)) %>% .$total_pop_est
# Note: All population estimates must use PERWT variable -- the Census weights at the person (respondent) level

## Two matrices

# Sex by age by race
sex_age_race_matrix_long <- clean_ipums_data %>% 
  group_by(sex_clean, age_clean, race_clean) %>% 
  summarize(pop_est = sum(PERWT, na.rm = T),
            share_est = (pop_est / total_pop_est)*1e2)

sex_age_race_matrix_wide <- sex_age_race_matrix_long %>% 
  select(-pop_est) %>% 
  spread(key = race_clean, value = share_est) %>% 
  select(age_clean, sex_clean, White, Asian, Mixed_race, Hispanic, Black_hispanic, Black, Native_american) %>% 
  arrange(age_clean, desc(sex_clean))

sex_age_race_matrix_wide %>% write_csv("sex_age_race_matrix_city_wide.csv")

# Sex by age by race by disability
sex_age_race_dis_matrix_long <- clean_ipums_data %>% 
  group_by(sex_clean, age_clean, race_clean, disability_clean) %>% 
  summarize(pop_est = sum(PERWT, na.rm = T),
            share_est = (pop_est / total_pop_est)*1e2)

sex_age_race_dis_matrix_wide <- sex_age_race_dis_matrix_long %>% 
  select(-pop_est) %>% 
  spread(key = race_clean, value = share_est) %>% 
  select(disability_clean, age_clean, sex_clean, White, Asian, Mixed_race, Hispanic, Black_hispanic, Black, Native_american, Other) %>% 
  arrange(disability_clean, age_clean, desc(sex_clean))

sex_age_race_dis_matrix_wide %>% write_csv("sex_age_race_dis_city_matrix_wide.csv")

## Fact checks
# Other than disability status, all comparison stats here: https://censusreporter.org/profiles/16000US3651000-new-york-ny/
# For disbility status, comparison here: https://www.riemerlawfirm.com/wiki/new-york-disability-statistics

# Total pop
clean_ipums_data %>% summarize(total_pop_est = sum(PERWT, na.rm = T)) %>% .$total_pop_est

# Age
clean_ipums_data %>% group_by(age_clean) %>% summarize(total_pop_est = (sum(PERWT, na.rm = T) / total_pop_est)*1e2)

# Sex
clean_ipums_data %>% group_by(sex_clean) %>% summarize(total_pop_est = (sum(PERWT, na.rm = T) / total_pop_est)*1e2)

# Race
clean_ipums_data %>% group_by(race_clean) %>% summarize(total_pop_est = (sum(PERWT, na.rm = T) / total_pop_est)*1e2)

# Disability
clean_ipums_data %>% group_by(disability_clean) %>% summarize(total_pop_est = (sum(PERWT, na.rm = T) / total_pop_est)*1e2)

  
  
