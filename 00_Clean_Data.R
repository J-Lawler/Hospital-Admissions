
########## Preamble ##########

# Load libraries
library(VIM)
library(tidyverse)
library(readxl)




########## Contents ##########

# Council Area Codes

# Health Board Codes

# Activity by Council Area, Age and Sex 

# Age Lookup Table

# Council Area Population Estimates

# Combine Hospitalisation with Council Area Populations

# Council Area Projections




########## Council Area Codes ##########

# Load in data
df_council <- read_csv("Data_Raw/Council Area Codes.csv")

# Select only council area data
df_council <- df_council |>
  select(starts_with("CA"))

# Need to check how much the boundary changes in 2018 and 2019 
# affect the population data

# Data frame of the 32 council area names, assigning each an ID
df_CA_ID <- tibble(CA_name = sort(unique(df_council$CAName)),
                        CA_ID = 1:32)

# Merge with data frame of council area codes
df_CA <- df_council |>
  select(CA, CAName) |>
  unique() |>
  left_join(df_CA_ID, by = c("CAName" = "CA_name"))|>
  arrange(CAName)

# write_csv(df_CA, file = "Data_Clean/Council Areas.csv")
# df_CA <- read_csv("Data_Clean/Council Areas.csv")




########## Health Board Codes ##########

# Load in data
df_health_board <- read_csv("Data_Raw/Health Board Codes.csv")

# Remove extraneous columns
df_health_board <- df_health_board |>
  select(HB, HBName)

# Data frame of the 14 health board names, assigning each an ID
df_HB_ID <- tibble(HB_name = sort(unique(df_HB$HBName)),
                   HB_ID = 1:14)

# Merge with data frame of health board codes
df_HB <- df_health_board |>
  left_join(df_HB_ID, by = c("HBName" = "HB_name"))|>
  arrange(HBName)

# write_csv(df_HB, file = "Data_Clean/Health Boards.csv")
# df_HB <- read_csv("Data_Clean/Health Boards.csv")




########## Activity by Council Area, Age and Sex ##########

# Age is character needs to be a factor
# Sex also a character
# Missing data - why? to do with transfer?

# Load in data
df_activ <- read_csv("Data_Raw/Activity by Council Area, Age and Sex.csv")

df_CA <- read_csv("Data_Clean/Council Areas.csv")


df_HA_clean <- df_activ|>
  # Remove qualifier and rate columns
  select(-ends_with("QF"), -ends_with("Rate"),-"_id")|>
  # Filter age and sex values
  filter(Sex!="All Sexes",
         !(Age %in% c("All Ages","Under 18 years",
                      "65 years and over", "75 years and over",
                      "85 years and over")))

# Visualise missingness
vis <- aggr(df_HA_clean[,6:10], plot = TRUE, numbers = TRUE) 

# df where episode is missing - Admission type always "Not Specified"
df_no_episode <- df_HA_clean|>
  filter(is.na(Episodes))

# df where stay is missing - Admission type always "Transfer"
df_no_stay <- df_HA_clean|>
  filter(is.na(Stays))


# All inpatients and day cases
df_inpat_day <- df_HA_clean|>
  filter(AdmissionType %in% c("Not Specified", "Elective", "Emergency",
                              "Day case", "All Inpatients and Day cases"))


df_HA <- df_inpat_day |>
  filter(!(CA %in% c("RA2701", # No Fixed Abode
                     "RA2702", # Rest of UK (Outside Scotland)
                     "RA2703", # Outside the UK
                     "RA2704", # Unknown Residency
                     "S92000003"))) |> # Country code for Scotland
  left_join(df_CA, by = "CA")|>
  select(-CA)

# Format before saving
df_HA <- df_HA|>
  mutate(Year = str_sub(FinancialYear, 1, 4),
         Year = as.integer(Year))|>
  select(Year, AdmissionType, CA_ID, Age, Sex,
         Stays, TotalLengthofStay, Patients)


# Alter any Stays = 0 to Stays = 1 due to data discrepancy discussed in report
df_HA <- df_HA|>
  mutate(Stays = if_else(Stays==0 & AdmissionType!="Not Specified",1,Stays))

# write_csv(df_HA, file = "Data_Clean/Hospitalisation Activity (CA).csv")
# df_HA <- read_csv("Data_Clean/Hospitalisation Activity (CA).csv")

# Check how Elective, Not Specified, Emergency and Day Case cateogories compare
# to "All Inpatients and Day Cases" category.

df_all <- df_HA|>
  filter(AdmissionType=="All Inpatients and Day cases")|>
  select(Year, CA_ID, Age, Sex, Stays, TotalLengthofStay)

df_in_and_out <- df_HA|>
  filter(AdmissionType!="All Inpatients and Day cases")|>
  group_by(Year,CA_ID,Age,Sex)|>
  summarise(Stays = sum(Stays),
            TotalLengthofStay = sum(TotalLengthofStay))

df_comparison <- df_in_and_out|>
  left_join(df_all, by = c("Year", "CA_ID", "Age", "Sex"),
            suffix = c("","_All"))|>
  mutate(Stays_perc = Stays / Stays_All *100,
         LoS_perc = TotalLengthofStay / TotalLengthofStay_All *100)



########## Age Lookup Table ##########

# df_HA <- read_csv("Data_Clean/Hospitalisation Activity (CA).csv")

ages <- unique(df_HA$Age) # check order correct each time you run this

df_ages <- tibble(Age = 0:94,
                  Age_Groups = rep(ages, each = 5)) # check categories each time

# write_csv(df_ages, file = "Data_Clean/Age Lookup Table.csv")
# df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")




########## Council Area Population Estimates ##########

# Load in data
df_CA_pop_raw <- read_csv("Data_Raw/Council Area Population Estimates.csv")

# Age levels in order
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Clean data
df_CA_pop_age_groups <- df_CA_pop_raw|>
  filter(CA != "S92000003", # Remove Scotland figure
         Sex != "All")|>
  rename(Age90 = Age90plus)|>
  select(-c(`_id`, CAQF, SexQF, AllAges))|>
  rename_with(.cols = -c(Year,CA,Sex), .fn = function(col){str_sub(col,4,-1)})|>
  pivot_longer(cols = -c(Year,CA,Sex), names_to = "Age", values_to = "Population")|>
  mutate(Age = as.integer(Age))

# Combine ages
df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")

df_CA_pop_clean <- df_CA_pop_age_groups |>
  left_join(df_ages, by = "Age") |>
  select(-Age)|>
  group_by(`Year`, `CA`, `Sex`,`Age_Groups`)|>
  summarise(Population = sum(Population)) |>
  ungroup()|>
  mutate(Age = factor(Age_Groups, levels = age_levels))

# Add CA ID and Format

df_CA <- read_csv("Data_Clean/Council Areas.csv")

df_CA_pop <- df_CA_pop_clean |>
  left_join(df_CA,  by = "CA")|>
  select(Year, CA_ID, Age, Sex, Population)|>
  arrange(Year, CA_ID, Age, Sex)

# Save data
# write_csv(df_CA_pop, file = "Data_Clean/Council Area Population.csv")
# df_CA_pop <- read_csv("Data_Clean/Council Area Population.csv")




########## Combine Hospitalisation with Council Area Populations ##########  

df_HA <- read_csv("Data_Clean/Hospitalisation Activity (CA).csv")
df_CA_pop <- read_csv("Data_Clean/Council Area Population.csv")

# Age levels in order
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Join
df_HA_model <- df_HA|>
  left_join(df_CA_pop, by = c("Year" = "Year",
                                "CA_ID" = "CA_ID",
                                "Age" = "Age",
                                "Sex" = "Sex"))|>
  mutate(Age = factor(Age, levels = age_levels))|>
  select(Year, CA_ID, Age, Sex, AdmissionType, Population, Stays, 
        TotalLengthofStay, Patients)|>
  arrange(Year, CA_ID, Age, Sex, AdmissionType)

# Save data
# write_csv(df_HA_model , file = "Data_Clean/HA Model Data.csv")
# df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")




########## Council Area Projections ##########  

# Load in data
df_CA_proj_raw <- read_csv("Data_Raw/Council Area Projections.csv")

# Age levels in order
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Clean data
df_CA_proj_age_groups <- df_CA_proj_raw|>
  filter(CA != "S92000003", # Remove Scotland figure
         Sex != "All")|>
  rename(Age90 = Age90plus)|>
  select(-c(CAQF, SexQF, AllAges))|>
  rename_with(.cols = -c(Year,CA,Sex), .fn = function(col){str_sub(col,4,-1)})|>
  pivot_longer(cols = -c(Year,CA,Sex), names_to = "Age", values_to = "Population")|>
  mutate(Age = as.integer(Age))

# Combine ages
df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")

df_CA_proj_clean <- df_CA_proj_age_groups |>
  left_join(df_ages, by = "Age") |>
  select(-Age)|>
  group_by(`Year`, `CA`, `Sex`,`Age_Groups`)|>
  summarise(Population = sum(Population)) |>
  ungroup()|>
  mutate(Age = factor(Age_Groups, levels = age_levels))

# Add CA ID and Format

df_CA <- read_csv("Data_Clean/Council Areas.csv")

df_CA_proj <- df_CA_proj_clean |>
  left_join(df_CA,  by = "CA")|>
  select(Year, CA_ID, Age, Sex, Population)|>
  arrange(Year, CA_ID, Age, Sex)

# Save data
# write_csv(df_CA_proj, file = "Data_Clean/Council Area Projections.csv")
# df_CA_proj <- read_csv("Data_Clean/Council Area Projections.csv")




