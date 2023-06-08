
########## Preamble ##########

# Load libraries
library(VIM)
library(tidyverse)
library(readxl)




########## Contents ##########

# Council Area Codes

# Health Board Codes

# Activity by Council Area, Age and Sex 

# Activity by Health Board of Treatment, Age and Sex 

# Age Lookup Table

# Age Lookup Table (BT)

# Council Area Population Estimates

# Health Board Population Estimates

# Combine Hospitalisation with Council Area Populations

# Combine Hospitalisation with Health Board Populations

# Council Area Projections

# Health Board Projections

# Mid-Year Population Estimates Time Series - NO LONGER USED

# Combine Hospitalisation with Population Histories - NO LONGER USED

# Population Projections - NO LONGER USED




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
  filter(AdmissionType == "All Inpatients and Day cases")


df_HA <- df_inpat_day |>
  filter(!(CA %in% c("RA2701", # No Fixed Abode
                     "RA2702", # Rest of UK (Outside Scotland)
                     "RA2703", # Outside the UK
                     "RA2704", # Unknown Residency
                     "S92000003"))) |> # Country code for Scotland
  left_join(df_CA, by = "CA")|>
  select(-AdmissionType,
         -CA)

# Format before saving
df_HA <- df_HA|>
  mutate(Year = str_sub(FinancialYear, 1, 4),
         Year = as.integer(Year))|>
  select(Year, CA_ID, Age, Sex,
         Episodes, Stays, TotalLengthofEpisode, TotalLengthofStay, Patients)

# write_csv(df_HA, file = "Data_Clean/Hospitalisation Activity (CA).csv")
# df_HA <- read_csv("Data_Clean/Hospitalisation Activity (CA).csv")




########## Activity by Health Board of Treatment, Age and Sex ##########

# Load in Health Board information
df_HB <- read_csv("Data_Clean/Health Boards.csv")

# Load in data
df_activ_BT <- read_csv("Data_Raw/Activity by Board of Treatment, Age and Sex.csv")



df_HA_BT_clean <- df_activ_BT|>
  # Remove qualifier columns
  select(-ends_with("QF"),-"_id")

# All inpatients and day cases
df_inpat_day_BT <- df_HA_BT_clean|>
  filter(AdmissionType == "All Inpatients and Day cases")

# Filter to health board level data, add health board names and IDs
df_HA_BT <- df_inpat_day_BT |>
  filter(HB %in% unique(df_HB$HB),
         Location %in% unique(df_HB$HB)) |> 
  left_join(df_HB, by = "HB")|>
  select(-AdmissionType,
         -HB, - Location)

# Format before saving
df_HA_BT <- df_HA_BT|>
  mutate(Year = str_sub(Quarter, 1, 4),
         Year = as.integer(Year),
         Quarter = str_sub(Quarter, 6, -1),
         Quarter = as.integer(Quarter),)|>
  select(Year, Quarter, HB_ID, Age, Sex,
         Episodes, Stays, TotalLengthofEpisode = LengthOfEpisode, 
         TotalLengthofStay = LengthOfStay)|>
  arrange(Year, Quarter, HB_ID, Age, Sex)

# write_csv(df_HA_BT, file = "Data_Clean/Hospitalisation Activity (BT).csv")
# df_HA_BT <- read_csv("Data_Clean/Hospitalisation Activity (BT).csv")

# Data frame is 6 rows shorter than expected. Some quarter / age combinations
# contain rows for one sex but not both. Four female, two male missing.




########## Age Lookup Table ##########

# df_HA <- read_csv("Data_Clean/Hospitalisation Activity (CA).csv")

ages <- unique(df_HA$Age) # check order correct each time you run this

df_ages <- tibble(Age = 0:94,
                  Age_Groups = rep(ages, each = 5)) # check categories each time

# write_csv(df_ages, file = "Data_Clean/Age Lookup Table.csv")
# df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")




########## Age Lookup Table (BT) ##########

df_HA_BT <- read_csv("Data_Clean/Hospitalisation Activity (BT).csv")

ages <- unique(df_HA_BT$Age) # check order correct each time you run this

df_ages_BT <- tibble(Age = 0:99,
                  Age_Groups = rep(ages, each = 10)) # check categories each time

# write_csv(df_ages_BT, file = "Data_Clean/Age Lookup Table (BT).csv")
# df_ages_BT <- read_csv("Data_Clean/Age Lookup Table (BT).csv")



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




########## Health Board Population Estimates ##########

# Load in data
df_HB_pop_raw <- read_csv("Data_Raw/Health Board Population Estimates.csv")

# Age levels in order
age_levels_BT <- c("0-9 years", "10-19 years", "20-29 years", "30-39 years", 
                   "40-49 years", "50-59 years", "60-69 years", "70-79 years", 
                   "80-89 years", "90 years and over")

# Clean data
df_HB_pop_age_groups <- df_HB_pop_raw|>
  filter(HB != "S92000003", # Remove Scotland figure
         Sex != "All")|>
  rename(Age90 = Age90plus)|>
  select(-c(`_id`, HBQF, SexQF, AllAges))|>
  rename_with(.cols = -c(Year,HB,Sex), .fn = function(col){str_sub(col,4,-1)})|>
  pivot_longer(cols = -c(Year,HB,Sex), names_to = "Age", values_to = "Population")|>
  mutate(Age = as.integer(Age))

# Combine ages
df_ages_BT <- read_csv("Data_Clean/Age Lookup Table (BT).csv")

df_HB_pop_clean <- df_HB_pop_age_groups |>
  left_join(df_ages_BT, by = "Age") |>
  select(-Age)|>
  group_by(`Year`, `HB`, `Sex`,`Age_Groups`)|>
  summarise(Population = sum(Population)) |>
  ungroup()|>
  mutate(Age = factor(Age_Groups, levels = age_levels_BT))

# Add HB ID and Format

df_HB <- read_csv("Data_Clean/Health Boards.csv")

df_HB_pop <- df_HB_pop_clean |>
  left_join(df_HB,  by = "HB")|>
  select(Year, HB_ID, Age, Sex, Population)|>
  arrange(Year, HB_ID, Age, Sex)

# Save data
# write_csv(df_HB_pop, file = "Data_Clean/Health Board Population.csv")
# df_HB_pop <- read_csv("Data_Clean/Health Board Population.csv")




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
  select(Year, CA_ID, Age, Sex, Population, Episodes, Stays, 
         TotalLengthofEpisode, TotalLengthofStay, Patients)|>
  arrange(Year, CA_ID, Age, Sex)

# Save data
# write_csv(df_HA_model , file = "Data_Clean/HA Model Data.csv")
# df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")




########## Combine Hospitalisation with Health Board Populations ##########  

df_HA_BT <- read_csv("Data_Clean/Hospitalisation Activity (BT).csv")
df_HB_pop <- read_csv("Data_Clean/Health Board Population.csv")

# Age levels in order
age_levels_BT <- c("0-9 years", "10-19 years", "20-29 years", "30-39 years", 
                   "40-49 years", "50-59 years", "60-69 years", "70-79 years", 
                   "80-89 years", "90 years and over")

# Join
df_HA_model_BT <- df_HA_BT|>
  left_join(df_HB_pop, by = c("Year" = "Year",
                              "HB_ID" = "HB_ID",
                              "Age" = "Age",
                              "Sex" = "Sex"))|>
  mutate(Age = factor(Age, levels = age_levels_BT))|>
  select(Year, Quarter, HB_ID, Age, Sex, Population, Episodes, Stays, 
         TotalLengthofEpisode, TotalLengthofStay)|>
  arrange(Year,Quarter, HB_ID, Age, Sex)

# Save data
# write_csv(df_HA_model_BT , file = "Data_Clean/HA Model Data (BT).csv")
# df_HA_model_BT <- read_csv("Data_Clean/HA Model Data (BT).csv")




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




########## Health Board Projections ##########  

# Load in data
df_HB_proj_raw <- read_csv("Data_Raw/Health Board Projections.csv")

# Age levels in order
age_levels_BT <- c("0-9 years", "10-19 years", "20-29 years", "30-39 years", 
                   "40-49 years", "50-59 years", "60-69 years", "70-79 years", 
                   "80-89 years", "90 years and over")

# Clean data
df_HB_proj_age_groups <- df_HB_proj_raw|>
  filter(HB != "S92000003", # Remove Scotland figure
         Sex != "All")|>
  rename(Age90 = Age90plus)|>
  select(-c(`_id`,HBQF, SexQF, AllAges))|>
  rename_with(.cols = -c(Year,HB,Sex), .fn = function(col){str_sub(col,4,-1)})|>
  pivot_longer(cols = -c(Year,HB,Sex), names_to = "Age", values_to = "Population")|>
  mutate(Age = as.integer(Age))

# Combine ages
df_ages_BT <- read_csv("Data_Clean/Age Lookup Table (BT).csv")

df_HB_proj_clean <- df_HB_proj_age_groups |>
  left_join(df_ages_BT, by = "Age") |>
  select(-Age)|>
  group_by(`Year`, `HB`, `Sex`,`Age_Groups`)|>
  summarise(Population = sum(Population)) |>
  ungroup()|>
  mutate(Age = factor(Age_Groups, levels = age_levels_BT))

# Add HB ID and Format

df_HB <- read_csv("Data_Clean/Health Boards.csv")

df_HB_proj <- df_HB_proj_clean |>
  left_join(df_HB,  by = "HB")|>
  select(Year, HB_ID, Age, Sex, Population)|>
  arrange(Year, HB_ID, Age, Sex)

# Save data
# write_csv(df_HB_proj, file = "Data_Clean/Health Board Projections.csv")
# df_HB_proj <- read_csv("Data_Clean/Health Board Projections.csv")




########## Mid-Year Population Estimates Time Series ##########

# NO LONGER USED

# Load in data
df_pop_hist_raw <- read_xlsx(path = "Data_Raw/Mid-Year Population Estimates Time Series.xlsx",
                  sheet = "Table_1",
                  skip = 5)|>
  rename(`90` = `90 and over`) # Remember the 90 category includes all those over 90

# Clean data
df_pop_hist_clean <- df_pop_hist_raw|>
  filter(Year >=2011,
         Sex != "Persons",
         `Area name`!= "Scotland") |>
  select(-`All Ages`)|>
  pivot_longer(cols = 5:95, # tidy format
               names_to = "Age",
               values_to = "Population")|>
  mutate(Age = as.integer(Age))

# Combine ages

df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")

df_pop_hist <- df_pop_hist_clean |>
  left_join(df_ages, by = "Age") |>
  select(-Age)|>
  group_by(`Area code`, `Area name`, `Sex`, `Year`, `Age_Groups`)|>
  summarise(Population = sum(Population)) |>
  ungroup()

df_CA <- read_csv("Data_Clean/Council Areas.csv")

# Format before saving
df_pop_hist <- df_pop_hist |>
  mutate(Sex = str_sub(Sex, 1, -2))|>
  left_join(df_CA,  by = c("Area code" = "CA"))|>
  select(Year, CA_ID, Age = Age_Groups, Sex, Population)|>
  arrange(Year, CA_ID, Age, Sex)

# Save data
# write_csv(df_pop_hist, file = "Data_Clean/Population History.csv")
# df_pop_hist <- read_csv("Data_Clean/Population History.csv")




########## Combine Hospitalisation with Population Histories ##########  

# NO LONGER USED

df_HA <- read_csv("Data_Clean/Hospitalisation Activity (CA).csv")
df_pop_hist <- read_csv("Data_Clean/Population History.csv")

# Age levels in order
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Join
df_HA_model <- df_HA|>
  left_join(df_pop_hist, by = c("Year" = "Year",
                                "CA_ID" = "CA_ID",
                                "Age" = "Age",
                                "Sex" = "Sex"))|>
  mutate(Age = factor(Age, levels = age_levels))|>
  select(Year, CA_ID, Age, Sex, Population, Episodes, Stays, 
         TotalLengthofEpisode, TotalLengthofStay, Patients)|>
  arrange(Year, CA_ID, Age, Sex)

# Save data
# write_csv(df_HA_model , file = "Data_Clean/HA Model Data.csv")
# df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")

  
  
  
########## Population Projections ##########  

# NO LONGER USED

### Various useful objects

# Column headers
column_headers <- c("Age",2018:2043)

# Location of population projections files
loc_files <- "Data_Raw/Population Projections by Council Area (2018-based)/"

# Age lookup table
df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")

# Council area lookup table
df_CA <- read_csv("Data_Clean/Council Areas.csv")

test <- df_CA |>
  select(-CA)|>
  unique()|>
  arrange(CAName)

### List of project files
list_proj_files <- list.files(loc_files)

# Remove files for whole Scotland and for metadata
mask_proj <- str_detect(list_proj_files, "Scotland|Metadata" )

list_proj_files <- list_proj_files[!mask_proj]

### Function for loading in data

f_proj_load <- function(sex, skip_num){
  
  # Create empty list
  list <- vector(mode = "list", length = 32)
  
  # Loop
  for(i in 1:32){
    
    list[[i]] <- read_csv(paste0(loc_files,list_proj_files[1]),
                            col_names = column_headers,
                            skip = skip_num,
                            n_max = 91)|>
      mutate(Age = str_sub(Age,1,2),
             Age = as.integer(Age))|>
      pivot_longer(cols = -Age,
                   names_to = "Year",
                   values_to = "Population")|>
      left_join(df_ages, by = "Age") |>
      select(-Age)|>
      group_by(`Year`, `Age_Groups`)|>
      summarise(Population = sum(Population)) |>
      ungroup()|>
      mutate(Sex = sex,
             CA_ID = i)
  }
  
  # Combine data frames
  df_proj <- bind_rows(list)
  
}

### Loading in data

# sex is "Male" or "Female"
# skip_num is 106 for male, 205 for female

df_proj_m <- f_proj_load(sex= "Male", skip_num = 106)

df_proj_f <- f_proj_load(sex= "Female", skip_num = 205)

# Combine male and female data frames and tidy
df_proj <- bind_rows(df_proj_m, df_proj_f)|>
  select(Year, CA_ID, Age = Age_Groups, Sex, Population)|>
  arrange(Year, CA_ID, Age, Sex)


# Save data
# write_csv(df_proj, file = "Data_Clean/Population Projections (2018).csv")
# df_proj <- read_csv("Data_Clean/Population Projections (2018).csv")
