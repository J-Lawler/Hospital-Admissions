
########## Preamble ##########

# Load libraries
library(VIM)
library(tidyverse)
library(readxl)




########## Contents ##########

# Council Area Codes

# Activity by Council Area, Age and Sex 

# Age Lookup Table

# Mid-Year Population Estimates Time Series

# Combine Hospitalisation with Population Histories

# Population Projections




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
  left_join(df_CA_ID, by = c("CAName" = "CA_name"))

# write_csv(df_CA, file = "Data_Clean/Council Areas.csv")
# df_CA <- read_csv("Data_Clean/Council Areas.csv")




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


# All inpatients
df_inpatients <- df_HA_clean|>
  filter(AdmissionType == "All Inpatients")


df_HA <- df_inpatients |>
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

# write_csv(df_HA, file = "Data_Clean/Hospitalisation Activity.csv")
# df_HA <- read_csv("Data_Clean/Hospitalisation Activity.csv")




########## Age Lookup Table ##########

# df_HA <- read_csv("Data_Clean/Hospitalisation Activity.csv")

ages <- unique(df_HA$Age)

df_ages <- tibble(Age = 0:94,
                  Age_Groups = rep(ages, each = 5))

# write_csv(df_ages, file = "Data_Clean/Age Lookup Table.csv")
# df_ages <- read_csv("Data_Clean/Age Lookup Table.csv")




########## Mid-Year Population Estimates Time Series ##########

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

df_HA <- read_csv("Data_Clean/Hospitalisation Activity.csv")
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
