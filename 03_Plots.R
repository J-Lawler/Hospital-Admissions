
########## Contents ##########

# Preamble

# Pre-Model Plots

# Post-Model Plots

########## Preamble ##########

# Load libraries
library(tidyverse)

# Age factor levels
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Load in data
df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

df_proj_mean <- read_csv("Results/Quick Projections Posterior Mean.csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels



########## Pre-Model Plots ##########

## Ratio of patients to total population by age, grouped by council area.

df_HA_model|>
  group_by(CA_ID, Age)|>
  summarise(Patients = sum(Patients),
            Population = sum(Population))|>
  mutate(patient_ratio = Patients / Population)|>
  ggplot(mapping = aes(x= Age, y = patient_ratio, group = CA_ID))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90))





########## Post-Model Plots ##########


## Posterior mean - by age
df_proj_mean|>
  filter(Year %in% c(2023,2033,2043))|>
  group_by(`Year`,`Age`)|>
  summarise(Patients = mean(Patients))|>
  ggplot(aes(x = Age, y = Patients, group = Year, col = Year))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90))







