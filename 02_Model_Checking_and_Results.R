
########## Contents ##########

# Preamble

# Model Checking

# Data Preparation
    
    ## Results from Quick Projections

########## Preamble ##########

# Load libraries
library(tidyverse)
library(arrow)

# Age factor levels
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Load in data
df_proj <- read_csv("Data_Clean/Population Projections (2018).csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

m_pat_proj <- readRDS("Models/Patient Projections (R).RDS")

samples_bin_prior <- readRDS(file="Models/samples_bin_prior.rds")



########## Model Checking ##########


# Plot Prior predictive
gather_draws(samples_bin_prior, mu[i], prob[i], pat_rep[i])|>
  ggplot(aes(x = .value))+
  geom_histogram(fill = "light blue", bins = 30)+
  facet_wrap(~.variable, scales = "free")+
  ggtitle("Prior Predictive Distribution")+
  ylab("Count")+
  xlab("Value")


########## Data Preparation ##########




#### Results from Quick Projections ####

# Clean patient projection data
df_pat_proj <- as_tibble(m_pat_proj)|>
  rename_with(.fn = function(col){str_sub(col,2,-1)})|> # Column names as numbers
  mutate(proj_ID = 1:nrow(m_pat_proj))|>
  pivot_longer(-proj_ID, # tidy format
               names_to = "Sample",
               values_to = "Patients")|>
  mutate(Sample = as.numeric(Sample))


# Combine with population projection data
df_proj_comb <- df_proj|>
  mutate(proj_ID = 1:nrow(df_proj))|>
  left_join(df_pat_proj, by = "proj_ID", multiple = "all")|>
  select(-proj_ID)

# Save data
# write_parquet(df_proj_comb,sink = "Results/Quick Projections.parquet")
# df_proj_comb <- read_parquet("Results/Quick Projections.parquet")


# Posterior mean quick projections
df_proj_mean <- df_proj_comb|>
  group_by(`Year`, `CA_ID`, `Age`, `Sex`, `Population`)|>
  summarise(Patients = mean(Patients))

# Save data
# write_csv(df_proj_mean, file = "Results/Quick Projections Posterior Mean.csv")
# df_proj_mean <- read_csv("Results/Quick Projections Posterior Mean.csv")

