
########## Contents ##########

# Preamble

# Data Preparation

# Simple Binomial Model
  
  ## Stan Code
  ## Prior Predictive
  ## Posterior
  ## Projections
  ## Quick Projections

########## Preamble ##########

# Load libraries
library(tidybayes)
library(tidyverse)
library(rstan)

# Age factor levels
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Load in data
df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

df_proj <- read_csv("Data_Clean/Population Projections (2018).csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

# Useful functions
f_inv_logit <-  function(x) exp(x)/(1+exp(x)) # Inverse_logit



########## Data Preparation ##########

age_level_num <- 1:19

df_HA_stan <- df_HA_model|>
  mutate(Age = age_level_num[df_HA_model$Age], # Convert factor to numeric
         Sex = if_else(Sex=="Female",1,2))

df_proj_stan <- df_proj|>
  mutate(Age = age_level_num[df_proj$Age], # Convert factor to numeric
         Sex = if_else(Sex=="Female",1,2))

########## Simple Binomial Model ##########

# Start here, can later add over-dispersion, or a Poisson element to 
# estimate total length of stays



#### Stan Code ####

code_bin <- 
  "data{
  
    // Data
    int<lower=0> n;
    array[n] int patients;
    array[n] int pop;
    array[n] int<lower=1, upper=32> CA;
    array[n] int<lower=1, upper=19> age;
    array[n] int<lower=1, upper=2> sex;
    
    // Switch to evaluate the likelihood
    int<lower = 0, upper = 1> condition_on_data;
}
parameters{
    
    // Hyperpriors
    real mu_CA;
    real mu_age;
    real mu_sex;
    real<lower = 0> sigma_CA;
    real<lower = 0> sigma_age;
    real<lower = 0> sigma_sex;
    
    // Model Parameters
    vector[32] b_CA;
    vector[19] b_age;
    vector[2] b_sex;
}
model{

    // Linear predictor
    vector[n] mu;
    
    // Hyperpriors
    mu_CA ~ normal(-0.3,0.1);
    mu_age ~ normal(-0.3,0.1);
    mu_sex ~ normal(-0.3,0.1);
    sigma_CA ~ normal(0,0.5);
    sigma_age ~ normal(0,0.5);
    sigma_sex ~ normal(0,0.5);
    
    // Priors
    //alpha ~ normal(-1,1); // skew towards probabilities < 0.5 
    b_CA ~ normal(mu_CA,sigma_CA);
    b_age ~ normal(mu_age,sigma_age);
    b_sex ~ normal(mu_sex,sigma_sex);
    
    // Model
    
    // Linear predictor 
    mu = b_CA[CA] + b_age[age] + b_sex[sex];
    
    // Condition on data
    if(condition_on_data==1){
      patients ~ binomial_logit(pop, mu);
    }
}
generated quantities{
    
    // Initialise
    vector[n] mu;
    vector[n] prob;
    array[n] int pat_rep;
    
    // Generate fitted values
    mu = b_CA[CA] + b_age[age] + b_sex[sex];
    prob = inv_logit(mu);
      
    // Generate replicated data
    pat_rep=binomial_rng(pop,prob);
}"



#### Prior Predictive ####

# Input list
list_bin_prior <- list(patients = df_HA_stan$Patients,
                       pop = df_HA_stan$Population,
                       CA = df_HA_stan$CA_ID,
                       age = df_HA_stan$Age,
                       sex = df_HA_stan$Sex,
                       condition_on_data = 0, # the model does not condition on the data
                       n = nrow(df_HA_stan))



if(!file.exists("Models/samples_bin_prior.rds")){
  # Compiles the model
  stan_bin <- stan_model(model_name = "stan_bin",model_code=code_bin)
  
  # Sample
  samples_bin_prior <-  sampling(stan_bin,
                                 data = list_bin_prior, 
                                 chains=1, iter = 1000,
                                 cores = parallel::detectCores())
  
  # Save file
  saveRDS(samples_bin_prior,file="Models/samples_bin_prior.rds")
}else{
  samples_bin_prior <- readRDS(file="Models/samples_bin_prior.rds")
}


#### Posterior ####


# Input list
list_bin_post <- list(patients = df_HA_stan$Patients,
                       pop = df_HA_stan$Population,
                       CA = df_HA_stan$CA_ID,
                       age = df_HA_stan$Age,
                       sex = df_HA_stan$Sex,
                       condition_on_data = 1, # the model conditions on the data
                       n = nrow(df_HA_stan))



if(!file.exists("Models/samples_bin.rds")){
  # Compiles the model
  stan_bin <- stan_model(model_name = "stan_bin",model_code=code_bin)
  
  # Sample
  samples_bin <-  sampling(stan_bin,
                           data = list_bin_post, 
                           chains=4, iter = 2000,
                           cores = parallel::detectCores())
  
  # Save file
  saveRDS(samples_bin,file="Models/samples_bin.rds")
}else{
  samples_bin <- readRDS(file="Models/samples_bin.rds")
}




#### Projections ####

# Better to integrate the below into the generated quantities block of
# the model above

code_bin_proj <- 
"data{

  // Data - Population Projections
  int<lower=0> n;
  array[n] int pop_proj;
  array[n] int<lower=1, upper=32> CA;
  array[n] int<lower=1, upper=19> age;
  array[n] int<lower=1, upper=2> sex;
  
  // Parameter Samples
  int<lower=0> n_samples;
  array[32, n_samples] real b_CA;
  array[19, n_samples] real b_age;
  array[2, n_samples] real b_sex;
}
parameters{
}
model{
}
generated quantities{

    // Initialise
    real mu;
    real prob;
    array[n,n_samples] int pat_proj;
    
    for(i in 1:n){
    
        for(j in 1:n_samples){
        
          // Generate fitted values
          mu = b_CA[CA[i],j] + b_age[age[i],j] + b_sex[sex[i],j];
          prob = inv_logit(mu);
      
          // Generate replicated data
          pat_proj[i,j]=binomial_rng(pop_proj[i],prob);
      
        }
    
    }

}"


m_CA_param <- as.matrix(samples_bin, pars = "b_CA")|> t()
m_age_param <- as.matrix(samples_bin, pars = "b_age")|> t()
m_sex_param <- as.matrix(samples_bin, pars = "b_sex")|> t()

# Input list
list_bin_proj <- list(pop_proj = df_proj_stan$Population,
                      CA = df_proj_stan$CA_ID,
                      age = df_proj_stan$Age,
                      sex = df_proj_stan$Sex,
                      n = nrow(df_proj_stan),
                      b_CA = m_CA_param,
                      b_age = m_age_param,
                      b_sex = m_sex_param,
                      n_samples = ncol(m_CA_param))



if(!file.exists("Models/samples_bin_proj.rds")){
  # Compiles the model
  stan_bin_proj <- stan_model(model_name = "stan_bin_proj",model_code=code_bin_proj)
  
  # Sample
  # samples_bin_proj <-  sampling(stan_bin_proj,
  #                          data = list_bin_proj, 
  #                          cores = parallel::detectCores(),
  #                          chains = 1, iter = 100,
  #                          algorithm = "Fixed_param" )
  
  # Save file
  #saveRDS(samples_bin_proj,file="Models/samples_bin_proj.rds")
}else{
  samples_bin_proj <- readRDS(file="Models/samples_bin_proj.rds")
}



#### Quick Projections ####

# Projections in R using samples from the parameter posteriors

n_pop = nrow(df_proj_stan)
n_samp = ncol(m_CA_param)

m_pat_proj <- matrix(data = 0, nrow = n_pop, ncol = n_samp)

for(i in 1:n_pop){
  
  for(j in 1:n_samp){

    # Generate fitted values
    mu = m_CA_param[df_proj_stan$CA_ID[i],j] + 
      m_age_param[df_proj_stan$Age[i],j] + 
      m_sex_param[df_proj_stan$Sex[i],j];
    prob = f_inv_logit(mu);
    
    # Generate replicated data
    m_pat_proj[i,j] <- rbinom(n = 1, size = df_proj_stan$Population[i], prob = prob);
    
  }
  
}



# Save data
# saveRDS(m_pat_proj, file = "Models/Patient Projections (R).RDS")
# m_pat_proj <- readRDS("Models/Patient Projections (R).RDS")


