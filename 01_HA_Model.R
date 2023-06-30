



########## Contents ##########

# Preamble

# Data Preparation

# Poisson Poisson Model 

  ## Stan Code

  ## Prior Predictive

  ## Posterior



########## Preamble ##########

# Load libraries
library(tidybayes)
library(tidyverse)
library(rstan)
library(arrow)
library(moments)

# Age factor levels
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

admission_levels <- c("Day case", "Elective", "Emergency")

# Load in data
df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")|>
  mutate(Age = factor(Age, levels = age_levels), # Restore factor levels
         AdmissionType = factor(AdmissionType, levels = admission_levels))|> 
  filter(AdmissionType != "All Inpatients and Day cases", # Not used now
         AdmissionType != "Not Specified") # Consider imputing in future
  
df_proj <- read_csv("Data_Clean/Council Area Projections.csv")|>
  mutate(Age = factor(Age, levels = age_levels))|> # Restore factor levels
  expand_grid(AdmissionType = admission_levels)|>
  mutate(AdmissionType = factor(AdmissionType, levels = admission_levels))

#write_parquet(df_proj,sink="Data_Clean/df_proj.parquet")

# Useful functions
f_inv_logit <-  function(x) exp(x)/(1+exp(x)) # Inverse_logit


########## Data Preparation ##########

age_level_num <- 1:19

admission_level_num <- 1:3

df_HA_stan <- df_HA_model|>
  mutate(Age = age_level_num[df_HA_model$Age], # Convert factor to numeric
         Male = if_else(Sex=="Female",0,1),
         Adm = admission_level_num[df_HA_model$AdmissionType])|>
  rename(Geo = CA_ID)

df_proj_stan <- df_proj|>
  mutate(Age = age_level_num[df_proj$Age], # Convert factor to numeric
         Male = if_else(Sex=="Female",0,1),
         Adm = admission_level_num[df_proj$AdmissionType])|>
  rename(Geo = CA_ID)


# For effect of geography - construct a Scotland where each CA has the same age structure 
df_fix_age <- df_HA_stan|>
  filter(Year == max(Year),
         AdmissionType=="Elective")|>
  group_by(Age)|>
  summarise(Population = sum(Population))|>
  mutate(Population = as.integer(Population/64)) # Divide population evenly through the 32 council areas and by male / female

df_fix_stan <- expand_grid(Geo = 1:32,
                           Age = 1:19,
                           Male = c(0,1),
                           Adm = 1:3)|>
  left_join(df_fix_age, by = "Age")

# write_parquet(df_fix_stan, sink = "Data_Clean/df_fix_stan.parquet")

########## Poisson Poisson Model ##########


#### Stan Code ####

code_pois <- 
  "data{
  
    // Data for Model Fitting
    int<lower=0> n;
    int<lower=0> n_adm;
    int<lower=0> n_age;
    int<lower=0> n_geo;
    array[n] int los; // Length of Stay
    array[n] int stays_int; // Stays as integer
    vector[n] log_stays; // Log stays as vector
    vector[n] log_pop;
    array[n] int<lower=1, upper=n_adm> adm;
    array[n] int<lower=1, upper=n_age> age;
    vector[n] male;
    array[n] int<lower=1, upper=n_geo> geo;
    
    // Data for Projection
    int<lower=0> n_proj;
    vector[n_proj] log_pop_proj;
    array[n_proj] int<lower=1, upper=n_adm> adm_proj;
    array[n_proj] int<lower=1, upper=n_age> age_proj;
    vector[n_proj] male_proj;
    array[n_proj] int<lower=1, upper=n_geo> geo_proj;
    
    // Data for Fixed Age Structure
    int<lower=0> n_fix;
    vector[n_fix] log_pop_fix;
    array[n_fix] int<lower=1, upper=n_adm> adm_fix;
    array[n_fix] int<lower=1, upper=n_age> age_fix;
    vector[n_fix] male_fix;
    array[n_fix] int<lower=1, upper=n_geo> geo_fix;
    
    // Switch to evaluate the likelihood
    int<lower = 0, upper = 1> condition_on_data;
  
}
parameters{
    
    // Model Parameters
    vector[n_adm] b_adm_sty;
    vector[n_age-1] b_age_raw_sty;
    real b_male_sty;
    vector[n_geo-1] b_geo_raw_sty;
    
    vector[n_adm] b_adm_los;
    vector[n_age-1] b_age_raw_los;
    real b_male_los;
    vector[n_geo-1] b_geo_raw_los;
    
}
transformed parameters{
    // Fix baseline cateogories for age and geo. (absorbed into intercept)
    
    vector[n_age] b_age_sty = append_row(0, b_age_raw_sty);
    vector[n_geo] b_geo_sty = append_row(0, b_geo_raw_sty);
    
    vector[n_age] b_age_los = append_row(0, b_age_raw_los);
    vector[n_geo] b_geo_los = append_row(0, b_geo_raw_los);
}
model{

    // Linear predictor
    vector[n] mu_sty;
    
    vector[n] mu_los;
    
    // Priors
    b_adm_sty ~ normal(-1.1,0.6); // Priors skew negative since Stay typically < Pop
    b_age_sty ~ normal(-1.1,0.6);
    b_male_sty ~ normal(-0.6,0.4);
    b_geo_raw_sty ~ normal(-0.6,0.4);
    
    b_adm_los ~ normal(0,2); // Priors centred on 0 
    b_age_los ~ normal(0,2); 
    b_male_los ~ normal(0,1);
    b_geo_raw_los ~ normal(0,1);
    
    // Model
    
    // Linear predictor 
    mu_sty = b_adm_sty[adm] + b_age_sty[age] + b_male_sty*male + b_geo_sty[geo];
    
    mu_los = b_adm_los[adm] + b_age_los[age] + b_male_los*male + b_geo_los[geo];
    
    // Condition on data
    if(condition_on_data==1){

      stays_int ~ poisson_log(log_pop + mu_sty); // = Poi(Pop .* exp(mu))    
      los ~ poisson_log(log_stays + mu_los);
    }
}
generated quantities{

    // Data Replication
    
    // Initialise
    vector[n] mu_sty;
    array[n] int rep_stays;
    
    vector[n] mu_los;
    array[n] int rep_los;
    
    // Generate fitted values
    mu_sty = b_adm_sty[adm] + b_age_sty[age] + b_male_sty*male + b_geo_sty[geo];
    
    mu_los = b_adm_los[adm] + b_age_los[age] + b_male_los*male + b_geo_los[geo];
      
    // Generate replicated data
    rep_stays=poisson_log_rng(log_pop + mu_sty); // = Poi(Pop .* exp(mu))  
        
    rep_los=poisson_log_rng(log_stays + mu_los);
    
    // Projection
    
    // Initialise
    vector[n_proj] mu_proj_sty;
    array[n_proj] int proj_stays_int;
    array[n_proj] int ones = rep_array(1,n_proj);
    
    vector[n_proj] mu_proj_los;
    array[n_proj] int proj_los;
    
    // Generate fitted values
    mu_proj_sty = b_adm_sty[adm_proj] + b_age_sty[age_proj] + b_male_sty*male_proj + b_geo_sty[geo_proj];
    
    mu_proj_los = b_adm_los[adm_proj] + b_age_los[age_proj] + b_male_los*male_proj + b_geo_los[geo_proj];
      
    // Generate replicated data
    proj_stays_int = poisson_log_rng(log_pop_proj + mu_proj_sty); // = Poi(Pop .* exp(mu))
    
    // Minimum Stays to 1 for reasons discussed in paper
    array[n_proj] real stays_proj_floor = fmax(proj_stays_int,ones);
    
    vector[n_proj] log_stays_proj = log(to_vector(stays_proj_floor)); 
    
    proj_los=poisson_log_rng(log_stays_proj + mu_proj_los);
    
    
    // Fixed Age Structure
    
    // Initialise
    vector[n_fix] mu_fix_sty;
    array[n_fix] int fix_stays_int;
    array[n_fix] int ones_fix = rep_array(1,n_fix);
    
    vector[n_fix] mu_fix_los;
    array[n_fix] int fix_los;
    
    // Generate fitted values
    mu_fix_sty = b_adm_sty[adm_fix] + b_age_sty[age_fix] + b_male_sty*male_fix + b_geo_sty[geo_fix];
    
    mu_fix_los = b_adm_los[adm_fix] + b_age_los[age_fix] + b_male_los*male_fix + b_geo_los[geo_fix];
      
    // Generate replicated data
    fix_stays_int = poisson_log_rng(log_pop_fix + mu_fix_sty); // = Poi(Pop .* exp(mu))
    
    // Minimum Stays to 1 for reasons discussed in paper
    array[n_fix] real stays_fix_floor = fmax(fix_stays_int,ones_fix);
    
    vector[n_fix] log_stays_fix = log(to_vector(stays_fix_floor)); 
    
    fix_los=poisson_log_rng(log_stays_fix + mu_fix_los);
    
}"


#### Prior Predictive ####

# Input list
list_pois_prior <- list(los = df_HA_stan$TotalLengthofStay,
                        log_stays = log(df_HA_stan$Stays),
                        stays_int = df_HA_stan$Stays,
                        log_pop = log(df_HA_stan$Population),
                        adm = df_HA_stan$Adm,
                        age = df_HA_stan$Age,
                        geo = df_HA_stan$Geo,
                        male = df_HA_stan$Male,
                        condition_on_data = 0, # the model doesn't condition on the data
                        n = nrow(df_HA_stan),
                        n_adm = 3,
                        n_age = 19,
                        n_geo = 32,
                        n_proj = 10,
                        log_pop_proj = rep(1,10),
                        adm_proj = rep(1,10),
                        age_proj = rep(1,10),
                        geo_proj = rep(1,10),
                        male_proj = rep(1,10),
                        n_fix = 10,
                        log_pop_fix = rep(1,10),
                        adm_fix = rep(1,10),
                        age_fix = rep(1,10),
                        geo_fix = rep(1,10),
                        male_fix = rep(1,10))


if(!file.exists("Models/samples_pois_prior.rds")){
  # Compiles the model
  stan_pois <- stan_model(model_name = "stan_pois",model_code=code_pois)
  
  # Sample
  samples_pois_prior <-  sampling(stan_pois,
                                  data = list_pois_prior, 
                                  chains=1, iter = 1000,
                                  cores = parallel::detectCores(),
                                  pars = c("mu_sty","mu_los", "rep_stays","rep_los"))
  
  # Save file
  saveRDS(samples_pois_prior,file="Models/samples_pois_prior.rds")
}else{
  samples_pois_prior <- readRDS(file="Models/samples_pois_prior.rds")
}


# Save Model Outputs
if(!file.exists("Model_Outputs/draws_prior_reps.parquet")){
  
  draws_prior_reps <- gather_draws(samples_pois_prior, 
                                   rep_stays[i], mu_sty[i],
                                   rep_los[i], mu_los[i])
  # Save file
  write_parquet(draws_prior_reps,sink="Model_Outputs/draws_prior_reps.parquet")
}else{
  draws_prior_reps <- read_parquet("Model_Outputs/draws_prior_reps.parquet")
}

# Plot Histogram of Prior Reps
if(!file.exists("Plots/plt_prior_reps.rds")){
  
  plt_prior_reps <- draws_prior_reps|>
    filter(!(.variable == "rep_stays" & .value >2000), # scale plot wrt 99th percentile
           !(.variable == "rep_los" & .value >15000))|>
    mutate(.variable = case_match(.variable,
                                  "mu_sty" ~ "Mu Stays",
                                  "mu_los" ~ "Mu Total Length of Stays",
                                  "rep_sty" ~ "Number of Stays",
                                  "rep_los" ~ "Total Length of Stay"))|>
    ggplot(aes(x = .value))+
    geom_histogram(fill="#440154", bins = 30)+
    facet_wrap(~.variable, scales = "free")+
    ggtitle("Prior Predictive Distribution")+
    ylab("Count")+
    xlab("Value")+
    scale_y_continuous(labels = ~ format(.x, scientific = FALSE))+
    theme_bw()
  
  # Save Plot
  saveRDS(plt_prior_reps, "Plots/plt_prior_reps.rds")
  
}else{
  plt_prior_reps <- readRDS("Plots/plt_prior_reps.rds")
}


# Statistics of original dataset 
df_stats_stays<- df_HA_model|> 
  summarise(`Minimum Number of Stays` = min(Stays), 
            `Maximum Number of Stays` = max(Stays), 
            `Median Number of Stays` = median(Stays), 
            `Skew of Number of Stays` = skewness(Stays))|>
  pivot_longer(cols = 1:4, names_to = "statistic")

df_stats_los<- df_HA_model|> 
  summarise(`Minimum Total Length of Stay` = min(TotalLengthofStay), 
            `Maximum Total Length of Stay` = max(TotalLengthofStay), 
            `Median Total Length of Stay` = median(TotalLengthofStay), 
            `Skew of Total Length of Stay` = skewness(TotalLengthofStay))|>
  pivot_longer(cols = 1:4, names_to = "statistic")


# Plot Prior Predictive Distribution - Number of Stays
if(!file.exists("Plots/plt_prior_pred_sty.rds")){
  
  plt_prior_pred_sty <- draws_prior_reps|>
    filter(.variable == "rep_stays")|>
    group_by(.draw)|>
    summarise(`Minimum Number of Stays` = min(.value), 
              `Maximum Number of Stays` = max(.value), 
              `Median Number of Stays` = median(.value), 
              `Skew of Number of Stays` = skewness(.value))|>
    pivot_longer(cols = 2:5, names_to = "statistic")|>
    filter(!(statistic == "Maximum Number of Stays" & value > 60000))|>
    ggplot(aes(x = value))+
    geom_histogram(bins = 30, fill = "#440154")+
    geom_vline(data = df_stats_stays, aes(xintercept = value),col="#5ec962",lwd=2)+
    facet_wrap(~statistic, nrow=2, scales = "free")+
    theme_bw()+
    scale_x_continuous(labels = ~ format(.x, scientific = FALSE))+
    ggtitle(paste0("Prior Predictive Number of Stays, Observed Values in Green."))+
    xlab("Statistic Value")+
    ylab("Count")
  
  # Save Plot
  saveRDS(plt_prior_pred_sty, "Plots/plt_prior_pred_sty.rds")
  
}else{
  plt_prior_pred_sty <- readRDS("Plots/plt_prior_pred_sty.rds")
}



# Plot Prior Predictive Distribution - Total Length of Stay
if(!file.exists("Plots/plt_prior_pred_los.rds")){
  
  plt_prior_pred_los <- draws_prior_reps|>
    filter(.variable == "rep_los")|>
    group_by(.draw)|>
    summarise(`Minimum Total Length of Stay` = min(.value), 
              `Maximum Total Length of Stay` = max(.value), 
              `Median Total Length of Stay` = median(.value), 
              `Skew of Total Length of Stay` = skewness(.value))|>
    pivot_longer(cols = 2:5, names_to = "statistic")|>
    filter(!(statistic == "Maximum Total Length of Stay" & value > 1000000),
           !(statistic == "Median Total Length of Stay" & value > 5000))|>
    ggplot(aes(x = value))+
    geom_histogram(bins = 30, fill = "#440154")+
    geom_vline(data = df_stats_los, aes(xintercept = value),col="#5ec962",lwd=2)+
    facet_wrap(~statistic, nrow=2, scales = "free")+
    theme_bw()+
    scale_x_continuous(labels = ~ format(.x, scientific = FALSE))+
    ggtitle(paste0("Prior Predictive Total Length of Stay, Observed Values in Green."))+
    xlab("Statistic Value")+
    ylab("Count")
  
  # Save Plot
  saveRDS(plt_prior_pred_los, "Plots/plt_prior_pred_los.rds")
  
}else{
  plt_prior_pred_los <- readRDS("Plots/plt_prior_pred_los.rds")
}





#### Posterior ####

# Input list
list_pois_post <- list(los = df_HA_stan$TotalLengthofStay,
                       log_stays = log(df_HA_stan$Stays),
                       stays_int = df_HA_stan$Stays,
                       log_pop = log(df_HA_stan$Population),
                       adm = df_HA_stan$Adm,
                       age = df_HA_stan$Age,
                       geo = df_HA_stan$Geo,
                       male = df_HA_stan$Male,
                       condition_on_data = 1, # the model conditions on the data
                       n = nrow(df_HA_stan),
                       n_adm = 3,
                       n_age = 19,
                       n_geo = 32,
                       n_proj = nrow(df_proj_stan),
                       log_pop_proj = log(df_proj_stan$Population),
                       adm_proj = df_proj_stan$Adm,
                       age_proj = df_proj_stan$Age,
                       geo_proj = df_proj_stan$Geo,
                       male_proj = df_proj_stan$Male,
                       n_fix = nrow(df_fix_stan),
                       log_pop_fix = log(df_fix_stan$Population),
                       adm_fix = df_fix_stan$Adm,
                       age_fix = df_fix_stan$Age,
                       geo_fix = df_fix_stan$Geo,
                       male_fix = df_fix_stan$Male)



if(!file.exists("Models/samples_pois_post.rds")){
  # Compiles the model
  stan_pois <- stan_model(model_name = "stan_pois",model_code=code_pois)
  
  # Sample
  samples_pois_post <-  sampling(stan_pois,
                                 data = list_pois_post, 
                                 chains=2, iter = 1000,
                                 cores = parallel::detectCores())
  
  # Save file
  saveRDS(samples_pois_post,file="Models/samples_pois_post.rds")
}else{
  samples_pois_post <- readRDS(file="Models/samples_pois_post.rds")
}

# Save Model Outputs
if(!file.exists("Model_Outputs/draws_post_param.parquet") |
   !file.exists("Model_Outputs/draws_post_reps.parquet") |
   !file.exists("Model_Outputs/draws_post_proj.parquet") |
   !file.exists("Model_Outputs/draws_post_fix.parquet")){
  
  draws_post_param <- gather_draws(samples_pois_post,
                                   b_adm_sty[i],b_age_sty[i],b_male_sty,b_geo_sty[i],
                                   b_adm_los[i],b_age_los[i],b_male_los,b_geo_los[i])

  #Save file
  write_parquet(draws_post_param,sink="Model_Outputs/draws_post_param.parquet")
  
  
  
  draws_post_reps <- gather_draws(samples_pois_post, 
                                  rep_stays[i], mu_sty[i],
                                  rep_los[i], mu_los[i])
  
  # Save file
  write_parquet(draws_post_reps,sink="Model_Outputs/draws_post_reps.parquet")
  
  
  
  draws_post_proj <- gather_draws(samples_pois_post, 
                                  proj_stays_int[i], proj_los[i])
  
  # Save file
  write_parquet(draws_post_proj,sink="Model_Outputs/draws_post_proj.parquet")
  
  
  
  draws_post_fix <- gather_draws(samples_pois_post, 
                                  fix_stays_int[i], fix_los[i])
  
  # Save file
  write_parquet(draws_post_fix,sink="Model_Outputs/draws_post_fix.parquet")
  
}else{
  draws_post_param <- read_parquet("Model_Outputs/draws_post_param.parquet")
  draws_post_reps <- read_parquet("Model_Outputs/draws_post_reps.parquet")
  draws_post_proj <- read_parquet("Model_Outputs/draws_post_proj.parquet")
  draws_post_fix <- read_parquet("Model_Outputs/draws_post_fix.parquet")
}




# Plot Posterior Predictive Distribution - Number of Stays
if(!file.exists("Plots/plt_post_pred_sty.rds")){
  
  plt_post_pred_sty <- draws_post_reps|>
    filter(.variable == "rep_stays")|>
    group_by(.draw)|>
    summarise(`Minimum Number of Stays` = min(.value), 
              `Maximum Number of Stays` = max(.value), 
              `Median Number of Stays` = median(.value), 
              `Skew of Number of Stays` = skewness(.value))|>
    pivot_longer(cols = 2:5, names_to = "statistic")|>
    filter(!(statistic == "Maximum Number of Stays" & value > 60000))|>
    ggplot(aes(x = value))+
    geom_histogram(bins = 30, fill = "#440154")+
    geom_vline(data = df_stats_stays, aes(xintercept = value),col="#5ec962",lwd=2)+
    facet_wrap(~statistic, nrow=2, scales = "free")+
    theme_bw()+
    scale_x_continuous(labels = ~ format(.x, scientific = FALSE))+
    ggtitle(paste0("Posterior Predictive Number of Stays, Observed Values in Green."))+
    xlab("Statistic Value")+
    ylab("Count")
  
  # Save Plot
  saveRDS(plt_post_pred_sty, "Plots/plt_post_pred_sty.rds")
  
}else{
  plt_post_pred_sty <- readRDS("Plots/plt_post_pred_sty.rds")
}



# Plot Posterior Predictive Distribution - Total Length of Stay
if(!file.exists("Plots/plt_post_pred_los.rds")){
  
  plt_post_pred_los <- draws_post_reps|>
    filter(.variable == "rep_los")|>
    group_by(.draw)|>
    summarise(`Minimum Total Length of Stay` = min(.value), 
              `Maximum Total Length of Stay` = max(.value), 
              `Median Total Length of Stay` = median(.value), 
              `Skew of Total Length of Stay` = skewness(.value))|>
    pivot_longer(cols = 2:5, names_to = "statistic")|>
    filter(!(statistic == "Maximum Total Length of Stay" & value > 1000000),
           !(statistic == "Median Total Length of Stay" & value > 5000))|>
    ggplot(aes(x = value))+
    geom_histogram(bins = 30, fill = "#440154")+
    geom_vline(data = df_stats_los, aes(xintercept = value),col="#5ec962",lwd=2)+
    facet_wrap(~statistic, nrow=2, scales = "free")+
    theme_bw()+
    scale_x_continuous(labels = ~ format(.x, scientific = FALSE))+
    ggtitle(paste0("Posterior Predictive Total Length of Stay, Observed Values in Green."))+
    xlab("Statistic Value")+
    ylab("Count")
  
  # Save Plot
  saveRDS(plt_post_pred_los, "Plots/plt_post_pred_los.rds")
  
}else{
  plt_post_pred_los <- readRDS("Plots/plt_post_pred_los.rds")
}








