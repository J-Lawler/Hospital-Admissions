
########## Contents ##########

# Preamble

# Appendix B: Diagnostic Plots

# Section 1: Population Projections

# Section 2: Factors Affecting Hospitalisation

# Section 3: Hospitalisation Projections


########## Preamble ##########

# Load libraries
library(tidyverse)
library(tidybayes)
library(arrow)
library(moments)
library(sf)

# Age factor levels
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

admission_levels <- c("Day case", "Elective", "Emergency")

# Load in data
df_HA_model <- read_csv("Data_Clean/HA Model Data.csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

df_proj_mean <- read_csv("Results/Quick Projections Posterior Mean.csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

df_HB <- read_csv("Data_Clean/Health Boards.csv")






########## Section 1: Population Projections ##########


# Age factor levels
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

# Load in data

df_CA_proj <- read_csv("Data_Clean/Council Area Projections.csv")|>
  mutate(Age = factor(Age, levels = age_levels)) # Restore factor levels

df_CA <- read_csv("Data_Clean/Council Areas.csv")



##### Data Preparation 

# Mapping Council Area Boundaries
df_CA_boundaries_raw <- st_read("Data_Raw/Local Authorities Spatial")

df_CA_boundaries <- df_CA_boundaries_raw|>
  left_join(df_CA, by = c("code" = "CA") )|>
  select(CAName, CA_ID, geometry)|>
  arrange(CAName)

# saveRDS(df_CA_boundaries, file = "Data_Clean/CA Boundaries.csv")
# df_CA_boundaries <- read_csv("Data_Clean/CA Boundaries.csv"))


# Creating data frames for proportion of population over 65 / 80
df_total_pop <- df_CA_proj|>
  group_by(Year,CA_ID)|>
  summarise(Population = sum(Population))

df_perc_65_CA <- df_CA_proj|>
  filter(Age %in% c("65-69 years", "70-74 years", "75-79 years", 
                    "80-84 years", "85-89 years", "90 years and over"))|>
  group_by(Year,CA_ID)|>
  summarise(Population = sum(Population))|>
  left_join(df_total_pop, by = c("Year", "CA_ID"),
            suffix = c("","_Total"))|>
  mutate(`Percentage Over 65` = Population / Population_Total *100)

df_perc_80_CA <- df_CA_proj|>
  filter(Age %in% c("80-84 years", "85-89 years", "90 years and over"))|>
  group_by(Year,CA_ID)|>
  summarise(Population = sum(Population))|>
  left_join(df_total_pop, by = c("Year", "CA_ID"),
            suffix = c("","_Total"))|>
  mutate(`Percentage Over 80` = Population / Population_Total *100)


df_perc_65_total <- df_CA_proj|>
  filter(Age %in% c("65-69 years", "70-74 years", "75-79 years", 
                    "80-84 years", "85-89 years", "90 years and over"))|>
  group_by(Year,CA_ID)|>
  summarise(Population = sum(Population))|>
  left_join(df_total_pop, by = c("Year", "CA_ID"),
            suffix = c("","_Total"))|>
  group_by(Year)|>
  summarise(Population = sum(Population),
            Population_Total = sum(Population_Total))|>
  mutate(`Percentage Over 65` = Population / Population_Total *100)

df_perc_80_total <- df_CA_proj|>
  filter(Age %in% c("80-84 years", "85-89 years", "90 years and over"))|>
  group_by(Year,CA_ID)|>
  summarise(Population = sum(Population))|>
  left_join(df_total_pop, by = c("Year", "CA_ID"),
            suffix = c("","_Total"))|>
  group_by(Year)|>
  summarise(Population = sum(Population),
            Population_Total = sum(Population_Total))|>
  mutate(`Percentage Over 80` = Population / Population_Total *100)

# Data frame for population changes since 2023

df_2023_pop <- df_CA_proj|>
  filter(Year == 2023)|>
  group_by(CA_ID)|>
  summarise(Population=sum(Population))

df_pop_changes <- df_CA_proj|>
  group_by(Year,CA_ID)|>
  summarise(Population = sum(Population))|>
  left_join(df_2023_pop, by = "CA_ID",
            suffix = c("","_2023"))|>
  mutate(`Percentage Change` = (Population / Population_2023 - 1)*100)


#### Plots

# Projected Total Population 
plt_proj_total <- df_CA_proj|>
  mutate(Age = str_replace(Age, " years",""))|>
  group_by(Year)|>
  summarise(Population = sum(Population)/1000)|>
  filter(Year>=2023)|>
  ggplot(aes(x = Year, y = Population))+
  geom_line(lwd=1.2, colour = "#440154")+
  ylab("Population (000s)")+
  ggtitle("Projected Total Population")+
  theme_bw()

plt_proj_total

#saveRDS(plt_proj_total, file = "Plots/plt_proj_total.rds")

# Population Over 65 and 80
plt_pop_65_80 <- df_perc_65_total|>
  left_join(df_perc_80_total, by = c("Year"))|>
  select(Year,`Over 65` = Population.x,
         `Over 80` = Population.y)|>
  pivot_longer(cols = c(`Over 65`,`Over 80`), 
               values_to = "Population", names_to = "Age")|>
  mutate(Population = Population / 1000)|>
  filter(Year>=2023)|>
  ggplot(aes(x= Year, y = Population, colour = Age))+
  geom_line()+
  geom_line(lwd=1.2)+
  ylab("Population (000s)")+
  ggtitle("Projected Population Over Given Ages")+
  theme_bw()+
  scale_color_viridis_d()

plt_pop_65_80

#saveRDS(plt_pop_65_80, file = "Plots/plt_pop_65_80.rds")


# Projected Percentage Over 65 / 80

plt_perc_65_80 <- df_perc_65_total|>
  left_join(df_perc_80_total, by = c("Year"))|>
  select(Year,`Percentage Over 65`,`Percentage Over 80`)|>
  pivot_longer(cols = c(`Percentage Over 65`,`Percentage Over 80`), 
               values_to = "Percentage", names_to = "Age")|>
  filter(Year>=2023)|>
  ggplot(aes(x= Year, y = Percentage, colour = Age))+
  geom_line()+
  #facet_wrap(~Age)+
  ylim(0,30)+
  geom_line(lwd=1.2)+
  ylab("Percentage (%)")+
  ggtitle("Projected Percentage of Population Over Given Ages")+
  theme_bw()+
  scale_color_viridis_d()

plt_perc_65_80

#saveRDS(plt_perc_65_80, file = "Plots/plt_perc_65_80.rds")


# Plot Population Changes
plt_pop_change_CA <- df_pop_changes|>
  filter(Year %in% c("2033","2043"))|>
  left_join(df_CA_boundaries, by = "CA_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = `Percentage Change`)) + 
  coord_sf()+
  facet_wrap(~Year)+
  theme_bw()+
  scale_fill_viridis_c()+
  ggtitle("Projected Percentage Population Change")+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt_pop_change_CA

#saveRDS(plt_pop_change_CA, file = "Plots/plt_pop_change_CA.rds")


# df_CA_unique <- df_CA|>
#   select(CA_ID,CAName)|>
#   unique()
# 
# test <-  df_pop_changes|>
#   filter(Year %in% c("2033","2043"))|>
#   left_join(df_CA_unique)
# 
# test <- test|>
#   filter(Year == 2043)

# Plot percentage over 65
plt_perc_65_CA <- df_perc_65_CA|>
  filter(Year %in% c("2023","2033","2043"))|>
  left_join(df_CA_boundaries, by = "CA_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = `Percentage Over 65`)) + 
  coord_sf()+
  facet_wrap("Year")+
  theme_bw()+
  scale_fill_viridis_c()+
  ggtitle("Projected Percentage Population Over 65")+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt_perc_65_CA

#saveRDS(plt_perc_65_CA, file = "Plots/plt_perc_65_CA.rds")



# Plot percentage over 80
plt_perc_80_CA <- df_perc_80_CA|>
  filter(Year %in% c("2023","2033","2043"))|>
  left_join(df_CA_boundaries, by = "CA_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = `Percentage Over 80`)) + 
  coord_sf()+
  facet_wrap("Year")+
  theme_bw()+
  scale_fill_viridis_c()+
  ggtitle("Projected Percentage Population Over 80")+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt_perc_80_CA

#saveRDS(plt_perc_80_CA, file = "Plots/plt_perc_80_CA.rds")












########## Section 2: Factors Affecting Hospitalisation ##########


# Load in Data
# draws_post_reps <- read_parquet("Model_Outputs/draws_post_reps.parquet")

# df_draws_format <- draws_post_reps|>
#   filter(.variable %in% c("rep_stays","rep_los"))|>
#   pivot_wider(names_from = .variable, values_from = .value)



## Effect of Age on Hospitalisation Overall

df_hosp_age_total <- df_HA_model|>
  filter(AdmissionType %in% admission_levels)|>
  group_by(Age)|>
  summarise(Stays = sum(Stays),
            TotalLengthofStay = sum(TotalLengthofStay), 
            Population = sum(Population)/3)|> # Divide out redundant population for each admission type
  mutate(`Annual Stays per 1000 People` = Stays / Population * 1000,
         `Mean Length of Stay (Days)` = TotalLengthofStay / Stays,
         Age = str_replace(Age, " years",""))|>
  mutate(Age = factor(Age, levels = str_replace(age_levels, " years","")))|>
  pivot_longer(cols = c(`Annual Stays per 1000 People`,`Mean Length of Stay (Days)`),
               names_to = "Statistic", values_to = "value")|>
  mutate(Statistic = factor(Statistic, levels = c("Annual Stays per 1000 People",
                                                  "Mean Length of Stay (Days)")))


age_breaks <- c("0-4",  "10-14",  "20-24",  
                "30-34",  "40-44",  "50-54", 
                "60-64",  "70-74",  "80-84", 
                "90 and over")


plt_hosp_age_total <- df_hosp_age_total|>
  ggplot(aes(x= Age, y = value, group = 1))+
  geom_line(colour = "#440154",lwd=1.2)+
  facet_wrap(~Statistic, scales = "free_y",
             strip.position = "left")+
  ggtitle("Effect of Age on Frequency and Length of Hospitalisation")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_discrete(breaks = age_breaks)


plt_hosp_age_total


#saveRDS(plt_hosp_age_total, file = "Plots/plt_hosp_age_total.rds")



## Effect of Age on Hospitalisation Break Down by Admission Type


df_hosp_age_adm <- df_HA_model|>
  filter(AdmissionType %in% admission_levels)|>
  group_by(Age, AdmissionType)|>
  summarise(Stays = sum(Stays),
            TotalLengthofStay = sum(TotalLengthofStay), 
            Population = sum(Population))|>
  mutate(`Annual Stays per 1000 People` = Stays / Population * 1000,
         `Mean Length of Stay (Days)` = TotalLengthofStay / Stays,
         Age = str_replace(Age, " years",""))|>
  mutate(Age = factor(Age, levels = str_replace(age_levels, " years","")))|>
  pivot_longer(cols = c(`Annual Stays per 1000 People`,`Mean Length of Stay (Days)`),
               names_to = "Statistic", values_to = "value")|>
  mutate(Statistic = factor(Statistic, levels = c("Annual Stays per 1000 People",
                                                  "Mean Length of Stay (Days)")))


age_breaks <- c("0-4",  "10-14",  "20-24",  
                "30-34",  "40-44",  "50-54", 
                "60-64",  "70-74",  "80-84", 
                "90 and over")


plt_hosp_age_adm <- df_hosp_age_adm|>
  ggplot(aes(x= Age, y = value, group = AdmissionType, colour = AdmissionType))+
  geom_line(lwd=1.2)+
  facet_wrap(~Statistic, scales = "free_y",
             strip.position = "left")+
  ggtitle("Effect of Age by Admission Type")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_discrete(breaks = age_breaks)+
  scale_colour_viridis_d()


plt_hosp_age_adm


#saveRDS(plt_hosp_age_adm, file = "Plots/plt_hosp_age_adm.rds")



## Geography

age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

admission_levels <- c("Day case", "Elective", "Emergency")

df_CA <- read_csv("Data_Clean/Council Areas.csv")|>
  select(CA_ID, CAName)|>
  unique()

df_CA_HB_map <- read_csv(file = "Data_Clean/df_CA_HB_map.csv")

df_CA <- read_csv("Data_Clean/Council Areas.csv")

# Mapping HB Boundaries
df_CA_boundaries_raw <- st_read("Data_Raw/Local Authorities Spatial")

df_CA_boundaries <- df_CA_boundaries_raw|>
  left_join(df_CA, by = c("code" = "CA") )|>
  select(CAName = CAName, CA_ID, geometry)|>
  arrange(CAName)


df_CA_unique <- read_csv("Data_Clean/Council Areas.csv")|>
  select(CA_ID, CAName)|>
  unique()


df_fix_stan <- read_parquet("Data_Clean/df_fix_stan.parquet")

df_fix <- df_fix_stan|>
  mutate(Age = age_levels[Age],
         AdmissionType = admission_levels[Adm])|>
  left_join(df_CA_unique, by = c("Geo" = "CA_ID"))|>
  mutate(i = 1:nrow(df_fix_stan))

draws_post_fix <- read_parquet("Model_Outputs/draws_post_fix.parquet")




df_mean_post_fix <- draws_post_fix|>
  mean_qi()|>
  select(i, variable = .variable, mean = .value)|>
  pivot_wider(names_from = variable,
              values_from = mean)|>
  left_join(df_fix, by = "i")|>
  select(Age, Male, CA_ID = Geo, CAName, AdmissionType,
         Population, fix_stays_int, fix_los)

df_summary_geo <- df_mean_post_fix|>
  group_by(CA_ID)|>
  summarise(Stays = sum(fix_stays_int),
            LoS = sum(fix_los),
            LoS = LoS/Stays)|>
  mutate(mean_stays = mean(Stays),
         mean_LoS = mean(LoS),
         `Number of Stays Relative to Average (%)` = (Stays/mean_stays-1)*100,
         `Mean Length of Stay Relative to Average (%)`= (LoS/mean_LoS-1)*100)|>
  pivot_longer(c(`Number of Stays Relative to Average (%)`,
                 `Mean Length of Stay Relative to Average (%)`),
               names_to = "Statistic",
               values_to = "Value")|>
  mutate(Statistic = factor(Statistic,
                            levels = c("Number of Stays Relative to Average (%)",
                                       "Mean Length of Stay Relative to Average (%)")))


# Plot geo
plt_summary_geo <- df_summary_geo|>
  left_join(df_CA_boundaries, by = "CA_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = Value)) + 
  coord_sf()+
  facet_wrap("Statistic")+
  theme_bw()+
  scale_fill_viridis_c()+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt_summary_geo

#saveRDS(plt_summary_geo, file = "Plots/plt_summary_geo.rds")




##### Health Board HB Plot




age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", 
                "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years",
                "90 years and over")

admission_levels <- c("Day case", "Elective", "Emergency")

df_CA <- read_csv("Data_Clean/Council Areas.csv")|>
  select(CA_ID, CAName)|>
  unique()

df_CA_HB_map <- read_csv(file = "Data_Clean/df_CA_HB_map.csv")


# Mapping HB Boundaries
df_HB_boundaries_raw <- st_read("Data_Raw/Health Boards Spatial")

df_HB_boundaries <- df_HB_boundaries_raw|>
  left_join(df_HB, by = c("HBCode" = "HB") )|>
  select(HBName = HBName.x, HB_ID, geometry)|>
  arrange(HBName)

#saveRDS(df_HB_boundaries, file = "Data_Clean/HB Boundaries.csv")
# df_HB_boundaries <- read_csv("Data_Clean/HB Boundaries.csv"))

df_fix_stan <- read_parquet("Data_Clean/df_fix_stan.parquet")

df_fix <- df_fix_stan|>
  mutate(Age = age_levels[Age],
         AdmissionType = admission_levels[Adm])|>
  left_join(df_CA, by = c("Geo" = "CA_ID"))|>
  mutate(i = 1:nrow(df_fix_stan))

draws_post_fix <- read_parquet("Model_Outputs/draws_post_fix.parquet")




df_mean_post_fix <- draws_post_fix|>
  mean_qi()|>
  select(i, variable = .variable, mean = .value)|>
  pivot_wider(names_from = variable,
              values_from = mean)|>
  left_join(df_fix, by = "i")|>
  select(Age, Male, CA_ID = Geo, CAName, AdmissionType,
         Population, fix_stays_int, fix_los)

df_CA_per_HB <- tibble(HB_ID = 1:14,
                       CA_per_HB = tabulate(df_CA_HB_map$HB_ID))

df_summary_geo <- df_mean_post_fix|>
  left_join(df_CA_HB_map, by = "CA_ID")|>
  group_by(HB_ID)|>
  summarise(Stays = sum(fix_stays_int),
            LoS = mean(fix_los))|>
  left_join(df_CA_per_HB, by = "HB_ID")|>
  mutate(Stays = Stays / CA_per_HB,
         mean_stays = mean(Stays),
         mean_LoS = mean(LoS),
         `Number of Stays Relative to Average (%)` = (Stays/mean_stays-1)*100,
         `Length of Stays Relative to Average (%)`= (LoS/mean_LoS-1)*100)|>
  pivot_longer(c(`Number of Stays Relative to Average (%)`,
                 `Length of Stays Relative to Average (%)`),
               names_to = "Statistic",
               values_to = "Value")|>
  mutate(Statistic = factor(Statistic,
                            levels = c("Number of Stays Relative to Average (%)",
                                       "Length of Stays Relative to Average (%)")))


# Plot geo
plt_summary_geo <- df_summary_geo|>
  left_join(df_HB_boundaries, by = "HB_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = Value)) + 
  coord_sf()+
  facet_wrap("Statistic")+
  theme_bw()+
  scale_fill_viridis_c()+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt_summary_geo

#saveRDS(plt_summary_geo, file = "Plots/plt_summary_geo.rds")













########## Section 3: Hospitalisation Projections ##########

admission_levels <- c("Day case", "Elective", "Emergency")

# Load in Data
draws_post_proj <- read_parquet("Model_Outputs/draws_post_proj.parquet")

df_proj <- read_parquet("Data_Clean/df_proj.parquet")

df_postcode <- read_csv("Data_Raw/Postcode Index.csv")|>
  select(CouncilArea2019Code,HealthBoardArea2019Code)|>
  unique()

# Data Cleaning


# Map Council Area to Health Board
df_CA_HB_map <- read_csv("Data_Clean/Council Areas.csv")|>
  left_join(df_postcode, by = c("CA" = "CouncilArea2019Code"))|>
  left_join(df_HB, by = c("HealthBoardArea2019Code"="HB"))|>
  select(CA_ID, HB_ID)|>
  filter(!is.na(HB_ID))|>
  unique()

#write_csv(df_CA_HB_map, file = "Data_Clean/df_CA_HB_map.csv")

# Filter to length of stay projections only
draws_proj_los <- draws_post_proj|>
  pivot_wider(names_from = .variable, values_from = .value)

# Add HB to projection df and add id column to match draws
df_proj_draws <- df_proj|>
  left_join(df_CA_HB_map, by = "CA_ID")|>
  mutate(i = 1:nrow(df_proj))|>
  full_join(draws_proj_los, by = "i", multiple="all")|>
  select(Year, CA_ID, HB_ID,Age,Sex, AdmissionType, 
         Population, Draw = .draw, LoS = proj_los, NoS= proj_stays_int)

#write_parquet(df_proj_draws, sink = "Model_Outputs/df_proj_draws.parquet")


# Overall hospital days

plt_proj_actual <- read_csv("Data_Clean/HA Model Data.csv")|>
  filter(AdmissionType %in% admission_levels)|>
  group_by(Year)|>
  summarise(LoS = sum(TotalLengthofStay))|>
  ggplot(aes(x = Year, y = LoS))+
  geom_line()

plt_proj_actual


df_los_summ <- df_proj_draws|>
  group_by(Year, Draw)|>
  summarise(LoS = sum(LoS)/1000,
            NoS = sum(NoS)/1000)|>
  filter(Year>=2023)|>
  mean_qi(LoS)|>
  mutate(Variable = "Total Days in Hospital (000s)")|>
  rename(Mean = LoS)|>
  select(Year, Variable, Mean, .lower, .upper)

df_stays_summ <- df_proj_draws|>
  group_by(Year, Draw)|>
  summarise(LoS = sum(LoS)/1000,
            NoS = sum(NoS)/1000)|>
  filter(Year>=2023)|>
  mean_qi(NoS)|>
  mutate(Variable = "Number of Hospital Stays (000s)")|>
  rename(Mean = NoS)|>
  select(Year, Variable, Mean, .lower, .upper)

df_overall_summ <- bind_rows(df_los_summ, df_stays_summ) 

plt_proj_overall <- df_overall_summ|>
  ggplot(aes(x = Year, y = Mean, ymin = .lower, ymax = .upper))+
  geom_lineribbon(colour = "#5ec962", fill = "#440154", alpha = 0.75, lwd = 1)+
  facet_wrap(~Variable, scales = "free_y",
             strip.position = "left")+
  ggtitle("National Hospitalisation Projections")+
  xlab("Year")+
  ylab("Total Days in Hospital (000s)")+
  theme_bw()+
  ylab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

plt_proj_overall


#saveRDS(plt_proj_overall, "Plots/plt_proj_overall.rds")


# Split by Admission Type

df_proj_adm_LoS <- df_proj_draws|>
  group_by(Year, Draw, AdmissionType)|>
  summarise(LoS = sum(LoS)/1000,
            NoS = sum(NoS)/1000, .groups = "keep")|>
  group_by(Year, AdmissionType)|>
  filter(Year>=2023)|>
  mean_qi(LoS)|>
  mutate(Variable = "Total Days in Hospital (000s)")|>
  rename(Mean = LoS)|>
  select(Year, AdmissionType, Variable, Mean, .lower, .upper)

df_proj_adm_NoS <- df_proj_draws|>
  group_by(Year, Draw, AdmissionType)|>
  summarise(LoS = sum(LoS)/1000,
            NoS = sum(NoS)/1000, .groups = "keep")|>
  group_by(Year, AdmissionType)|>
  filter(Year>=2023)|>
  mean_qi(NoS)|>
  mutate(Variable = "Number of Hospital Stays (000s)")|>
  rename(Mean = NoS)|>
  select(Year, AdmissionType, Variable, Mean, .lower, .upper)

df_proj_adm <- bind_rows(df_proj_adm_LoS, df_proj_adm_NoS)
  
plt_proj_adm <- df_proj_adm|>
  ggplot(aes(x = Year, y = Mean, ymin = .lower, ymax = .upper,
             colour = AdmissionType, fill = AdmissionType))+
  geom_line(alpha = 0.75, lwd = 1)+
  facet_wrap(~Variable, scales = "free_y",
             strip.position = "left")+
  ggtitle("Hospitalisation Projections by Admission Type")+
  xlab("Year")+
  ylab("Total Days in Hospital (000s)")+
  theme_bw()+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  ylab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

plt_proj_adm

#saveRDS(plt_proj_adm, "Plots/plt_proj_adm.rds")


## Projecting Mean Los

df_mean_los_proj_adm <- df_proj_draws|>
  group_by(Year, Draw, AdmissionType)|>
  summarise(LoS = sum(LoS),
            NoS = sum(NoS), .groups = "keep")|>
  group_by(Year, AdmissionType)|>
  filter(Year>=2023)|>
  mean_qi(LoS, NoS)|>
  select(Year, AdmissionType, LoS, NoS)

df_mean_los_proj <- df_proj_draws|>
  group_by(Year, Draw)|>
  summarise(LoS = sum(LoS),
            NoS = sum(NoS), .groups = "keep")|>
  group_by(Year)|>
  filter(Year>=2023)|>
  mean_qi(LoS, NoS)|>
  select(Year,LoS, NoS)|>
  mutate(AdmissionType = "Overall")

plt_mean_los <- bind_rows(df_mean_los_proj, df_mean_los_proj_adm)|>
  mutate(`Mean Length of Stay (Days)` = LoS / NoS,
         `Admission Type` = AdmissionType)|>
  ggplot(aes(x = Year, y = `Mean Length of Stay (Days)`, colour = `Admission Type`))+
  geom_line(lwd = 1.2)+
  theme_bw()+
  scale_colour_viridis_d()+
  ggtitle("Length of Hospital Stay Projections")


plt_mean_los

#saveRDS(plt_mean_los,"Plots/plt_mean_los.rds")
  


# Split by Health Board

# Mapping Council Area Boundaries
df_HB_boundaries_raw <- st_read("Data_Raw/Health Boards Spatial")

df_HB_boundaries <- df_HB_boundaries_raw|>
  left_join(df_HB, by = c("HBCode" = "HB") )|>
  select(HBName = HBName.x, HB_ID, geometry)|>
  arrange(HBName)

# saveRDS(df_HB_boundaries, file = "Data_Clean/HB Boundaries.csv")
# df_HB_boundaries <- read_csv("Data_Clean/HB Boundaries.csv"))


df_los_HB_2023 <- df_proj_draws|>
  group_by(Year, Draw, HB_ID)|>
  summarise(LoS = sum(LoS)/1000, .groups = "keep")|>
  group_by(Year, HB_ID)|>
  filter(Year == 2023)|>
  mean_qi(LoS)|>
  select(HB_ID, LoS)

df_los_HB_frac <- df_proj_draws|>
  group_by(Year, Draw, HB_ID)|>
  summarise(LoS = sum(LoS)/1000, .groups = "keep")|>
  group_by(Year, HB_ID)|>
  filter(Year %in% c(2033,2043))|>
  mean_qi(LoS)|>
  left_join(df_los_HB_2023, by = "HB_ID")|>
  select(Year, HB_ID, LoS = LoS.x, LoS_2023 = LoS.y)|>
  mutate(`Increase (%)` = (LoS / LoS_2023 -1) *100)


plt_proj_HB <- df_los_HB_frac|> 
  left_join(df_HB_boundaries, by = "HB_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = `Increase (%)`)) + 
  coord_sf()+
  facet_wrap(~Year)+
  theme_bw()+
  scale_fill_viridis_c()+
  ggtitle("Percentage Increase of Days in Hospital \nby Health Board")+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())


plt_proj_HB


#saveRDS(plt_proj_HB, "Plots/plt_proj_HB.rds")


df_mean_los_HB_2023 <- df_proj_draws|>
  group_by(Year, Draw, HB_ID)|>
  summarise(LoS = sum(LoS), NoS = sum(NoS), .groups = "keep")|>
  group_by(Year, HB_ID)|>
  filter(Year %in% c(2023))|>
  mean_qi(LoS, NoS)|>
  select(HB_ID, LoS, NoS)|>
  mutate(mean_los = LoS / NoS)

df_mean_los_HB_frac <- df_proj_draws|>
  group_by(Year, Draw, HB_ID)|>
  summarise(LoS = sum(LoS), NoS = sum(NoS), .groups = "keep")|>
  group_by(Year, HB_ID)|>
  filter(Year %in% c(2033,2043))|>
  mean_qi(LoS,NoS)|>
  mutate(mean_los = LoS / NoS)|>
  left_join(df_mean_los_HB_2023, by = "HB_ID")|>
  select(Year = Year.x, HB_ID, mean_los = mean_los.x, mean_los_2023 = mean_los.y)|>
  mutate(`Increase (%)` = (mean_los / mean_los_2023 -1) *100)


plt_proj_los_HB <- df_mean_los_HB_frac|> 
  left_join(df_HB_boundaries, by = "HB_ID")|>
  ggplot() + 
  geom_sf(aes(geometry = geometry,fill = `Increase (%)`)) + 
  coord_sf()+
  facet_wrap(~Year)+
  theme_bw()+
  scale_fill_viridis_c()+
  ggtitle("Percentage Increase of Length of Stay \nby Health Board")+
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

plt_proj_los_HB

#saveRDS(plt_proj_los_HB, "Plots/plt_proj_los_HB.rds")








########## Appendix B: Diagnostic Plots ##########

# Load in Parameter Data
draws_post_param <- read_parquet("Model_Outputs/draws_post_param.parquet")


# Admission Type Trace Plot - Stays
plt_trace_adm_sty <- draws_post_param|>
  filter(.variable == "b_adm_sty")|>
  mutate(.chain = as.factor(.chain),
         adm_type = admission_levels[i])|>
  rename(Chain = .chain)|>
  ggplot(aes(x = .iteration, y = .value, colour = Chain))+
  geom_line()+
  facet_wrap(~adm_type, scales = "free_y", nrow = 3)+
  ggtitle("Trace Plot for Admission Type Parameters - Number of Stays")+
  xlab("Sample")+
  ylab("Value")+
  scale_color_viridis_d()+
  theme_bw()

plt_trace_adm_sty


#saveRDS(plt_trace_adm_sty, file = "Plots/plt_trace_adm_sty.rds")




# Age Trace Plot - Stays
plt_trace_age_sty <- draws_post_param|>
  filter(.variable == "b_age_sty")|>
  mutate(.chain = as.factor(.chain),
         age = age_levels[i],
         age = if_else(age=="90 years and over","90 years +",age))|>
  rename(Chain = .chain)|>
  ggplot(aes(x = .iteration, y = .value, colour = Chain))+
  geom_line()+
  facet_wrap(~age, scales = "free_y", nrow = 6)+
  ggtitle("Trace Plot for Age Parameters - Number of Stays")+
  xlab("Sample")+
  ylab("Value")+
  scale_color_viridis_d()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,size = 8),
        axis.text.y = element_text(size = 8))+
  scale_y_continuous(labels = function(x) round(x,2))

plt_trace_age_sty

#saveRDS(plt_trace_age_sty, file = "Plots/plt_trace_age_sty.rds")


# Sex Trace Plot - Stays
plt_trace_male_sty <- draws_post_param|>
  filter(.variable == "b_male_sty")|>
  mutate(.chain = as.factor(.chain))|>
  rename(Chain = .chain)|>
  ggplot(aes(x = .iteration, y = .value, colour = Chain))+
  geom_line()+
  ggtitle("Trace Plot for Male Parameter - Number of Stays")+
  xlab("Sample")+
  ylab("Value")+
  scale_color_viridis_d()+
  theme_bw()

plt_trace_male_sty

#saveRDS(plt_trace_male_sty, file = "Plots/plt_trace_male_sty.rds")

### Length of Stay Parameter

# Admission Type Trace Plot - Length of Stay
plt_trace_adm_los <- draws_post_param|>
  filter(.variable == "b_adm_los")|>
  mutate(.chain = as.factor(.chain),
         adm_type = admission_levels[i])|>
  rename(Chain = .chain)|>
  ggplot(aes(x = .iteration, y = .value, colour = Chain))+
  geom_line()+
  facet_wrap(~adm_type, scales = "free_y", nrow = 3)+
  ggtitle("Trace Plot for Admission Type Parameters - Length of Stay")+
  xlab("Sample")+
  ylab("Value")+
  scale_color_viridis_d()+
  theme_bw()

plt_trace_adm_los


#saveRDS(plt_trace_adm_los, file = "Plots/plt_trace_adm_los.rds")




# Age Trace Plot - Length of Stay
plt_trace_age_los <- draws_post_param|>
  filter(.variable == "b_age_los")|>
  mutate(.chain = as.factor(.chain),
         age = age_levels[i],
         age = if_else(age=="90 years and over","90 years +",age))|>
  rename(Chain = .chain)|>
  ggplot(aes(x = .iteration, y = .value, colour = Chain))+
  geom_line()+
  facet_wrap(~age, scales = "free_y", nrow = 6)+
  ggtitle("Trace Plot for Age Parameters - Length of Stay")+
  xlab("Sample")+
  ylab("Value")+
  scale_color_viridis_d()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,size = 8),
        axis.text.y = element_text(size = 8))+
  scale_y_continuous(labels = function(x) round(x,2))

plt_trace_age_los

#saveRDS(plt_trace_age_los, file = "Plots/plt_trace_age_los.rds")


# Sex Trace Plot - Length of Stay
plt_trace_male_los <- draws_post_param|>
  filter(.variable == "b_male_los")|>
  mutate(.chain = as.factor(.chain))|>
  rename(Chain = .chain)|>
  ggplot(aes(x = .iteration, y = .value, colour = Chain))+
  geom_line()+
  ggtitle("Trace Plot for Male Parameter - Length of Stay")+
  xlab("Sample")+
  ylab("Value")+
  scale_color_viridis_d()+
  theme_bw()

plt_trace_male_los

#saveRDS(plt_trace_male_los, file = "Plots/plt_trace_male_los.rds")



# Rhat

df_param_summary <- draws_post_param|>summarise_draws()|>
  arrange(.variable,i)

max(df_param_summary$rhat, na.rm = TRUE)

# All Rhat values are below 1.01


