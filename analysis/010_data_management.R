# Program Information  ----------------------------------------------------

# Program:     010_data_management  
# Author:      Anna Schultze 
# Description: Conduct data management for presentation of baseline data 
# Input:       input.csv 
#              Arguments 1 = input data name, 2 = output data name, 3 = extra output data name
#              Note: only runs through command line w. supplied arguments 
# Output:      study_population.csv into output/

# Housekeeping  -----------------------------------------------------------

# load packages 
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

# Read in arguments supplied through project.yaml and check what they are 

args = commandArgs(trailingOnly=TRUE)

print("These are my input arguments")
print(args[1])
print(args[2])
print(args[3])

inputdata <- toString(args[1]) 
outputdata <- toString(args[2])
outputdata_extra <- toString(args[3])

# Read in Data -----------------------------------------------------------
input <- fread(inputdata, data.table = FALSE, na.strings = "")

# Data Cleaning and Tests--------------------------------------------------

# Raw prevalences and types -----------------------------------------------

print("distribution of age")
summary(input$age)
print("prevalence of key variables")
apply(input[c("ethnicity", "sex", "rural_urban", "region", "care_home_type")], 2, tabyl)

# Date Covariates ---------------------------------------------------------

# list of variables to change from date to indicator 
vars = c("esrf", "diabetes", "chronic_liver_disease", "esrf",
         "chronic_cardiac_disease", "chronic_respiratory_disease", "stroke", 
         "dementia")

# if the date is not missing, change the variable to a binary indicator variable
study_population <- input %>% 
  mutate_at((c(vars)), ~if_else(!is.na(.), 1, 0)) 

# tabulate each variable in the list to check that the creation of categories worked
apply(study_population[c(vars)], 2, tabyl)

# Demographics ------------------------------------------------------------
# data management for demographic variables 

##-- Age 
print("Age")

summary(study_population$age)

age_summary <- study_population %>% 
  group_by(ageband_narrow) %>% 
  summarise(min_age = min(age), max_age = max(age))

age_summary 

##-- Ethnicity 
print("Ethnicity")

# data checks 
ifelse(!is.integer(study_population$ethnicity), "ethnicity is not an integer", "ethnicity is an integer as expected")
tabyl(study_population$ethnicity)

# create categories  
study_population <- study_population %>%    
  mutate(ethnicity_cat = case_when(
    ethnicity == 1 ~ "White", 
    ethnicity == 2 ~ "Mixed", 
    ethnicity == 3 ~ "Asian or British Asian", 
    ethnicity == 4 ~ "Black", 
    ethnicity == 5 ~ "Other", 
    TRUE ~ "Missing"))  %>% 
  mutate(ethnicity_cat = fct_relevel(ethnicity_cat, c("White", "Asian or British Asian", "Black", "Mixed", "Other"))) 

# check variable creation 
tabyl(study_population$ethnicity_cat)

##-- Sex
print("Sex")

# data checks
tabyl(study_population$sex)

# create categories 
study_population <- study_population %>%     
  mutate(sex = as.factor(sex))  %>% 
  mutate(sex = fct_recode(sex, "Male" = "M", "Female" = "F")) 

# check variable creation 
tabyl(study_population$sex)

##-- IMD 
# note that this is outputted as quintiles from absolute values in the study definition 
print("IMD")

# data check
ifelse(!is.numeric(study_population$imd), "imd is not numeric", "imd is numeric as expected")
summary(study_population$imd)

# put missing to missing 
study_population <- study_population %>% 
  mutate(imd = replace(imd, imd <= 0, NA), 
         imd_cat = case_when(imd == 1 ~ "1 - Least Deprived", 
                             imd == 2 ~ "2", 
                             imd == 3 ~ "3", 
                             imd == 4 ~ "4", 
                             imd == 5 ~ "5 - Most Deprived", 
                             TRUE ~ "Missing"), 
         imd_cat = as_factor(imd_cat), 
         imd_cat = fct_relevel(imd_cat, c("1 - Least Deprived", "2", "3", "4", "5 - Most Deprived", "Missing")))  

# check variable creation 
tabyl(study_population$imd_cat)

##-- Rural Urban 
# this is a numeric variable that takes a series of different values with -1 missing
print("Rural Urban")

#  data check
ifelse(!is.numeric(study_population$rural_urban), "rural_urban is not numeric", "rural_urban is numeric as expected")
summary(study_population$rural_urban)

# create categories 
study_population <- study_population %>% 
  mutate(urban = case_when(
    rural_urban >=1 & rural_urban <5 ~ "Urban", 
    rural_urban <= 0 ~ "Missing", 
    TRUE ~ "Rural"
  ))  

# check variable creation 
tabyl(study_population$urban)

##-- Region 

study_population <- study_population %>% 
  mutate(region = as_factor(region))

# Care Home Variables -----------------------------------------------------
print("Care Home Variables")

# data check
ifelse(!is.character(study_population$care_home_type), "care_home_type is not a string", "care_home_type is a string as expected")
tabyl(study_population$care_home_type)

# create categories
study_population <- study_population %>% 
  mutate(care_home_cat = case_when(
    care_home_type == "PC" ~ "Care Home", 
    care_home_type == "PN" ~ "Nursing Home", 
    care_home_type == "PS" ~ "Care or Nursing Home", 
    TRUE ~ "Private Home"), 
    care_home_cat = as_factor(care_home_cat)) %>% 
  mutate(care_home = case_when(
    care_home_type == "PC" ~ 1, 
    care_home_type == "PS" ~ 1, 
    care_home_type == "PN" ~ 1, 
    TRUE ~ 0)) 

# check categories 
tabyl(study_population$care_home_cat)
tabyl(study_population$care_home)

crosstab <- study_population %>% 
  tabyl(care_home_cat, care_home)

crosstab

# New care home resident? 

# create categories
study_population <- study_population %>% 
  mutate(new_resident = case_when(
    care_home_type == "PC" & care_home_prior == "U" ~ 1,  
    care_home_type == "PS" & care_home_prior == "U" ~ 1,  
    care_home_type == "PN" & care_home_prior == "U" ~ 1,  
    TRUE ~ 0))

tabyl(study_population$new_resident)

# Other Covariates -----------------------------------------------------

##-- Flu Vaccine 
print("Flu Vaccine")

# data check
tabyl(study_population$flu_vaccine) 

# replace missing
study_population <- study_population %>%   
  mutate(flu_vaccine = replace_na(flu_vaccine, 0))

# check variable creation 
tabyl(study_population$flu_vaccine) 

##-- CKD 
# create ckd as function of esrd and creatinine status 
print("CKD")

# data check 
summary(study_population$creatinine)

# calculate egfr and categorise as ckd 
# translated from Stata code https://github.com/opensafely/ics-research
# REVIEWER - please check this although variable is not crucial for the project    
study_population <- study_population %>% 
  mutate(creatinine = replace(creatinine, creatinine <20 | creatinine >3000, NA)) %>% 
  mutate(SCR_adj = creatinine/88.4) %>% 
  mutate(min1 = case_when(
    sex == "Male" ~ SCR_adj/0.9, 
    sex == "Female" ~ SCR_adj/0.7)) %>% 
  mutate(min2 = case_when(
    sex == "Male" ~ min1^(-0.411), 
    sex == "Female" ~ min1^(-0.329))) %>% 
  mutate(max1 = case_when(
    sex == "Male" ~ SCR_adj/0.9, 
    sex == "Female" ~ SCR_adj/0.7)) %>% 
  mutate(max2 = max1^(-1.209)) %>% 
  mutate(max2 = replace(max2, max2>1, 1)) %>% 
  mutate(egfr = min2*max2*141) %>% 
  mutate(egfr = egfr*(0.993^age)) %>% 
  mutate(egfr = case_when(
    sex == "Female" ~ egfr*1.018, 
    TRUE ~ egfr)) %>% 
  mutate(ckd_egfr = case_when(
    egfr < 15 ~ 5, 
    egfr >= 15 & egfr < 30 ~ 4, 
    egfr >= 30 & egfr < 44 ~ 3, 
    egfr >= 45 & egfr < 59 ~ 2, 
    egfr >= 90 ~ 1)) %>% 
  mutate(ckd = case_when(
    ckd_egfr >= 3 ~ 1, 
    esrf == 1 ~ 1, 
    TRUE ~ 0)) %>% 
  mutate(ckd = replace_na(ckd, 0))

# check variable creation 
summary(study_population$egfr)
apply(study_population[c("ckd_egfr", "esrf", "ckd")], 2, tabyl)

# Outcome Variables -------------------------------------------------------

# create indicator variables and tabulate 

study_population <- study_population  %>%  
  mutate(tpp_death = case_when(
    !is.na(tpp_death_date) ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(ons_covid_death = case_when(
    !is.na(ons_covid_death_date) ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(ons_death = case_when(
    !is.na(ons_any_death_date) ~ 1, 
    TRUE ~ 0
  ))

tabyl(study_population$tpp_death)
tabyl(study_population$ons_covid_death)
tabyl(study_population$ons_death)

# check overlap between ONS and TPP deaths 
crosstab <- study_population %>% 
  tabyl(tpp_death, ons_death)

crosstab

# check correlation between TPP and ONS deaths 
study_population <- study_population  %>%    
  mutate(tpp_death_date = as.numeric(ymd(tpp_death_date))) %>% 
  mutate(ons_any_death_date = as.numeric(ymd(ons_any_death_date)))  %>% 
  mutate(discrepancy = case_when(
    tpp_death_date != ons_any_death_date ~ 1, 
    TRUE ~ 0
  )) 

print("Number of deaths that do not match in TPP and ONS")
tabyl(study_population$discrepancy)

date_check <- study_population %>% 
  filter(discrepancy == 1) %>% 
  mutate(date_difference = tpp_death_date - ons_any_death_date, na.rm = TRUE) 

print("Difference in days between death date in TPP and ONS, where this exists")
summary(date_check$date_difference)

# Save Dataset as CSV -----------------------------------------------------
write.csv(study_population, outputdata, row.names = FALSE)

# reduce dataset to only new residents 
study_population_new <- study_population %>% 
  filter(new_resident == 1) 

write.csv(study_population_new, outputdata_extra, row.names = FALSE)


