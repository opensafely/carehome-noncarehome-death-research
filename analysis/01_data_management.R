# Program Information  ----------------------------------------------------

# Program:     01_data_management  
# Author:      Anna Schultze 
# Description: Conduct data management for presentation of baseline data 
# Input:       input.csv 
# Output:      study_population.csv 
#              log file of console output 
#              a complete log (incl input and warnings) is generated running this through project.yaml 
# Edits:      

# Housekeeping  -----------------------------------------------------------

# change wd if detected is /analysis 
# needed for running through project.yaml if your Rproj like mine lives in /analysis
# relies on your wd not having analysis anywhere else in the name... 
if (grepl("/analysis", getwd())) { 
  setwd("..") 
  getwd() 
} else {
    getwd() 
}

# load packages 
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

# Read in Data -----------------------------------------------------------
input <- fread("./output/input.csv", data.table = FALSE, na.strings = "")

# Data Cleaning -----------------------------------------------------------

# check missingness and distribution of age 
summary(input$age)

# list of variables to change from date to indicator 
vars = c("lung_cancer", "haem_cancer", "other_cancer", "esrf", 
         "creatinine_date", "diabetes", "chronic_liver_disease", 
         "chronic_cardiac_disease", "chronic_respiratory_disease", "stroke", 
         "dementia")



# recode variables to indicators 
# recode categorical variables where needed 

study_population <- input %>% 
  mutate_at((c(vars)), ~if_else(!is.na(.), 1, 0)) 

# check other variables 

apply(study_population[c(vars)], 2, tabyl)
apply(study_population[c("ethnicity", "sex", "rural_urban", "region", "imd", "care_home_type")], 2, tabyl)


study_population <- study_population %>%   
  mutate(flu_vaccine = replace_na(flu_vaccine, 0)) %>% 
  mutate(ethnicity_cat = case_when(
    ethnicity == 1 ~ "White", 
    ethnicity == 2 ~ "Mixed", 
    ethnicity == 3 ~ "Asian or British Asian", 
    ethnicity == 4 ~ "Black", 
    ethnicity == 5 ~ "Other", 
    TRUE ~ "Missing"))  %>% 
  mutate(ethnicity_cat = fct_relevel(ethnicity_cat, c("White", "Asian or British Asian", "Black", "Mixed", "Other"))) %>%  
  mutate(sex = as.factor(sex))  %>% 
  mutate(sex = fct_recode(sex, "Male" = "M", "Female" = "F")) %>% 
  mutate(urban = case_when(
    rural_urban == 5 ~ 1, 
    rural_urban == 8 ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(care_home_cat = case_when(
    care_home_type == "PC" ~ "Care Home", 
    care_home_type == "PN" ~ "Nursing Home", 
    care_home_type == "PS" ~ "Care or Nursing Home", 
    TRUE ~ "Private Home"))  %>% 
  mutate(imd = replace(imd, imd <= 0, NA)) %>% 
  mutate(imd_cat = ntile(imd,5))

# check categorisations worked as expected 

apply(study_population[c(vars)], 2, tabyl)
apply(study_population[c("ethnicity_cat", "sex", "rural_urban", "region", "imd_cat", "care_home_cat")], 2, tabyl)

# create ckd as function of esrd and creatinine status 
# check distribution 
summary(study_population$creatinine)

# calculate egfr and categorise as ckd 
# translated from Stata code. this needs checking by someone who knows ckd 

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

summary(study_population$egfr)
apply(study_population[c("ckd_egfr", "esrf", "ckd")], 2, tabyl)

# create care home indicator variable and check 

study_population <- study_population %>% 
  mutate(care_home = case_when(
    care_home_type == "PC" ~ 1, 
    care_home_type == "PS" ~ 1, 
    care_home_type == "PN" ~ 1, 
    care_home_type == "U" ~ 0)) 

crosstab <- study_population %>% 
  tabyl(care_home_type, care_home)

tabyl(study_population$care_home_type)
tabyl(study_population$care_home)

crosstab

# Save Dataset as CSV -----------------------------------------------------

write.csv(study_population, "./data/study_population.csv", row.names = FALSE)




