# Program Information  ----------------------------------------------------

# Program:     010_data_management  
# Author:      Anna Schultze 
# Description: Conduct data management for presentation of baseline data 
# Input:       input.csv 
#              Arguments 1 = input data name, 2 = output data name 
#              Note: only runs through command line w. supplied arguments 
# Output:      study_population.csv into output/
# Edits:       22 Dec 2020: make data management discrete (per variable) to help debugging

# Housekeeping  -----------------------------------------------------------

# change wd to be one step higher if the detected wd is /analysis 
# needed for running through project.yaml if your Rproj like mine lives in /analysis
# ensures script can be run both locally and on server without commenting out code 
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

# Read in arguments supplied through project.yaml and check what they are 

args = commandArgs(trailingOnly=TRUE)

print("These are my input arguments")
print(args[1])
print(args[2])

inputdata <- toString(args[1]) 
outputdata <- toString(args[2])

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
vars = c("lung_cancer", "haem_cancer", "other_cancer", "esrf", 
         "diabetes", "chronic_liver_disease", 
         "chronic_cardiac_disease", "chronic_respiratory_disease", "stroke", 
         "dementia")

# if the date is not missing, change the variable to a binary indicator variable
study_population <- input %>% 
  mutate_at((c(vars)), ~if_else(!is.na(.), 1, 0)) 

# tabulate each variable in the list to check that the creation of categories worked
apply(study_population[c(vars)], 2, tabyl)

# Demographics ------------------------------------------------------------
# data management for demographic variables 

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
print("IMD")

# data check
ifelse(!is.numeric(study_population$imd), "imd is not numeric", "imd is numeric as expected")
summary(study_population$imd)

# create variable with quantiles 
# ideally, this should be done relative to external quartiles - ongoing to incorporate this in wider OS work. In the meantime, create quartiles
study_population <- study_population %>% 
  mutate(imd = replace(imd, imd <= 0, NA)) %>% 
  mutate(imd_cat = ntile(imd,5))

# check variable creation 
tabyl(study_population$imd_cat)

##-- Rural Urban 
print("Rural Urban")

#  data check
ifelse(!is.numeric(study_population$rural_urban), "rural_urban is not numeric", "rural_urban is numeric as expected")
summary(study_population$rural_urban)

# create categories 
study_population <- study_population %>% 
  mutate(urban = case_when(
    rural_urban == 5 ~ 1, 
    rural_urban == 8 ~ 1, 
    TRUE ~ 0
  ))  

# check variable creation 
tabyl(study_population$urban)

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
    TRUE ~ "Private Home")) %>% 
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

# Other Covariates -----------------------------------------------------

##-- Flu Vaccine 
print("Flu Vaccine")

# cata check
tabyl(study_population$flu_vaccine) 

# replace missing
study_population <- study_population %>%   
  mutate(flu_vaccine = replace_na(flu_vaccine, 0))

# check variable creation 
missing_test(dataname = study_population, varname = flu_vaccine)
tabyl(study_population$flu_vaccine) 

##-- CKD 
# create ckd as function of esrd and creatinine status 
print("CKD")

# data check 
summary(study_population$creatinine)

# calculate egfr and categorise as ckd 
# translated from Stata code. 
# QC of this outstanding, however, not crucial variable for project 
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

# create indicator variables and tabublate 

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
  mutate(tpp_death_date = as.numeric(ymd(tpp_death_date)))  %>% 
  mutate(ons_any_death_date = as.numeric(ymd(ons_any_death_date)))  %>% 
  mutate(discrepancy = case_when(
    tpp_death_date != ons_any_death_date ~ 1, 
    TRUE ~ 0
  )) %>% 
  mutate(date_difference = tpp_death_date - ons_any_death_date, na.rm = TRUE) 

print("Number and percentage of deaths that do not match in TPP and ONS")
tabyl(study_population$discrepancy)

print("Difference in days between death date in TPP and ONS, where this exists")
summary(study_population$date_difference)

# Save Dataset as CSV -----------------------------------------------------

write.csv(study_population, outputdata, row.names = FALSE)


