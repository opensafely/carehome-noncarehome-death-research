# Program Information  ----------------------------------------------------

# Program:     035_comparative_mortality_rates
# Author:      Anna Schultze 
# Description: PPresent mortality rates as relative risk and risk differences 
# Input:       measure_[outcome]_[group].csv
# Output:      analysis/outfiles/table5.txt
#              analysis/outfiles/figure4-5.png
# Edits:      

# Housekeeping  -----------------------------------------------------------

# load packages 
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(Hmisc)

# make sure my favoured output folder exists

mainDir <- getwd() 
subDir <- "./analysis/outfiles"

if (file.exists(subDir)){
  print("Out directory exists")
} else {
  dir.create(file.path(mainDir, subDir))
  print("Out directory didn't exist, but I created it")
}

# Read in Data ------------------------------------------------------------

# all-cause death 
measure_any_all <- fread("./output/measure_allcause_death_all.csv", data.table = FALSE, na.strings = "")
measure_any_sex <- fread("./output/measure_allcause_death_sex.csv", data.table = FALSE, na.strings = "")
measure_any_age <- fread("./output/measure_allcause_death_age.csv", data.table = FALSE, na.strings = "")
measure_any_sex_age <- fread("./output/measure_allcause_death_sex_age.csv", data.table = FALSE, na.strings = "")

# covid death 
measure_covid_all <- fread("./output/measure_covid_death_all.csv", data.table = FALSE, na.strings = "")
measure_covid_sex <- fread("./output/measure_covid_death_sex.csv", data.table = FALSE, na.strings = "")
measure_covid_age <- fread("./output/measure_covid_death_age.csv", data.table = FALSE, na.strings = "")
measure_covid_sex_age <- fread("./output/measure_covid_death_sex_age.csv", data.table = FALSE, na.strings = "")

# non covid death 
measure_noncovid_all <- fread("./output/measure_noncovid_death_all.csv", data.table = FALSE, na.strings = "")
measure_noncovid_sex <- fread("./output/measure_noncovid_death_sex.csv", data.table = FALSE, na.strings = "")
measure_noncovid_age <- fread("./output/measure_noncovid_death_age.csv", data.table = FALSE, na.strings = "")
measure_noncovid_sex_age <- fread("./output/measure_noncovid_death_sex_age.csv", data.table = FALSE, na.strings = "")

# Remove empty COVID rows--------------------------------------------------

measure_covid_all <- measure_covid_all %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

measure_covid_sex <- measure_covid_sex %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

measure_covid_age <- measure_covid_age %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

measure_covid_sex_age <- measure_covid_sex_age %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

# Create Comparative Measures  --------------------------------------------

# Overall  ----------------------------------------------------------------

table_5a <- measure_any_all %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) 

tab5aout <- table_5a %>% 
  select(Date, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI)  
  
write.table(tab5aout, file = "./analysis/outfiles/table_5a.txt", sep = "\t", na = "", row.names=FALSE)

table_6a <- measure_covid_all %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) 

tab6aout <- table_6a %>% 
  select(Date, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI)  

write.table(tab6aout, file = "./analysis/outfiles/table_6a.txt", sep = "\t", na = "", row.names=FALSE)

table_7a <- measure_noncovid_all %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) 

tab7aout <- table_7a %>% 
  select(Date, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI)  

write.table(tab7aout, file = "./analysis/outfiles/table_7a.txt", sep = "\t", na = "", row.names=FALSE)

# Sex Stratified ----------------------------------------------------------

table_5b <- measure_any_sex %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Gender = sex) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) 

tab5bout <- table_5b %>% 
  select(Date, Gender, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Gender, Date)

write.table(tab5bout, file = "./analysis/outfiles/table_5b.txt", sep = "\t", na = "", row.names=FALSE)

table_6b <- measure_covid_sex %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Gender = sex) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) 

tab6bout <- table_6b %>% 
  select(Date, Gender, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Gender, Date)

write.table(tab6bout, file = "./analysis/outfiles/table_6b.txt", sep = "\t", na = "", row.names=FALSE)

table_7b <- measure_noncovid_sex %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Gender = sex) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) 

tab7bout <- table_7b %>% 
  select(Date, Gender, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Gender, Date)

write.table(tab7bout, file = "./analysis/outfiles/table_7b.txt", sep = "\t", na = "", row.names=FALSE)

# Age Stratified  ---------------------------------------------------------

table_5c <- measure_any_age %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Age = ageband_narrow) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) 

tab5cout <- table_5c %>% 
  select(Date, Age, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Date)

write.table(tab5cout, file = "./analysis/outfiles/table_5c.txt", sep = "\t", na = "", row.names=FALSE)

table_6c <- measure_covid_age %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Age = ageband_narrow) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age))

tab6cout <- table_6c %>% 
  select(Date, Age, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Date)

write.table(tab6cout, file = "./analysis/outfiles/table_6c.txt", sep = "\t", na = "", row.names=FALSE)

table_7c <- measure_noncovid_age %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Age = ageband_narrow) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) 

tab7cout <- table_7c %>% 
  select(Date, Age, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Date)

write.table(tab7cout, file = "./analysis/outfiles/table_7c.txt", sep = "\t", na = "", row.names=FALSE)

# Age and Sex Stratified --------------------------------------------------

table_5d <- measure_any_sex_age %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Age = ageband_narrow) %>% 
  rename(Gender = sex) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) 

tab5dout <- table_5d %>% 
  select(Date, Age, Gender, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Gender, Date)

write.table(tab5dout, file = "./analysis/outfiles/table_5d.txt", sep = "\t", na = "", row.names=FALSE)

table_6d <- measure_covid_sex_age %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Age = ageband_narrow) %>% 
  rename(Gender = sex) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) 

tab6dout <- table_6d %>% 
  select(Date, Age, Gender, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Gender, Date)

write.table(tab6dout, file = "./analysis/outfiles/table_6d.txt", sep = "\t", na = "", row.names=FALSE)

table_7d <- measure_noncovid_sex_age %>% 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename variabels to easier names 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Mortality_Rate = round((value*1000),2)) %>% 
  rename(Age = ageband_narrow) %>% 
  rename(Gender = sex) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, value, Mortality_Rate), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # create comparative measures 
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value)) %>% 
  mutate(rd = (Care_or_Nursing_Home_value - Private_Home_value)) %>% 
  mutate(Relative_Risk = round(rr,2)) %>% 
  mutate(Risk_Difference = round(rd*1000,2)) %>% 
  # calculate confidence intervals for relative risk
  mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N))) %>% 
  mutate(ef = exp(1.96 * se_log_rr)) %>% 
  mutate(rr_lcl = rr/ef) %>% 
  mutate(rr_ucl = rr*ef) %>% 
  # calculate confidence interval for risk difference 
  mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N)) %>%  
  mutate(rd_lcl = rd - 1.96*se_rd) %>% 
  mutate(rd_ucl = rd + 1.96*se_rd) %>% 
  mutate(Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-")) %>% 
  mutate(Risk_Difference_CI = paste(round(rd_lcl,2), round(rd_ucl,2), sep = "-")) %>% 
  # select variables to present in tables and rename as relevant
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) 

tab7dout <- table_7d %>% 
  select(Date, Age, Gender, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Gender, Date)

write.table(tab7dout, file = "./analysis/outfiles/table_7d.txt", sep = "\t", na = "", row.names=FALSE)

# Figures  ----------------------------------------------------------------

# Figures: Overall --------------------------------------------------------

# all-cause 
y_value <- (max(table_5a$rr_ucl) + (max(table_5a$rr_ucl)/4)) 

plot_4a <- ggplot(table_5a, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk)) + 
  geom_line(size =1) +
  geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), alpha = 0.3) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative All-cause Mortality Risk") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_4a.png")
plot_4a
dev.off()

# covid 
y_value <- (max(table_6a$rr_ucl) + (max(table_6a$rr_ucl)/4)) 

plot_5a <- ggplot(table_6a, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk)) + 
  geom_line(size =1) +
  geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), alpha = 0.3) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative Risk of COVID death") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_5a.png")
plot_5a
dev.off()

# non-covid death
y_value <- (max(table_7a$rr_ucl) + (max(table_7a$rr_ucl)/4)) 

plot_6a <- ggplot(table_7a, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk)) + 
  geom_line(size =1) +
  geom_ribbon(aes(ymin=rr_lcl, ymax=rr_ucl), alpha = 0.3) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative Risk of non-COVID death") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_6a.png")
plot_6a
dev.off()

# Figures: Sex Stratified -------------------------------------------------

# all-cause 
y_value <- (max(table_5b$Relative_Risk) + (max(table_5b$Relative_Risk)/4)) 

plot_4b <- ggplot(table_5b, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Gender, colour = Gender, )) + 
  geom_line(size =1) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative All-cause Mortality Risk") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_4b.png")
plot_4b
dev.off()

# covid
y_value <- (max(table_6b$Relative_Risk) + (max(table_6b$Relative_Risk)/4)) 

plot_5b <- ggplot(table_6b, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Gender, colour = Gender, )) + 
  geom_line(size =1) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative Risk of COVID-19 mortality") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_5b.png")
plot_5b
dev.off()

# non-covid
y_value <- (max(table_7b$Relative_Risk) + (max(table_7b$Relative_Risk)/4)) 

plot_6b <- ggplot(table_7b, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Gender, colour = Gender, )) + 
  geom_line(size =1) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative Risk of non COVID-19 mortality") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_6b.png")
plot_6b
dev.off()

# Figures: Age Stratified  ------------------------------------------------

# all-cause 
y_value <- (max(table_5c$Relative_Risk) + (max(table_5c$Relative_Risk)/4)) 

plot_4c <- ggplot(table_5c, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Age, colour = Age, )) + 
  geom_line(size =1) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative All-cause Mortality Risk") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_colour_viridis_d(option = "plasma") + 
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_4c.png")
plot_4c
dev.off()

# covid
y_value <- (max(table_6c$Relative_Risk) + (max(table_6c$Relative_Risk)/4)) 

plot_5c <- ggplot(table_6c, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Age, colour = Age, )) + 
  geom_line(size =1) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative Risk of COVID-19 death") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_colour_viridis_d(option = "plasma") + 
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_5c.png")
plot_5c
dev.off()

# non-covid
y_value <- (max(table_7c$Relative_Risk) + (max(table_7c$Relative_Risk)/4)) 

plot_6c <- ggplot(table_7c, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Age, colour = Age)) + 
  geom_line(size =1) +
  labs(x = "Time Period", 
       y = "Relative Risk (care homes vs. private homes)", 
       title = "Crude Relative Risk of non COVID-19 death") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_colour_viridis_d(option = "plasma") + 
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_6c.png")
plot_6c
dev.off()


