# Program Information  ----------------------------------------------------

# Program:     030_descriptive_mortality_rates
# Author:      Anna Schultze 
# Description: Present crude mortality rates generated from measures framework
# Input:       measure_[outcome]_[group].csv
# Output:      analysis/outfiles/table2-4.txt
#              analysis/outfiles/figure1-3.png
# Edits:      

# Housekeeping  -----------------------------------------------------------

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

# Data cleaning and checks-------------------------------------------------

missing_test <- function(dataname, varname) {
  
  ifelse(length(which(is.na(dataname$varname)) > 1), paste(deparse(substitute(varname)),"in", deparse(substitute(dataname)), "has unexpected missing values"), paste(deparse(substitute(varname)),"in", deparse(substitute(dataname)),"is OK")) 
  
} 

print("Missing data checks for all-cause mortality")
missing_test(dataname = measure_any_all, varname = care_home_type)
missing_test(dataname = measure_any_age, varname = care_home_type)
missing_test(dataname = measure_any_sex, varname = care_home_type)
missing_test(dataname = measure_any_sex_age, varname = care_home_type)

missing_test(dataname = measure_any_age, varname = ageband_narrow)
missing_test(dataname = measure_any_sex, varname = sex)
missing_test(dataname = measure_any_sex_age, varname = ageband_narrow)
missing_test(dataname = measure_any_sex_age, varname = sex)

missing_test(dataname = measure_any_all, varname = ons_any_death)
missing_test(dataname = measure_any_age, varname = ons_any_death)
missing_test(dataname = measure_any_sex, varname = ons_any_death)
missing_test(dataname = measure_any_sex_age, varname = ons_any_death)

print("Missing data checks for covid mortality")
missing_test(dataname = measure_covid_all, varname = care_home_type)
missing_test(dataname = measure_covid_age, varname = care_home_type)
missing_test(dataname = measure_covid_sex, varname = care_home_type)
missing_test(dataname = measure_covid_sex_age, varname = care_home_type)

missing_test(dataname = measure_covid_age, varname = ageband_narrow)
missing_test(dataname = measure_covid_sex, varname = sex)
missing_test(dataname = measure_covid_sex_age, varname = ageband_narrow)
missing_test(dataname = measure_covid_sex_age, varname = sex)

missing_test(dataname = measure_covid_all, varname = ons_covid_death)
missing_test(dataname = measure_covid_age, varname = ons_covid_death)
missing_test(dataname = measure_covid_sex, varname = ons_covid_death)
missing_test(dataname = measure_covid_sex_age, varname = ons_covid_death)

print("Missing data checks for non-covid mortality")
missing_test(dataname = measure_noncovid_all, varname = care_home_type)
missing_test(dataname = measure_noncovid_age, varname = care_home_type)
missing_test(dataname = measure_noncovid_sex, varname = care_home_type)
missing_test(dataname = measure_noncovid_sex_age, varname = care_home_type)

missing_test(dataname = measure_noncovid_age, varname = ageband_narrow)
missing_test(dataname = measure_noncovid_sex, varname = sex)
missing_test(dataname = measure_noncovid_sex_age, varname = ageband_narrow)
missing_test(dataname = measure_noncovid_sex_age, varname = sex)

missing_test(dataname = measure_noncovid_all, varname = ons_noncovid_death)
missing_test(dataname = measure_noncovid_age, varname = ons_noncovid_death)
missing_test(dataname = measure_noncovid_sex, varname = ons_noncovid_death)
missing_test(dataname = measure_noncovid_sex_age, varname = ons_noncovid_death)

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

# Confidence Intervals ----------------------------------------------------

measure_any_all <- as_tibble(cbind(measure_any_all,((binconf(measure_any_all$ons_any_death, measure_any_all$population, alpha = 0.05, method = "wilson")))))
measure_any_sex <- as_tibble(cbind(measure_any_sex,((binconf(measure_any_sex$ons_any_death, measure_any_sex$population, alpha = 0.05, method = "wilson")))))
measure_any_age <- as_tibble(cbind(measure_any_age,((binconf(measure_any_age$ons_any_death, measure_any_age$population, alpha = 0.05, method = "wilson")))))
measure_any_sex_age <- as_tibble(cbind(measure_any_sex_age,((binconf(measure_any_sex_age$ons_any_death, measure_any_sex_age$population, alpha = 0.05, method = "wilson")))))

measure_covid_all <- as_tibble(cbind(measure_covid_all,((binconf(measure_covid_all$ons_covid_death, measure_covid_all$population, alpha = 0.05, method = "wilson")))))
measure_covid_sex <- as_tibble(cbind(measure_covid_sex,((binconf(measure_covid_sex$ons_covid_death, measure_covid_sex$population, alpha = 0.05, method = "wilson")))))
measure_covid_age <- as_tibble(cbind(measure_covid_age,((binconf(measure_covid_age$ons_covid_death, measure_covid_age$population, alpha = 0.05, method = "wilson")))))
measure_covid_sex_age <- as_tibble(cbind(measure_covid_sex_age,((binconf(measure_covid_sex_age$ons_covid_death, measure_covid_sex_age$population, alpha = 0.05, method = "wilson")))))

measure_noncovid_all <- as_tibble(cbind(measure_noncovid_all,((binconf(measure_noncovid_all$ons_noncovid_death, measure_noncovid_all$population, alpha = 0.05, method = "wilson")))))
measure_noncovid_sex <- as_tibble(cbind(measure_noncovid_sex,((binconf(measure_noncovid_sex$ons_noncovid_death, measure_noncovid_sex$population, alpha = 0.05, method = "wilson")))))
measure_noncovid_age <- as_tibble(cbind(measure_noncovid_age,((binconf(measure_noncovid_age$ons_noncovid_death, measure_noncovid_age$population, alpha = 0.05, method = "wilson")))))
measure_noncovid_sex_age <- as_tibble(cbind(measure_noncovid_sex_age,((binconf(measure_noncovid_sex_age$ons_noncovid_death, measure_noncovid_sex_age$population, alpha = 0.05, method = "wilson")))))
                              
# Make and print tables 

# Tables ------------------------------------------------------------------
# long term aim to automate to have cleaner code 

# Tabels: All Cause -------------------------------------------------------

table_2a <- measure_any_all %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>% 
  select(c(care_home_group, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) 
  
write.table(table_2a, file = "./analysis/outfiles/table_2a.txt", sep = "\t", na = "", row.names=FALSE)

table_2b <- measure_any_sex %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  rename(Gender = sex) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>% 
  select(c(care_home_group, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) %>% 
  select(Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Gender, Date)

write.table(table_2b, file = "./analysis/outfiles/table_2b.txt", sep = "\t", na = "", row.names=FALSE)

table_2c <- measure_any_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Age = ageband_narrow) %>% 
  select(c(care_home_group, Age, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) %>% 
  select(Age, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Date)

write.table(table_2c, file = "./analysis/outfiles/table_2c.txt", sep = "\t", na = "", row.names=FALSE)

table_2d <- measure_any_sex_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Age = ageband_narrow) %>% 
  rename(Gender = sex) %>% 
  select(c(care_home_group, Age, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) %>% 
  select(Age, Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Gender, Date)

write.table(table_2d, file = "./analysis/outfiles/table_2d.txt", sep = "\t", na = "", row.names=FALSE)

# Tables: COVID mortality -------------------------------------------------

table_3a <- measure_covid_all %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  select(c(care_home_group, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) 

write.table(table_3a, file = "./analysis/outfiles/table_3a.txt", sep = "\t", na = "", row.names=FALSE)

table_3b <- measure_covid_sex %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Gender = sex) %>% 
  select(c(care_home_group, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) %>% 
  select(Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Gender, Date)

write.table(table_3b, file = "./analysis/outfiles/table_3b.txt", sep = "\t", na = "", row.names=FALSE)

table_3c <- measure_covid_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Age = ageband_narrow) %>% 
  select(c(care_home_group, Age, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) %>% 
  select(Age, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Date)

write.table(table_3c, file = "./analysis/outfiles/table_3c.txt", sep = "\t", na = "", row.names=FALSE)

table_3d <-  measure_covid_sex_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Age = ageband_narrow) %>% 
  rename(Gender = sex) %>% 
  select(c(care_home_group, Age, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) %>% 
  select(Age, Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Gender, Date)

write.table(table_3d, file = "./analysis/outfiles/table_3d.txt", sep = "\t", na = "", row.names=FALSE)

# Tables: Non-COVID mortality ---------------------------------------------

table_4a <- measure_noncovid_all %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  select(c(care_home_group, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) 

write.table(table_4a, file = "./analysis/outfiles/table_4a.txt", sep = "\t", na = "", row.names=FALSE)

table_4b <- measure_noncovid_sex %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Gender = sex) %>% 
  select(c(care_home_group, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) %>% 
  select(Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Gender, Date)

write.table(table_4b, file = "./analysis/outfiles/table_4b.txt", sep = "\t", na = "", row.names=FALSE)

table_4c <- measure_noncovid_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Age = ageband_narrow) %>% 
  select(c(care_home_group, Age, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) %>% 
  select(Age, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Date)

write.table(table_4c, file = "./analysis/outfiles/table_4c.txt", sep = "\t", na = "", row.names=FALSE)

table_4d <-  measure_noncovid_sex_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  mutate(Mortality_Rate = round((PointEst*1000),2)) %>% 
  rename(n = ons_noncovid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>%  
  rename(Age = ageband_narrow) %>% 
  rename(Gender = sex) %>% 
  select(c(care_home_group, Age, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) %>% 
  select(Age, Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Gender, Date)

write.table(table_4d, file = "./analysis/outfiles/table_4d.txt", sep = "\t", na = "", row.names=FALSE)


# Figures  -------------------------------------------------------------
# currently automatically puts an y axis value - scaled by 10,000 

# Figures: All-cause Mortality --------------------------------------------

# all-cause mortality 
y_value <- (max(measure_any_all$value) + (max(measure_any_all$value)/4)) * 1000


plot_1a <- ggplot(measure_any_all, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, group = care_home_type, colour = care_home_type)) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "All-cause Mortality Rate per 1,000 individuals", 
       title = "Crude All-cause Mortality Rate") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_color_discrete(name="Care Home",
                       labels=c("No", "Yes")) + 
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_1a.png")
plot_1a
dev.off()

y_value <- (max(measure_any_sex$value) + (max(measure_any_sex$value)/4)) * 1000

plot_1b <- ggplot(measure_any_sex, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = sex, linetype = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "All-cause Mortality Rateper 1,000 individuals", 
       title = "Crude All-cause Mortality Rate by Sex", 
       linetype = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_1b.png")
plot_1b
dev.off()

y_value <- (max(measure_any_age$value) + (max(measure_any_age$value)/4)) * 1000

plot_1c <- ggplot(measure_any_age, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = ageband_narrow, linetype = care_home_type, group = interaction(ageband_narrow, care_home_type))) + 
  geom_line(size = 1.3) + 
  labs(x = "Time Period", 
       y = "All-cause Mortality Rate per 1,000 individuals", 
       title = "Crude All-cause Mortality Rate by Age", 
       linetype = "Care Home", 
       colour = "Age") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  scale_colour_viridis_d(option = "plasma") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_1c.png")
plot_1c
dev.off()

# Figures: COVID mortality ------------------------------------------------
y_value <- (max(measure_covid_all$value) + (max(measure_covid_all$value)/4)) * 1000

plot_2a <- ggplot(measure_covid_all, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, group = care_home_type, colour = care_home_type)) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Crude COVID-19 Mortality Rate") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  scale_color_discrete(name="Care Home",
                       labels=c("No", "Yes")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_2a.png")
plot_2a
dev.off()

y_value <- (max(measure_covid_sex$value) + (max(measure_covid_sex$value)/4)) * 1000

plot_2b <- ggplot(measure_covid_sex, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = sex, linetype = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Crude COVID-19 Mortality Rate by Sex", 
       linetype = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_2b.png")
plot_2b
dev.off()

y_value <- (max(measure_covid_age$value) + (max(measure_covid_age$value)/4)) * 1000

plot_2c <- ggplot(measure_covid_age, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = ageband_narrow, linetype = care_home_type, group = interaction(ageband_narrow, care_home_type))) + 
  geom_line(size = 1.3) + 
  labs(x = "Time Period", 
       y = "COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Crude COVID-19 Mortality Rate by Age", 
       linetype = "Care Home", 
       colour = "Age") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  scale_colour_viridis_d(option = "plasma") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_2c.png")
plot_2c
dev.off()

# Figures: Non-COVID mortality ------------------------------------------------
y_value <- (max(measure_noncovid_all$value) + (max(measure_noncovid_all$value)/4)) * 1000

plot_3a <- ggplot(measure_noncovid_all, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, group = care_home_type, colour = care_home_type)) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "Non COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Crude Non-COVID-19 Mortality Rate") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  scale_color_discrete(name="Care Home",
                       labels=c("No", "Yes")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_3a.png")
plot_3a
dev.off()

y_value <- (max(measure_noncovid_sex$value) + (max(measure_noncovid_sex$value)/4)) * 1000

plot_3b <- ggplot(measure_noncovid_sex, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = sex, linetype = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "Non-COVID-19 Mortality Rate  per 1,000 individuals", 
       title = "Crude Non-COVID-19 Mortality Rate by Sex", 
       linetype = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_3b.png")
plot_3b
dev.off()

y_value <- (max(measure_noncovid_age$value) + (max(measure_noncovid_age$value)/4)) * 1000

plot_3c <- ggplot(measure_covid_age, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = ageband_narrow, linetype = care_home_type, group = interaction(ageband_narrow, care_home_type))) + 
  geom_line(size = 1.3) +
  labs(x = "Time Period", 
       y = "Non-COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Crude Non-COVID-19 Mortality Rate by Age", 
       linetype = "Care Home", 
       colour = "Age") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  scale_colour_viridis_d(option = "plasma") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_3c.png")
plot_3c
dev.off()
