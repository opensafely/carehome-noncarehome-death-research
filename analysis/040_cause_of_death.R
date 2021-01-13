# Program Information  ----------------------------------------------------

# Program:     040_cause_of_death 
# Author:      Anna Schultze 
# Description: Look at most common causes of deaths among those who died, in and out of care homes 
# Input:       input_measure_[date].csv
# Output:      tables & figures into: analysis/outfiles
# Edits:      

# Housekeeping  -----------------------------------------------------------

# 
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

# Generic code for formatting each dataset before merging -----------------

cause_of_death_format <- function(inputdata, care_home_filter) {

input <- fread(inputdata, data.table = FALSE, na.strings = "")
filtervar <- enquo(care_home_filter)

table_part <- input %>% 
  # classify causes of deaths into chapters 
  filter(care_home_type == !!filtervar) %>% 
  rename(Care_Home = care_home_type) %>% 
  mutate(cause_chapter = str_sub(died_cause_ons,1,1)) %>% 
  mutate(cause_number = str_sub(died_cause_ons,2,3)) %>% 
  mutate(Cause_of_Death = case_when(
    cause_chapter == "A" | cause_chapter == "B" ~ "Certain infectious and parasitic diseases", 
    cause_chapter == "C" | (cause_chapter == "D" & cause_number < 50) ~ "Neoplasms", 
    cause_chapter == "D" & cause_number > 50 ~ "Diseases of the blood and blood-forming organs", 
    cause_chapter == "E" ~ "Endocrine, nutritional and metabolic diseases", 
    cause_chapter == "F" ~ "Mental and behavioural disorders", 
    cause_chapter == "G" ~ "Diseases of the nervous system", 
    cause_chapter == "H" ~ "Diseases of the eye and adnexa", 
    cause_chapter == "E" ~ "Endocrine, nutritional and metabolic diseases", 
    cause_chapter == "F" ~ "Mental and behavioural disorders", 
    cause_chapter == "G" ~ "Diseases of the nervous system", 
    cause_chapter == "H" & cause_number < 60 ~ "Diseases of the eye and adnexa", 
    cause_chapter == "H" & cause_number > 60 ~ "Diseases of the ear and mastoid process", 
    cause_chapter == "I" ~ "Diseases of the circulatory system", 
    cause_chapter == "J" ~ "Diseases of the respiratory system", 
    cause_chapter == "K" ~ "Diseases of the digestive system", 
    cause_chapter == "L" ~ "Diseases of the skin and subcutaneous tissue", 
    cause_chapter == "M" ~ "Diseases of the musculoskeletal system and connective tissue", 
    cause_chapter == "N" ~ "Diseases of the genitourinary system", 
    cause_chapter == "O" ~ "Pregnancy, childbirth and the puerperium", 
    cause_chapter == "P" ~ "Certain conditions originating in the perinatal period", 
    cause_chapter == "Q" ~ "Diseases of the digestive system", 
    cause_chapter == "R" ~ "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified", 
    cause_chapter == "S" ~ "Injury, poisoning and certain other consequences of external causes", 
    cause_chapter == "V" ~ "External causes of morbidity and mortality",     
    cause_chapter == "S" ~ "Injury, poisoning and certain other consequences of external causes", 
    cause_chapter == "V" ~ "External causes of morbidity and mortality", 
    cause_chapter == "Z" ~ "Factors influencing health status and contact with health services", 
    cause_chapter == "U" ~ "Codes for special purposes (including COVID-19)", 
    TRUE ~ "Missing")) %>% 
  # calculate frequency of each cod
  group_by(Cause_of_Death) %>% 
  summarise(Count = n()) %>% 
  mutate(Percentage = round((Count/sum(Count)),4)*100) %>% 
  # reduce to top 10 only 
  arrange(desc(Percentage)) %>% 
  select(Cause_of_Death, Percentage, Count) %>% 
  slice(1:10)

  # add the time period 
  time_period <- ymd(str_sub(inputdata, 31, 40))
  
  table_part <- table_part %>% 
  mutate(Year = time_period) %>% 
  select(Year, Cause_of_Death, Percentage, Count)
  
  bind_rows(table, table_part)
  
}


# Care Homes --------------------------------------------------------------

table <- NULL

table <- cause_of_death_format(inputdata = "./output/input_cause_of_death_2019-02-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-03-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-04-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-05-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-06-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-07-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-08-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-09-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-10-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-11-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2019-12-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-01-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-02-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-03-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-04-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-05-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-06-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-07-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-08-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-09-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-10-01.csv", care_home_filter = "Y")
table <- cause_of_death_format("./output/input_measures_2020-11-01.csv", care_home_filter = "Y")

# make it wide
output <- table %>%
  group_by(Year) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = id, 
    names_from = Year,
    values_from = (c("Cause_of_Death", "Percentage")))

# save as text
write.table(output, file = "./analysis/outfiles/table_8a.txt", sep = "\t", na = "", row.names=FALSE)


# Private Homes -----------------------------------------------------------

table <- NULL

table <- cause_of_death_format(inputdata = "./output/input_cause_of_death_2019-02-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-03-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-04-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-05-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-06-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-07-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-08-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-09-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-10-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-11-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2019-12-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-01-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-02-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-03-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-04-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-05-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-06-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-07-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-08-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-09-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-10-01.csv", care_home_filter = "N")
table <- cause_of_death_format("./output/input_cause_of_death_2020-11-01.csv", care_home_filter = "N")

# make it wide
output <- table %>%
  group_by(Year) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = id, 
    names_from = Year,
    values_from = (c("Cause_of_Death", "Percentage")))

# save as text
write.table(output, file = "./analysis/outfiles/table_8b.txt", sep = "\t", na = "", row.names=FALSE)

