# Program Information  ----------------------------------------------------

# Program:     025_carehome_characteristics.R
# Author:      Anna Schultze 
# Description: Summarise characteristics of care home residents at care home level
#              NOTE: analyses not restricted to TPP care homes. 
#              Need to be interpreted bearing in mind an unknown % of residents are missing. 
# Input:       study_population-[year].csv 
# Output:      series of tables with summary characteristics on the care home level, written to log file 
#              note, intended to be referenced in text, therefore not outputted into tables 
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

# make sure my favoured output folder exists

mainDir <- getwd() 
subDir <- "./analysis/outfiles"

if (file.exists(subDir)){
  print("Out directory exists")
} else {
  dir.create(file.path(mainDir, subDir))
  print("Out directory didn't exist, but I created it")
}

# Read in arguments supplied through project.yaml 
# this allows the script to be run for several study populations at different times 

args = commandArgs(trailingOnly=TRUE)

print("These are my input arguments")
print(args[1])

inputdata <- toString(args[1]) 

# Send output to an output text file 
logfile <- file("./analysis/outfiles/carehome_characteristics.txt")
sink(logfile, append=TRUE)
sink(logfile, append=TRUE, type="message")

# Read in Data -----------------------------------------------------------
study_population <- fread(inputdata, data.table = FALSE, na.strings = "")

# Apply Study Population Criteria -----------------------------------------

# number of rows in the data
print("number of rows")
nrow(study_population)

study_population <- study_population %>% 
  filter(care_home == 1)

print("number of rows, care home only")
nrow(study_population)

# Generate Summary Statistics ---------------------------------------------

##-- Number of care homes

table_carehomes <- study_population  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  tabyl(care_home_cat) %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_totals() 

table_carehomes

##-- Geographical distribution of care homes 

table_region <- study_population  %>% 
  distinct(household_id, .keep_all = TRUE)  %>% 
  tabyl(region, care_home_cat)  %>% 
  adorn_totals() %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()

table_region

##-- size of care homes, per care home type 
# note, this cannot just use the sum of individuals in a household as this is not an accurate summary
# household size is adjusted based on updated addresses, and therefore a more accurate way of determining the household size
missing_test <- function(dataname, varname) {
  
  ifelse(length(which(is.na(dataname$varname)) > 1), paste(deparse(substitute(varname)),"has unexpected missing values"), paste(deparse(substitute(varname)), "has no unexpected missing values")) 
  
} 

missing_test(dataname = study_population, varname = household_size)

summary(study_population$household_size)

table_size <- study_population  %>% 
  group_by(care_home_cat) %>% 
  summarise(mean_ch_size = mean(household_size), 
            median_ch_size = median(household_size), 
            min_ch_size = min(household_size), 
            max_ch_size = max(household_size), 
            q25_ch_size = quantile(household_size, 0.25), 
            q75_ch_size = quantile(household_size, 0.75)) 
table_size

##-- Percentage of residents with dementia, per care home type

table_dementia <- study_population  %>% 
  group_by(household_id) %>% 
  mutate(n_dementia_in_home = sum(dementia), 
            N_dementia_in_home = n(), 
            p_dementia_in_home = round(n_dementia_in_home/N_dementia_in_home,2)*100) %>% 
  distinct(household_id, .keep_all = TRUE) 

summary(table_dementia$p_dementia_in_home)

table_dementia <- table_dementia %>%  
  group_by(care_home_cat) %>% 
  summarise(median_dementia_p = median(p_dementia_in_home),           
            q25_dementia_p = quantile(p_dementia_in_home, 0.25), 
            q75_dementia_p = quantile(p_dementia_in_home, 0.75))

table_dementia 

##-- Average age of residents, per care home type 

table_age <- study_population %>%  
  group_by(care_home_cat) %>% 
  summarise(median_age = median(age),           
            q25_age = quantile(age, 0.25), 
            q75_age = quantile(age, 0.75))

table_age 

# send output back to screen

sink() 
sink(type="message")
