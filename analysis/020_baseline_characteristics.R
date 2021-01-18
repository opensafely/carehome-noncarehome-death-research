# Program Information  ----------------------------------------------------

# Program:     020_baseline_characteristics 
# Author:      Anna Schultze 
# Description: Summarise cleaned study_population file into a table one of 
#              baseline characteristics 
# Input:       study_population_[year].csv 
# Output:      table1_[year].txt 
# Edits:      

# Housekeeping  -----------------------------------------------------------

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
print(args[2])

inputdata <- toString(args[1]) 
outputdata <- toString(args[2]) 

# Read in Data ------------------------------------------------------------

study_population <- fread(inputdata, data.table = FALSE, na.strings = "")

# Data Management ----------------------------------------------------------
# table of baseline characteristics by care home vs. not care home and overall 

# create row grouping for overall estimates
overall_summary <-study_population %>% 
  mutate(care_home_group = "Overall") 

# create row grouping for care gome status and add overall grouping 
summary <- study_population %>% 
  mutate(care_home_group = ifelse(care_home, "Care_or_Nursing_Home", "Private_Home")) %>% 
  bind_rows(overall_summary) 

# create variable always == 1 for a total row 

summary <- summary %>% 
  mutate(total = 1)

tabyl(summary$care_home_group)

# Define Functions --------------------------------------------------------
# these are not particularly generalised at the moment 
# I wanted to write small discrete functions that summarised and then reformatted a single variable, which can then be applied many times to build a table one
# suggestions for improvements v welcome, particularly around how to get these to save what they are doing to the dataset, current solution a little hacky

# function for summarising categorical variable in table format
#-- the input values are: 
#-- x = the variable (binary or factor)
#-- y = A "display name" for the table, as I can't get R to like labels 
#-- the function 1) calculates counts & percentages by care home, 
#--              2) reorders the columns to be in a logical order 
#--              3) adds some descriptive text for printing (variable and level names)

tabulate_me <- function(x, y) {
  
    table <- summary %>% 
    group_by(care_home_group, {{x}}) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = round((Count/sum(Count)),4)*100) %>% 
    pivot_wider(names_from = c(care_home_group), values_from=c(Count, Percentage), 
                names_glue = "{care_home_group}_{.value}") %>% 
    rename(varlevel = {{x}}) %>% 
    mutate(varlevel = as.character(varlevel)) %>% 
    select(varlevel, (matches("Over*")), (matches("Care*")), (matches("Priv*"))) %>% 
    mutate(row = row_number()) %>% 
    mutate(varname = case_when(
      row == 1 ~ as_label(enquo(y))
      )) %>% 
    select(varname, everything()) %>% 
    select(-c(row))
    
    bind_rows(table_1, table)
    
}

# function for summarising continous variables 
#-- the input values are: 
#-- x = the variable (numeric)
#-- y = display name 
#-- the function is as above, but presents mean and SD 

summarise_me <- function(x, y) { 
  
    table <- summary %>% 
    group_by(care_home_group) %>% 
    summarise(Mean = round(mean({{x}}),0), SD = round(sd({{x}}),0)) %>% 
    pivot_wider(names_from = c(care_home_group), values_from=c(Mean, SD), 
                names_glue = "{care_home_group}_{.value}") %>% 
    rename(Overall_Count = Overall_Mean, 
           Overall_Percentage = Overall_SD,
           Care_or_Nursing_Home_Count = Care_or_Nursing_Home_Mean,
           Care_or_Nursing_Home_Percentage = Care_or_Nursing_Home_SD,
           Private_Home_Count = Private_Home_Mean,
           Private_Home_Percentage = Private_Home_SD) %>% 
    mutate(varlevel = "Mean, SD",
           varlevel = as.character(varlevel)) %>% 
    select(varlevel, (matches("Over*")), (matches("Care*")), (matches("Priv*"))) %>% 
    mutate(row = row_number()) %>% 
    mutate(varname = case_when(
      row == 1 ~ as_label(enquo(y))
    )) %>% 
    select(varname, everything()) %>% 
    select(-c(row)) 
    
    bind_rows(table_1, table)
}

# create an empty table structure to append generated tables to 
# if someone can think of a neater way to do this, please let me know
# i think I might be approaching it too much like table outputting in SAS 

table_1 <- NULL

# tabulate and summarise the variables I want 

table_1 <- tabulate_me(x = total, y = Total)
table_1 <- tabulate_me(x = care_home_cat, y = Care_Home_Type)
table_1 <- tabulate_me(x = sex, y = Gender) 
table_1 <- summarise_me(x = age, y = Age_in_Years)
table_1 <- tabulate_me(x = ethnicity_cat, y = Self-reported_Ethnicity)
table_1 <- tabulate_me(x = region, y = Geographical_Region)
table_1 <- tabulate_me(x = urban, y = Urban_Area)
table_1 <- tabulate_me(x = imd_cat, y = Quintile_of_Index_of_Multiple_Deprivation)
table_1 <- tabulate_me(x = diabetes, y = Diabetes)
table_1 <- tabulate_me(x = ckd, y = Chronic_Kidney_Disease)
table_1 <- tabulate_me(x = lung_cancer, y = Lung_Cancer)
table_1 <- tabulate_me(x = haem_cancer, y = Haematological_Cancer)
table_1 <- tabulate_me(x = other_cancer, y = Other_Cancer)
table_1 <- tabulate_me(x = chronic_liver_disease, y = Chronic_Liver_Disease)
table_1 <- tabulate_me(x = chronic_cardiac_disease, y = Chronic_Cardiac_Disease)
table_1 <- tabulate_me(x = chronic_respiratory_disease, y = Chronic_Respiratory_Disease)
table_1 <- tabulate_me(x = stroke, y = History_of_Stroke)
table_1 <- tabulate_me(x = dementia, y = Dementia)

# export tbe table as a nice text file 
write.table(table_1, file = outputdata, sep = "\t", na = "", row.names=FALSE)
