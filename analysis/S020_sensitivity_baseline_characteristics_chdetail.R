# Program Information  ----------------------------------------------------

# Program:     S020_sensitivity_chdetail.R
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

# create output folders if they do not exist (if exist, will throw warning which is suppressed)

dir.create(file.path("./output/tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./output/plots"), showWarnings = FALSE, recursive = TRUE)

# Read in arguments supplied through project.yaml 
# this allows the script to be run for several study populations at different times 

args = commandArgs(trailingOnly=TRUE)

print("These are my input arguments")
print(args[1])
print(args[2])

inputdata <- toString(args[1]) 
outputdata <- toString(args[2])

# Functions ---------------------------------------------------------------
# these are work in progress, and currently don't handle missing values very explicitly (they are removed if not in factor)
# long term best option is likely to make actual package like CreateTableOne available on OS docker image

# 1. Function to summarise a vector of binary variables
# Returns a tibble of counts when the variable == 1, and percentages 
indicator_summary <- function(data, groupvar, ...) {
  
  {{data}} %>% 
    select({{groupvar}}, all_of(...)) %>% 
    group_by({{groupvar}}) %>% 
    summarise(
      across(
        .cols  = all_of(...),
        list(count = sum, percent = mean), 
        na.rm = TRUE, 
        .names = "{.fn}-{.col}")) %>% 
    mutate(across(contains("percent"), ~round(.x*100, digits = 2))) %>% 
    pivot_longer(!care_home_group, 
                 names_to = c(".value", "variable"), 
                 names_sep = "-") %>% 
    pivot_wider(names_from = care_home_group, values_from = c(count,percent)) 
  
}

# 2. Function to summarise a vector of factor variables 
# Returns a tibble of counts and percentages for each level of the factor 
factor_summary <- function(data, groupvar, ...) {
  
  {{data}} %>% 
    select({{groupvar}}, all_of(...)) %>% 
    pivot_longer(c(all_of(...)), names_to = "variable", values_to = "value") %>% 
    group_by(care_home_group, variable, value) %>% 
    summarise(count = n()) %>% 
    mutate(percent = (round((count / sum(count))*100, digits = 2))) %>% 
    pivot_wider(names_from = care_home_group, values_from = c(count, percent)) %>% 
    arrange(variable)
}

# 3. Function to summarise a vector of continous variables 
# Returns a tibble of mean and standard deviation (called counts and percentages to enable binding into a table)
cont_summary <- function(data, groupvar, ...) {
  
  {{data}} %>% 
    select({{groupvar}}, all_of(...)) %>% 
    group_by({{groupvar}}) %>% 
    summarise(
      across(
        .cols  = all_of(...),
        list(count = mean, percent = sd), 
        na.rm = TRUE, 
        .names = "{.fn}-{.col}")) %>% 
    mutate(across(where(is.numeric), ~round(.x, digits = 2))) %>% 
    pivot_longer(!care_home_group, 
                 names_to = c(".value", "variable"), 
                 names_sep = "-") %>% 
    pivot_wider(names_from = care_home_group, values_from = c(count,percent)) 
}

# Read in Data ------------------------------------------------------------

study_population <- fread(inputdata, data.table = FALSE, na.strings = "")

# Data Management ----------------------------------------------------------

# create an additional row which is the same value for all observations, to enable a "total" column in the table
overall_summary <-study_population %>% 
  mutate(care_home_group = "Overall") 

# slightly rename the grouping variable for clarity, and bind on your overall dataset (dataset doubles in size)
summary <- study_population %>% 
  mutate(care_home_group = case_when(
    care_home_type == "PC" ~ "Care_Home", 
    care_home_type == "PN" ~ "Nursing_Home", 
    care_home_type == "PS" ~ "Care_or_Nursing", 
    TRUE ~ "Private_Home")) %>% 
  bind_rows(overall_summary) %>% 
  # create a total row 
  mutate(total = 1)

# Summarise Data ----------------------------------------------------------

# list the variables you want to have in your table according to type 
factorvars <- c("ethnicity_cat", "region", "care_home_cat", "imd_cat")
indicatorvars <- c("stroke", "dementia", "diabetes", "ckd", "cancer", "chronic_liver_disease", "chronic_cardiac_disease", "chronic_respiratory_disease") 

# invoke functions to summarise different types of variables 
row_one <- indicator_summary(summary, care_home_group, "total")
row_gender <- factor_summary(summary, care_home_group, "sex")
row_age <- cont_summary(summary, care_home_group, "age")
factor_table <- factor_summary(summary, care_home_group, factorvars) 
indicator_table <- indicator_summary(summary, care_home_group, indicatorvars)

# Format Table  -----------------------------------------------------------

# add descriptor for continous variable
row_age <- row_age %>% 
  mutate(value = "Mean, SD")

# put together as one table 
tableone <- bind_rows(row_one, row_gender, row_age, factor_table, indicator_table)

# basic formatting of columns 
tableone <- tableone %>% 
  mutate(across(starts_with("count"), ~round(.x, digits = 0)), 
         across(starts_with("percent"), ~round(.x, digits = 2))) %>% 
  relocate(variable, value, contains("Overall"), contains("Care_Home"), contains("Nursing_Home"), contains("Care_or"), contains("Priv")) 

# Export Table ------------------------------------------------------------

# export the table as a text file 
write.table(tableone, file = outputdata, sep = "\t", na = "", row.names=FALSE)
