# Program Information  ----------------------------------------------------

# Program:     S040_sensitivity_causes_of_death
# Author:      Anna Schultze 
# Description: display causes of death measures over time in a single graph, to compare relative increases over time
# Input:       input_measures_[date].csv
# Output:      figure in output/plots 
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

# Functions ---------------------------------------------------------------

# 1. Function to directly standardise rates calculated by measures 
# formulas for CIs available in Kirkwood and Sterne (DSR, p266) vs Clayton and Hills (CMF, p138-139)

standardise <- function(data, outcome) { 
  
  {{data}} %>% 
    left_join(european_standard, by = c("ageband_five")) %>% 
    # expected deaths is mortality rate times the standard groupsize 
    mutate(expected_deaths = value * groupsize) %>% 
    # sum by group 
    group_by(date, sex, care_home_type) %>% 
    mutate(total_expected = sum(expected_deaths)) %>% 
    ungroup() %>%
    # the directly standardised rate is expected deaths over total standard population size 
    # calculate the SE around the dsri 
    mutate(dsr = total_expected/total, 
           se_dsri = (groupsize^2*value * (1- value))/registered_at_start) %>% 
    # sum standard error per category
    group_by(date, sex, care_home_type) %>% 
    mutate(se_dsr = (sqrt(sum(se_dsri)))/total) %>% 
    ungroup() %>% 
    # calculate standard deviation (needed for calculating CI for ratio of DSRs)
    mutate(sdi = sqrt({{outcome}})/registered_at_start, 
           sdiw_squared = ((sdi * (groupsize/total))^2)) %>% 
    group_by(date, sex, care_home_type) %>% 
    mutate(sd_sum = sum(sdiw_squared), 
           sd = sqrt(sd_sum)) %>% 
    ungroup() %>% 
    mutate(log_sd = sd/dsr) %>% 
    # keep only one row per unique group 
    select(date, care_home_type, sex, dsr, se_dsr, log_sd) %>% 
    distinct() %>% 
    # Finalise calculating and formatting confidence interval 
    mutate(lcl = dsr - 1.96 * se_dsr, 
           ucl = dsr + 1.96 * se_dsr, 
           Confidence_Interval = paste(round(lcl*1000,2), round(ucl*1000,2), sep = "-"),
           Standardised_Rate = round(dsr * 1000,2)) 
}

# Read in Data ------------------------------------------------------------

# opensafely population 

respiratory <- fread("./output/measure_respiratory.csv", data.table = FALSE, na.strings = "")
cancer <- fread("./output/measure_cancer.csv", data.table = FALSE, na.strings = "")
cv <- fread("./output/measure_cv.csv", data.table = FALSE, na.strings = "")
dementia <- fread("./output/measure_dementia.csv", data.table = FALSE, na.strings = "")

# standard population 
european_standard <- fread("./data/european_standard_population.csv", data.table = FALSE, na.strings = "")

# Data Management - Standard Population  ----------------------------------

european_standard <- european_standard %>% 
  # remove redundant age groups 
  filter(AgeGroup != "0-4",
         AgeGroup != "5-9", 
         AgeGroup != "10-14", 
         AgeGroup != "15-19", 
         AgeGroup != "20-24", 
         AgeGroup != "25-29", 
         AgeGroup != "30-34", 
         AgeGroup != "35-39", 
         AgeGroup != "40-44", 
         AgeGroup != "45-49", 
         AgeGroup != "50-54", 
         AgeGroup != "55-59", 
         AgeGroup != "60-64") %>% 
  # calculate total pop size 
  mutate(total = sum(EuropeanStandardPopulation)) %>% 
  # rename the age band and group size variable for merging and ease of handling
  rename(ageband_five = AgeGroup, 
         groupsize = EuropeanStandardPopulation) %>% 
  # keep only relevant variables 
  select(ageband_five, groupsize, total)

# Calculate DSRs  ------------------------------------------------------------

respiratory_standard <- standardise(respiratory, ons_respiratory_death) %>% 
  mutate(group = "Respiratory") %>% 
  select(date, care_home_type, sex, dsr, group)
dementia_standard <- standardise(dementia, ons_dementia_death) %>% 
  mutate(group = "Dementia") %>% 
  select(date, care_home_type, sex, dsr, group)
cancer_standard <- standardise(cancer, ons_cancer_death) %>% 
  mutate(group = "Cancer") %>% 
  select(date, care_home_type, sex, dsr, group)
cv_standard <- standardise(cv, ons_cv_death) %>% 
  mutate(group = "Cardiovascular") %>% 
  select(date, care_home_type, sex, dsr, group)

all_causes <- rbind(respiratory_standard, dementia_standard, cancer_standard, cv_standard)

# DSR figures --------------------------------------------------------------

plot_m <- all_causes %>% 
    filter(sex == "M") %>% 
    ggplot(aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = group, linetype = care_home_type)) + 
    geom_line(size = 1) + geom_point() + 
    labs(x = "Time Period", 
         y = "Standardised Rate per 1,000 individuals", 
         title = "Causes of Death over time among Men",
         colour = "Cause of Death", 
         linetype = "Care Home") + 
    scale_y_continuous() +
    scale_color_viridis_d(option = "plasma") +
    scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "gray"), 
          panel.grid.major.y = element_line(color = "gainsboro"))

png(filename = "./output/plots/S4a_plot_causes_men.png")
plot_m
dev.off()

plot_f <- all_causes %>% 
  filter(sex == "F") %>% 
  ggplot(aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = group, linetype = care_home_type)) + 
  geom_line(size = 1) + geom_point() + 
  labs(x = "Time Period", 
       y = "Standardised Rate per 1,000 individuals", 
       title = "Causes of Death over time among Women",
       colour = "Cause of Death", 
       linetype = "Care Home") + 
  scale_y_continuous() +
  scale_color_viridis_d(option = "plasma") +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro"))

png(filename = "./output/plots/S4b_plot_causes_women.png")
plot_f
dev.off()
