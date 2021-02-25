# Program Information  ----------------------------------------------------

# Program:     S030_sensitivity_descriptive_mortality_rates_tpp.R
# Author:      Anna Schultze 
# Description: Table and plot of mortality rates going back many more years, using TPP death only 
# Input:       measure_[outcome]_[group].csv
# Output:      analysis/outfiles/table[].txt
#              analysis/outfiles/figure[].png
# Edits:      

# Housekeeping  -----------------------------------------------------------

# load packages 
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(Hmisc)

# create output folders if they do not exist (if exist, will throw warning which is suppressed)

dir.create(file.path("./output/tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./output/plots"), showWarnings = FALSE, recursive = TRUE)

# Functions ---------------------------------------------------------------

# 1. Format Table
# function to format the measures output into something more typical of a paper 
# only runs after added confidence intervals 

format_table <- function(data, outcome) { 
  
  {{data}} %>% 
    # create a labelled variable for outputting in table formats 
    mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
    # rename key variables to how you want them displayed in tables and select the relevant ones
    mutate(Mortality_Rate = round((PointEst*1000),2), 
           Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>% 
    rename(n = {{outcome}}, 
           N = population, 
           Age = ageband_narrow) %>% 
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
    rename(Date = Private_Home_date, 
           Age = Private_Home_Age) %>% 
    select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) %>% 
    select(Age, Date, (matches("Care*")), (matches("Priv*"))) %>% 
    arrange(Age, Date)  
  
}

# 2. Plot Figure 
# function to plot and format the measures into a line plot, stratified by care home 

plot_figure <- function(data, axistext) { 
  
  y_value <- (max({{data}}$value) + (max({{data}}$value)/4)) * 1000
  ystring <- paste(as_label(enquo(axistext)), "Mortality Rate per 1,000 individuals")
  titlestring <- paste(as_label(enquo(axistext)), "Mortality Rate by Age, crude")
  
  ggplot({{data}}, aes (x = as.Date(date, "%Y-%m-%d"), y = value*1000, colour = ageband_narrow, shape = care_home_type, group = interaction(ageband_narrow, care_home_type))) + 
    geom_line(size = 1) + geom_point() + 
    labs(x = "Time Period", 
         y = ystring, 
         title = titlestring, 
         shape = "Care Home", 
         colour = "Age") + 
    scale_y_continuous(limits = c(0,y_value)) +
    scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
    scale_colour_viridis_d() + 
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "gray"), 
          panel.grid.major.y = element_line(color = "gainsboro")) 
}

# Read in Data ------------------------------------------------------------

measure_any_age <- fread("./output/measure_tpp_death_age.csv", data.table = FALSE, na.strings = "")

# Confidence Intervals ----------------------------------------------------
# calculate confidence intervals using binconf function from Hmisc 
# bind output into the original dataset 

measure_any_age <- as_tibble(cbind(measure_any_age,((binconf(measure_any_age$tpp_death, measure_any_age$population, alpha = 0.05, method = "wilson")))))

# Set rows with < 5 events to NA ----------------------------------------
# removing events, percentage and CIs 

measure_any_age <- measure_any_age %>% 
  mutate(value = ifelse(tpp_death <= 5, NA, value), 
         ons_any_death = ifelse(tpp_death <= 5, NA, tpp_death), 
         PointEst = ifelse(tpp_death <= 5, NA, PointEst), 
         Lower = ifelse(tpp_death <= 5, NA, Lower), 
         Upper = ifelse(tpp_death <= 5, NA, Upper), 
         ) 

# Tables ------------------------------------------------------------------

S_table_descriptive_allcause_tpp <- format_table(measure_any_age, tpp_death)
write.table(S_table_descriptive_allcause_tpp, file = "./output/tables/S_table_descriptive_allcause_tpp.txt", sep = "\t", na = "", row.names=FALSE)

# Figures  -------------------------------------------------------------

# all-cause mortality 
S_plot_descriptive_allcause_tpp <- plot_figure(measure_any_age, TPP)

png(filename = "./output/plots/S_plot_descriptive_allcause_tpp.png")
S_plot_descriptive_allcause_tpp
dev.off()

