# Program Information  ----------------------------------------------------

# Program:     030_descriptive_mortality_rates
# Author:      Anna Schultze 
# Description: Edit the crude mortality rates already calculated by the measures framework 
#              Add confidence intervals, format the table, and plot it as a line plot (accounting for missing values)
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
    mutate(care_home_group = ifelse((care_home_type == "Yes"), "Care_or_Nursing_Home", "Private_Home")) %>%
    # scale mortality risks and confidence intervals 
    mutate(PointEst = PointEst/(days_in_month(as.Date(date, "%Y-%m-%d")))*30, 
           Lower = Lower/(days_in_month(as.Date(date, "%Y-%m-%d")))*30, 
           Upper = Upper/(days_in_month(as.Date(date, "%Y-%m-%d")))*30) %>% 
    # rename key variables to how you want them displayed in tables and select the relevant ones
    mutate(Mortality_Rate = round((PointEst*1000),2), 
           Confidence_Interval = paste(round(Lower*1000,2), round(Upper*1000,2), sep = "-")) %>% 
    rename(n = {{outcome}}, 
           N = registered_at_start, 
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
  ystring <- paste(as_label(enquo(axistext)), "Risk per 1,000 individuals")
  titlestring <- paste(as_label(enquo(axistext)), "Risks by Age, crude")
  
  ggplot({{data}}, aes (x = as.Date(date, "%Y-%m-%d"), y = ((value/(days_in_month(as.Date(date, "%Y-%m-%d")))*30)*1000), colour = ageband_narrow, linetype = care_home_type, group = interaction(ageband_narrow, care_home_type))) + 
    geom_line(size = 1) + geom_point() + 
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-02-01"),y=+Inf,label="Wave 1",vjust=1, geom="label", size = 3) +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-09-01"),y=+Inf,label="Wave 2",vjust=1, geom="label", size = 3) +
    labs(x = "Calendar Month", 
         y = ystring, 
         title = titlestring, 
         linetype = "Care Home", 
         colour = "Age") + 
    scale_x_date(date_labels = "%B %y", date_breaks = "2 months") +
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

measure_tested_covid_age <- fread("./output/measure_tested_covid_age.csv", data.table = FALSE, na.strings = "")
measure_admitted_covid_age <- fread("./output/measure_admitted_covid_age.csv", data.table = FALSE, na.strings = "")
measure_admitted_any_age <- fread("./output/measure_admitted_any_age.csv", data.table = FALSE, na.strings = "")
measure_admitted_noncovid_age <- fread("./output/measure_admitted_noncovid_age.csv", data.table = FALSE, na.strings = "")

# Confidence Intervals ----------------------------------------------------
# calculate confidence intervals using binconf function from Hmisc 
# bind output into the original dataset 

measure_tested_covid_age <- as_tibble(cbind(measure_tested_covid_age,((binconf(measure_tested_covid_age$tested_covid, measure_tested_covid_age$registered_at_start, alpha = 0.05, method = "wilson")))))
measure_admitted_covid_age <- as_tibble(cbind(measure_admitted_covid_age,((binconf(measure_admitted_covid_age$admitted_covid, measure_admitted_covid_age$registered_at_start, alpha = 0.05, method = "wilson")))))
measure_admitted_any_age <- as_tibble(cbind(measure_admitted_any_age,((binconf(measure_admitted_any_age$admitted_any, measure_admitted_any_age$registered_at_start, alpha = 0.05, method = "wilson")))))
measure_admitted_noncovid_age <- as_tibble(cbind(measure_admitted_noncovid_age,((binconf(measure_admitted_noncovid_age$admitted_noncovid, measure_admitted_noncovid_age$registered_at_start, alpha = 0.05, method = "wilson")))))

# Set rows with < 5 events to NA ----------------------------------------
# removing events, percentage and CIs 

measure_tested_covid_age <- measure_tested_covid_age %>% 
  mutate(value = ifelse(tested_covid <= 5, NA, value), 
         tested_covid = ifelse(tested_covid <= 5, NA, tested_covid), 
         PointEst = ifelse(tested_covid <= 5, NA, PointEst), 
         Lower = ifelse(tested_covid <= 5, NA, Lower), 
         Upper = ifelse(tested_covid <= 5, NA, Upper), 
         ) 

measure_admitted_covid_age <- measure_admitted_covid_age %>% 
  mutate(value = ifelse(admitted_covid <= 5, NA, value), 
         admitted_covid = ifelse(admitted_covid <= 5, NA, admitted_covid), 
         PointEst = ifelse(admitted_covid <= 5, NA, PointEst),
         Lower = ifelse(admitted_covid <= 5, NA, Lower),
         Upper = ifelse(admitted_covid <= 5, NA, Upper)) 

measure_admitted_any_age <- measure_admitted_any_age %>% 
  mutate(value = ifelse(admitted_any <= 5, NA, value), 
         admitted_any = ifelse(admitted_any <= 5, NA, admitted_any), 
         PointEst = ifelse(admitted_any <= 5, NA, PointEst),          
         Lower = ifelse(admitted_any <= 5, NA, Lower),
         Upper = ifelse(admitted_any <= 5, NA, Upper)) 

measure_admitted_noncovid_age <- measure_admitted_noncovid_age %>% 
  mutate(value = ifelse(admitted_noncovid <= 5, NA, value), 
         admitted_noncovid = ifelse(admitted_noncovid <= 5, NA, admitted_noncovid), 
         PointEst = ifelse(admitted_noncovid <= 5, NA, PointEst),          
         Lower = ifelse(admitted_noncovid <= 5, NA, Lower),
         Upper = ifelse(admitted_noncovid <= 5, NA, Upper)) 
# Tables ------------------------------------------------------------------

table_descriptive_tested <- format_table(measure_tested_covid_age, tested_covid)
write.table(table_descriptive_tested, file = "./output/tables/S3a_table_descriptive_tested.txt", sep = "\t", na = "", row.names=FALSE)

table_descriptive_admitted_covid <- format_table(measure_admitted_covid_age, admitted_covid)
write.table(table_descriptive_admitted_covid, file = "./output/tables/S3b_table_descriptive_admitted_covid.txt", sep = "\t", na = "", row.names=FALSE)

table_descriptive_admitted_any <- format_table(measure_admitted_any_age, admitted_any)
write.table(table_descriptive_admitted_any, file = "./output/tables/S3c_table_descriptive_admitted_any.txt", sep = "\t", na = "", row.names=FALSE)

table_descriptive_admitted_noncovid <- format_table(measure_admitted_noncovid_age, admitted_noncovid)
write.table(table_descriptive_admitted_noncovid, file = "./output/tables/S3d_table_descriptive_admitted_noncovid.txt", sep = "\t", na = "", row.names=FALSE)

# Figures  -------------------------------------------------------------

# Test
plot_descriptive_tested <- plot_figure(measure_tested_covid_age, Testing)

png(filename = "./output/plots/S3a_plot_descriptive_tested.png")
plot_descriptive_tested
dev.off()

# COVID admission 
plot_descriptive_admitted_covid <- plot_figure(measure_admitted_covid_age, Covid_Admission)

png(filename = "./output/plots/S3b_plot_descriptive_admitted_covid.png")
plot_descriptive_admitted_covid
dev.off()

# Any Admission  
plot_descriptive_admitted_any <- plot_figure(measure_admitted_any_age, Admission)

png(filename = "./output/plots/S3c_plot_descriptive_admitted_any.png")
plot_descriptive_admitted_any
dev.off()

# NonCOVID Admission  
plot_descriptive_admitted_noncovid <- plot_figure(measure_admitted_noncovid_age, NonCovid_Admission)

png(filename = "./output/plots/S3d_plot_descriptive_admitted_noncovid.png")
plot_descriptive_admitted_noncovid
dev.off()

