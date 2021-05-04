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

# create output folders if they do not exist (if exist, will throw warning which is suppressed)

dir.create(file.path("./output/tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./output/plots"), showWarnings = FALSE, recursive = TRUE)

# Functions  --------------------------------------------------------------

# 1. Function to format table for comparative estimates 

format_comparative_table <- function(data, outcome) { 
  
  {{data}} %>% # Nice - didn't know you could do this with curly brackets!
    mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
    # rename variabels to easier names 
    rename(n = {{outcome}}, 
           N = registered_at_start, 
           Age = ageband_narrow) %>% 
    mutate(Mortality_Rate = round((value*1000),2)) %>% 
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
    # select variables to present in tables 
    rename(Date = Private_Home_date, 
           Age = Private_Home_Age) %>% 
    select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) 
}

# 2. Function to calculate comparative measures 

calculate_measures <- function(data) { 
  
  {{data}} %>%
  mutate(rr = (Care_or_Nursing_Home_value/Private_Home_value), 
         rd = (Care_or_Nursing_Home_value - Private_Home_value), 
         Relative_Risk = round(rr,2), 
         Risk_Difference = round(rd*1000,2)) %>% 
    # calculate confidence intervals for relative risk (Kirkwood and Sterne, p156)
    mutate(se_log_rr = sqrt((1/Care_or_Nursing_Home_n) - (1/Care_or_Nursing_Home_N) + (1/Private_Home_n) - (1/Private_Home_N)), 
           ef = exp(1.96 * se_log_rr), 
           rr_lcl = rr/ef, 
           rr_ucl = rr*ef) %>% 
    # calculate confidence interval for risk difference (Kirkwood and Sterne, p152)
    mutate(se_rd = sqrt((Care_or_Nursing_Home_value*(1-Care_or_Nursing_Home_value)/Care_or_Nursing_Home_N))+(Private_Home_value*(1-Private_Home_value)/Private_Home_N), 
           rd_lcl = rd - 1.96*se_rd, 
           rd_ucl = rd + 1.96*se_rd, 
           Relative_Risk_CI = paste(round(rr_lcl,2), round(rr_ucl,2), sep = "-"), 
           Risk_Difference_CI = paste(round(rd_lcl*1000,2), round(rd_ucl*1000,2), sep = "-"))
}

# 3. Function to plot comparative measures 

plot_comparative_figure <- function(data, axistext) {
  
  y_value <- (max({{data}}$Relative_Risk) + (max({{data}}$Relative_Risk)/4)) 
  titlestring <- paste(as_label(enquo(axistext)), "mortality relative risk, crude")
  
  ggplot({{data}}, aes (x = as.Date(Date, "%Y-%m-%d"), y = Relative_Risk, group = Age, colour = Age)) + # Rogue comma at the end of aes() here?
    geom_line(size =1) +
    labs(x = "Time Period", 
         y = "Relative Risk (care homes vs. private homes)", 
         title = titlestring) + 
    scale_y_continuous(trans = "log10", limits = c(1,40)) +
    scale_colour_viridis_d() + 
    scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "gray"), 
          panel.grid.major.y = element_line(color = "gainsboro")) 
}

# Read in Data ------------------------------------------------------------

measure_any_age <- fread("./output/measure_allcause_death_age.csv", data.table = FALSE, na.strings = "")
measure_covid_age <- fread("./output/measure_covid_death_age.csv", data.table = FALSE, na.strings = "")
measure_noncovid_age <- fread("./output/measure_noncovid_death_age.csv", data.table = FALSE, na.strings = "")

# Remove empty COVID rows--------------------------------------------------

# Not sure I follow why these rows are "empty"? Is this because 2020-03-01 is the first time covid is recorded, so any rows prior don't have covid cases?
# No problem for functionality just my understanding
measure_covid_age <- measure_covid_age %>% filter(ymd(date) >= ymd("20200301"))

# Set rows with < 5 events to NA ----------------------------------------
# removing events and percentages 

measure_any_age <- measure_any_age %>% 
  mutate(value = ifelse(ons_any_death <= 5, NA, value), 
         ons_any_death = ifelse(ons_any_death <= 5, NA, ons_any_death)) 

measure_covid_age <- measure_covid_age %>% 
  mutate(value = ifelse(ons_covid_death <= 5, NA, value), 
         ons_covid_death = ifelse(ons_covid_death <= 5, NA, ons_covid_death))

measure_noncovid_age <- measure_noncovid_age %>% 
  mutate(value = ifelse(ons_noncovid_death <= 5, NA, value), 
         ons_noncovid_death = ifelse(ons_noncovid_death <= 5, NA, ons_noncovid_death)) 

# Tables ------------------------------------------------------------------

##-- All cause 

comparative_allcause <- format_comparative_table(measure_any_age, ons_any_death)
comparative_allcause <- calculate_measures(comparative_allcause) 
# Latter doesn't work on dummy data as end up with -ve RR and RD variance - "value" supposed to be a proportion but gt 1?
# Maybe not an issue with real data though?

table_comparative_allcause <- comparative_allcause %>% 
  select(Date, Age, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Date)

write.table(table_comparative_allcause, file = "./output/tables/3a_table_comparative_allcause.txt", sep = "\t", na = "", row.names=FALSE)

##-- Covid 
comparative_covid <- format_comparative_table(measure_covid_age, ons_covid_death)
comparative_covid <- calculate_measures(comparative_covid)

table_comparative_covid <- comparative_covid %>% 
  select(Date, Age, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Date)

write.table(table_comparative_covid, file = "./output/tables/3b_table_comparative_covid.txt", sep = "\t", na = "", row.names=FALSE)

##-- Non Covid 
comparative_noncovid <- format_comparative_table(measure_noncovid_age, ons_noncovid_death)
comparative_noncovid <- calculate_measures(comparative_noncovid)

table_comparative_noncovid <- comparative_noncovid %>% 
  select(Date, Age, Care_or_Nursing_Home_Mortality_Rate, Private_Home_Mortality_Rate, Relative_Risk, Relative_Risk_CI, Risk_Difference, Risk_Difference_CI) %>% 
  arrange(Age, Date)

write.table(table_comparative_noncovid, file = "./output/tables/3c_table_comparative_noncovid.txt", sep = "\t", na = "", row.names=FALSE)

# Figures  ----------------------------------------------------------------

##-- All cause 

plot_comparative_allcause <- plot_comparative_figure(comparative_allcause, Allcause)

png(filename = "./output/plots/3a_plot_comparative_allcause.png")
plot_comparative_allcause
dev.off()

##-- Covid

plot_comparative_covid <- plot_comparative_figure(comparative_covid, Covid)

png(filename = "./output/plots/3b_plot_comparative_covid.png")
plot_comparative_covid
dev.off()

##-- Noncovid 

plot_comparative_noncovid <- plot_comparative_figure(comparative_noncovid, Noncovid)

png(filename = "./output/plots/3c_plot_comparative_noncovid.png")
plot_comparative_noncovid
dev.off()
