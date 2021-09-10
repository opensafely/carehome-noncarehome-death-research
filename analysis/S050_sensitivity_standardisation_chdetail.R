# Program Information  ----------------------------------------------------

# Program:     S050_sensitivity_standardisation_chdetail
# Author:      Anna Schultze 
# Description: calculate directly standardised mortality rates 
# Input:       measure_[outcome]_[group].csv
# Output:      tables into analysis/outfiles as per project.yaml 
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
    # remove care or nursing as expected small numbers
    filter(care_home_detail != "Care_Or_Nursing") %>% 
    # expected deaths is mortality rate times the standard groupsize 
    mutate(expected_deaths = value * groupsize) %>% 
    # sum by group 
    group_by(date, sex, care_home_detail) %>% 
    mutate(total_expected = sum(expected_deaths)) %>% 
    ungroup() %>%
    # the directly standardised rate is expected deaths over total standard population size 
    # calculate the SE around the dsri 
    mutate(dsr = total_expected/total, 
           se_dsri = (groupsize^2*value * (1- value))/registered_at_start) %>% 
    # sum standard error per category
    group_by(date, sex, care_home_detail) %>% 
    mutate(se_dsr = (sqrt(sum(se_dsri)))/total) %>% 
    ungroup() %>% 
    # calculate standard deviation (needed for calculating CI for ratio of DSRs)
    mutate(sdi = sqrt({{outcome}})/registered_at_start, 
           sdiw_squared = ((sdi * (groupsize/total))^2)) %>% 
    group_by(date, sex, care_home_detail) %>% 
    mutate(sd_sum = sum(sdiw_squared), 
           sd = sqrt(sd_sum)) %>% 
    ungroup() %>% 
    mutate(log_sd = sd/dsr) %>% 
    # keep only one row per unique group 
    select(date, care_home_detail, sex, dsr, se_dsr, log_sd) %>% 
    distinct() %>% 
    # Finalise calculating and formatting confidence interval 
    mutate(lcl = dsr - 1.96 * se_dsr, 
           ucl = dsr + 1.96 * se_dsr) %>% 
  # scale mortality risks and confidence intervals 
  mutate(dsr = dsr/(days_in_month(as.Date(date, "%Y-%m-%d")))*30, 
         lcl = lcl/(days_in_month(as.Date(date, "%Y-%m-%d")))*30, 
         ucl = ucl/(days_in_month(as.Date(date, "%Y-%m-%d")))*30) %>% 
    # format CI and names 
    mutate(Confidence_Interval = paste(round(lcl*1000,2), round(ucl*1000,2), sep = "-"),
           Standardised_Rate = round(dsr * 1000,2))
}

# 2. Format table of standardised rates
# function to format table of the DSRs to output, by gender 

format_standardised_table <- function(data) { 
  
  {{data}} %>% 
    # create a labelled variable for outputting in table formats 
    # rename and select what to present in tables
    select(c(care_home_detail, sex, date, Standardised_Rate, Confidence_Interval)) %>% 
    # need to create a unique ID for reshaping the data
    group_by(care_home_detail) %>% 
    mutate(id = row_number()) %>% 
    ungroup %>% 
    # reshape wide
    pivot_wider(
      id_cols = id, 
      names_from = care_home_detail, 
      values_from = c(date, sex, Standardised_Rate, Confidence_Interval), 
      names_glue = "{care_home_detail}_{.value}") %>% 
    # tidy up, remove unnecessary variables and sort by the grouping vars 
    rename(Date = Care_Home_date, 
           Gender = Care_Home_sex) %>% 
    select(Gender, Date, matches("Standardised*"), matches("Confidence*")) %>% 
    select(Gender, Date, (matches("Care_Home*")), (matches("Nursing_Home*")), (matches("Care_Or*")), (matches("Private_Home*"))) %>% 
    arrange(Gender, Date)

  }

# 3. Plot standardised rates 
# function to plot the standardised rates w. CIs. 

plot_standardised_rates <- function(data, titletext, sex, grouptext) {
  
  y_value <- (max({{data}}$dsr) + (max({{data}}$dsr)/4)) * 1000
  sexfilter <- enquo(sex)
  titlestring <- paste("Age-standardised", titletext, "Mortality", grouptext)
  
  {{data}} %>% 
    filter(if (!!sexfilter == "F") (sex == "F") else TRUE) %>% 
    filter(if (!!sexfilter == "M") (sex == "M") else TRUE) %>% 
    ggplot(aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = care_home_detail)) + 
    geom_line(size = 1) + geom_point() + 
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-02-01"),y=+Inf,label="Wave 1",vjust=2,geom="label") +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-09-01"),y=+Inf,label="Wave 2",vjust=2,geom="label") +
    labs(x = "Calendar Month", 
         y = "Standardised Risk per 1,000 individuals", 
         title = titlestring,
         colour = "Carehome Type") + 
    scale_y_continuous(limits = c(0,150)) +
    scale_color_viridis_d(option = "plasma") +
    scale_x_date(date_labels = "%B %y", date_breaks = "2 months") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "gray"), 
          panel.grid.major.y = element_line(color = "gainsboro"))
  
}


# Read in Data ------------------------------------------------------------

# opensafely population 
allcause <- fread("./output/measure_allcause_death_age_chdetail.csv", data.table = FALSE, na.strings = "")
covid <- fread("./output/measure_covid_death_age_chdetail.csv", data.table = FALSE, na.strings = "")
noncovid <- fread("./output/measure_noncovid_death_age_chdetail.csv", data.table = FALSE, na.strings = "")

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

all_cause_standard <- standardise(allcause, ons_any_death)
covid_standard <- standardise(covid, ons_covid_death)
noncovid_standard <- standardise(noncovid, ons_noncovid_death)

# DSR tables  ----------------------------------------------------------------

S_table_standardised_allcause_chdetail <- format_standardised_table(all_cause_standard)
write.table(S_table_standardised_allcause_chdetail, file = "./output/tables/S2a_table_standardised_allcause_chdetail.txt", sep = "\t", na = "", row.names=FALSE)

S_table_standardised_covid_chdetail <- format_standardised_table(covid_standard)
write.table(S_table_standardised_covid_chdetail, file = "./output/tables/S2b_table_standardised_covid_chdetail.txt", sep = "\t", na = "", row.names=FALSE)

S_table_standardised_noncovid_chdetail <- format_standardised_table(noncovid_standard)
write.table(S_table_standardised_noncovid_chdetail, file = "./output/tables/S2c_table_standardised_noncovid_chdetail.txt", sep = "\t", na = "", row.names=FALSE)

# DSR figures --------------------------------------------------------------

# All cause 
S_plot_standardised_allcause_chdetail_m <- plot_standardised_rates(all_cause_standard, "All-Cause", "M", "Among Men")
S_plot_standardised_allcause_chdetail_f <- plot_standardised_rates(all_cause_standard, "All-Cause", "F", "Among Women")

png(filename = "./output/plots/S2a1_plot_standardised_allcause_chdetail_m.png")
S_plot_standardised_allcause_chdetail_m
dev.off()

png(filename = "./output/plots/S2a2_plot_standardised_allcause_chdetail_f.png")
S_plot_standardised_allcause_chdetail_f
dev.off()

# Covid
S_plot_standardised_covid_chdetail_m <- plot_standardised_rates(covid_standard, "Covid", "M", "Among Men")
S_plot_standardised_covid_chdetail_f <- plot_standardised_rates(covid_standard, "Covid", "F", "Among Women")

png(filename = "./output/plots/S2b1_plot_standardised_covid_chdetail_m.png")
S_plot_standardised_covid_chdetail_m
dev.off()

png(filename = "./output/plots/S2b2_plot_standardised_covid_chdetail_f.png")
S_plot_standardised_covid_chdetail_f
dev.off()

# Noncovid
S_plot_standardised_noncovid_chdetail_m <- plot_standardised_rates(noncovid_standard, "Covid", "M", "Among Men")
S_plot_standardised_noncovid_chdetail_f <- plot_standardised_rates(noncovid_standard, "Covid", "F", "Among Women")

png(filename = "./output/plots/S2c1_plot_standardised_noncovid_chdetail_m.png")
S_plot_standardised_noncovid_chdetail_m
dev.off()

png(filename = "./output/plots/S2c2_plot_standardised_noncovid_chdetail_f.png")
S_plot_standardised_noncovid_chdetail_f
dev.off()
