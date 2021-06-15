# Program Information  ----------------------------------------------------

# Program:     050_standardisation 
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
# formulas for CIs available in Kirkwood and Sterne (DSR, p266) vs Clayton and Hills (CMF, p138-139) and are copied into the below issue 
# REVIEWER: PLEASE CHECK THESE HAVE BEEN ACCURATELY CODED AS THIS HAS BEEN DONE MANUALLY 
# https://github.com/opensafely/carehome-noncarehome-death-research/issues/42 

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
    # calculate the SE around the dsr for each row (dsri)
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
    # the variable is called log because it calcluates the SD for the SMR on a log scale, it's not meant to be logged
    mutate(log_sd = sd/dsr) %>% 
    # keep only one row per unique group 
    select(date, care_home_type, sex, dsr, se_dsr, log_sd) %>% 
    distinct() %>% 
    # Finalise calculating and formatting confidence interval around the dsr 
    mutate(lcl = dsr - 1.96 * se_dsr, 
           ucl = dsr + 1.96 * se_dsr, 
           Confidence_Interval = paste(round(lcl*1000,2), round(ucl*1000,2), sep = "-"),
           Standardised_Rate = round(dsr * 1000,2)) 
}

# 2. Format table of standardised rates
# function to format table of the DSRs to output, by gender 

format_standardised_table <- function(data) { 
  
  {{data}} %>% 
    # create a labelled variable for outputting in table formats 
    mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
    # rename and select what to present in tables 
    rename(Gender = sex) %>% 
    select(c(care_home_group, Gender, date, Standardised_Rate, Confidence_Interval)) %>% 
    # need to create a unique ID for reshaping the data
    group_by(care_home_group) %>% 
    mutate(id = row_number()) %>% 
    ungroup %>% 
    # reshape wide
    pivot_wider(
      id_cols = id, 
      names_from = care_home_group, 
      values_from = c(date, Gender, Standardised_Rate, Confidence_Interval), 
      names_glue = "{care_home_group}_{.value}") %>% 
    # tidy up, remove unnecessary variables and sort by the grouping vars 
    rename(Date = Private_Home_date) %>% 
    rename(Gender = Private_Home_Gender) %>% 
    select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) %>% 
    select(Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
    arrange(Gender, Date)

  }

# 3. Plot standardised rates 
# function to plot the standardised rates w. CIs. 

plot_standardised_rates <- function(data, titletext) {
  
  y_value <- (max({{data}}$dsr) + (max({{data}}$dsr)/4)) * 1000
  titlestring <- paste("Age-standardised", titletext, "Mortality by Sex and Care Home")
  
  plot_8a <- ggplot({{data}}, aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = sex, linetype = care_home_type, group = interaction(sex, care_home_type))) + 
    geom_line(size = 1) + geom_point() + 
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-02-01"),y=+Inf,label="Wave 1",vjust=2,geom="label") +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-09-01"),y=+Inf,label="Wave 2",vjust=2,geom="label") +
    labs(x = "Time Period", 
         y = "Standardised Rate per 1,000 individuals", 
         title = titlestring,
         linetype = "Care Home", 
         colour = "Gender") + 
    scale_y_continuous(limits = c(0,100)) +
    scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "gray"), 
          panel.grid.major.y = element_line(color = "gainsboro"))
  
}

# 4. Calculate comparative mortality figures 
# function to calculat CMRs

calculate_cmr <- function(data) {
  
  {{data}} %>% 
    # make wide on care home status, which are the dsrs to be compared 
    group_by(care_home_type) %>% 
    mutate(id = row_number()) %>% 
    ungroup %>% 
    # reshape wide 
    pivot_wider(
      id_cols = id, 
      names_from = care_home_type, 
      values_from = c(date, sex, dsr, log_sd), 
      names_glue = "{care_home_type}_{.value}") %>% 
    # calculate CI 
    mutate(cmr = Y_dsr/N_dsr, 
           sd_log_cmr = sqrt(Y_log_sd^2 + N_log_sd^2),
           ef_cmr = exp(1.96 * sd_log_cmr), 
           lcl_cmr = cmr/ef_cmr, 
           ucl_cmr = cmr*ef_cmr) 
}
 
# 5. Format table of CMRs 
# function to format and output table of CMRs

format_cmr_table <- function(data) {
  
  {{data}} %>% 
    rename(Date = Y_date, 
           Gender = Y_sex) %>% 
    mutate(Confidence_Interval = paste(round(lcl_cmr,2), round(ucl_cmr,2), sep = "-"), 
           Comparative_Mortality_Rate = round(cmr,2)) %>% 
    select(Gender, Date, Comparative_Mortality_Rate, Confidence_Interval) %>% 
    arrange(Gender, Date)
}

# 6. Plot CMRs
# function to plot CMRs 

plot_cmrs <- function(data, titletext) { 
  
  y_value <- (max({{data}}$ucl_cmr) + (max({{data}}$ucl_cmr)/4)) 
  titlestring <- paste("Age-standardised", titletext, "CMR by Sex and Care Home")
  
  ggplot({{data}}, aes (x = as.Date(Y_date, "%Y-%m-%d"), y = cmr, colour = Y_sex, fill = Y_sex)) + 
    geom_line(size = 1) + 
    geom_ribbon(aes(ymin=lcl_cmr, ymax=ucl_cmr), alpha = 0.1, colour = NA, show.legend = F) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-02-01"),y=+Inf,label="Wave 1",vjust=1, size = 3, geom="label") +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-09-01"),y=+Inf,label="Wave 2",vjust=1, size = 3, geom="label") +
    labs(x = "Time Period", 
         y = "Ratio of Standardised Rates per 1,000 individuals", 
         title = titlestring,
         colour = "Gender") + 
    scale_y_continuous(trans = 'log10', limits = c(1,60)) +
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

# opensafely population 
allcause <- fread("./output/measure_allcause_death_sex_age_five.csv", data.table = FALSE, na.strings = "")
covid <- fread("./output/measure_covid_death_sex_age_five.csv", data.table = FALSE, na.strings = "")
noncovid <- fread("./output/measure_noncovid_death_sex_age_five.csv", data.table = FALSE, na.strings = "")

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

table_standardised_allcause <- format_standardised_table(all_cause_standard)
write.table(table_standardised_allcause, file = "./output/tables/5a_table_standardised_allcause.txt", sep = "\t", na = "", row.names=FALSE)

table_standardised_covid <- format_standardised_table(covid_standard)
write.table(table_standardised_covid, file = "./output/tables/5b_table_standardised_covid.txt", sep = "\t", na = "", row.names=FALSE)

table_standardised_noncovid <- format_standardised_table(noncovid_standard)
write.table(table_standardised_noncovid, file = "./output/tables/5c_table_standardised_noncovid.txt", sep = "\t", na = "", row.names=FALSE)

# DSR figures --------------------------------------------------------------

plot_standardised_allcause <- plot_standardised_rates(all_cause_standard, "All-Cause")

png(filename = "./output/plots/5a_plot_standardised_allcause.png")
plot_standardised_allcause
dev.off()

plot_standardised_covid <- plot_standardised_rates(covid_standard, "Covid")

png(filename = "./output/plots/5b_plot_standardised_covid.png")
plot_standardised_covid
dev.off()

plot_standardised_noncovid <- plot_standardised_rates(noncovid_standard, "Non-Covid")

png(filename = "./output/plots/5c_plot_standardised_noncovid.png")
plot_standardised_noncovid
dev.off()

# Calculate CMRs ----------------------------------------------------------

all_cause_cmr <- calculate_cmr(all_cause_standard)
covid_cmr <- calculate_cmr(covid_standard)
noncovid_cmr <- calculate_cmr(noncovid_standard)

# CMR tables --------------------------------------------------------------

table_cmr_allcause <- format_cmr_table(all_cause_cmr)
write.table(table_cmr_allcause, file = "./output/tables/6a_table_cmr_allcause.txt", sep = "\t", na = "", row.names=FALSE)

table_cmr_covid <- format_cmr_table(covid_cmr)
write.table(table_cmr_covid, file = "./output/tables/6b_table_cmr_covid.txt", sep = "\t", na = "", row.names=FALSE)

table_cmr_noncovid <- format_cmr_table(noncovid_cmr)
write.table(table_cmr_noncovid, file = "./output/tables/6c_table_cmr_noncovid.txt", sep = "\t", na = "", row.names=FALSE)

# CMR figures -------------------------------------------------------------

plot_cmr_allcause <- plot_cmrs(all_cause_cmr, "All-Cause")

png(filename = "./output/plots/6a_plot_cmr_allcause.png")
plot_cmr_allcause
dev.off()

plot_cmr_covid <- plot_cmrs(covid_cmr, "Covid")

png(filename = "./output/plots/6b_plot_cmr_covid.png")
plot_cmr_covid
dev.off()

plot_cmr_noncovid <- plot_cmrs(noncovid_cmr, "Non-Covid")

png(filename = "./output/plots/6c_plot_cmr_noncovid.png")
plot_cmr_noncovid
dev.off()
