# Program Information  ----------------------------------------------------

# Program:     055_standardisation 
# Author:      Anna Schultze 
# Description: calculate directly standardised mortality rates - separately for each age group over and below 80 
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
# formulas for CIs available in [enter reference]

standardise <- function(data, outcome) { 
  
    {{data}} %>% 
    left_join(european_standard, by = c("ageband_five")) %>% 
    # expected deaths is mortality rate times the standard groupsize 
    mutate(expected_deaths = value * groupsize) %>% 
    # sum by group 
    group_by(date, sex, over80, care_home_type) %>% 
    mutate(total_expected = sum(expected_deaths)) %>% 
    ungroup() %>%
    # the directly standardised rate is expected deaths over total standard population size 
    # calculate the SE around the dsri 
    mutate(dsr = total_expected/total, 
           se_dsri = groupsize^2*(value * (1- value)/population)) %>% 
    # sum standard error per category
    group_by(date, sex, over80, care_home_type) %>% 
    mutate(se_dsr = (sqrt(sum(se_dsri)))/total) %>% 
    ungroup() %>% 
    # calculate standard deviation (needed for calculating CI for ratio of DSRs)
    mutate(sdi = sqrt({{outcome}})/population, 
           sdiw_squared = ((sdi * (groupsize/total))^2)) %>% 
    group_by(date, sex, over80, care_home_type) %>% 
    mutate(sd_sum = sum(sdiw_squared), 
           sd = sqrt(sd_sum)) %>% 
    ungroup() %>% 
    mutate(log_sd = sd/dsr) %>% 
    # keep only one row per unique group 
    select(date, care_home_type, sex, over80, dsr, se_dsr, log_sd) %>% 
    distinct() %>% 
    # Finalise calculating and formatting confidence interval 
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
    select(c(care_home_group, Gender, over80, date, Standardised_Rate, Confidence_Interval)) %>% 
    # need to create a unique ID for reshaping the data
    group_by(care_home_group) %>% 
    mutate(id = row_number()) %>% 
    ungroup %>% 
    # reshape wide
    pivot_wider(
      id_cols = id, 
      names_from = care_home_group, 
      values_from = c(date, Gender, over80, Standardised_Rate, Confidence_Interval), 
      names_glue = "{care_home_group}_{.value}") %>% 
    # tidy up, remove unnecessary variables and sort by the grouping vars 
    rename(Date = Private_Home_date, 
           Gender = Private_Home_Gender, 
           over80 = Private_Home_over80) %>% 
    select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender, Care_or_Nursing_Home_over80)) %>% 
    select(Gender, over80, Date, (matches("Care*")), (matches("Priv*"))) %>% 
    arrange(Gender, over80, Date)

  }

# 3. Plot standardised rates 
# function to plot the standardised rates w. CIs. 

plot_standardised_rates <- function(data, titletext, sex, grouptext) {
  
  y_value <- (max({{data}}$dsr) + (max({{data}}$dsr)/4)) * 1000
  sexfilter <- enquo(sex)
  titlestring <- paste("Age-standardised", titletext, "Mortality by Age and Care Home", grouptext)

  {{data}} %>% 
    filter(if (!!sexfilter == "F") (sex == "F") else TRUE) %>% 
    filter(if (!!sexfilter == "M") (sex == "M") else TRUE) %>% 
    ggplot(aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = over80, shape = care_home_type, group = interaction(over80, care_home_type))) + 
    geom_line(size = 1) + geom_point() + 
    labs(x = "Time Period", 
         y = "Standardised Rate per 1,000 individuals", 
         title = titlestring,
         shape = "Carehome", 
         colour = "Over 80") + 
    scale_y_continuous(limits = c(0,y_value)) +
    scale_colour_manual(values = c("#FF934F", "#2E4052")) +
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
      values_from = c(date, sex, over80, dsr, log_sd), 
      names_glue = "{care_home_type}_{.value}") %>% 
    # calculate CI 
    mutate(cmr = Y_dsr/N_dsr, 
           sd_log_cmr = sqrt(Y_log_sd^2 + N_log_sd^2),
           ef_cmr = exp(1.96 * sd_log_cmr), 
           lcl_cmr = cmr/ef_cmr, 
           ucl_cmr = cmr*ef_cmr) %>% 
    rename(Date = Y_date, 
           Gender = Y_sex, 
           over80 = Y_over80) 
}

# 5. Format table of CMRs 
# function to format and output table of CMRs

format_cmr_table <- function(data) {
  
  {{data}} %>% 
    mutate(Confidence_Interval = paste(round(lcl_cmr,2), round(ucl_cmr,2), sep = "-"), 
           Comparative_Mortality_Rate = round(cmr,2)) %>% 
    select(Gender, over80, Date, Comparative_Mortality_Rate, Confidence_Interval) %>% 
    arrange(Gender, over80, Date)
}

# 6. Plot CMRs
# function to plot CMRs 

plot_cmrs <- function(data, titletext, sex, grouptext) { 
  
  y_value <- (max({{data}}$ucl_cmr) + (max({{data}}$ucl_cmr)/4)) 
  sexfilter <- enquo(sex)
  titlestring <- paste(titletext, "CMR by Agegroup", grouptext)
  
  {{data}} %>% 
    filter(if (!!sexfilter == "F") (Gender == "F") else TRUE) %>% 
    filter(if (!!sexfilter == "M") (Gender == "M") else TRUE) %>% 
    ggplot(aes (x = as.Date(Date, "%Y-%m-%d"), y = cmr, colour = over80, fill = over80)) + 
    geom_line(size = 1) + 
    geom_ribbon(aes(ymin=lcl_cmr, ymax=ucl_cmr), alpha = 0.1, colour = NA, show.legend = F) +
    labs(x = "Time Period", 
         y = "Ratio of Standardised Rates per 1,000 individuals", 
         title = titlestring,
         colour = "Over 80") + 
    scale_y_continuous(limits = c(0,y_value)) +
    scale_colour_manual(values = c("#FF934F", "#2E4052")) +
    scale_fill_manual(values = c("#FF934F", "#2E4052")) +
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
  # create young/old variable 
  mutate(over80 = case_when(
    AgeGroup == "65-69" ~ 0, 
    AgeGroup == "70-74" ~ 0, 
    AgeGroup == "75-79" ~ 0, 
    TRUE ~ 1)) %>% 
  # calculate total pop size within each ageband 
  group_by(over80) %>% 
  mutate(total = sum(EuropeanStandardPopulation)) %>% 
  ungroup() %>% 
  # rename the age band and group size variable for merging and ease of handling
  rename(ageband_five = AgeGroup, 
         groupsize = EuropeanStandardPopulation) %>% 
  # keep only relevant variables 
  select(ageband_five, groupsize, total)


# Data Management - TPP Data  ---------------------------------------------
# create smaller datasets, one for each age

allcause <- fread("./output/measure_allcause_death_sex_age_five.csv", data.table = FALSE, na.strings = "")
covid <- fread("./output/measure_covid_death_sex_age_five.csv", data.table = FALSE, na.strings = "")
noncovid <- fread("./output/measure_noncovid_death_sex_age_five.csv", data.table = FALSE, na.strings = "")

# drop empty covid rows  
covid <- covid %>% 
  filter(ymd(date) >= ymd("20200301"))

allcause <- allcause %>% 
  # create young/old variable 
  mutate(over80 = case_when(
    ageband_five == "65-69" ~ "No", 
    ageband_five == "70-74" ~ "No", 
    ageband_five == "75-79" ~ "No", 
    TRUE ~ "Yes")) 

covid <- covid %>% 
  # create young/old variable 
  mutate(over80 = case_when(
    ageband_five == "65-69" ~ "No", 
    ageband_five == "70-74" ~ "No", 
    ageband_five == "75-79" ~ "No", 
    TRUE ~ "Yes")) 

noncovid <- noncovid %>% 
  # create young/old variable 
  mutate(over80 = case_when(
    ageband_five == "65-69" ~ "No", 
    ageband_five == "70-74" ~ "No", 
    ageband_five == "75-79" ~ "No", 
    TRUE ~ "Yes")) 

# Calculate DSRs  ------------------------------------------------------------

all_cause_standard <- standardise(allcause, ons_any_death)
covid_standard <- standardise(covid, ons_covid_death)
noncovid_standard <- standardise(noncovid, ons_noncovid_death)

# DSR tables  ----------------------------------------------------------------

table_standardised_allcause_age <- format_standardised_table(all_cause_standard)
write.table(table_standardised_allcause_age, file = "./output/tables/7a_table_standardised_allcause_age.txt", sep = "\t", na = "", row.names=FALSE)

table_standardised_covid_age <- format_standardised_table(covid_standard)
write.table(table_standardised_covid_age, file = "./output/tables/7b_table_standardised_covid_age.txt", sep = "\t", na = "", row.names=FALSE)

table_standardised_noncovid_age <- format_standardised_table(noncovid_standard)
write.table(table_standardised_noncovid_age, file = "./output/tables/7c_table_standardised_noncovid_age.txt", sep = "\t", na = "", row.names=FALSE)

# DSR figures --------------------------------------------------------------

# All cause 
plot_standardised_allcause_age_m <- plot_standardised_rates(all_cause_standard, "All-Cause", "M","Among Men")
plot_standardised_allcause_age_f <- plot_standardised_rates(all_cause_standard, "All-Cause", "F","Among Women")

png(filename = "./output/plots/7a1_plot_standardised_allcause_age.png")
plot_standardised_allcause_age_m
dev.off()

png(filename = "./output/plots/7a2_plot_standardised_allcause_age.png")
plot_standardised_allcause_age_f
dev.off()

# Covid
plot_standardised_covid_age_m <- plot_standardised_rates(covid_standard, "Covid", "M","Among Men")
plot_standardised_covid_age_f <- plot_standardised_rates(covid_standard, "Covid", "F","Among Women")

png(filename = "./output/plots/7b1_plot_standardised_covid_age.png")
plot_standardised_covid_age_m
dev.off()

png(filename = "./output/plots/7b2_plot_standardised_covid_age.png")
plot_standardised_covid_age_f
dev.off()

# Non Covid
plot_standardised_noncovid_age_m <- plot_standardised_rates(noncovid_standard, "Non-Covid", "M","Among Men")
plot_standardised_noncovid_age_f <- plot_standardised_rates(noncovid_standard, "Non-Covid", "F","Among Women")

png(filename = "./output/plots/7c1_plot_standardised_noncovid_age.png")
plot_standardised_noncovid_age_m
dev.off()

png(filename = "./output/plots/7c2_plot_standardised_noncovid_age.png")
plot_standardised_noncovid_age_f
dev.off()

# Calculate CMRs ----------------------------------------------------------

all_cause_cmr <- calculate_cmr(all_cause_standard)
covid_cmr <- calculate_cmr(covid_standard)
noncovid_cmr <- calculate_cmr(noncovid_standard)

# CMR tables --------------------------------------------------------------

table_cmr_allcause_age <- format_cmr_table(all_cause_cmr)
write.table(table_cmr_allcause_age, file = "./output/tables/8a_table_cmr_allcause_age.txt", sep = "\t", na = "", row.names=FALSE)

table_cmr_covid_age <- format_cmr_table(covid_cmr)
write.table(table_cmr_covid_age, file = "./output/tables/8b_table_cmr_covid_age.txt", sep = "\t", na = "", row.names=FALSE)

table_cmr_noncovid_age <- format_cmr_table(noncovid_cmr)
write.table(table_cmr_noncovid_age, file = "./output/tables/8c_table_cmr_noncovid_age.txt", sep = "\t", na = "", row.names=FALSE)

# CMR figures -------------------------------------------------------------

# All cause 
plot_cmr_allcause_age_m <- plot_cmrs(all_cause_cmr, "All-Cause", "M", "Among Men")
plot_cmr_allcause_age_f <- plot_cmrs(all_cause_cmr, "All-Cause", "F",  "Among Women")

png(filename = "./output/plots/8a1_plot_cmr_allcause_age.png")
plot_cmr_allcause_age_m
dev.off()

png(filename = "./output/plots/8a2_plot_cmr_allcause_age.png")
plot_cmr_allcause_age_f
dev.off()

# Covid
plot_cmr_covid_age_m <- plot_cmrs(covid_cmr,"Covid", "M", "Among Men")
plot_cmr_covid_age_f <- plot_cmrs(covid_cmr,"Covid", "F", "Among Women")

png(filename = "./output/plots/8b1_plot_cmr_covid_age.png")
plot_cmr_covid_age_m
dev.off()

png(filename = "./output/plots/8b2_plot_cmr_covid_age.png")
plot_cmr_covid_age_f
dev.off()

# Non Covid
plot_cmr_noncovid_age_m <- plot_cmrs(noncovid_cmr, "Non-Covid", "M", "Among Men")
plot_cmr_noncovid_age_f <- plot_cmrs(noncovid_cmr, "Non-Covid", "F", "Among Women")

png(filename = "./output/plots/8c1_plot_cmr_noncovid_age.png")
plot_cmr_noncovid_age_m
dev.off()

png(filename = "./output/plots/8c2_plot_cmr_noncovid_age.png")
plot_cmr_noncovid_age_f
dev.off()
