# Program Information  ----------------------------------------------------

# Program:     050_standardisation 
# Author:      Anna Schultze 
# Description: calculate standardised mortality rates 
# Input:       measure_[outcome]_[group].csv
# Output:      tables into analysis/outfiles as per project.yaml 
# Edits:      

# Housekeeping  -----------------------------------------------------------

# load packages 
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)
library(Hmisc)

# make sure my favoured output folder exists

mainDir <- getwd() 
subDir <- "./analysis/outfiles"

if (file.exists(subDir)){
  print("Out directory exists")
} else {
  dir.create(file.path(mainDir, subDir))
  print("Out directory didn't exist, but I created it")
}

# Read in Data ------------------------------------------------------------

# all-cause death 
allcause <- fread("./output/measure_allcause_death_sex_age_five.csv", data.table = FALSE, na.strings = "")

# covid death 
covid <- fread("./output/measure_covid_death_sex_age_five.csv", data.table = FALSE, na.strings = "")

# non covid death 
noncovid <- fread("./output/measure_noncovid_death_sex_age_five.csv", data.table = FALSE, na.strings = "")

# standard population 
european_standard <- fread("./data/european_standard_population.csv", data.table = FALSE, na.strings = "")


# Standardisation  --------------------------------------------------

##-- Format standard population 

european_standard <- european_standard %>% 
  # remove redundant age groups 
  filter(AgeGroup != "0-4") %>% 
  filter(AgeGroup != "5-9") %>% 
  filter(AgeGroup != "10-14") %>% 
  filter(AgeGroup != "15-19") %>% 
  filter(AgeGroup != "20-24") %>% 
  filter(AgeGroup != "25-29") %>% 
  filter(AgeGroup != "30-34") %>% 
  filter(AgeGroup != "35-39") %>% 
  filter(AgeGroup != "40-44") %>% 
  filter(AgeGroup != "45-49") %>% 
  filter(AgeGroup != "50-54") %>% 
  filter(AgeGroup != "55-59") %>% 
  filter(AgeGroup != "60-64") %>% 
  # calculate total pop size 
  mutate(total = sum(EuropeanStandardPopulation)) %>% 
  # rename the age band for merging 
  rename(ageband_five = AgeGroup) %>% 
  rename(groupsize = EuropeanStandardPopulation) %>% 
  # keep only relevant variables 
  select(ageband_five, groupsize, total)

##-- all cause mortality 

# Merge with standard population on ageband  
all_cause_standard <- allcause %>% 
  left_join(european_standard, by = c("ageband_five")) 

# Calculate standardised rates by sex and care home 
all_cause_standard <- all_cause_standard %>% 
  # expected deaths is mortality rate times the standard groupsize 
  mutate(expected_deaths = value * groupsize) %>% 
  # sum by group 
  group_by(date, sex, care_home_type) %>% 
  mutate(total_expected = sum(expected_deaths)) %>% 
  # standardised rates expected deaths over total, 
  mutate(dsr = total_expected/total) %>% 
  ungroup() %>% 
  # calculate standard error 
  mutate(se_dsri = groupsize^2*(value * (1- value)/population)) %>% 
  # sum standard error per category
  group_by(date, sex, care_home_type) %>% 
  mutate(se_dsr = (sqrt(sum(se_dsri)))/total) %>% 
  ungroup() %>% 
  # keep only one row per unique group 
  select(date, care_home_type, sex, dsr, se_dsr) %>% 
  distinct() %>% 
  # confidence interval 
  mutate(lcl = dsr - 1.96 * se_dsr, 
         ucl = dsr + 1.96 * se_dsr, 
         Confidence_Interval = paste(round(lcl*1000,2), round(ucl*1000,2), sep = "-"),
         Standardised_Rate = round(dsr * 1000,2)) 

# covid 

# merge with standard population  
covid_standard <- covid %>% 
  left_join(european_standard, by = c("ageband_five")) 

# calculate standardised rates by sex and care home 
covid_standard <- covid_standard %>% 
  # expected deaths is mortality rate times the standard groupsize 
  mutate(expected_deaths = value * groupsize) %>% 
  # sum by group 
  group_by(date, sex, care_home_type) %>% 
  mutate(total_expected = sum(expected_deaths)) %>% 
  # standardised rates expected deaths over total, 
  mutate(dsr = total_expected/total) %>% 
  ungroup() %>% 
  # calculate standard error 
  mutate(se_dsri = groupsize^2*(value * (1- value)/population)) %>% 
  # sum standard error per category
  group_by(date, sex, care_home_type) %>% 
  mutate(se_dsr = (sqrt(sum(se_dsri)))/total) %>% 
  ungroup() %>% 
  # keep only one row per unique group 
  select(date, care_home_type, sex, dsr, se_dsr) %>% 
  distinct() %>% 
  # confidence interval 
  mutate(lcl = dsr - 1.96 * se_dsr, 
         ucl = dsr + 1.96 * se_dsr, 
         Confidence_Interval = paste(round(lcl*1000,2), round(ucl*1000,2), sep = "-"),
         Standardised_Rate = round(dsr * 1000,2))

##-- noncovid 

# merge with standard population  
noncovid_standard <- noncovid %>% 
  left_join(european_standard, by = c("ageband_five")) 

# calculate standardised rates by sex and care home 
noncovid_standard <- noncovid_standard %>% 
  # expected deaths is mortality rate times the standard groupsize 
  mutate(expected_deaths = value * groupsize) %>% 
  # sum by group 
  group_by(date, sex, care_home_type) %>% 
  mutate(total_expected = sum(expected_deaths)) %>% 
  # standardised rates expected deaths over total, 
  mutate(dsr = total_expected/total) %>% 
  ungroup() %>% 
  # calculate standard error 
  mutate(se_dsri = groupsize^2*(value * (1- value)/population)) %>% 
  # sum standard error per category
  group_by(date, sex, care_home_type) %>% 
  mutate(se_dsr = (sqrt(sum(se_dsri)))/total) %>% 
  ungroup() %>% 
  # keep only one row per unique group 
  select(date, care_home_type, sex, dsr, se_dsr) %>% 
  distinct() %>% 
  # confidence interval 
  mutate(lcl = dsr - 1.96 * se_dsr, 
         ucl = dsr + 1.96 * se_dsr, 
         Confidence_Interval = paste(round(lcl*1000,2), round(ucl*1000,2), sep = "-"), 
         Standardised_Rate = round(dsr * 1000,2)) 

# Tables: Standardised Rates ----------------------------------------------

# all-cause mortality 
table_9a <- all_cause_standard %>% 
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


write.table(table_9a, file = "./analysis/outfiles/table_9a.txt", sep = "\t", na = "", row.names=FALSE)

# covid mortality 
table_9b <- covid_standard %>% 
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


write.table(table_9b, file = "./analysis/outfiles/table_9b.txt", sep = "\t", na = "", row.names=FALSE)

# noncovid mortality 
table_9c <- noncovid_standard %>% 
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


write.table(table_9c, file = "./analysis/outfiles/table_9c.txt", sep = "\t", na = "", row.names=FALSE)

 
# Figure: Standardised Rates ----------------------------------------------

# all cause mortality 
y_value <- (max(all_cause_standard$dsr) + (max(all_cause_standard$dsr)/4)) * 1000

plot_8a <- ggplot(all_cause_standard, aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = sex, shape = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line(size = 1) + geom_point() + 
labs(x = "Time Period", 
     y = "Standardised All-Cause Mortality Rate per 1,000 individuals", 
     title = "Age-standardidised All-Cause Mortality Rate by Sex and Care Home", 
     shape = "Care Home", 
     colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_8a.png")
plot_8a
dev.off()

# covid mortality 
y_value <- (max(covid_standard$dsr) + (max(covid_standard$dsr)/4)) * 1000

plot_8b <- ggplot(covid_standard, aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = sex, shape = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line(size = 1) + geom_point() + 
  labs(x = "Time Period", 
       y = "Standardised COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Age-standardidised COVID-19 Mortality Rate by Sex and Care Home", 
       shape = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_8b.png")
plot_8b
dev.off()
  
# noncovid mortality 
y_value <- (max(covid_standard$dsr) + (max(covid_standard$dsr)/4)) * 1000

plot_8c <- ggplot(covid_standard, aes (x = as.Date(date, "%Y-%m-%d"), y = dsr*1000, colour = sex, shape = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line(size = 1) + geom_point() + 
  labs(x = "Time Period", 
       y = "Standardised non COVID-19 Mortality Rate per 1,000 individuals", 
       title = "Age-standardidised non COVID-19 Mortality Rate by Sex and Care Home", 
       shape = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_8c.png")
plot_8c
dev.off()

  