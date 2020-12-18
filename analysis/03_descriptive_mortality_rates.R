# Program Information  ----------------------------------------------------

# Program:     03_descriptive_mortality_rates
# Author:      Anna Schultze 
# Description: Present raw mortality rates generated from measures framework
# Input:       measure_[outcome]_[group].csv
# Output:      table2.txt
#              figure1a-c.png
# Edits:      

# Housekeeping  -----------------------------------------------------------

if (grepl("/analysis", getwd())) { 
  setwd("..") 
  getwd() 
} else {
  getwd() 
}

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
# To do; develop function for thus 

# all-cause death 
measure_any_all <- fread("./output/measure_allcause_death_all.csv", data.table = FALSE, na.strings = "")
measure_any_sex <- fread("./output/measure_allcause_death_sex.csv", data.table = FALSE, na.strings = "")
measure_any_age <- fread("./output/measure_allcause_death_age.csv", data.table = FALSE, na.strings = "")
measure_any_sex_age <- fread("./output/measure_allcause_death_sex_age.csv", data.table = FALSE, na.strings = "")

# covid death 
measure_covid_all <- fread("./output/measure_covid_death_all.csv", data.table = FALSE, na.strings = "")
measure_covid_sex <- fread("./output/measure_covid_death_sex.csv", data.table = FALSE, na.strings = "")
measure_covid_age <- fread("./output/measure_covid_death_age.csv", data.table = FALSE, na.strings = "")
measure_covid_sex_age <- fread("./output/measure_covid_death_sex_age.csv", data.table = FALSE, na.strings = "")

# Remove empty COVID rows--------------------------------------------------

measure_covid_all <- measure_covid_all %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

measure_covid_sex <- measure_covid_sex %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

measure_covid_age <- measure_covid_age %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

measure_covid_sex_age <- measure_covid_sex_age %>% 
  mutate(dateform = ymd(date)) %>% 
  filter(dateform >= ymd("20200301"))

# Confidence Intervals ----------------------------------------------------

# debugging code 

print(colnames(measure_any_all))
print(colnames(measure_covid_all))

tabyl(measure_any_all$care_home_type)
tabyl(measure_covid_all$care_home_type)

# function for calculating CIs (from Hmisc)
# I'm a little unsure how best to save the output from this function to the dataset
# fairly sure this is not how you're meant to do it, but it seems to work... 

confidence <- function(input, outcome) {
  
  temp <- Hmisc::binconf(x = input[,outcome], n = input$population, method = c("wilson"))
  temp <- as_tibble(temp)

  input <- bind_cols(input, temp) 

  input <- input %>% 
    mutate(PointEst = round((PointEst * 100),2)) %>% 
    mutate(Lower = round((Lower * 100),2)) %>% 
    mutate(Upper = round((Upper * 100),2)) 

} 

# Apply confidence intervals 
measure_any_all <- confidence(input = measure_any_all, outcome = "ons_any_death")
measure_any_sex <- confidence(input = measure_any_sex, outcome = "ons_any_death")
measure_any_age <- confidence(input = measure_any_age, outcome = "ons_any_death")
measure_any_sex_age <- confidence(input = measure_any_sex_age, outcome = "ons_any_death")

measure_covid_all <- confidence(input = measure_covid_all, outcome = "ons_covid_death")
measure_covid_sex <- confidence(input = measure_covid_sex, outcome = "ons_covid_death")
measure_covid_age <- confidence(input = measure_covid_sex_age, outcome = "ons_covid_death")
measure_covid_sex_age <- confidence(input = measure_covid_sex_age, outcome = "ons_covid_death")

# Make and print tables 

# Tables ------------------------------------------------------------------
# long term aim to automate to have cleaner code, but each block is only used twice at the moment (if similar)

# All cause mortality 

table_2a <- measure_any_all %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
  select(c(care_home_group, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) 
  
write.table(table_2a, file = "./analysis/outfiles/table_2a.txt", sep = "\t", na = "", row.names=FALSE)

table_2b <- measure_any_sex %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  rename(Gender = sex) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
  select(c(care_home_group, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) %>% 
  select(Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Gender, Date)

write.table(table_2b, file = "./analysis/outfiles/table_2b.txt", sep = "\t", na = "", row.names=FALSE)

table_2c <- measure_any_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  rename(Age = ageband) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
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
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) %>% 
  select(Age, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Date)

write.table(table_2c, file = "./analysis/outfiles/table_2c.txt", sep = "\t", na = "", row.names=FALSE)

table_2d <- measure_any_sex_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_any_death) %>% 
  rename(N = population) %>% 
  rename(Age = ageband) %>% 
  rename(Gender = sex) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
  select(c(care_home_group, Age, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) %>% 
  select(Age, Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Gender, Date)

write.table(table_2d, file = "./analysis/outfiles/table_2d.txt", sep = "\t", na = "", row.names=FALSE)

# Covid mortality 

table_3a <- measure_covid_all %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
  select(c(care_home_group, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide 
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables 
  rename(Date= Private_Home_date) %>% 
  select(-c(Care_or_Nursing_Home_date)) %>% 
  select(Date, (matches("Care*")), (matches("Priv*"))) 

write.table(table_3a, file = "./analysis/outfiles/table_3a.txt", sep = "\t", na = "", row.names=FALSE)

table_3b <- measure_covid_sex %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  rename(Gender = sex) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
  select(c(care_home_group, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Gender)) %>% 
  select(Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Gender, Date)

write.table(table_3b, file = "./analysis/outfiles/table_3b.txt", sep = "\t", na = "", row.names=FALSE)

table_3c <- measure_covid_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  rename(Age = ageband) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
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
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age)) %>% 
  select(Age, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Date)

write.table(table_3c, file = "./analysis/outfiles/table_3c.txt", sep = "\t", na = "", row.names=FALSE)

table_3d <-  measure_covid_sex_age %>% 
  # create a labelled variable for outputting in table formats 
  mutate(care_home_group = ifelse((care_home_type == "Y"), "Care_or_Nursing_Home", "Private_Home")) %>%
  # rename key variables to how you want them displayed in tables and select the relevant ones
  rename(Mortality_Rate = PointEst) %>% 
  rename(n = ons_covid_death) %>% 
  rename(N = population) %>% 
  rename(Age = ageband) %>% 
  rename(Gender = sex) %>% 
  mutate(Confidence_Interval = paste(Lower, Upper, sep = "-")) %>% 
  select(c(care_home_group, Age, Gender, date, n, N, Mortality_Rate, Confidence_Interval)) %>% 
  # need to create a unique ID for reshaping the data
  group_by(care_home_group) %>% 
  mutate(id = row_number()) %>% 
  ungroup %>% 
  # reshape wide
  pivot_wider(
    id_cols = id, 
    names_from = care_home_group, 
    values_from = c(date, Age, Gender, n, N, Mortality_Rate, Confidence_Interval), 
    names_glue = "{care_home_group}_{.value}") %>% 
  # tidy up, remove unnecessary variables and sort by the grouping vars 
  rename(Date = Private_Home_date) %>% 
  rename(Age = Private_Home_Age) %>% 
  rename(Gender = Private_Home_Gender) %>% 
  select(-c(Care_or_Nursing_Home_date, Care_or_Nursing_Home_Age, Care_or_Nursing_Home_Gender)) %>% 
  select(Age, Gender, Date, (matches("Care*")), (matches("Priv*"))) %>% 
  arrange(Age, Gender, Date)

write.table(table_3d, file = "./analysis/outfiles/table_3d.txt", sep = "\t", na = "", row.names=FALSE)

# Line Plots  -------------------------------------------------------------
# currently automatically puts an y axis value, but aim is to amend these to be neater once I know the rates

# all-cause mortality 
y_value <- (max(measure_any_all$value) + (max(measure_any_all$value)/5)) * 100

plot_1a <- ggplot(measure_any_all, aes (x = date, y = value*100, group = care_home_type, colour = care_home_type)) + 
  geom_line() + 
  labs(x = "Time Period", 
       y = "All-cause Mortality Rate", 
       title = "Crude All-cause Mortality Rate") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_color_discrete(name="Care Home",
                       labels=c("No", "Yes")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_1a.png")
plot_1a
dev.off()

y_value <- (max(measure_any_sex$value) + (max(measure_any_sex$value)/5)) * 100

plot_1b <- ggplot(measure_any_sex, aes (x = date, y = value*100, colour = sex, shape = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line() + geom_point() + 
  labs(x = "Time Period", 
       y = "All-cause Mortality Rate", 
       title = "Crude All-cause Mortality Rate by Sex", 
       shape = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_1b.png")
plot_1b
dev.off()

y_value <- (max(measure_any_age$value) + (max(measure_any_age$value)/5)) * 100

plot_1c <- ggplot(measure_any_age, aes (x = date, y = value*100, colour = ageband, shape = care_home_type, group = interaction(ageband, care_home_type))) + 
  geom_line() + geom_point() + 
  labs(x = "Time Period", 
       y = "All-cause Mortality Rate", 
       title = "Crude All-cause Mortality Rate by Age", 
       shape = "Care Home", 
       colour = "Age") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_colour_viridis_d(option = "plasma") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_1c.png")
plot_1c
dev.off()

# covid mortality 
y_value <- (max(measure_covid_all$value) + (max(measure_covid_all$value)/5)) * 100

plot_2a <- ggplot(measure_covid_all, aes (x = date, y = value*100, group = care_home_type, colour = care_home_type)) + 
  geom_line() + 
  labs(x = "Time Period", 
       y = "COVID-19 Mortality Rate", 
       title = "Crude COVID-19 Mortality Rate") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_color_discrete(name="Care Home",
                       labels=c("No", "Yes")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_2a.png")
plot_2a
dev.off()

y_value <- (max(measure_covid_sex$value) + (max(measure_covid_sex$value)/5)) * 100

plot_2b <- ggplot(measure_covid_sex, aes (x = date, y = value*100, colour = sex, shape = care_home_type, group = interaction(sex, care_home_type))) + 
  geom_line() + geom_point() + 
  labs(x = "Time Period", 
       y = "All-cause Mortality Rate", 
       title = "Crude All-cause Mortality Rate by Sex", 
       shape = "Care Home", 
       colour = "Gender") + 
  scale_y_continuous(limits = c(0,y_value)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_2b.png")
plot_2b
dev.off()

y_value <- (max(measure_covid_age$value) + (max(measure_covid_age$value)/5)) * 100

plot_2c <- ggplot(measure_covid_age, aes (x = date, y = value*100, colour = ageband, shape = care_home_type, group = interaction(ageband, care_home_type))) + 
  geom_line() + geom_point() + 
  labs(x = "Time Period", 
       y = "COVID-19 Mortality Rate", 
       title = "Crude COVID-19 Mortality Rate by Age", 
       shape = "Care Home", 
       colour = "Age") + 
  scale_y_continuous(limits = c(0,y_value)) +
  scale_colour_viridis_d(option = "plasma") + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) 

png(filename = "./analysis/outfiles/plot_2c.png")
plot_2c
dev.off()
