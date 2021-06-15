# Program Information  ----------------------------------------------------

# Program:     040_cause_of_death 
# Author:      Anna Schultze 
# Description: Plot key causes of death over time, stratified by care home residency 
# Input:       input_measure_[date].csv
# Output:      tables & figures into: analysis/outfiles
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

# create a list of all of the files I want to read in 
file_list <- as.list(dir(path = "./output", pattern = glob2rx("input_measures_*"), full.names = TRUE)) 

# Functions ---------------------------------------------------------------

# 1. Function to format data for plotting common causes of death 
# has some optional filters 

cause_of_death_format <- function(inputdata, care_home_filter, remove_covid_deaths) {
  
  # read in data
  data <- fread(inputdata, data.table = FALSE, na.strings = "")
  
  # extract time period from the variable name for creating a date variable to separate datasets once appended 
  time_period <- ymd(word(inputdata, 3, sep = "_"))
  
  # input arguments in quotes
  carehomefilter <- enquo(care_home_filter)
  covidfilter <- enquo(remove_covid_deaths)
  
  data <- data %>% 
    filter(ons_any_death == 1) %>% 
    filter(care_home_type == !!carehomefilter) %>% 
    filter(if (!!covidfilter == "Y") (ons_covid_death != 1) else TRUE) %>% 
    mutate(Time_Period = time_period) %>% 
    rename(Care_Home = care_home_type) %>%
    # extract relevant parts of the ICD-10 codes to classify deaths
    mutate(cause_chapter = str_sub(died_cause_ons,1,1)) %>% 
    mutate(cause_number = as.numeric(str_sub(died_cause_ons,2))) %>% 
    # create specific causes of death to match K Baskharan's analysis
    # assumes COVID-19 if more than one primary/underlying 
    mutate(Cause_of_Death = case_when(
      cause_chapter == "C" ~ "Cancer",
      cause_chapter == "I" ~ "Cardiovascular Disease",
      cause_chapter == "J" ~ "Respiratory Disease",
      # dementia codes should be F01, F02, F03 and G30     
      cause_chapter == "F" & (cause_number >= 0 & cause_number < 4) ~ 'Dementia', 
      cause_chapter == "G" & cause_number == 30 ~ 'Dementia', 
      ons_covid_death == 1 ~ "COVID-19", 
      TRUE ~ "Other"),
      Cause_of_Death = factor(Cause_of_Death, levels = c("Respiratory Disease", "Dementia", "Cardiovascular Disease", "Cancer", "Other", "COVID-19"))) 
 
# check the formatting of causes of death greater than 5 classed as dementia and other 
   check <- data %>% 
    filter(Cause_of_Death == "Other") %>% 
    group_by(died_cause_ons) %>% 
    summarise(count = n()) %>% 
    mutate(count = ifelse(count <= 5, NA, count)) 
  
   print(check) 
   
   data <- data %>% 
    # calculate frequency of each code 
    group_by(Cause_of_Death) %>% 
    summarise(Count = n()) %>% 
    mutate(Percentage = round((Count/sum(Count)),4)*100) %>% 
    mutate(Time_Period = time_period) %>% 
    # select only relevant variables
    select(Time_Period, Cause_of_Death, Percentage, Count)

} 

# 2. Function to plot of causes of death

cause_of_death_plot <- function(data, axistext) {
  
  titlestring <- paste("Cause of Death over Time in ", axistext)
  
  ggplot({{data}}, aes(x = Time_Period, y = Percentage, fill = Cause_of_Death), position = "stack") + 
  geom_area(alpha=0.6 , size=.5, colour="white") + 
  scale_x_date(date_labels = "%B %y", date_breaks = "8 weeks") +
  scale_fill_viridis_d(limits = c("Respiratory Disease", "Dementia", "Cardiovascular Disease", "Cancer", "Other", "COVID-19")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "gray"), 
        panel.grid.major.y = element_line(color = "gainsboro")) + 
  labs(x = "Time Period", 
       y = "% of all Deaths", 
       title = titlestring, 
       fill = "Cause of Death") 
  
} 

# Tables  ------------------------------------------------------------------

##-- With COVID deaths
# care homes
table_allcauses_ch <- map_dfr(file_list, ~cause_of_death_format(.x, care_home_filter = "Y", remove_covid_deaths = "N"))
write.table(table_allcauses_ch, file = "./output/tables/4a_table_allcauses_ch.txt", sep = "\t", na = "", row.names=FALSE)

# private homes 
table_allcauses_ph <- map_dfr(file_list, ~cause_of_death_format(.x, care_home_filter = "N", remove_covid_deaths = "N"))
write.table(table_allcauses_ph, file = "./output/tables/4b_table_allcauses_ph.txt", sep = "\t", na = "", row.names=FALSE)

##-- Without COVID deaths 
# care homes
table_nccauses_ch <- map_dfr(file_list, ~cause_of_death_format(.x, care_home_filter = "Y", remove_covid_deaths = "Y"))
write.table(table_nccauses_ch, file = "./output/tables/4c_table_nccauses_ch.txt", sep = "\t", na = "", row.names=FALSE)

# private homes 
table_nccauses_ph <- map_dfr(file_list, ~cause_of_death_format(.x, care_home_filter = "N", remove_covid_deaths = "Y"))
write.table(table_nccauses_ph, file = "./output/tables/4d_table_nccauses_ph.txt", sep = "\t", na = "", row.names=FALSE)

# Figures -----------------------------------------------------------------

##-- With COVID deaths 
# care homes
plot_allcauses_ch <- cause_of_death_plot(table_allcauses_ch, "Care Home Residents")

png(filename = "./output/plots/4a_plot_allcauses_ch.png")
plot_allcauses_ch
dev.off()

# private homes
plot_allcauses_ph <- cause_of_death_plot(table_allcauses_ph, "Private Home Residents")

png(filename = "./output/plots/4b_plot_allcauses_ph.png")
plot_allcauses_ph
dev.off()

##-- With non COVID deaths 
# care homes
plot_nccauses_ch <- cause_of_death_plot(table_nccauses_ch, "Care Home Residents")

png(filename = "./output/plots/4c_plot_nccauses_ch.png")
plot_nccauses_ch
dev.off()

# non care homes
plot_nccauses_ph <- cause_of_death_plot(table_nccauses_ph, "Private Home Residents")

png(filename = "./output/plots/4d_plot_nccauses_ph.png")
plot_nccauses_ph
dev.off()

