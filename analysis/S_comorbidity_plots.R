# Program Information  ----------------------------------------------------

# Program:     S_comorbiditiy_plots
# Author:      Anna Schultze 
# Description: plot comorbidities over time 
# Input:       measure_[COMORB]_[comorbidity].csv
# Output:      plots into output/plots as per project.yaml 
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

# 1. Plot comorbities over time
# function to plot a single comorbidity over time, by care home status  
plot_comorbidities <- function(comorb) {

  titlestring <- paste("Prevalence of", comorb, "over time")
  inputdata <- paste0("./output/measure_COMORB_",comorb, ".csv") 
                     
  data <- fread(inputdata, data.table = FALSE, na.strings = "")
  
  plot_8a <- ggplot(data, aes (x = as.Date(date, "%Y-%m-%d"), y = value*100, colour = care_home_type)) + 
    geom_line(size = 1) + geom_point() + 
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-02-01"),y=+Inf,label="Wave 1",vjust=2,geom="label") +
    geom_vline(xintercept = as.numeric(as.Date("2020-09-01", "%Y-%m-%d")), colour = "gray48", linetype = "longdash") + 
    annotate(x=as.Date("2020-09-01"),y=+Inf,label="Wave 2",vjust=2,geom="label") +
    labs(x = "Calendar Month", 
         y = "Prevalence (percentage)", 
         title = titlestring,
         colour = "Care Home") + 
    scale_y_continuous(limits = c(0,100)) +
    scale_x_date(date_labels = "%B %y", date_breaks = "2 months") +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
          axis.text.x = element_text(angle = 75, vjust = 0.9, hjust=1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "gray"), 
          panel.grid.major.y = element_line(color = "gainsboro"))
  
}

# Comorbidity plots-------------------------------------------------------------

## Dementia 
S9a_dementia <- plot_comorbidities("dementia")
png(filename = "./output/plots/S9a_dementia.png")
S9a_dementia
dev.off()

## Stroke  
S9b_stroke <- plot_comorbidities("stroke")
png(filename = "./output/plots/S9b_stroke.png")
S9b_stroke
dev.off()

## Cancer  
S9c_cancer <- plot_comorbidities("cancer")
png(filename = "./output/plots/S9c_cancer.png")
S9c_cancer
dev.off()

## chronic_liver_disease  
S9d_chronic_liver_disease <- plot_comorbidities("chronic_liver_disease")
png(filename = "./output/plots/S9d_chronic_liver_disease.png")
S9d_chronic_liver_disease
dev.off()

## chronic_cardiac_disease  
S9e_chronic_cardiac_disease <- plot_comorbidities("chronic_cardiac_disease")
png(filename = "./output/plots/S9e_chronic_cardiac_disease.png")
S9e_chronic_cardiac_disease
dev.off()

## chronic_respiratory_disease  
S9f_chronic_respiratory_disease <- plot_comorbidities("chronic_respiratory_disease")
png(filename = "./output/plots/S9f_chronic_respiratory_disease.png")
S9f_chronic_respiratory_disease
dev.off()

## diabetes  
S9g_diabetes <- plot_comorbidities("diabetes")
png(filename = "./output/plots/S9g_diabetes.png")
S9g_diabetes
dev.off()

