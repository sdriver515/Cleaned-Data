title: "Final Project: Slow Vital Capacity Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console 
#SLOW VITAL CAPACITY 

#Read data into R
SVC_data <- read.csv(file = "./data/svc.csv") 
head(SVC_data, 10) #Looking at everything: the headers are a bit irregular 

#Select time frame and rename columns: 
SVC_bl <- SVC_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Slow_vital_Capacity_Delta,
                Subject_Liters_Trial_2 = Subject_Liters__Trial_2_,
                Subject_Liters_Trial_3 = Subject_Liters__Trial_3_) %>%
  #right time frame 
  dplyr::filter(t <= 14 & t >= -90) 

sum(is.na (SVC_bl$Subject_Liters_Trial_1)) #This says zero NA
sum(is.na (SVC_bl$Subject_Liters_Trial_2)) #1007 NA
sum(is.na (SVC_bl$Subject_Liters_Trial_3)) #1007 NA

SVC_bl <- SVC_bl %>%
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t)

#Selecting the timeframe removed a lot of information. The extra trials don't have anything in them.
unique(SVC_bl$Subject_Liters_Trial_2) #NA  
unique(SVC_bl$Subject_Liters_Trial_3) #NA

#Remove more unnecessary columns 
SVC_bl <- SVC_bl %>%
  dplyr::select(-Subject_Liters_Trial_2:-Slow_Vital_Capacity_Units, -pct_of_Normal_Trial_1) %>%
  dplyr::rename(svc_bl = Subject_Liters_Trial_1)

#Checking no repeated patients 
dim(SVC_bl)[1] == length(unique(SVC_bl$patientID)) #No repeated patients 

#Save 
write.csv(SVC_bl, file = "./data/svc_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())
