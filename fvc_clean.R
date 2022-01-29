title: "Final Project: Forced Vital Capacity Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console 

#FORCED VITAL CAPACITY

#Read data into R
FVC_data <- read.csv(file = "./data/fvc.csv") 

#Select time frame and rename columns: 
FVC_bl <- FVC_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Forced_Vital_Capacity_Delta) %>%
  #right time frame 
  dplyr::filter(t <= 14 & t >= -90) %>%
  dplyr::arrange(patientID, t)

#Setting up another dataframe to use in finding the mean across trials:
selected <- FVC_bl
selected <- selected %>% select(-patientID, -pct_of_Normal_Trial_1, -pct_of_Normal_Trial_2, -pct_of_Normal_Trial_3, -Subject_Normal, -Forced_Vital_Capacity_Units, -t)

#make a new column(fvc_bl) in original dataframe for the mean + correcting the extra parts with t
FVC_bl <- FVC_bl %>% dplyr::mutate(fvc_bl = rowMeans(selected, na.rm = TRUE)) %>% 
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t)

#Checking no repeated patients 
dim(FVC_bl)[1] == length(unique(FVC_bl$patientID)) #No repeated patients 

#Narrowing dataset down for use in baseline dataset
FVC_bl <- FVC_bl %>% select(-Subject_Liters_Trial_1:-Subject_Normal, -Forced_Vital_Capacity_Units)

#Save:
write.csv(FVC_bl, file = "./data/fvc_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())