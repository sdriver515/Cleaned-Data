title: "Final Project: ALSFRS Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console

#ALSFRS DATA
ALSFRS_bl <- read.csv(file = "./data/ALSFRS_data1.csv")

ALSFRS_bl <- ALSFRS_bl %>%
  dplyr::filter(t <= 14) %>%
  dplyr::rename(patientID = subject_id,
                f_gastronomy = Q5) %>%
  # For each patient arrange records in order by time
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect certain columns
  dplyr::select(-t, -Q10_Respiratory, -ALSFRS_Total:-ALSFRS_old_score_computed, -Overlapping_Scores, -filtered_times, -alsfrsr_derv)

dim(ALSFRS_bl)[1] == length(unique(ALSFRS_bl$patientID)) #Says TRUE

#Saving for later use
write.csv(ALSFRS_bl, file = "./data/ALSFRS_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())
