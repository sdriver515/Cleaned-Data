title: "Final Project: Riluzole Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"

#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console 

#RILUZOLE DATA
Riluzole_data <- read.csv(file = "./data/Riluzole.csv") #patient use of riluzole data

riluzole_bl <- Riluzole_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Riluzole_use_Delta,
                f_riluzole = Subject_used_Riluzole) %>%
  # only consider records up to 14 days on study
  dplyr::filter(t <= 14) %>%
  # Subject used riluzole: Yes = 1, No = 0
  dplyr::mutate(f_riluzole = ifelse(f_riluzole == "Yes", 1, 0)) %>%
  # For each patient arrange records in order by time
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t)

# confirm 1 record per patient
dim(riluzole_bl)[1] == length(unique(riluzole_bl$patientID)) # TRUE

#Saving for later use
write.csv(riluzole_bl, file = "./data/riluzole_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())
