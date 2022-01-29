title: "Final Project: Treatment Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console 

#TREATMENT DATA
Treatment_data <- read.csv(file = "./data/Treatment.csv") #El Escorial criteria assessment data

treatment_bl <- Treatment_data #making a second dataset so that I can compare between both

treatment_bl <- treatment_bl %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Treatment_Group_Delta) %>%
  #setting placebo and treatment respectively equal to 0 and 1
  dplyr::mutate(f_placebo = ifelse(Study_Arm == "Placebo", 0, 1))

treatment_bl <- treatment_bl %>% dplyr::select(-Study_Arm, -t)

#Saving for later use
write.csv(treatment_bl, file = "./data/treatment_cleaned.csv",
          row.names = FALSE)
