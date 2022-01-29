title: "Final Project: ALS History Analysis"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console

#ALS HISTORY 

#Read in daata
alshistory_data <- read.csv(file = "./data/AlsHistory.csv")

alshistory_bl <- alshistory_data #making a second dataset so that I can compare between both

#Filtering and renaming 
alshistory_bl <- alshistory_bl %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                onset_days_bl = Onset_Delta) %>%
  dplyr::group_by(patientID, onset_days_bl) %>%
  dplyr::slice(1L) %>%
  dplyr::select()

unique(alshistory_bl$onset_days_bl) #checking onset days: none close to 9999, which I will use below 

#Setting NAs to 9999
alshistory_bl[is.na(alshistory_bl)] <- 9999

#Filter out the 9999s correlating to NAs
alshistory_bl <- alshistory_bl %>%
  filter(onset_days_bl < 9999)

#Checking no repeated patients 
dim(alshistory_bl)[1] == length(unique(alshistory_bl$patientID)) #Says TRUE

#Saving for later use
write.csv(alshistory_bl, file = "./data/alshistory_cleaned.csv",
          row.names = FALSE)
