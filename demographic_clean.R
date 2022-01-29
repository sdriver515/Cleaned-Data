title: "Final Project: Demographics Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console 

Demographics_data <- read.csv(file = "./data/demographics.csv")

#Creating a cleaned dataset
Demographic_bl <- Demographics_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Demographics_Delta) %>% 
  filter(Race_Americ_Indian_Alaska_Native == 1 | Race_Asian == 1 | 
           Race_Black_African_American==1 | Race_Hawaiian_Pacific_Islander==1 | 
           Race_Unknown==1 | Race_Caucasian==1 | Race_Other==1) %>%
  #putting relevant race info into one column 
  pivot_longer(col = Race_Americ_Indian_Alaska_Native:Race_Caucasian,
               names_to = "Race",
               values_drop_na = TRUE) %>% 
  dplyr::filter(t <= 14 & t >= -90) %>%
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  select(-Race_Other, -Race_Other_Specify, -Ethnicity, -Date_of_Birth, -value) #dropping columns that are not needed 

#Dropping race column, too--separated from prior step because info can be revisited and may be needed 
Demographic_bl <- Demographic_bl %>% dplyr::select(-Race)

#Checking for NA
sum(is.na (Demographic_bl$Sex)) #Says there are zero NA
sum(is.na (Demographic_bl$Age)) #941 NAs

#I labelled the NAs for dropping because the normal na.omit functions, etc. don't seem to work 
Demographic_bl$Age[is.na(Demographic_bl$Age)] <- 9999
Demographic_bl$Sex[is.na(Demographic_bl$Sex)] <- 9999

Demographic_bl$Sex[Demographic_bl$Sex==""] <- 9999
Demographic_bl$Age[Demographic_bl$Age==""] <- 9999 

Demographic_bl <- Demographic_bl %>%
  filter(!Sex == 9999)

Demographic_bl <- Demographic_bl %>%
  filter(Age < 9999)

#Double-checking for NAs after dropping them
sum(is.na (Demographic_bl$Sex)) #0 NA
sum(is.na (Demographic_bl$Age)) #0 NA

#Changing Sex to binary 
Demographic_bl$Sex <- ifelse(Demographic_bl$Sex == "Female",0,1) 

#Changing more column names 
Demographic_bl <- Demographic_bl %>%
  rename(sex_bl = Sex,
         age_bl = Age)

#Checking no repeated patients 
dim(Demographic_bl)[1] == length(unique(Demographic_bl$patientID)) #Says TRUE

#Check before saving:
head(Demographic_bl, 10)

#Saving for later use
write.csv(Demographic_bl, file = "./data/demographics_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())
