title: "Final Project: Labs Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console

#LABS DATA

#Loading data
Labs_data <- read.csv(file = "./data/Labs.csv")

Labs_bl_CK <- Labs_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Laboratory_Delta) %>%
  dplyr::filter(t <= 14 & t >= -90) %>%
  dplyr::filter(Test_Name == "CK") %>%
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t, -Test_Name, -Test_Unit) %>%
  dplyr::rename(ck_bl = Test_Result)

class(Labs_bl_CK$patientID) #integer
class(Labs_bl_CK$ck_bl) #checking class and it says character. Need to change to some numeric form.
unique(Labs_bl_CK$ck_bl) #when I look at this I see the word "Normal"

#Removing "Normal"
Labs_bl_CK <- Labs_bl_CK %>%
  filter(!str_detect(ck_bl, "Normal")) #removed "Normal" and the dataset size only changed by 1

#Checking further for characters
Labs_bl_CK %>%
  select_if(is.character) %>%
  view() #they're all characters

#Changing column to numeric without deleting contents
#found framework at https://www.geeksforgeeks.org/how-to-convert-dataframe-column-from-character-to-numeric-in-r/ and applied general concept
Labs_bl_CK <- transform(Labs_bl_CK, ck_bl = as.numeric(ck_bl))
class(Labs_bl_CK$ck_bl) #checking and it is numeric now

Labs_bl_Creatinine <- Labs_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Laboratory_Delta) %>%
  dplyr::filter(t <= 14 & t >= -90) %>%
  dplyr::filter(Test_Name == "Creatinine") %>%
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t, -Test_Name, -Test_Unit) %>%
  dplyr::rename(creatinine_bl = Test_Result)

Labs_bl_Phosphorus <- Labs_data %>%
  # rename variables
  dplyr::rename(patientID = subject_id,
                t = Laboratory_Delta) %>%
  dplyr::filter(t <= 14 & t >= -90) %>%
  dplyr::filter(Test_Name == "Phosphorus") %>%
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t, - Test_Name, -Test_Unit) %>%
  dplyr::rename(phosphorous_bl = Test_Result)

Labs_bl <- Labs_bl_CK %>% left_join(Labs_bl_Creatinine, by = "patientID") %>% left_join(Labs_bl_Phosphorus, by = "patientID")

head(Labs_bl, 10)

#Checking no repeated patients
dim(Labs_bl)[1] == length(unique(Labs_bl$patientID))

#Saving for later use
write.csv(Labs_bl, file = "./data/labs_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())
