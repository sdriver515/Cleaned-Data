title: "Final Project: Vital Signs Data Cleaning"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"

#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console 

#VITALS

#Read data into R
VS_data <- read.csv(file = "./data/VitalSigns.csv") 

VS_bl <- VS_data #Making a duplicate 

VS_bl <- VS_bl %>% dplyr::rename(patientID = subject_id,
                                 t = Vital_Signs_Delta) %>%
  # only consider records up to 14 days on study
  dplyr::filter(t <= 14) %>%
  # For each patient arrange records in order by time
  dplyr::arrange(patientID, t) %>%
  # group by patient
  dplyr::group_by(patientID) %>%
  # select most recent record for each patient (up to 14 days on study)
  dplyr::slice(1L) %>%
  # unselect time (baseline is non-temporal, time can be ignored)
  dplyr::select(-t)

#How many NA after selecting correct info above:
sum(is.na (VS_bl$Height)) #3748 NA
sum(is.na (VS_bl$Weight)) #1093 NA

#Units after selecting correct info above:
unique(VS_bl$Height_Units) #Height_Units contains centimeters, inches, and missing spaces 
unique(VS_bl$Weight_Units) #Weight_Units contains pounds, kilograms, and missing spaces  

#Converting to standardized measurements:
VS_bl <- VS_bl %>% 
  dplyr::mutate(height_cm_to_meters = ifelse(Height_Units == "Centimeters", Height/100, NA))

VS_bl <- VS_bl %>% 
  dplyr::mutate(height_inches_to_meters = ifelse(Height_Units == "Inches", Height*0.0254, NA))

VS_bl <- VS_bl %>% 
  dplyr::mutate(lbs_to_kilograms = ifelse(Weight_Units == "Pounds", Weight*0.453592, NA))

VS_bl <- VS_bl %>% 
  dplyr::mutate(extracted_kilograms = ifelse(Weight_Units == "Kilograms", Weight*1, NA))

#Set up blank height units for future use by converting to 9999
VS_bl$Height_Units[VS_bl$Height_Units==""] <- 9999 

#When I looked through the dataset earlier, it appeared that the height numbers correlating to blank height units were in centimeter form 
VS_bl <- VS_bl %>% 
  dplyr::mutate(assumed_height = ifelse(Height_Units == 9999, Height/100, NA))

#If blanks correlate to centimeters, they will also likely correlate to kilograms 
VS_bl <- VS_bl %>% 
  dplyr::mutate(assumed_kilogram = ifelse(Height_Units == 9999, Weight*1, NA))

#ignoring NA and putting all the height-in-meters columns together 
VS_bl <- VS_bl %>% 
  dplyr::mutate(meters = coalesce(height_cm_to_meters, height_inches_to_meters, assumed_height))

#ignoring NA and putting together all standardized weights 
VS_bl <- VS_bl %>% 
  dplyr::mutate(kilograms = coalesce(lbs_to_kilograms, extracted_kilograms, assumed_kilogram))

#Checking if many relevant numbers are left NA: an acceptable quantity remains 
sum(is.na (VS_bl$meters)) #3748 NA
sum(is.na (VS_bl$kilograms)) #1093 NA

#Creating BMI variable 
VS_bl <- VS_bl %>% 
  dplyr::mutate(bmi_bl = kilograms/meters^2)

#Removing columns that are no longer needed 
VS_bl <- VS_bl %>% select(-Height, -Height_Units, - Weight, -Weight_Units, -height_cm_to_meters:-assumed_kilogram)

head(VS_bl, 10)

#Saving in-depth dataframe for future viewing 
write.csv(VS_bl, file = "./data/vs_bl.csv",
          row.names = FALSE)

#Removing columns that won't be needed in baseline
VS_bl <- VS_bl %>% select(-Blood_Pressure_Diastolic:-kilograms)

write.csv(VS_bl, file = "./data/vs_cleaned.csv",
          row.names = FALSE)

# clear environment
rm(list = ls())
