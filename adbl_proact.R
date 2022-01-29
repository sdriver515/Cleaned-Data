title: "Final Project: Baseline Analysis"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console

#Selecting data
riluzole_bl <- read.csv(file = "./data/riluzole_cleaned.csv")
treatment_bl <- read.csv(file = "./data/treatment_cleaned.csv")
ALSFRS_bl <- read.csv(file = "./data/ALSFRS_cleaned.csv")
demographic_bl <- read.csv(file = "./data/demographics_cleaned.csv")
labs_bl <- read.csv(file = "./data/labs_cleaned.csv")
fvc_bl <- read.csv(file = "./data/fvc_cleaned.csv")
svc_bl <- read.csv(file = "./data/svc_cleaned.csv")
vs_bl <- read.csv(file = "./data/vs_cleaned.csv")
alshistory_bl <- read.csv(file = "./data/alshistory_cleaned.csv")

########################################

#Joining datasets via piping
adbl_proact <- full_join(x=ALSFRS_bl, y=riluzole_bl, by = "patientID")
adbl_proact <- adbl_proact %>%
  full_join(treatment_bl, by = "patientID") %>%
  full_join(demographic_bl, by = "patientID") %>%
  full_join(labs_bl, by = "patientID") %>%
  full_join(fvc_bl, by = "patientID") %>%
  full_join(svc_bl, by = "patientID") %>%
  full_join(vs_bl, by = "patientID") %>%
  full_join(alshistory_bl, by = "patientID")

head(adbl_proact) #checking info

#Moving some columns around for better readability
adbl_proact <- adbl_proact %>% relocate(f_gastronomy, .before = f_riluzole)
adbl_proact <- adbl_proact %>% relocate(bmi_bl, .before = ck_bl)

#Checking for NAs in some key columns
sum(is.na(adbl_proact$f_riluzole)) #2007 NA
sum(is.na(adbl_proact$f_placebo)) #1064 NA
sum(is.na(adbl_proact$fvc_bl)) #3378 NA
sum(is.na(adbl_proact$svc_bl)) #10010 NA
sum(is.na(adbl_proact$bmi_bl)) #7042 NA
sum(is.na(adbl_proact$sex_bl)) #4816 NA
sum(is.na(adbl_proact$age_bl)) #4816 NA
sum(is.na(adbl_proact$onset_days_bl)) #3827 NA
sum(is.na(adbl_proact$f_gastronomy)) #7684 NA
sum(is.na(adbl_proact$ck_bl)) #5214 NA

#Checking for class
#all are a form of numeric, good
class(adbl_proact$f_riluzole) #integer
class(adbl_proact$f_placebo) #integer
class(adbl_proact$fvc_bl) #numeric
class(adbl_proact$svc_bl) #numeric
class(adbl_proact$bmi_bl) #numeric
class(adbl_proact$sex_bl) #integer
class(adbl_proact$age_bl) #numeric
class(adbl_proact$onset_days_bl) #integer
class(adbl_proact$f_gastronomy) #integer
class(adbl_proact$resp_subscore_bl) #numeric
class(adbl_proact$ck_bl) #numeric
class(adbl_proact$creatinine_bl) #numeric
class(adbl_proact$phosphorous_bl) #numeric
class(adbl_proact$q1_bl) #numeric
class(adbl_proact$q2_bl) #numeric
class(adbl_proact$q3_bl) #numeric
class(adbl_proact$q4_bl) #numeric
class(adbl_proact$q5_bl) #numeric
class(adbl_proact$q6_bl) #numeric
class(adbl_proact$q7_bl) #numeric
class(adbl_proact$q8_bl) #numeric
class(adbl_proact$q9_bl) #numeric

#Checking some for type
typeof(adbl_proact$f_riluzole) #integer
typeof(adbl_proact$ck_bl) #double
typeof(adbl_proact$f_placebo) #integer
typeof(adbl_proact$fvc_bl) #double
typeof(adbl_proact$svc_bl) #double
typeof(adbl_proact$bmi_bl) #double
typeof(adbl_proact$sex_bl) #double
typeof(adbl_proact$age_bl) #double
typeof(adbl_proact$onset_days_bl) #double
typeof(adbl_proact$f_gastronomy) #integer

#Removing NAs and missing spaces from demographic info:
#Step 1
adbl_proact$onset_days_bl[is.na(adbl_proact$onset_days_bl)] <- 9999
adbl_proact$age_bl[is.na(adbl_proact$age_bl)] <- 9999
adbl_proact$sex_bl[is.na(adbl_proact$sex_bl)] <- 9999

adbl_proact$onset_days_bl[adbl_proact$onset_days_bl==""] <- 9999
adbl_proact$sex_bl[adbl_proact$sex_bl==""] <- 9999
adbl_proact$age_bl[adbl_proact$age_bl==""] <- 9999

#Step 2 - filter out the 9999s correlating to NAs and missing spaces
adbl_proact <- adbl_proact %>%
  filter(onset_days_bl < 15) #also ensuring no days beyond the baseline slipped through

adbl_proact <- adbl_proact %>%
  filter(!sex_bl == 9999)

adbl_proact <- adbl_proact %>%
  filter(age_bl < 9999)

#Step 3 - check
sum(is.na(adbl_proact$sex_bl)) #0 NA
sum(is.na(adbl_proact$age_bl)) #0 NA
sum(is.na(adbl_proact$onset_days_bl)) #0 NA

#Checking no repeated patients 
dim(adbl_proact)[1] == length(unique(adbl_proact$patientID)) #No repeated patients 

#######################################

#Saving as .rda:
save(adbl_proact, file="adbl_proact.rda")

#Saving as .csv:
write.csv(adbl_proact, file = "./data/adbl_proact.csv",
          row.names = FALSE)
