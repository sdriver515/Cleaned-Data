title: "Final Project: Endpoint Analysis"
author: "Sarah Driver"
date: "`r Sys.Date()`"
output: pdf_document
urlcolor: "blue"


#Load library
library(tidyverse)

########################################I set my working directory at the start of this project in the console

#Loading all data
#I set my working directory at the start of this in the console
ALSFRS_data <- read.csv(file = "./data/alsfrs.csv") #should be good for the reader to access
PROACT_data <- read.csv(file = "./data/PROACT-El-Escorial-criteria-20200901.csv") #I use this to check naming conventions

#Checking things out
head(ALSFRS_data, 10)
head(PROACT_data, 10) #delta seems to refer to days here

#Fixing Q5 Gastronomy:
#If q5A is non-null, assign 0; if q5B is non-null, assign 1
unique(ALSFRS_data$Q5a_Cutting_without_Gastrostomy) #says: 2.0 1.5 1.0 3.0 4.0 NA 3.5 0.0 2.5 0.5

#Setting up a column for baseline later
ALSFRS_data <- ALSFRS_data %>%
  mutate(q5_bl = coalesce(Q5a_Cutting_without_Gastrostomy,Q5b_Cutting_with_Gastrostomy))

ALSFRS_data$Q5a_Cutting_without_Gastrostomy[is.na(ALSFRS_data$Q5a_Cutting_without_Gastrostomy)] <- 9999 #replaces NA with 9999 because NA was giving me trouble
ALSFRS_data1 <- ALSFRS_data %>%
  dplyr::mutate(Q5a_Cutting_without_Gastrostomy = ifelse(Q5a_Cutting_without_Gastrostomy < 9999, 0, NA)) #then making anything less than 9999 equal to 0 and 9999 equal to NA

ALSFRS_data1$Q5b_Cutting_with_Gastrostomy[is.na(ALSFRS_data1$Q5b_Cutting_with_Gastrostomy)] <- 9999 #I am doing the same process as above except for a different column
ALSFRS_data1 <- ALSFRS_data1 %>%
  dplyr::mutate(Q5b_Cutting_with_Gastrostomy = ifelse(Q5b_Cutting_with_Gastrostomy < 9999, 1, NA)) #again, sorting and then getting rid of the 9999s so they won't mess things up

view(ALSFRS_data1) #making sure this worked: looks good, no random 9999s left

#Merging columns without combining into one cell
ALSFRS_data1 <- ALSFRS_data1 %>%
  mutate(Q5 = coalesce(Q5a_Cutting_without_Gastrostomy,Q5b_Cutting_with_Gastrostomy))

#Moving Q5 and q5_bl to the correct spots for better readability
ALSFRS_data1 <- ALSFRS_data1 %>% relocate(Q5, .before = Q6_Dressing_and_Hygiene)
ALSFRS_data1 <- ALSFRS_data1 %>% relocate(q5_bl, .before = Q5)

#Dropping what I don't need
ALSFRS_data1 <- select (ALSFRS_data1,-c(Q5a_Cutting_without_Gastrostomy,Q5b_Cutting_with_Gastrostomy))

#Checking for missing value totals, to see if anything looks very off:
sum(is.na (ALSFRS_data1$Q1_Speech)) #says there are 1303 NA
sum(is.na (ALSFRS_data1$Q2_Salivation)) #1305 NA
sum(is.na (ALSFRS_data1$Q3_Swallowing)) #1306 NA
sum(is.na (ALSFRS_data1$Q4_Handwriting)) #1308 NA
sum(is.na (ALSFRS_data1$Q5)) #1327 NA
sum(is.na (ALSFRS_data1$q5_bl)) #1327 NA
sum(is.na (ALSFRS_data1$Q6_Dressing_and_Hygiene)) #1309 NA
sum(is.na (ALSFRS_data1$Q7_Turning_in_Bed)) #1313 NA
sum(is.na (ALSFRS_data1$Q8_Walking)) #1309 NA
sum(is.na (ALSFRS_data1$Q9_Climbing_Stairs)) #1307 NA
sum(is.na (ALSFRS_data1$Q10_Respiratory)) #24,391 NA
sum(is.na (ALSFRS_data1$R_1_Dyspnea)) #30,570 NA
sum(is.na (ALSFRS_data1$R_2_Orthopnea)) #30,570 NA
sum(is.na (ALSFRS_data1$R_3_Respiratory_Insufficiency)) #30,568
#they overlap in ways that will get the data close to the correct 55,503 number

#Converting scores:
ALSFRS_data1 <- ALSFRS_data1 %>%
  mutate(ALSFRS_old_score_computed = Q1_Speech + Q2_Salivation + Q3_Swallowing +
           Q4_Handwriting + Q5 + Q6_Dressing_and_Hygiene +
           Q7_Turning_in_Bed + Q8_Walking + Q9_Climbing_Stairs +
           (3*Q10_Respiratory)) #adding and multiplying everything into a new column

unique(ALSFRS_data1$ALSFRS_old_score_computed) #looks to have the right range of numbers: does not go over 48

#Checking the data for overlapping scores as part of analysis
ALSFRS_data1$Overlapping_Scores <- ifelse(ALSFRS_data1$ALSFRS_old_score_computed==ALSFRS_data1$ALSFRS_R_Total,"Overlapping","Not_Overlapping") #labeling if they're equal to each other
table(ALSFRS_data1$Overlapping_Scores) #table() writes everything out for me to view: 462 overlapping

#Combining Scoring Columns:
ALSFRS_data1 <- ALSFRS_data1 %>%
  mutate(alsfrsr_derv = coalesce(ALSFRS_old_score_computed,ALSFRS_R_Total)) #combines the two columns into a new one

#Renaming ALSFRS_Delta to t because delta refers to days in the PROACT_data, aka time, and there is no column called t otherwise
#Also renaming other variables for later use
ALSFRS_data1 <- ALSFRS_data1 %>%
  rename(t = ALSFRS_Delta,
         q1_bl = Q1_Speech,
         q2_bl = Q2_Salivation,
         q3_bl = Q3_Swallowing,
         q4_bl = Q4_Handwriting,
         q6_bl = Q6_Dressing_and_Hygiene,
         q7_bl = Q7_Turning_in_Bed,
         q8_bl = Q8_Walking,
         q9_bl = Q9_Climbing_Stairs)

unique(ALSFRS_data1$t) #there are some negative values

#If t is more than 0, label "keep_time" in the (new) column filtered_times. If not, label "remove" in the new column
ALSFRS_data1$filtered_times <- ifelse(ALSFRS_data1$t > 0,"keep_time","remove")

#Filter by the new category and don't delete everything
ALSFRS_data1 <- filter(ALSFRS_data1, filtered_times != "remove"| is.na(filtered_times))

#Selecting which columns are needed and put into adept_proact:
adep_proact <- ALSFRS_data1 %>% dplyr::select(subject_id, alsfrsr_derv, t)
head(adep_proact, 10) #checking

#Adding columns for later baseline dataframe:
ALSFRS_data1 <- ALSFRS_data1 %>%
  mutate(resp_subscore_bl = ifelse((!is.na(R_1_Dyspnea + R_2_Orthopnea + R_3_Respiratory_Insufficiency)), (R_1_Dyspnea + R_2_Orthopnea + R_3_Respiratory_Insufficiency), (3*Q10_Respiratory)))

#Checking NAs and then removing:
sum(is.na (adep_proact$alsfrsr_derv)) #this sums up how many NA are in the specific column: 50
sum(is.na (adep_proact)) #238 NAs
adep_proact <- na.omit(adep_proact) #removing the NAs gives me: 55,503 observations

#Saving as .rda:
save(adep_proact, file="adep_proact.rda")

#Saving adep_proact as .csv:
setwd("~/Documents/American_University_Various_Things/STAT_412/project")
write.csv(adep_proact, file = "./data/adep_proact.csv",
          row.names = FALSE)

#For use in baseline datasets:
write.csv(ALSFRS_data1, file = "./data/ALSFRS_data1.csv",
          row.names = FALSE)













