#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Links the study  population to GP read codes to apply exclusions for CVD use the HDR UK Phenotypes based on paper by Kuan (2019) A chronological map of 308 physical and mental health conditions from 4 million individuals in the English National Health Service. Lancet Digit Health 1, e63-e77.

# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)
#------------------------------------------------------------------------------#
## Open datasets on study population SMR CVD exclusions and outcome from ICD codes
load("Study_with_ICD_CVD_exclusions_and_outcomes.RData")

#Open dataset study A including everyone with CVD
load("study_prior_to_cardio_exclusions.RData")

#set working directory
setwd("")

#Open GP Read codes for phenotypes
gp_regC <-read.delim(".tsv")
glimpse(gp_regC)


cardio_pheno <- gp_regC %>% 
  filter(PhenotypeGroup == "Cardiovascular") %>% 
  mutate(event_date = as.Date(EventDate))

#following script saves the cardiovascular phenotypes for exploration go to line 62 for application of exclusions and outcomes.

#check what categories are included
gp_cardiac_events <- cardio_pheno %>% 
  group_by(PhenotypeName, CodeDesc) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

#save this dataset and identify phenotypes to be used in the study


#----------------------------------------------------------------------------------------#

#(2) after exploration of cardiovascular phenotypeGroup following phenotypeNames to be included for exclusions and then investigated as outcomes - these correspond to ICD codes

#Stable angina 
#Coronary heart disease not otherwise specified
#Myocardial infarction
#Unstable Angina
#Transient ischaemic attack
#Stroke NOS
#Subarachnoid haemorrhage
#Ischaemic stroke
#Intracerebral haemorrhage
#Subdural haematoma - nontraumatic

#identify the following codes


IHD <- c("Stable angina", "Coronary heart disease not otherwise specified",
         "Myocardial infarction","Unstable Angina")

stroke <- c("Stroke NOS","Subarachnoid haemorrhage","Ischaemic stroke","Intracerebral haemorrhage","Subdural haematoma - nontraumatic")

tia <- "Transient ischaemic attack"

#tidy gp records keeping columns of interest in correct format

cardio_pheno <- cardio_pheno %>% 
  select(ppid, event_date, PhenotypeName, FullReadCode, CodeDesc) %>% 
  rename(cvd_date = "event_date",
         cvd = "CodeDesc")

cvd_event_flag <- cardio_pheno %>% 
  mutate(
    cvd_event = case_when(
      PhenotypeName %in% IHD ~ "ihd_gp",
      PhenotypeName %in% stroke ~ "stroke_gp",
      PhenotypeName %in% tia ~ "tia_gp",
      TRUE ~ NA_character_
    ) 
  )

table(cvd_event_flag$cvd_event)

#only keep those with primary outcomes of interest. 

cvd_group <- cvd_event_flag %>% 
  filter(!is.na(cvd_event))

table(cvd_group$cvd_event) # same number of events
#save these codes for manuscript

read_codes_prior_cvd <- cvd_group %>% 
  select(FullReadCode, PhenotypeName, cvd) %>% 
  distinct()

read_codes_prior_cvd <- read_codes_prior_cvd %>% 
  rename(CodeDesc = cvd)

write.csv(read_codes_prior_cvd, file = "/GP_read_codes_prior_CVD.csv")

# check the group with NA's in cvd_events to make sure no events have been missed. 

unselected_cvd <- cvd_event_flag %>% 
  filter(is.na(cvd_event))

table(unselected_cvd$PhenotypeName) # no phenotypes here that should be in the cvd_group

#examine the cvd group in GP records

length(unique(cvd_group$ppid)) #number of people included

#check how many people have more than one cvd_date
Multiple_cvd_date <- cvd_group %>% 
  group_by(ppid) %>% 
  summarise(n_cvd_dates = n_distinct(cvd_date), .groups = "drop") %>% 
  summarise(n_people_more_than_one_event = sum (n_cvd_dates > 1))


# for each person identify the first index date and keep the event for this first date only.
cvd_earliest_date <- cvd_group %>% 
  group_by(ppid) %>% 
  slice(which.min(cvd_date))

# using slice we have only kept one cvd_date for each person

#--------------------------------------------------------------------------------------#
# tidy environment

rm(cvd_event_flag, cardio_pheno,  cvd_group, Multiple_cvd_date, unselected_cvd, gp_regC)
# (3) join to study which includes everyone with cardiovascular disease. 
# (i) identify those with prior cvd from GP records. see if any of these ID numbers remain in the study with ICD exclusions - exclude these and record their diagnoses for the flowchart. save this dataset with all cvd exclusions for primary outcomes


study <- study_with_CVD %>% 
  left_join(cvd_earliest_date, by = "ppid", relationship = "many-to-many")

#check length is the same
length(unique(studyA$ppid)) #

# remove those whose cvd_date is on or before the index date and examine them - these will only be from GP records

excluded_gp <- studyA %>% 
  filter(cvd_date <= index_date)

excluded_conditions <- excluded_gp %>% 
  distinct(ppid, cvd_event) %>% 
  group_by(cvd_event) %>% 
  summarise(count = n())


print(excluded_conditions)

length(unique(excluded_gp$ppid)) #

# Some of these people may already be excluded from the study with icd exclusions.

ID_not_excluded <- study_icd_exclusions_outcomes %>% 
  filter(ppid %in% excluded_gp$ppid)

View(ID_not_excluded)
length(unique(ID_not_excluded$ppid)) #number still remaining which will need to be removed

ID_overlap <- intersect(excluded_gp$ppid, studyA_icd_exclusions_outcomes$ppid) #  another way to check to make sure correct number excluded from GP records

Additional_exclusions <- excluded_gp %>% 
  filter(ppid %in% ID_not_excluded$ppid)


# check diagnosese for flowchart of exclusions
excluded_conditions <- Additional_exclusions %>% 
  distinct(ppid, cvd_event) %>% 
  group_by(cvd_event) %>% 
  summarise(count = n())


#save dataset of those remaining who have had all exclusions but only include ICD codes for outcomes - main analysis primary outcome

study_all_exclusions <- study_icd_exclusions_outcomes %>% 
  filter(!ppid %in% ID_not_excluded$ppid)

length(unique(study_all_exclusions$ppid)) #total number in study

save(study_all_exclusions, file = "Study_without_CVD_ICD_outcomes.RData")