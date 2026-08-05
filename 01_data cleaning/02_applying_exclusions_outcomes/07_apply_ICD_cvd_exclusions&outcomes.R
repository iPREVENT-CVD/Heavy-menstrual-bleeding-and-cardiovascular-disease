#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease


#Tidy SMR_CP this is SMR01 with ICD codes and HDR UK Phenotypes based on Kuan (2019) A chronological map of 308 physical and mental health conditions from 4 million individuals in the English National Health Service. Lancet Digit Health 1, e63-e77.
# exclude those with icd 10 codes prior to index date.
# keep icd 10 primary outcomes for positions 1 and 2 using the earliest date
# Use the combined exposed and comparison groups that have had exclusions for surgical procedures. This is the study population prior to excluding CVD.
#Exclude SMR CVD and identify ICD code outcomes in position 1 and 2

#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)
#------------------------------------------------------------------------------#
## Open datasets on study exposed and comparison populations and ICD/OPCS codes for phenotypes

load("study_prior_to_cardio_exclusions.RData")

#set working directory
setwd("")

SMR_CP <- read.delim("SMR_CP.tsv") # this is SMR01

#Check the structure of variable in Code, removing blank spaces and checking for decimals

SMR_CP <- SMR_CP %>% 
  mutate(code_trimmed = str_trim(Code))

# check whether there are any . in the codes
check_decimals_CP <- SMR_CP %>% 
  mutate(has_dot = grepl("\\.", code_trimmed))

table(check_decimals_CP$has_dot) #no decimal places

# using CALIBER phenotypes Kuan (2019) so filter only for cardiovascular phenotypes 
cardio_pheno <- SMR_CP %>% 
  filter(PhenotypeGroup == "Cardiovascular") %>% 
  mutate(event_date = as.Date(EventDate))

# Explore the phenotypes included in the HDR UK cardiovascular disease
types_cardiac_events <- cardio_pheno %>% 
  group_by(PhenotypeName, CodeDesc) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

#There are conditions which are not relevant to this study included. Only include CVD conditions of interest using icd 10 codes. In exploratory data cleaning no revascularisation codes were identified. Check again before proceeding

#######---------------------------------------------------------------------------------#
#(2) look for revascularisation events
#Operation codes look for  OPCS (1) carotid angiplasty L311, CABG K40-46, percutaneous transluminal coronary angioplasty K75, carotid endarterectomy L295

revasc <- SMR_CP %>% 
  filter(grepl("^L311|^K4[0-6]|K75|L295).*", code_trimmed))

table(revasc$PhenotypeName) # this identified automimmune liver disease and fatty liver only

op_codes <- SMR_CP %>% 
  filter(grepl("^L.*", code_trimmed))

#check codes beginning with L
table(op_codes$PhenotypeName) # only renal disease, peripheral arterial disease and psoriasis

#check OPCS
surgical_codes_CP <- SMR_CP %>% 
  filter(CodeType == "OPCS4")

surgical_codes_frequency_CP <- surgical_codes_CP %>% 
  group_by(code_trimmed) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

#check all codes beginning with L again in OPCS

surgical_codes_L <- surgical_codes_CP %>% 
  filter(grepl("^L.*", code_trimmed)) # no L295 or L311

surgical_codes_K <- surgical_codes_CP %>% 
  filter(grepl("^K.*", code_trimmed)) # 0 codes beginning with K

# no  carotid angiplasty L311, CABG K40-46, percutaneous transluminal coronary angioplasty K75, carotid endarterectomy L295


#------------------------------------------------------------------------------#
#tidy environment
rm(surgical_codes_CP, surgical_codes_frequency_CP, revasc, op_codes, surgical_codes_K, surgical_codes_L, check_decimals_CP)

# (3) Identify CVD outcomes from ICD codes using CALIBER codes
#(i) CHD - includes PhenotypeNames of myocardial infarction, CHD not specified and angina. Which include ICD codes I20-25 - so easiest to use codes rather than Phenotype names
#(ii) TIA ICD codes G45 plus G46.0- G46.2, I65-66
#(iii) Stroke I60-64, I67-69, G46.3-46.8 
#

cardio_cvd_flags <- cardio_pheno %>% 
  mutate(
    cvd_event = case_when(
      grepl("^((I(6[0-4]|6[7-9]))|(G4(6[3-8]))).*", code_trimmed) ~ "stroke",
      grepl("^((G45)|(G46[0-2])|(I(6[5-6]))).*", code_trimmed) ~ "tia",
      grepl("^I(2[0-5]).*", code_trimmed) ~ "ihd",
      TRUE ~ NA_character_
    )
  )

table(cardio_cvd_flags$cvd_event)

#only keep those with primary outcomes of interest. 

cvd_group <- cardio_cvd_flags %>% 
  filter(!is.na(cvd_event))

table(cvd_group$cvd_event) # same number of events

# check the group with NA's in cvd_events to make sure no events have been missed. 

unselected_cvd <- cardio_cvd_flags %>% 
  filter(is.na(cvd_event))

table(unselected_cvd$PhenotypeName) # no phenotypes here that should be in the cvd_group

#tidy the cvd group with primary outcomes to only include ppid, event date, Position, cvd_event, code_trimmed, CodeDesc #rename event date to cvd_date and code_trimmed to code, CodeDesc to cvd

cvd_group <- cvd_group %>% 
  select(ppid, event_date, Position, cvd_event, code_trimmed, CodeDesc) %>% 
  rename(cvd_date = "event_date",
         code = "code_trimmed",
         cvd = "CodeDesc",
         position = "Position")

length(unique(cvd_group$ppid)) # check number of events prior to linkage to study population

#check how many people have more than one cvd_date
Multiple_cvd_date <- cvd_group %>% 
  group_by(ppid) %>% 
  summarise(n_cvd_dates = n_distinct(cvd_date), .groups = "drop") %>% 
  summarise(n_people_more_than_one_event = sum (n_cvd_dates > 1))


save(cvd_group, file = "SMR_ICD_CVD_exclusions_and_outcomes.RData")

#------------------------------------------------------------------------------------------#
#(4) To exclude those that had CVD prior to index date for those that have more than one event use the earliest date and keep this date. This date will be used to exclude those that had CVD coded prior to index date

# tidy environment

rm(cardio_cvd_flags, cardio_pheno, Multiple_cvd_date, unselected_cvd,SMR_CP)

cvd_earliest_date <- cvd_group %>% 
  group_by(ppid) %>% 
  slice(which.min(cvd_date))

#use index date and ppid from study population to exclude those with prior CVD using earliest date.

study_ID <- study_prior_to_cardio_exclusions %>% 
  select(ppid, index_date) %>% 
  distinct()

#join the CVD_earliest date to study  ID

study_ICD_cvd <- study_ID %>% 
  left_join(cvd_earliest_date, by = "ppid")

#check length is the same
length(unique(study_ICD_cvd$ppid)) 

# remove those whose cvd_date is on or before the index date and examine

ICD_excluded <- study_ICD_cvd %>% 
  filter(cvd_date <= index_date)

excluded_conditions <- ICD_excluded %>% 
  distinct(ppid, cvd_event) %>% 
  group_by(cvd_event) %>% 
  summarise(count = n())


print(excluded_conditions)

length(unique(ICD_excluded$ppid)) 

# exclude these ID's from the main study dataset and from the cvd_group dataset.

study_icd_exclusions <- study_prior_to_cardio_exclusions %>% 
  filter(!ppid %in% ICD_excluded$ppid)

cvd_group_with_exclusions <- cvd_group %>% 
  filter(!ppid %in% ICD_excluded$ppid)

length(unique(study_icd_exclusions$ppid)) 
length(unique(cvd_group_with_exclusions$ppid)) 

#-------------------------------------------------------------------------------------#
# (5) Now create ICD outcomes for the dataset with CVD then join this to the study population

#tidy environment. 
rm(ICD_excluded, excluded_conditions, study_ICD_cvd, study_with_CVD, cvd_earliest_date, cvd_group)


#For the cvd_group which has had those with exclusions removed. For ICD outcomes only use positions 1 and 2. Those with other positions will not be included

cvd_group_with_exclusions <- cvd_group_with_exclusions %>% 
  filter(position %in% c(1,2))

table(cvd_group_with_exclusions$position)
length(unique(cvd_group_with_exclusions$ppid)) #

#Check number with more than one cvd_date
Multiple_cvd_date <- cvd_group_with_exclusions %>% 
  group_by(ppid) %>% 
  summarise(n_cvd_dates = n_distinct(cvd_date), .groups = "drop") %>% 
  summarise(n_people_more_than_one_event = sum (n_cvd_dates > 1))

#X have more than one date. Keep the earliest date. 

cvd_outcomes_earliest_date <- cvd_group_with_exclusions %>% 
  group_by(ppid) %>% 
  slice(which.min(cvd_date))

#cvd codes and position not required so remove

cvd_outcomes_earliest_date <- cvd_outcomes_earliest_date %>% 
  select(-c(code, position)) %>% 
  distinct()

#join to study with ICD exclusion

study_icd_exclusions_outcomes <- study_icd_exclusions %>% 
  left_join(cvd_outcomes_earliest_date, by = "ppid")

length(unique(study_icd_exclusions_outcomes$ppid)) 

save(study_icd_exclusions_outcomes, file = "Study_with_ICD_CVD_exclusions_and_outcomes.RData")

#---------------------------------------------------------------------------#

