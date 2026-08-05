#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Anti-hypertensive and lipid lowering drugs
#(1) keep record of those that had paid date prior to index date and within 1 year. 
#(2) identify those who did not have a previous prescription of anti-hypertensive or lipid lowering agent but then received a new prescription after the index date. These will be included within the composite outcomes.

#-------------------------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


#open drugs in cardiology chapter found for study 
load("study_cardio_drugs.RData")

#change headings into lower case and then filter using BNF. Root descriptions identified in 
study_cardio <- study_cardio %>% 
  rename_with(~ tolower(.x))

# These are the BNF Root Descriptions for lipid lowering and anti-hypertensive drugs.  HTN drug will be used in composite HTN definition and both will be assessed as potential outcomes

#SIMVASTATIN
#ATORVASTATIN
#COLESEVELAM HYDROCHLORIDE
#COLESTYRAMINE
#ROSUVASTATIN CALCIUM
#EZETIMIBE
#FENOFIBRATE
#PRAVASTATIN SODIUM
#FLUVASTATIN SODIUM
#BEZAFIBRATE
#COLESTIPOL HYDROCHLORIDE

lipid_lowering <- c("SIMVASTATIN", "ATORVASTATIN", "ROSUVASTATIN CALCIUM", "PRAVASTATIN SODIUM", "FLUVASTATIN SODIUM", "COLESEVELAM HYDROCHLORIDE", "COLESTYRAMINE", "EZETIMIBE", "FENOFIBRATE", "BEZAFIBRATE", "COLESTIPOL HYDROCHLORIDE")

anti_htn <- c("ATENOLOL", "BISOPROLOL FUMARATE","RAMIPRIL", "LABETALOL HYDROCHLORIDE", "OXPRENOLOL HYDROCHLORIDE", "METOPROLOL TARTRATE", "TIMOLOL", "NEBIVOLOL", "BENDROFLUMETHIAZIDE", "BUMETANIDE", "CHLORTALIDONE", "AMLODIPINE", "NIFEDIPINE", "LERCANIDIPINE HYDROCHLORIDE", "FELODIPINE", "INDAPAMIDE", "VERAPAMIL HYDROCHLORIDE", "DILTIAZEM HYDROCHLORIDE", "LISINOPRIL", "CANDESARTAN CILEXETIL",  "ENALAPRIL MALEATE", "LOSARTAN POTASSIUM",  "TELMISARTAN", "VALSARTAN","PERINDOPRIL ERBUMINE", "IRBESARTAN" , "RAMIPRIL", "LOSARTAN POTASSIUM WITH DIURETIC", "LISINOPRIL WITH DIURETIC" , "VALSARTAN WITH DIURETIC", "ENALAPRIL MALEATE WITH DIURETIC", "DOXAZOSIN MESILATE","METHYLDOPA", "MOXONIDINE")

#check timolol tabs not eye drops
timolol <- studyA_cardio %>% 
  filter(pi.bnf.root.drug.description == "TIMOLOL") # tablets so include

#------------------------------------------------------------------------------------------#
#Code below for individual drugs

#betablockers <-c("ATENOLOL", "BISOPROLOL FUMARATE","RAMIPRIL", "LABETALOL HYDROCHLORIDE", "OXPRENOLOL HYDROCHLORIDE", "METOPROLOL TARTRATE", "TIMOLOL", "NEBIVOLOL", "CARVEDILOL" )

#diuretics <- c("BENDROFLUMETHIAZIDE", "BUMETANIDE", "CHLORTALIDONE")

#calcium_channel <- c("AMLODIPINE", "NIFEDIPINE", "LERCANIDIPINE HYDROCHLORIDE", "FELODIPINE", "INDAPAMIDE", "VERAPAMIL HYDROCHLORIDE", "DILTIAZEM HYDROCHLORIDE")

#ACE_ARB <- c("LISINOPRIL", "CANDESARTAN CILEXETIL",  "ENALAPRIL MALEATE", "LOSARTAN POTASSIUM",  "TELMISARTAN", "VALSARTAN","PERINDOPRIL ERBUMINE", "IRBESARTAN" , "RAMIPRIL" )

#renin_diuretic <- c("LOSARTAN POTASSIUM WITH DIURETIC", "LISINOPRIL WITH DIURETIC" , "VALSARTAN WITH DIURETIC", "ENALAPRIL MALEATE WITH DIURETIC")

#alpha_blockers <- c("DOXAZOSIN MESILATE", "PRAZOSIN HYDROCHLORIDE")

#alpha_agonist <- c("METHYLDOPA", "CLONIDINE HYDROCHLORIDE" )

#others <- c("SPIRONOLACTONE", "MINOXIDIL","MOXONIDINE")

#----------------------------------------------------------------------------------------#

#(1) create flags in dataset for each type of drug

cardio_drug_flag <- studyA_cardio %>% 
  mutate(
    cardio_drug = case_when(
      pi.bnf.root.drug.description %in% lipid_lowering~"lipid_lowering",
      pi.bnf.root.drug.description %in% anti_htn ~ "antihypertensive",
      TRUE ~ NA_character_
    ) 
  )

table(cardio_drug_flag$cardio_drug)

length(unique(cardio_drug_flag$ppid)) # 

# index date from 30 April 2010 for 1 year look back
cardio_drug_flag <- cardio_drug_flag %>% 
  filter(index_date >= "2010-04-30")

length(unique(cardio_drug_flag$ppid))

prior_cardio_1 <- cardio_drug_flag %>% 
  filter(!is.na(cardio_drug)) %>% 
  filter(paid_date <= index_date)

table(prior_cardio_1$cardio_drug) # the numbers will be smaller as these are people who had scripts before index date. An individual may have more than one drug type and multiple drugs within that drug group

any(is.na(prior_cardio_1$cardio_drug))

#only include those who paid date atleast 12m before index date,

prior_cardio <- prior_cardio_1 %>% 
  filter(between(paid_date, index_date - 365.25, index_date))

length(unique(prior_cardio$ppid)) #176


#tidy dataset with only ppid, index and cardiac drug included here will have atleast one paid date for that drug which was within 12 m of index date. 

prior_cardio_drugs <- prior_cardio %>% 
  select(ppid, cardio_drug) %>% 
  distinct() # 

# if anyone ever had at least one script of they are recorded here.

table(prior_cardio_drugs$cardio_drug) #

length(unique(prior_cardio_drugs$ppid)) #

#create lipid lowering and hypertensives column as will be used for descriptive purposes
prior_cardio_drugs <- prior_cardio_drugs %>% 
  mutate(value = "y") %>% 
  pivot_wider(
    id_cols = "ppid",
    names_from = cardio_drug,
    values_from = value,
    values_fill = "n"
  )

#add prior so able to see which people had drugs before index
prior_cardio_drugs <- prior_cardio_drugs %>% 
  rename(prior_anti_HTN = antihypertensive,
         prior_lipid_Rx = lipid_lowering)

#save in medications folder
save(prior_cardio_drugs, file = "study_cardio_drugs_prior_index.RData")

#----------------------------------------------------------------------------------------#
# identify how many people are prescribed lipid lowering medication and antihypertensives after index date.

#first check number who ever had prescription before index date
length(unique(prior_cardio_1$ppid)) # cardiac drugs at any time prior to index
#These people will need to be excluded when adding  anti-hypertensive or lipid lowering medication as an outcome.

study_ids_ever_prior_htn_drug <- prior_cardio_1 %>% 
  select(ppid, pi.bnf.root.drug.description) %>%
  distinct()

#save dataset of those with prior ever htn drugs to exclude
save(study_ids_ever_prior_htn_drug, file = "study_ids_ever_prior_htn_drug.RData")



# remove those who had paid date before index date for cardiac drugs (prior_cardio_1 - anyone who ever received cardiac drug) and remove those with paid dates from other drugs

cardio_drug_outcome <- cardio_drug_flag %>% 
  filter(!ppid %in% prior_cardio_1$ppid) %>% 
  filter(!is.na(cardio_drug))

length(unique(cardio_drug_outcome$ppid)) #

table(cardio_drug_outcome$cardio_drug)

#only select ppid, paid date, index date and cardio_drug, drug name

study_cardio_drug_outcome <- cardio_drug_outcome %>% 
  select(ppid, index_date, paid_date, cardio_drug) %>% 
  distinct()

#all paid dates should be after index date
check_dates <- study_cardio_drug_outcome %>% 
  filter(paid_date <= index_date) # 

#keep earliest paid date for either drugs to allow censoring as an outcome indicator
earliest_date <- study_cardio_drug_outcome %>% 
  group_by(ppid) %>% 
  slice(which.min(paid_date))

#keep paid date only to add the future_cardio_drug for censoring
drug_outcome_earliest_date <- earliest_date %>% 
  select(ppid, paid_date) %>% 
  distinct() %>% 
  rename(cardiac_drug_date = paid_date)

#create lipidlowering and antihypertensive with Y and N for descriptive analysis
#Now only need ppid and cardiac drug columns

cardiac_drugs <- study_cardio_drug_outcome %>% 
  select(ppid, cardio_drug) %>% 
  distinct()

cardiac_drugs <- cardiac_drugs %>% 
  mutate(value = "y") %>% 
  pivot_wider(
    id_cols = "ppid",
    names_from = cardio_drug,
    values_from = value,
    values_fill = "n")


#create third column for both medications as this will be used as an outcome and change medications to indicate after_index
study_cardiac_drugs <- cardiac_drugs %>% 
  mutate(future_cardio_drug = "y") %>% 
  rename(future_anti_HTN = antihypertensive,
         future_lipid_Rx = lipid_lowering)

#now join the earliest paid date for censoring
study_cardiac_drugs_outcome <- study_cardiac_drugs %>% 
  left_join(drug_outcome_earliest_date, by = "ppid")


View(study_cardiac_drugs_outcome)


#save dataset for descriptive summary
save(study_cardiac_drugs_outcome, file = "study_future_cardio_drugs.RData")

