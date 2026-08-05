#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease


# adding prescription data to study population



#(2) Add endo, gynae and cardiovacular drugs to create descriptive dataset for studyA-2 this will model HTN/lipid Rx as outcomes with and without all cause death
#(3) Include CHCP and create composite variable HTN.

#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(lubridate)

#--------------------------------------------------------------------------------------------
# load study  population

load("study.RData")

study <- study %>% 
  filter(index_date >= "2010-04-30")# check the index date is correct

length(unique(studyA_model2$ppid)) #

#-----------------------------------------------------------------------------------#
# Open gynae, endo drugs for description only.
load("/study_endo_descriptive_only.RData")

load("study_gynae_drugs_descriptive_only.RData")

#open Combined hormonal contraceptive pill to be used as confounder
load("study_chcp_covariates.RData")

#open prior cardio drugs to be used as description and to create a composite variable for hypertension
load("study_cardio_drugs_prior_index.RData")

#open future cardio drugs to be used a description and outcome.
load("/study_future_cardio_drugs.RData")


#join each to study population

study_model_gynae <- study_model2 %>% 
  left_join(prior_gynae_drugs, by = c("ppid"),
            relationship = "many-to-many")


study_model_gynae_endo <- study_model_gynae %>% 
  left_join(prior_endo_drugs, by = c("ppid"),
            relationship = "many-to-many")

study_model_all_prior_drugs <- study_model_gynae_endo %>% 
  left_join(prior_cardio_drugs, by = c("ppid"),
            relationship = "many-to-many")


study_model_all_drugs <- study_model_all_prior_drugs %>% 
  left_join(study_cardiac_drugs_outcome, by = c("ppid"),
            relationship = "many-to-many")

#check dataset ready for descriptive purposes
View(study_model_all_drugs)

#check any duplicated data

any(duplicated(study_model_all_drugs)) #

length(unique(study_model_all_drugs$ppid))


#tidy envirnoment



#join combined hormonal contraceptive pill as a confounder
study_complete_model <- study_model %>% 
  left_join(chcp_covariates, by = c("ppid"),
            relationship = "many-to-many")

#---------------------------------------------------------------------------------#
#create composite variable for hypertension (HTN)
#Will use composite definition for HTN of read codes and anti-hypertensive within 12M of index date.
#check how many people with anti_HTN drug have HTN read codes. For both Hypertension read codes and anti_htn medication there is either yes or no

study_complete_model <- study_complete_model %>% 
  mutate(HTN_check = case_when(
    Hypertension == "Y" & prior_anti_HTN == "y" ~ "both_coded",
    Hypertension == "N" & prior_anti_HTN == "y" ~ "drug_coded",
    Hypertension == "Y" & prior_anti_HTN == "n" ~ "read_coded",
    Hypertension == "N" & prior_anti_HTN == "n" ~ "not_coded",
    TRUE ~ NA_character_
  ))


#check number within each category
study_complete_model %>% 
  distinct(ppid, HTN_check) %>% 
  count(HTN_check, name = "number")

#using composite definition identifies more people than only using read codes

study_complete_model <- study_complete_model %>% 
  mutate(HTN_composite = case_when(
    HTN_check == "both_coded" ~ "y",
    HTN_check == "drug_coded" ~ "y",
    HTN_check == "read_coded" ~ "y",
    HTN_check == "not_coded" ~"n",
    TRUE ~ NA_character_
  ))
table(studyA2_complete_model$HTN_composite) 
table(studyA2_complete_model$Hypertension) #

#remove HTN check and surgical subgroup

study <- study_complete_model %>% 
  select(-c("HTN_check", "surgical_subgroup")) %>% 
  distinct()


#change the NA  to N
study <- study %>% 
  mutate(
    across(c("chcp", "future_cardio_drug", "prior_anti_HTN", "prior_lipid_Rx"),
           ~ replace(., is.na(.), "n"))
  )

glimpse(study)


save(study, file = ".RData")
