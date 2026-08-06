#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Exploring the prescribing data for study to identify drugs in chapters endocrine, gynaecology and cardiovascular. 1 year look back applied for all drugs and prescription must be issued within a year prior index date to be defined as present.


#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(lubridate)

#--------------------------------------------------------------------------------------------
# load study population

load("~study.RData")

#open prescription data
setwd("")
pis <-read.delim("prescription_data.tsv")


# only need ppid and index to link with PIS
study_ID <- study %>% 
  select(ppid, index_date) %>% 
  distinct()


# identify only chapters of interest in prescription data then tidying data to only required information
table(pis$PI.BNF.Chapter.Description)


#(1) gynae drugs
gynae_drugs <- pis %>% 
  filter(PI.BNF.Chapter.Code == 7)

gynae_drugs <- gynae_drugs %>% 
  select(ppid, Paid.Date, PI.BNF.Item.Description, PI.BNF.Root.Drug.Description, PI.Approved.Name, PI.Drug.Formulation, PI.Prescribable.Item.Name) %>% 
  distinct()

#link to study_IDs

study_gynae <- study_ID_PIS %>% 
  left_join(gynae_drugs, by = "ppid")


# check if any non oral formulations
table(study_gynae$PI.Drug.Formulation) # includes all formulations

#paid date to correct format. Keep for now as may use to define regular drug use
study_gynae <- study_gynae %>% 
  mutate(paid_date = as.Date(Paid.Date)) %>% 
  select(-c(Paid.Date))

length(unique(study_gynae$ppid)) #  correct number of people

#Include all drugs used within study period. Once decided which drugs to include as confounders then apply filters for paid date within 12 months of index date.


#(2) endocrine drugs
endo_drugs <- pis %>% 
  filter(PI.BNF.Chapter.Code == 6)

endo_drugs <- endo_drugs %>% 
  select(ppid, Paid.Date, PI.BNF.Item.Description, PI.BNF.Root.Drug.Description, PI.Approved.Name, PI.Drug.Formulation, PI.Prescribable.Item.Name)

#link to study_ID_PIS

study_endo <- study_ID_PIS %>% 
  left_join(endo_drugs, by = "ppid")

# check if any non oral formulations
table(study_endo$PI.Drug.Formulation) # includes all formulations

#tidy dataset
study_endo <- study_endo %>% 
  mutate(paid_date = as.Date(Paid.Date)) %>% 
  select(-c(Paid.Date))

#----------------------------------------------------------------------------------------
# tidy environment
rm(endo_drugs)

#(2) cardiac drugs
cardio_drugs <- pis %>% 
  filter(PI.BNF.Chapter.Code == 2)

cardio_drugs <- cardio_drugs %>% 
  select(ppid, Paid.Date, PI.BNF.Item.Description, PI.BNF.Root.Drug.Description, PI.Approved.Name, PI.Drug.Formulation, PI.Prescribable.Item.Name)


#link to study_ID_PIS

study_cardio <- study_ID_PIS %>% 
  left_join(cardio_drugs, by = "ppid")


# check if any non oral formulations
table(study_cardio$PI.Drug.Formulation) # includes all formulations

#tidy dataset
study_cardio <- study_cardio %>% 
  mutate(paid_date = as.Date(Paid.Date)) %>% 
  select(-c(Paid.Date))

length(unique(study_cardio$ppid)) #  correct number of people


#save datasets of tidied drugs - gynae, endo and cardiac

#prior gynae drugs - save Rdata set
save(study_gynae, file = "study_gynae_drugs.RData")

#save csv file of names of drugs to decide which drugs to flag as confounders. Only need those who had drug prescription so removed NA for paid date

list_gynae_drugs <- study_gynae %>% 
  filter(!is.na(paid_date))

list_gynae_drugs <- list_gynae_drugs %>% 
  group_by(PI.BNF.Root.Drug.Description, 
           PI.BNF.Item.Description, PI.Approved.Name) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

write.csv(list_gynae_drugs, file = "study_all_gynae_drugs_types.csv")

#Endocrine drugs
save(study_endo, file = "study_endo_drugs.RData")

list_endo_drugs <- study_endo %>% 
  filter(!is.na(paid_date))

list_endo_drugs <- list_endo_drugs %>% 
  group_by(PI.BNF.Root.Drug.Description, 
           PI.BNF.Item.Description, PI.Approved.Name) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

write.csv(list_endo_drugs, file = "study_all_endo_drugs_types.csv")

# Cardiac drugs
save(study_cardio, file = "study_cardio_drugs.RData")

list_cardiac_drugs <- studyA_cardio %>% 
  filter(!is.na(paid_date))

list_cardiac_drugs <- list_cardiac_drugs %>% 
  group_by(PI.BNF.Root.Drug.Description, 
           PI.BNF.Item.Description, PI.Approved.Name) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))


write.csv(list_cardiac_drugs, file = "study_all_cardio_drugs_types.csv")


