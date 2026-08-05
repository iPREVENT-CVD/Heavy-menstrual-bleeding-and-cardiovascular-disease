#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Steps 
#(1) link with gp_read codes using phenotypeNames as these are created by DataLoch based on CALIBER (HDR UK Phenotypes) using paper Kuan. 
#(2) identify these phenotypes: hypertension, obesity, diabetes and iron deficiency anaemia
#(3) identify people with read codes on or prior to index date and add label Y/N if present


#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)

#--------------------------------------------------------------------------------------------
# load study population and GP read codes

load("/Study_without_CVD_ICD_outcomes_death.RData")

#set working directory
setwd("")

#Open GP Read codes for phenotypes
gp_regC <-read.delim(".tsv")


#identify the groups of interest after tidying dataset

gp_codes <- gp_regC %>% 
  select(ppid, EventDate, FullReadCode,CodeDesc,PhenotypeName) %>% 
  mutate(covariate_date = as.Date(EventDate)) %>% 
  distinct()

covariate_codes <- gp_codes %>% 
  filter(PhenotypeName %in% c("Hypertension", "Obesity", 
                              "Diabetes", "Iron deficiency anaemia"))

table(covariate_codes$PhenotypeName)

#for each condition identify the earliest date coded as likely to be coded multiple times. Use this date to identify it as covariate occurring on or before index date.

#first check number of people with each covariate type

covariate_types <- covariate_codes %>% 
  group_by(PhenotypeName) %>% 
  summarise(count = n_distinct(ppid))

#now only keep the earliest date for every

covariates <- covariate_codes %>% 
  arrange(ppid, PhenotypeName, covariate_date) %>% 
  group_by(ppid, PhenotypeName) %>% 
  slice(1)

#check same number of covariates after slice to keep earliest event
covariate_types_1 <- covariates %>% 
  group_by(PhenotypeName) %>% 
  summarise(count = n_distinct(ppid))


#identify only those in study 
covariates <- covariates %>% 
  filter(ppid %in% study$ppid)

#link with index date and ppid only from study  to find those that had covariates coded before or on index date

index_dates <- study %>% 
  select(ppid, index_date) %>% 
  distinct()

linked_covariates <- covariates %>% 
  left_join(index_dates, by = "ppid")

# as each person will only have one covariate date (which is the earliest recorded date) for each covariate only keep covariates for individuals whose covariate date is on or before index date

study_covariates <- linked_covariates %>% 
  filter(covariate_date <= index_date)

#number of people with clinical covariates
length(unique(study_covariates$ppid)) 

#check the read codes descriptions included under each covariate and save. Then remove columns not required.

read_codes_covariate <- study_covariates %>% 
  group_by(PhenotypeName, CodeDesc, FullReadCode) %>% 
  summarise(Counts = n()) %>% 
  arrange(PhenotypeName, desc(Counts))

#save as cvs file if needed 

#tidy covariates and save

study_covariates <- study_covariates %>% 
  select(-c(EventDate, FullReadCode,CodeDesc,covariate_date, index_date)) %>% 
  distinct()

length(unique(studyA_covariates$ppid)) #number in study

glimpse(study_covariates)
table(study_covariates$PhenotypeName)

# create a dataset for each phenotypeName there will be Y for yes and N for not present for each person ID 

study_covariates_1 <- study_covariates %>% 
  pivot_wider(
    id_cols = ppid,
    names_from = PhenotypeName,
    values_from = PhenotypeName,
    values_fn = function(x) ifelse (length(x) > 0, "Y", "N"),
    values_fill = "N"
  )


# now join this dataset to the main study

study_with_covariates <- study %>% 
  left_join(study_covariates_1, by = "ppid")


#check correct number of people included

length(unique(study_with_covariates$ppid)) #

#change the NA's in clinical covariates to N
study_with_covariates <- study_with_covariates %>% 
  mutate(
    across(c("Iron deficiency anaemia", "Obesity", "Diabetes", "Hypertension"),
           ~ replace(., is.na(.), "N"))
  )


save(study_with_covariates, file = "study_clinical_covariates.RData")
