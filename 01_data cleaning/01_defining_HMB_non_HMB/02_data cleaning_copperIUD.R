###################################################################################
# General information
###################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Clean the comparison group who received copper intra-uterine device IUD
# ID number for those with copper IUD already provided.
#(1) Link to date of birth and death dates using ppid which is person_ID
# (2) remove anyone with index date before 18 years
# 



###################################################################################################

#Clear global environment
#rm(list = ls())

# Load Packages 

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Load dataset into R from the RData file created in 01_data cleaning_date of birth and deaths and combine
load("date_of_birth.RData")
load("deaths.RData")


#set working directory to linked data provided
setwd("")

full_cohort <-read.delim("iud_initial") # open copper IUD dataset
glimpse(full_cohort)

#check names of cohort
unique(full_cohort$cohort)


#--------------------------------------------------------------------------------------------------------
#(1) link date of birth and death data. 

hmb_demog_death <- merge(date_of_birth, deaths, by = "ppid", all = TRUE)
head(hmb_demog_death)

# Only include date of death, date of birth, ethnicity and ppid for now

hmb_demog_death <- hmb_demog_death %>% 
  select(ppid, date_of_birth_updated, death_date, ethnicity)
length(unique(hmb_demog_death$ppid)) 

#-------------------------------------------------------------------------------------
# only keep ppid from those with copper iud in the hmb_demog_death dataset

ids_iud <- hmb_demog_death %>% 
  filter(ppid %in% iud_initial$ppid)

# combine this with index date

iud_interim <- ids_iud %>% 
  left_join(iud_initial, by = "ppid")

glimpse(iud_interim)

#---------------------------------------------------------------------------------------------------------------------------------------------------#
# calculate age_at_index and see how many were under 18 at index

iud_interim <- iud_interim %>% 
  rename(date_of_birth = "date_of_birth_updated",
         index_date = "Index_Date")

iud_interim$index_date <- iud_interim$index_date %>%  as.Date()

iud_interim <- iud_interim %>% 
    mutate(age_at_IUD = as.numeric(index_date - date_of_birth)/365.25)

table(iud_interim$age_at_IUD >=18) # number removed
table(iud_interim$index_date > "2023-04-30") #

iud_final <- iud_interim %>% 
  filter(age_at_IUD >= 18)

#save
save(iud_final,file = "/copper_IUD_group.RData")


         