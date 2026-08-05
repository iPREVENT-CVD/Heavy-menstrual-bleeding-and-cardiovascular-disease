#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Applying deaths as outcomes

#Steps 
#(1) open the study population without CVD with ICD outcomes and open cleaned death dataset
# (2) For the death dataset identify CVD deaths only and create cause of CVD death column which is CVD death if in Position 1 or position 2.
# (3) link to the study population by ppid and death date and save dataset


#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)

#--------------------------------------------------------------------------------------------
# load study population without CVD disease and with ICD outcomes

load("Study_without_CVD_ICD_outcomes.RData")

#open data set cleaned for death outcomes
load("deaths.RData")
# remove any spaces before filtering Phenotypes - no need to have cause of death only Phenotype needed for CVD or non CVD

deaths_clean_1 <- deaths_clean_1 %>% 
  mutate(
    death_1 = str_trim(`PhenoGroup_all_deaths 1`),
    death_2 = str_trim(`PhenoGroup_all_deaths 2`)
  )


#(1) keep only death in PhenoGroup 1 and 2 which correspondes to position 1 and 2

deaths_clean_2 <- deaths_clean_1 %>% 
  select(ppid, death_date, death_1, death_2) %>% 
  distinct()

#if there is Cardiovascular in death_1 or death_2 flag as Y otherwise N. If N use condition in death_1 as the non CVD death

clean_deaths <- deaths_clean_2 %>% 
  mutate(
    cvd_death = if_else(
      str_detect(death_1, "Cardiovascular") | str_detect(death_2, "Cardiovascular"),
      "y", "n"),
    other_death = if_else(
      cvd_death == "n",
      death_1,
      NA_character_
    )
  )

# only need cvd_death and other_death

clean_deaths <- clean_deaths %>% 
  select(ppid, death_date, cvd_death, other_death) %>% 
  distinct()

table(clean_deaths$other_death) #

study <- study_all_exclusions %>% 
  left_join(clean_deaths, by = c("ppid", "death_date"))

length(unique(study_all_exclusions$ppid)) #check correct number

#check number of CVD deaths and non CVD deaths in whole population of study

cardio_deaths <- study %>% 
  filter(cvd_death == "y") 

length(unique(cardio_deaths$ppid)) # number of deaths

non_cardio_deaths <- study %>% 
  filter(cvd_death == "n")

length(unique(non_cardio_deaths$ppid)) #non cardiac deaths


# save dataset to add clinical covariates
save(study, file = "Study_without_CVD_ICD_outcomes_deaths.RData")
