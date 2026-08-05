#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Joining covariates for socioeconomic deprivation status - simd, family history - FH and smoking 


#--------------------------------------------------------------------------------------------
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(lubridate)

#set working directory
setwd("")


simd <-read.delim("SIMD.tsv") # provided in deciles at index date based on GP records

#open study  population
load("~study_clinical_covariates.RData")

length(unique(study_with_covariates$ppid)) # correct number

study <- study_with_covariates %>% 
  left_join(simd, by = "ppid")

study <- study %>% 
  rename(simd = simd2020v2_sc_decile)

#
 
 # load FH of CVD
 setwd("")
 fh<-read.delim("FamilyHist_CVD.tsv")
 length(unique(fh$ppid))
 
 table(fh$FullReadCode)
 
 #no dates only ppid tidy dataset then link up. There are different codes for FH but as needed for descriptive only no need to keep these.
 
 fh_ID <- fh %>% 
   select(ppid) %>% 
   distinct()
 
 fh_ID <- fh_ID %>% 
   mutate(FH = "Y")
 
 
 #link study including simd
 
 study_FH <- study %>% 
   left_join(fh_ID, by = "ppid")
 
 
 #------------------------------------------------------------------------------#
 #now join to smoking data which has been tidied
 
# open smoking data
 load("/cleaned_smoking.RData")


#use ID 
 study_ID <- study_FH %>% 
   select(ppid, index_date) %>% 
   distinct()
 
 #link observations and smoking to study ppid/index. Filter for each variable of interest
 study_smoking <- study_ID %>% 
   left_join(smoking, by = "ppid")
 
 
 #can only keep smoking records on or prior to index date
 smoking_prior_index <- study_smoking %>% 
   filter(index_date >= obs_date)
 
 length(unique(smoking_prior_index$ppid)) # number who had smoking status recorded prior to index date 
 
 # during how length of study period how many people have no smoking history recorded
 
 missing_smoking <- study_smoking %>% 
   filter(is.na(smoking_status)) %>% 
   distinct(ppid)

 
 #---------------------------------------------------------------------------------------
 # For those with smoking status recorded on or prior to index date some people have multiple readings. Check how many people have more than one smoking status
 
 check_smoking <- smoking_prior_index %>% 
   group_by(ppid) %>% 
   summarise(n_smoking_types = n_distinct(smoking_status)) %>% 
   filter(n_smoking_types >1) %>% 
   nrow()
 

 
 #Keep the value of the obs_date closest to the index date
 
 smoking_prior_index <- smoking_prior_index %>% 
   mutate(days_diff = as.numeric(index_date - obs_date))
 
 #now keep the closest date
 smoking_prior_index <- smoking_prior_index%>% 
   group_by(ppid) %>% 
   slice(which.min(days_diff))
 
 length(unique(smoking_prior_index$ppid))
 
 # this dataset has a smoking status of 5333 on or prior to index date. Check years since index
 
 smoking_prior_index <- smoking_prior_index %>% 
   mutate(years_prior = round(days_diff/365.25, 2))
 
 summary(smoking_prior_index$years_prior) # 
 
 ggplot(smoking_prior_index, aes(x = years_prior)) +
   geom_histogram()
 
 # most people had smoking recorded within 5 years. Create cut offs and check
 
 smoking_prior_index <- smoking_prior_index %>% 
   mutate(cut_offs = case_when(
     years_prior >= 0 & years_prior <= 5 ~ "within 5 years",
     years_prior > 5 & years_prior <= 10 ~ "5-10 year",
     years_prior > 10 ~ "more than 10 years"
   ))
 
 table(smoking_prior_index$cut_offs) # most within 5 years
 
 # check distribution of dates for smoking. Mostly recent dates 2010 to 2020
 smoking_prior_index %>% 
   group_by(obs_date) %>% 
   summarise(count = n()) %>% 
   ggplot(aes(x = obs_date, y = count)) +
   geom_line()
 
 
 #-----------------------------------------------------------------------------------#
 # link to study population
 
 # only need ppid and smoking status 
 
 smoking_IDs <- smoking_prior_index %>% 
   select(ppid, smoking_status) %>% 
   distinct()
 
 study_with_smoking <- study_FH  %>% 
   left_join(smoking_IDs, by = "ppid", relationship = "many-to-many")
 
 save(study_with_smoking, file = "study_smoking.RData")
 
 


 

 
 
 