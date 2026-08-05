#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Applying exclusions related to surgical procedures for exposed group
#Apply one year look back as minimum

#Steps
#(1) Join ablation and hysterectomy dates and identify first surgical index date with 1 year look back. Tidy tranexamic acid with 1 year look back. Join exposed group and for those with both Tranexamic acid and ablation/hysterectomy find first index date.
# (2) Join sterilisation.Exclude those with prior sterilsiation
# (3) Check BSO exclude those who had prior BSO or at same time as index date
# (4) open ICD CVD and GP CVD exclusions. Apply exclusions and check number excluded.



#-------------------------------------------------------------------------------#
# load packages

#Clear global environment, restart R session
rm(list = ls())

library(dplyr)
library(tidyr)

#------------------------------------------------------------------------------#
# #(1) Open exposed datasets

# Open file with Ablation and Broad_hmb_hysterectomy dates
load("Ablation_dates.RData")

load("s/Broad_Hmb_hysterectomy_dates.RData")

#open tranexamic acid group
load("TA_exposed_group.RData")

#first tidy TA group only need index date
TA_exposed <- TA_exposure_final %>% 
  select(ppid, index_date) %>% 
  distinct()

#rename to TA_index_date and only keep those with 1 year look back
TA_exposed <- TA_exposed %>% 
  filter(index_date >= "2010-04-30") %>% 
  rename(TA_index_date = index_date)


#keep both dataset for ablation and hysterectomy so use full_join

Exposure_group <- full_join(Ablation_dates, Broad_criteria_Hmb_hysterectomy, by = "ppid")

length(unique(Exposure_group$ppid)) #check number of people

#remove Hmb_date and only keep those who had procedures after 30th April 2010 (1 year look back)

Exposed <- Exposure_group %>% 
  select (-Hmb_date) %>% 
  distinct()

length(unique(Exposed$ppid)) #check number

Exposed_main <- Exposed %>% 
  mutate(
    surg_index_date = pmin(Ablation_date, Hysterectomy_date, na.rm =TRUE),
    surg_index_date = if_else(is.infinite(surg_index_date),
                         as.Date(NA),
                         surg_index_date)
  )

sum(is.na(Exposed_main$surg_index_date))

#only include those from 30th April 2010

Exposed_main <- Exposed_main %>% 
  filter(surg_index_date >= "2010-04-30")


#combined exposed dataset
Main_exposed_prior_exclusions <- full_join(Exposed_main, TA_exposed, by = "ppid")
length(unique(Main_exposed_prior_exclusions$ppid))

#check those that have both TA and surgical index dates

both <- Main_exposed_prior_exclusions %>% 
  filter(!is.na(surg_index_date) & !is.na(TA_index_date))

#for most people the TA would be given first so filter those where this was not the case

check_exposed_dates <- both %>% 
  filter(TA_index_date >= surg_index_date) # check if any had tranexamic acid after surgery

#----------------------------------------------------------------------------------#
# (2) Join sterilization. only excluding those with prior sterilization. 

# tidy environment and load steri dates


load("/Sterilisation_dates.RData")

#for combined exposed group only exclude those with sterilsitaion prior to index date so create index date for those with combined exposures.
Main_exposed_prior_exclusions <- Main_exposed_prior_exclusions %>% 
  mutate(
         index_date = pmin(TA_index_date, surg_index_date, na.rm =TRUE),
         index_date = if_else(is.infinite(index_date),
                                       as.Date(NA),
                                       index_date)
           )

sum(is.na(Main_exposed_prior_exclusions$index_date))

#check for those that have both correct has been used

both_check <- Main_exposed_prior_exclusions %>% 
  filter(!is.na(surg_index_date) & !is.na(TA_index_date))

check_exposed_dates <- both_check %>% 
  filter(TA_index_date >= surg_index_date)

# left join the Sterilisation group to see if anyone in the Ablation and Hysterectomy group had a sterilisation.

Exposed_with_steri <- Main_exposed_prior_exclusions %>% 
  left_join(Steri_final, by="ppid")

length(unique(Exposed_with_steri$ppid)) # check number in cohort

# Examine everyone with a sterilisation date. If sterilisation occur at same time as ablation then these should not be counted

Check_sterilisations <- Exposed_with_steri %>% 
    mutate(days_diff = as.numeric(index_date - Steri_date)) %>% 
      mutate(criteria_applied = case_when(
        days_diff == 0 ~ "same date",
        days_diff > 0 ~ "exposed_after_steri",
        days_diff < 0 ~ "exposed_before_steri"
      ))

table(Check_sterilisations$criteria_applied) #

same_date_check <- Check_sterilisations %>% 
  filter(criteria_applied == "same date") %>% 
  filter(!is.na(TA_index_date))
# n

#find those that had sterilisation before index date these will be excluded. Those who had sterilisation at same time as ablation to remain. Those that had sterilsiation after index date will be censored

excluded_steri <- Exposed_with_steri %>% 
  filter(Steri_date < index_date)

length(unique(excluded_steri$ppid))

#Exclude these people
Exposed_excluded_prior_steri <- Exposed_with_steri %>% 
  filter(!ppid %in% excluded_steri$ppid)


#---------------------------------------------------------------------------#
# (3) exclude BSO groups at same time as hysterectomy or keep oophrectomy date for censoring exclude copper IUD

# tidy envirnoment
rm(Check_sterilisations, both_check, check_exposed_dates, Steri_final, Main_exposed_prior_exclusions, Exposed_with_steri, excluded_steri, Exposed_excluded_steri)

# open BSO broad criteria and copper IUD group after 18 year cut off

load("BSO_Broad_Hmb_hysterectomy_dates.RData")

load("copper_IUD_group.RData")


#(i) Examine those that had BSO in this group. Only need oophrectomy date and whether this was same date as hysterectomy

BSO <- BSO_hysterectomy %>% 
  select(ppid, Oophrectomy_date, same_date) %>% 
  distinct()


Exposure_group_excluding_steri_with_BSO <- Exposed_excluded_prior_steri %>% 
  left_join(BSO, by = "ppid")

length(unique(Exposure_group_excluding_steri_with_BSO$ppid)) #check number

#Now examine only those with BSO - the BSO dataset includes if hysterectomy date was the same as BSO date and the number of days difference
Exposed_with_BSO <- Exposure_group_excluding_steri_with_BSO %>% 
  filter(!is.na(Oophrectomy_date)) # 

# Check is how many occurred at same time as hysterectomy. 
table(Exposed_with_BSO$same_date)#

#check index date as if index is tranexamic acid they can remain in the study until hysterectomy and BSO.

BSO_excluded <- Exposed_with_BSO %>% 
  filter(Oophrectomy_date <= index_date)#

 Exposed_excluded_prior_steri_BSO <- Exposed_excluded_prior_steri %>% 
   filter(!ppid %in% BSO_excluded$ppid)

# finally check for copper IUD

Exposure_group_excluding_all <- Exposed_excluded_prior_steri_BSO %>% 
  filter(!ppid %in% iud_final$ppid)

length(unique(Exposure_group_excluding_all$ppid)) 

#save this dataset
save("Exposure_group_excluding_all")

#combine this with the comparison dataset to create the study population.
#------------------------------------------------------------------------------------

