#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Applying exclusions related to surgical procedures for comparison group
#Apply one year look back as minimum


#(1) Open Sterilisation and copper IUD - can have both if present. Make sure 1 year look back period applied.
# (2) Join ablation dates and hysterectomy. If occurred prior to sterilisation or at the same time remove from sterilisation group if occurred late remain will be censored at this point.
# (3) Join Tranexamic acid, exclude those who had prior TA and censor those that have TA after sterilisation.


# load packages

#Clear global environment, restart R session
rm(list = ls())

library(dplyr)
library(tidyr)

# Open file with Sterilisation and copper IUD
load("~s/Sterilisation_dates.RData")

load("~/copper_IUD_group.RData")

# The comparison group can have had a previous copper IUD.Tidy copper IUD to only ppid and index_date

View(iud_final) # remove date of birth and deaths as not needed for this

iud <- iud_final %>% 
  select(ppid, index_date) %>% 
  distinct()

iud <- iud %>% 
  filter(index_date >= "2010-04-30") %>% 
  rename(Cu_date = index_date)

steri <- Steri_final %>% 
  filter(Steri_date >= "2010-04-30")

Comparison_group <- full_join(steri, iud, by = "ppid")

both <- Comparison_group %>% 
  filter(!is.na(Steri_date) & !is.na(Cu_date)) #check number who had both sterilisation and copper IUD


#create index date for further exclusions
Comparison_prior_exclusions <- Comparison_group %>% 
  mutate(
    index_date = pmin(Cu_date, Steri_date, na.rm =TRUE),
    index_date = if_else(is.infinite(index_date),
                         as.Date(NA),
                         index_date)
  )

sum(is.na(Comparison_prior_exclusions$index_date))

# (2) Join ablation dates and hysterectomy dates. If occurred prior to sterilisation or at the same time remove from sterilisation group if occurred later remain will be censored at this point.

#clear environment
rm(iud, iud_final, Steri_final, both, steri, Comparison_group)

#load ablation and hysterectomy dates
load("~Ablation_dates.RData")

load("~/Broad_Hmb_hysterectomy_dates.RData")

Comparison_with_ablation <- Comparison_prior_exclusions %>% 
  left_join(Ablation_dates, by = "ppid")

Comparison_with_surgical <- Comparison_with_ablation %>% 
  left_join(Broad_criteria_Hmb_hysterectomy, by = "ppid")

ablation_prior <- Comparison_with_surgical %>% 
  filter(index_date >= Ablation_date)


hysterectomy_prior <- Comparison_with_surgical %>% 
  filter(index_date >= Hysterectomy_date)

Comparison_excluding_surgical <- Comparison_with_surgical %>% 
  filter(!ppid %in% ablation_prior$ppid) #

Comparison_excluding_surgical <- Comparison_excluding_surgical %>% 
  filter(!ppid %in% hysterectomy_prior$ppid)

#-------------------------------------------------------------------------------------------#
rm(Comparison_with_ablation, Comparison_with_surgical, Comparison_prior_exclusions, ablation_prior, hysterectomy_prior)
rm(Broad_criteria_Hmb_hysterectomy, Ablation_dates)

#check BSO there should be no additional exclusions as these should be excluded at hysterectomy date
load("/BSO_Broad_Hmb_hysterectomy_dates.RData") 

BSO <- BSO_hysterectomy %>% 
  select(ppid, Oophrectomy_date, same_date) %>% 
  distinct()

Comparison_with_BSO <- Comparison_excluding_surgical %>% 
  left_join(BSO, by = "ppid")

check_BSO <- Comparison_with_BSO %>% 
  filter(!is.na(Oophrectomy_date)) 

check_BSO <- check_BSO %>% 
  filter(index_date >= Oophrectomy_date)

#now check tranexamic acid
load("~/TA_exposed_group.RData") # 

length(unique(TA_exposure_final$ppid)) #check number of people

# tidy dataset only ppid and index date for tranexamic acid
TA_group <- TA_exposure_final %>% 
  select(ppid, index_date) %>% 
  distinct() %>% 
  rename(TA_date = index_date)


#Join TA dataset and then explore dates of TA. Exclude those that had TA prior to sterilisation and censor those that get TA after sterilisation.

Comparison_with_TA <- Comparison_with_BSO %>% 
  left_join(TA_group, by = "ppid")

#exclude this with prior TA
TA_prior <- Comparison_with_TA %>% 
  filter(index_date >= TA_date)

Comparison_excluding_TA <- Comparison_with_TA %>% 
  filter(!ppid %in% TA_prior$ppid)

#save
save("Comparison_excluding_TA") # this is the comparison group with CVD. 


#------------------------------------------------------------------------------------








