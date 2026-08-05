#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Creating flags for hormonal confounders found in gynaecology chapter in BNF for people included in the study

#(1) create flags for each gynae drug which are types of contraception, combined contraceptive pill chcp (confounder), progesterone and topical oestrogen for description.
#(2) create a confounders variable contraception_1 which is ever had recorded gynae drug within 1 year of index date 


#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(stringr)
library(lubridate)


#open drugs in gynae chapter found for study

load("/study_gynae_drugs.RData")


#change headings into lower case and then filter using BNF. Root descriptions identified in 
study_gynae <- study_gynae %>% 
  rename_with(~ tolower(.x))

# These are the BNF Root Descriptions for progesterone. These will not be confounders but can be included in description of study population
#DESOGESTREL
#MEDROXYPROGESTERONE ACETATE
#NORETHISTERONE
#LEVONORGESTREL
#ETONOGESTREL
#ETONOGESTREL
#ETYNODIOL DIACETATE

progesterone_contraception <- c("DESOGESTREL", "MEDROXYPROGESTERONE ACETATE", "NORETHISTERONE","LEVONORGESTREL", "ETONOGESTREL", "ETYNODIOL DIACETATE")

#BNF Root descriptions for CHCP
#COMBINED ETHINYLESTRADIOL 30MCG
#COMBINED ETHINYLESTRADIOL 35MCG
#COMBINED ETHINYLESTRADIOL 20MCG
#PHASED FORMULATIONS OF ETHINYLESTRADIOL
#COMBINED MESTRANOL

chcp <- c("COMBINED ETHINYLESTRADIOL 30MCG", "COMBINED ETHINYLESTRADIOL 35MCG", "COMBINED ETHINYLESTRADIOL 20MCG", "PHASED FORMULATIONS OF ETHINYLESTRADIOL", "COMBINED MESTRANOL")
  
#BNF root descriptions for topical oestrogens not included as do not increase CVD risk as only topical however can be included in description
#ESTRADIOL - VAGIFEM_VAG TAB 10MCG, ESTRING_VAG RING 2MG, ESTRADIOL_VAG RING 2MG
#ESTRIOL - OVESTIN_CRM 1MG, ESTRADIOL_PESS 10MCG, ESTRIOL_CRM 0.01%, ORTHO-GYNEST_PESS 0.5MG, ESTRIOL_PESS 0.5MG

oestrogen_top <- c("ESTRADIOL", "ESTRIOL")

#(1) create flags in dataset for each type of contraception

gynae_drug_flag <- study_gynae %>% 
  mutate(
    gynae_drug = case_when(
      pi.bnf.root.drug.description %in% chcp ~ "chcp",
      pi.bnf.root.drug.description %in% oestrogen_top ~"vaginal oestrogen",
      pi.bnf.root.drug.description %in% progesterone_contraception ~ "progesterone contraception",
      TRUE ~ NA_character_
    ) 
  )

table(gynae_drug_flag$gynae_drug) # chcp recorded 3638 times 

length(unique(gynae_drug_flag$ppid)) # 2502 people

#apply 1 year look back so remove those with index dates prior to 30th April 2010

gynae_drug_flag <- gynae_drug_flag %>% 
  filter(index_date >= "2010-04-30")#

length(unique(gynae_drug_flag$ppid)) # correct

#check number of people that have more than one type of chcp drug
Multiple_chcp <- gynae_drug_flag %>% 
  filter(gynae_drug == "chcp") %>% 
  group_by(ppid) %>% 
  summarise(n_drug_types = n_distinct(pi.bnf.item.description), .groups = "drop") %>% 
  filter(n_drug_types > 1)

summary(Multiple_chcp$n_drug_types) #

#All scripts need to be prior to index date

prior_gynae <- gynae_drug_flag %>% 
  filter(!is.na(gynae_drug)) %>% 
  filter(paid_date <= index_date)

table(prior_gynae$gynae_drug) # the numbers will be smaller as these are people who had scripts before index date. An individual may have more than one drug type and multiple drugs within that drug group

any(is.na(prior_gynae$gynae_drug))

#only include those who gynae paid date atleast 12m before index date,

prior_gynae <- prior_gynae %>% 
  filter(between(paid_date, index_date - 365.25, index_date))

length(unique(prior_gynae$ppid)) # people had a gynae drug paid date 1 year before index date

#tidy dataset with only ppid, index and gynae_drug 

prior_gynae_drugs <- prior_gynae %>% 
  select(ppid, gynae_drug) %>% 
  distinct() # 

# if anyone ever had at least one script of chcp/progesterone or vaginal oestrogen 1 year before index they are recorded here.

table(prior_gynae_drugs$gynae_drug) # 

#save in medications folder
save(prior_gynae_drugs, file = "study_gynae_drugs_descriptive_only.RData")
     
# for chcp which will be confounder create covariate dataset with Y, N

chcp_covariates <- prior_gynae_drugs %>%
  filter(gynae_drug == "chcp") %>% 
  mutate(chcp = "Y") %>% 
  select(ppid, chcp) %>% 
  distinct()

#correct numbers. Save dataset.
save(chcp_covariates, file = "study_chcp_covariates.RData")

#------------------------------------------------------------------------------------
#Now examine endocrine drugs - HRT

#(1) create flags for each endocrine drug which are types of HRT. Decision not to include progesterone to treat HMB as currently no association with CVD for people without CVD. The progesterone category include people Rx for AUB as well as those given progesterone for endometrial protection component of HRT.
#(2) paid date must be before index date and 1 year. 
#(3) save dataset

#---------------------------------------------------------------------------#
# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#(1) for endocrine chapter which has HRT
load("study_endo_drugs.RData")

#change headings into lower case and then filter using BNF. Root descriptions identified in 
study_endo <- study_endo %>% 
  rename_with(~ tolower(.x))

# BNF Root descriptions for hrt - combined, oestrogen alone (includes patches, gels) and tibolone
#OESTROGENS CONJUGATED WITH PROGESTOGEN
#ESTRADIOL WITH PROGESTOGEN
#ESTRADIOL AND ESTRIOL WITH PROGESTOGEN
#TIBOLONE
#ESTRADIOL
#OESTROGENS CONJUGATED
#ESTRADIOL VALERATE


#BNF Root description for progesterone as management for AUB no increased risk for CVD so not as confounder but will be included for description
#MEDROXYPROGESTERONE ACETATE
#NORETHISTERONE
#PROGESTERONE

hrt <- c("OESTROGENS CONJUGATED WITH PROGESTOGEN", "ESTRADIOL WITH PROGESTOGEN", "TIBOLONE",
         "ESTRADIOL", "OESTROGENS CONJUGATED", "ESTRADIOL VALERATE", 	
         "ESTRADIOL AND ESTRIOL WITH PROGESTOGEN")

progesterone <- c("MEDROXYPROGESTERONE ACETATE", "NORETHISTERONE", "PROGESTERONE")
# depending on dose medroxyprogesterone and norethisterone can be given to control AUB or as part of HRT, the progesterone is micronised progesterone as in Utrogestron. This progesterone is either for AUB or given separately as progesterone component of HRT (to protect the endometrium).

endo_drug_flag <- study_endo %>% 
  mutate(
    endo_drug = case_when(
      pi.bnf.root.drug.description %in% hrt ~ "hrt",
      pi.bnf.root.drug.description %in% progesterone ~ "non-contraceptive progesterone",
      TRUE ~ NA_character_
    ) 
  )

#Explore drug use across the study period

table(endo_drug_flag$endo_drug)

#apply 1 year look back so remove those with index dates prior to 30th April 2010

endo_drug_flag <- endo_drug_flag %>% 
  filter(index_date >= "2010-04-30")#

length(unique(endo_drug_flag$ppid)) # correct


#check number of people within each category
number_endo_drugs <- endo_drug_flag %>% 
  group_by(endo_drug) %>% 
  summarise(n_people = n_distinct(ppid))


#check number of people that have more than one type of hrt
Multiple_hrt <- endo_drug_flag %>% 
  filter(endo_drug == "hrt") %>% 
  group_by(ppid) %>% 
  summarise(n_drug_types = n_distinct(pi.bnf.item.description), .groups = "drop") %>% 
  filter(n_drug_types > 1)

# people have used more than one named drug.

summary(Multiple_hrt$n_drug_types) # 

#All scripts need to be prior to index date

prior_endo <- endo_drug_flag %>% 
  filter(!is.na(endo_drug)) %>% 
  filter(paid_date <= index_date)

table(prior_endo$endo_drug) # the numbers will be smaller as these are people who had scripts before index date. An individual may have more than one drug type and multiple drugs within that drug group

#only include those who gynae paid date atleast 12m before index date,

prior_endo <- prior_endo %>% 
  filter(between(paid_date, index_date - 365.25, index_date))

length(unique(prior_endo$ppid)) 

#tidy dataset with only ppid, index and gynae_drug 

prior_endo_drugs <- prior_endo %>% 
  select(ppid, endo_drug) %>% 
  distinct() # 

#
table(prior_endo_drugs$endo_drug) #


# create covariate dataset with Y, N for descriptive analysis
hrt_covariates <- prior_endo_drugs %>%
  filter(endo_drug == "hrt") %>% 
  mutate(hrt = "Y") %>% 
  select(ppid, hrt) %>% 
  distinct()

#correct numbers. Save dataset.
save(hrt_covariates, file = "hrt_covariates.RData")

