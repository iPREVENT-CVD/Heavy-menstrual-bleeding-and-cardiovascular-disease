#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# This script is for comparison group for each model. 

#Model 1 outcomes - CVD event and CVD death
#Model 2 outcomes - CVD event and all cause death
#Model 3 outcomes - CVD event, CVD death, cardiac medication
# Model 4 outcomes - CVD event, all cause death, cardiac medication

#Dates for each outcome
#CVD event = cvd_date
#CVD death - create new death_date which is CVD_death_date
#All cause death = death_date
#Cardiac medication = cardiac_drug_date

#------------------------------------------------------------------------------------------#
#load libraries
library(dplyr)
library(tidyr)
library(lubridate)


#Open comparison group
load("comparison_model_prep_data.RData")

length(unique(comparison$ppid)) # 

# need to calculate follow up period for each individual for Model 1 and 2.
# for each individual set study end date which will be the first time the following occurs
# (i) cvd_date
#(ii) death_date (which can be due to cvd or not)
#(iii) censoring by ablation or hysterectomy or tranexamic (study_date). oophrectomy always occurred either on hysterectomy date or afterwards so can censor on hysterectomy.
#(v) study end date 30th April 2023 - if no other events

table(comparison$study_event) # ablation and hysterectomy censoring

#create flags for censored events

comparison <- comparison %>% 
  mutate(censor_event = case_when(
    study_event %in% c("ablation", "hysterectomy") ~ TRUE,
    TRUE ~ FALSE
  ))


# change into long format. This is required as there may be people who have multiple events which could remove them from the analysis (e.g., censoring event prior to cvd_event). 

comparison_v1 <- comparison %>% 
  pivot_longer(
    cols = c(study_date, death_date, cvd_date),
    names_to = "type",
    values_to = "date"
  ) %>% 
  filter(type == "study_date" & censor_event == TRUE |
           type %in% c("cvd_date", "death_date")) %>% 
  arrange(ppid, date) %>% 
  group_by(ppid) %>% 
  slice(1) %>% 
  select(ppid, end_date = date) %>% 
  mutate(end_date = replace(end_date, is.na(end_date), as.Date("2023-04-30")) 
  ) %>% 
  left_join(comparison, by = "ppid")


length(unique(comparison_v1$ppid)) # correct number

#calculate follow up time
comparison_v1 <- comparison_v1 %>% 
  mutate(follow_up = round(as.numeric(end_date - index_date)/365.25, 2))

#tidy dataset
comparison_v2 <- comparison_v1 %>% 
  select(-c(study_event, study_date, censor_event)) %>% 
  distinct()

#For Model 1 and 2 create two datasets with different outcomes. This will be used for multiple imputations

#Model 1
#Code each outcomes to allow combinations: cvd, cvd_death

comparison_model <- comparison_v2 %>% 
  mutate(outcome_1 = case_when(
    end_date == cvd_date ~ "y",
    end_date == cvd_death_date ~ "y",
    TRUE ~ NA_character_
  ))

table(comparison_model$outcome_1) #correct 

#create outcome for model2
comparison_model <- comparison_model %>% 
  mutate(outcome_2 = case_when(
    end_date == cvd_date ~ "y",
    end_date == death_date ~ "y",
    TRUE ~ NA_character_
  ))

table(comparison_model$outcome_2)

#Tidy dataset for modelling
# end date and index date not required, cvd_date, death_date, drug_date

comparison_model_v1 <- comparison_model %>% 
  select(-c(end_date, index_date, cvd_date, death_date, cardiac_drug_date, 
            cvd_death_date)) %>% 
  distinct()

#change all categorical variables to factors
comparison_model_v1 <- comparison_model_v1 %>% 
  mutate(across(where(is.character), ~ as.factor(tolower(.x))))

#change column names to lowercase
colnames(comparison_model_v1) <-tolower(colnames(comparison_model_v1))

glimpse(comparison_model_v1)
#check for NA's there should only be in simd, smoking_status
summary(comparison_model_v1)

#replace any NA values to N for outcomes and nsaid, htn composite
comparison_complete <- comparison_model_v1 %>% 
  mutate(
    across(c(outcome_1, outcome_2, htn_composite, nsaid,), as.character)) %>% 
  mutate(
    across(c(outcome_1, outcome_2, htn_composite, nsaid), ~ replace(., is.na(.), "n"))
  )

sum(is.na(comparison_complete$outcome_1))
sum(is.na(comparison_complete$outcome_2))
sum(is.na(comparison_complete$htn_composite))
sum(is.na(comparison_complete$nsaid))

#create groups for model 1 and model 2
combined_model_1_comparison <- comparison_complete %>% 
  select(-outcome_2) %>% 
  distinct()

combined_model_2_comparison <- comparison_complete %>% 
  select(-outcome_1) %>% 
  distinct()

#save these comparison groups
save(combined_model_1_comparison, file = "/comparison_model_1_data.RData")

save(combined_model_2_comparison, file = "comparison_model_2_data.RData")


#-----------------------------------------------------------------------------------------#
#Now create comparison groups for Model 3 and 4

#tidy environment
rm(comparison_complete, comparison_model, comparison_model_v1, comparison_v1, comparison_v2)

#for model 3 and 4 remove those with ever prescription for HTN drugs
load("study_ids_ever_prior_htn_drug.RData")

length(unique(study_ids_ever_prior_htn_drug$ppid)) #


comparison_v1 <- comparison %>% 
  filter(!ppid %in% study_ids_ever_prior_htn_drug$ppid)

length(unique(comparison_v1$ppid))# number in study now those with prior HTN drug removed

#check if anyone with htn-composite left in the study (theses are prior to index date)
table(comparison_v2$HTN_composite)# still includes people who had htn read code but have excluded all the had prior drugs. Exclude these individuals as well from models 3 and 4.

ids_htn_composite <- comparison_v2 %>% 
  filter(HTN_composite == "y")

length(unique(ids_htn_composite$ppid)) #

comparison_v3 <- comparison_v2 %>% 
  filter(!ppid %in% ids_htn_composite$ppid)

length(unique(comparison_v3$ppid)) #

#use comparison with censoring events to include cardiac_drugs as outcome for models 3 and 4.
# include cardiac_drug_date

comparison_v4 <- comparison_v3 %>% 
  pivot_longer(
    cols = c(study_date, death_date, cvd_date, cardiac_drug_date),
    names_to = "type",
    values_to = "date"
  ) %>% 
  filter(type == "study_date" & censor_event == TRUE |
           type %in% c("cvd_date", "death_date", "cardiac_drug_date")) %>% 
  arrange(ppid, date) %>% 
  group_by(ppid) %>% 
  slice(1) %>% 
  select(ppid, end_date = date) %>% 
  mutate(end_date = replace(end_date, is.na(end_date), as.Date("2023-04-30")) 
  ) %>% 
  left_join(comparison_v3, by = "ppid")


#check all dates are correct for those that have cvd_date and cardiac_drug_date
check_dates <- comparison_v4 %>% 
  select(ppid, index_date, end_date, cvd_date, cardiac_drug_date, study_event, study_date) %>% 
  filter(!is.na(cvd_date) & !is.na(cardiac_drug_date))

comparison_v4 <- comparison_v4 %>% 
  mutate(follow_up = round(as.numeric(end_date - index_date)/365.25, 2))

#tidy dataset
comparison_v5 <- comparison_v4 %>% 
  select(-c(study_event, study_date, censor_event, HTN_composite)) %>% 
  distinct()

#-----------------------------------------------------------------------------------------#
#For Model 3 and 4 create two datasets with different outcomes. This will be used for multiple imputations

#Model 3
#Code each outcomes to allow combinations: cvd, cvd_death, cardiac_drug

comparison_model <- comparison_v5 %>% 
  mutate(outcome_3 = case_when(
    end_date == cvd_date ~ "y",
    end_date == cvd_death_date ~ "y",
    end_date == cardiac_drug_date ~ "y",
    TRUE ~ NA_character_
  ))

table(comparison_model$outcome_3) #

#create outcome for model4
comparison_model <- comparison_model %>% 
  mutate(outcome_4 = case_when(
    end_date == cvd_date ~ "y",
    end_date == death_date ~ "y",
    end_date == cardiac_drug_date ~ "y",    
    TRUE ~ NA_character_
  ))

table(comparison_model$outcome_4) #

#Tidy dataset for modelling
# end date and index date not required, cvd_date, death_date, drug_date

comparison_model_v1 <- comparison_model %>% 
  select(-c(end_date, index_date, cvd_date, death_date, cardiac_drug_date, 
            cvd_death_date)) %>% 
  distinct()

#change all categorical variables to factors
comparison_model_v1 <- comparison_model_v1 %>% 
  mutate(across(where(is.character), ~ as.factor(tolower(.x))))

#change column names to lowercase
colnames(comparison_model_v1) <-tolower(colnames(comparison_model_v1))

glimpse(comparison_model_v1)
#check for NA's there should only be in simd, smoking_status
summary(comparison_model_v1)

#replace any NA values to N for outcomes and nsaid, htn composite
comparison_complete <- comparison_model_v1 %>% 
  mutate(
    across(c(outcome_3, outcome_4, nsaid), as.character)) %>% 
  mutate(
    across(c(outcome_3, outcome_4, nsaid), ~ replace(., is.na(.), "n"))
  )

sum(is.na(comparison_complete$outcome_3))
sum(is.na(comparison_complete$outcome_4))
sum(is.na(comparison_complete$nsaid))

#create exposed groups for model 1 and model 2
combined_model_3_comparison <- comparison_complete %>% 
  select(-outcome_4) %>% 
  distinct()

combined_model_4_comparison <- comparison_complete %>% 
  select(-outcome_3) %>% 
  distinct()

#save these exposure groups
save(combined_model_3_comparison, file = "/comparison_model_3_data.RData")

save(combined_model_4_comparison, file = "/combined_comparison_model_4_data.RData")


