#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Creating four composite outcomes for exposed group with heavy menstrual bleeding

#Model 1 outcomes - CVD event and CVD death
#Model 2 outcomes - CVD event and all cause death
#Model 3 outcomes - CVD event, CVD death, cardiac medication
# Model 4 outcomes - CVD event, all cause death, cardiac medication

#CVD now includes thromboembolism as well as IHD and cerebrovascular disease

#Dates for each outcome
#CVD event = cvd_date
#CVD death - create new death_date which is CVD_death_date
#All cause death = death_date
#Cardiac medication = cardiac_drug_date

#steps
#(1) tidy dataset for modelling
#(2) calculate follow_up period by calculate end date for each individual taking into account censoring events, deaths, outcomes and end of study period.
#(3) create 4 models for each composite outcome
#(4) save dataset with only required variables for analysis

#----------------------------------------------------------------------------------------#
#Load libraries
library(dplyr)
library(tidyr)
library(lubridate)

# load descriptive dataset for exposed
load("exposed_descriptive.RData")

exposed_complete <- final_complete

#tidy model so only covariates for model and outcomes are included then save exposed and comparison group for censoring

exposed_clean <- exposed_complete %>% 
  select(ppid, index_date, Group, date_of_birth, study_event, study_date, cvd_date, cvd_death, other_death, death_date, Obesity, HTN_composite, Diabetes, simd, smoking_status, chcp, nsaid, cardiac_drug_date) %>% 
  distinct()

#Create age and year categories for whole dataset
exposed_clean <- exposed_clean %>% 
  mutate(age = round(as.numeric(index_date - date_of_birth)/365.25, 2)) %>% 
  mutate(year = year(index_date)) 

#For those that have cvd_death create new variable cvd_death_date which is the same date as death_date. This will be used to define outcomes later.

exposed_clean <- exposed_clean %>% 
  mutate(
    cvd_death_date = if_else(
      cvd_death == "y",
      death_date,
      as.Date(NA)
    )
)

#check whether this worked
exposed_clean %>% 
  filter(cvd_death == "y") %>% 
  summarise(all_match = all(cvd_death_date == death_date, na.rm = TRUE))
#

exposed_clean %>% 
  summarise(
    total_cvd_death = sum(cvd_death == "y", na.rm = TRUE),
    cvd_death_filled = sum(!is.na(cvd_death_date))
  )
#check for matching values.

#tidy dataset 
exposed <- exposed_clean %>% 
   select(-c(date_of_birth, cvd_death, other_death)) %>% 
   distinct()

length(unique(exposed$ppid))


 
#---------------------------------------------------------------------------------------# 
#tidy environment 
rm(exposed_complete, exposed_clean, exposed_tidy)

#Start with the exposed group. Model1 and 2

#check censoring events
table(exposed$study_event)  

# for each individual set study end date which will be the first time the following occurs
# (i) cvd_date
#(ii) death_date 
#(iii) censoring by BSO or sterilisation or IUD (study_date)
#(v) study end date 30th April 2023 - if no other events

#first start with censoring, there are no IUD so need to censor
#those that had sterilisation at the same time as ablation don't need to count sterilisation as censoring so remove these event

steri_same <- exposed %>% 
  filter(study_event == "ablation") %>% 
  inner_join(exposed %>% 
               filter(study_event == "sterilisation"), 
             by = c("ppid", "study_date")) %>% 
  select(ppid, study_date, study_event.x, study_event.y)

exposed_v1 <- exposed %>% 
  filter(!(study_event == "sterilisation" & ppid%in% steri_same$ppid
           & study_date %in% steri_same$study_date))

length(unique(exposed_v1$ppid)) # correct number

# create flag for censored events

exposed_v1 <- exposed_v1 %>% 
  mutate(censor_event = case_when(
    study_event %in% c("sterilisation", "oophrectomy") ~ TRUE,
    TRUE ~ FALSE
  ))


# change into long format. This is required as there may be people who have multiple events which could remove them from the analysis (e.g., censoring event prior to cvd_event). use term exposed for dataset as this will be used for summary statistics

exposed_v2 <- exposed_v1 %>% 
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
  left_join(exposed_v1, by = "ppid")

View(exposed_v2)

exposed_v2 <- exposed_v2 %>% 
  mutate(follow_up = round(as.numeric(end_date - index_date)/365.25, 2))

#tidy dataset
exposed_v3 <- exposed_v2 %>% 
  select(-c(study_event, study_date, censor_event)) %>% 
  distinct()

#-----------------------------------------------------------------------------------------#
#For Model 1 and 2 create two data sets with different outcomes. This will be used for multiple imputations

#tidy environment keeping exposed_v1 which will be used for models 3 and 4
rm(exposed, exposed_v2, steri_same)

#Model 1
#Code each outcomes to allow combinations: cvd, cvd_death

exposed_model <- exposed_v3 %>% 
  mutate(outcome_1 = case_when(
    end_date == cvd_date ~ "y",
    end_date == cvd_death_date ~ "y",
    TRUE ~ NA_character_
  ))

table(exposed_model$outcome_1) #

#create outcome for model2
exposed_model <- exposed_model %>% 
  mutate(outcome_2 = case_when(
    end_date == cvd_date ~ "y",
    end_date == death_date ~ "y",
    TRUE ~ NA_character_
  ))

table(exposed_model$outcome_2) #correct

#Tidy dataset for modelling
# end date and index date not required, cvd_date, death_date, drug_date

exposed_model_v1 <- exposed_model %>% 
  select(-c(end_date, index_date, cvd_date, death_date, cardiac_drug_date, 
            cvd_death_date)) %>% 
  distinct()

#change all categorical variables to factors
exposed_model_v1 <- exposed_model_v1 %>% 
  mutate(across(where(is.character), ~ as.factor(tolower(.x))))

#change column names to lowercase
colnames(exposed_model_v1) <-tolower(colnames(exposed_model_v1))

glimpse(exposed_model_v1)
#check for NA's there should only be in simd, smoking_status
summary(exposed_model_v1)


#replace any NA values to N for outcomes, htn_composite and nsaids
exposed_complete <- exposed_model_v1 %>% 
  mutate(
    across(c(outcome_1, outcome_2, htn_composite, nsaid), as.character)) %>% 
  mutate(
    across(c(
      outcome_1, outcome_2, htn_composite, nsaid), ~ replace(., is.na(.), "n"))
  )

sum(is.na(exposed_complete$outcome_1))
sum(is.na(exposed_complete$outcome_2))
sum(is.na(exposed_complete$htn_composite))
sum(is.na(exposed_complete$nsaid))

#create exposed groups for model 1 and model 2
combined_model_1_exposed <- exposed_complete %>% 
  select(-outcome_2) %>% 
  distinct()

combined_model_2_exposed <- exposed_complete %>% 
  select(-outcome_1) %>% 
  distinct()

#save these exposure groups
save(combined_model_1_exposed, file = "exposed_model_1_data.RData")

save(combined_model_2_exposed, file = "exposed_model_2_data.RData")

#=------------------------------------------------------------------------------------#
# NOw create exposure groups for models 3 and 4

#tidy environment
rm(combined_model_1_exposed, combined_model_2_exposed, exposed_v3, exposed_model, exposed_complete, exposed_model_v1)

#for model 3 and 4 remove those with ever prescription for HTN drugs 
load("study_ids_ever_prior_htn_drug.RData")

length(unique(study_ids_ever_prior_htn_drug$ppid)) 

exposed_v2 <- exposed_v1 %>% 
  filter(!ppid %in% study_ids_ever_prior_htn_drug$ppid)

length(unique(exposed_v2$ppid))#

#check if anyone with htn-composite left in the study (theses are prior to index date)
table(exposed_v3$HTN_composite)# still includes people who had htn read code but did not have prescription data. exclude these individuals

ids_htn_composite <- exposed_v3 %>% 
  filter(HTN_composite == "y")

length(unique(ids_htn_composite$ppid)) #

exposed_v4 <- exposed_v3 %>% 
  filter(!ppid %in% ids_htn_composite$ppid)

length(unique(exposed_v4$ppid))#

#use exposed_v1 with censoring events to include cardiac_drugs as outcome for models 3 and 4.
# include cardiac_drug_date

exposed_v5 <- exposed_v4 %>% 
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
  left_join(exposed_v4, by = "ppid")


#check all dates are correct for those that have cvd_date and cardiac_drug_date
check_dates <- exposed_v5 %>% 
  select(ppid, index_date, end_date, cvd_date, cardiac_drug_date, study_event, study_date) %>% 
  filter(!is.na(cvd_date) & !is.na(cardiac_drug_date))

exposed_v5 <- exposed_v5 %>% 
  mutate(follow_up = round(as.numeric(end_date - index_date)/365.25, 2))

#tidy dataset
exposed_v6 <- exposed_v5 %>% 
  select(-c(study_event, study_date, censor_event, HTN_composite)) %>% 
  distinct()

#-----------------------------------------------------------------------------------------#
#For Model 3 and 4 create two datasets with different outcomes. This will be used for multiple imputations

#Model 3
#Code each outcomes to allow combinations: cvd, cvd_death, cardiac_drug

exposed_model <- exposed_v6 %>% 
  mutate(outcome_3 = case_when(
    end_date == cvd_date ~ "y",
    end_date == cvd_death_date ~ "y",
    end_date == cardiac_drug_date ~ "y",
    TRUE ~ NA_character_
  ))

table(exposed_model$outcome_3) #

#create outcome for model4
exposed_model <- exposed_model %>% 
  mutate(outcome_4 = case_when(
    end_date == cvd_date ~ "y",
    end_date == death_date ~ "y",
    end_date == cardiac_drug_date ~ "y",    
    TRUE ~ NA_character_
  ))

table(exposed_model$outcome_4) #

#Tidy dataset for modelling
# end date and index date not required, cvd_date, death_date, drug_date

exposed_model_v1 <- exposed_model %>% 
  select(-c(end_date, index_date, cvd_date, death_date, cardiac_drug_date, 
            cvd_death_date)) %>% 
  distinct()

#change all categorical variables to factors
exposed_model_v1 <- exposed_model_v1 %>% 
  mutate(across(where(is.character), ~ as.factor(tolower(.x))))

#change column names to lowercase
colnames(exposed_model_v1) <-tolower(colnames(exposed_model_v1))

glimpse(exposed_model_v1)
#check for NA's there should only be in simd, smoking_status
summary(exposed_model_v1)

#replace any NA values to N for outcomes, nsaid
exposed_complete <- exposed_model_v1 %>% 
  mutate(
    across(c(outcome_3, outcome_4, nsaid), as.character)) %>% 
  mutate(
    across(c(outcome_3, outcome_4, nsaid), ~ replace(., is.na(.), "n"))
  )

sum(is.na(exposed_complete$outcome_3))
sum(is.na(exposed_complete$outcome_4))
sum(is.na(exposed_complete$nsaid))

#create exposed groups for model 1 and model 2
combined_model_3_exposed <- exposed_complete %>% 
  select(-outcome_4) %>% 
  distinct()

combined_model_4_exposed <- exposed_complete %>% 
  select(-outcome_3) %>% 
  distinct()

#save these exposure groups
save(combined_model_3_exposed, file = "exposed_model_3_data.RData")

save(combined_model_4_exposed, file = "exposed_model_4_data.RData")


