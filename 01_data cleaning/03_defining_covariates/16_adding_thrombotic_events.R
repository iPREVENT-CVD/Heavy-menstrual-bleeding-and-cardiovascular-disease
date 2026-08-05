#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Adding PE and DVT which occurred 6 week post-operatively to composite outcomes for main combined study population and excluding those with prior venous thromboemobilism
# use SMR_CP - HDRUK Phenotypes using Kuan (2019) in SMR records and GP read codes

# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
#------------------------------------------------------------------------------#
#set working directory
setwd("")

SMR_CP <- read.delim("SMR_CP.tsv") # phenotypes

#------------------------------------------------------------------------------

#Check the structure of variable in Code, removing blank spaces and checking for decimals

SMR_CP <- SMR_CP %>% 
  mutate(code_trimmed = str_trim(Code))

# check whether there are any . in the codes
check_decimals_CP <- SMR_CP %>% 
  mutate(has_dot = grepl("\\.", code_trimmed))

table(check_decimals_CP$has_dot) #no decimal places

# using CALIBER phenotypes by Kuan so filter only for cardiovascular 
cardio_pheno <- SMR_CP %>% 
  filter(PhenotypeGroup == "Cardiovascular") %>% 
  mutate(event_date = as.Date(EventDate))

# Explore the phenotypes included in the HDR UK cardiovascular disease
types_cardiac_events <- cardio_pheno %>% 
  group_by(PhenotypeName) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

#only keep phenotypeName PE and DVT uisng HDR phenotypes PE and venous thromboembolic disease excluding PE

thrombotic <- cardio_pheno %>% 
  filter(PhenotypeName == "Pulmonary embolism" |
           PhenotypeName == "Venous thromboembolic disease (Excl PE)")


#check what is included in Venous thromboembolic disease
types_thrombotic <- thrombotic %>% 
  group_by(CodeDesc) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))


#tidy and link to combined study population
rm(cardio_pheno, types_cardiac_events, types_thrombotic,SMR_CP, check_decimals_CP)

all_thrombotic_events <- thrombotic %>% 
  select(ppid, EventDate, PhenotypeName, CodeDesc, code_trimmed) %>% 
  distinct()


#check if any of these had more than one EventDate
additional_eventdate <- all_thrombotic_events %>% 
  group_by(ppid) %>% 
  summarise(event_count = n_distinct(EventDate)) %>% 
  filter(event_count > 1)


#only keep the earliest date of either DVT/PE

event_earliest_date_all_smr <- all_thrombotic_events %>% 
  mutate(event_date = as.Date(EventDate)) %>% 
  group_by(ppid) %>% 
  slice(which.min(event_date))


# load study population
load("study.RData")

#keep study population ppid, index date group and group for exclusions
study_pop <- combined_df %>% 
  select(ppid, index_date, Group) %>% 
  distinct()

#join datasets
study_pop_thrombotic_smr <- study_pop %>% 
  left_join(event_earliest_date_all_smr, by = "ppid")

prior_thrombotic_smr <- study_pop_thrombotic_smr %>% 
  filter(event_date <= index_date)

length(unique(prior_thrombotic_smr$ppid)) #check number that will be excluded
table(prior_thrombotic_smr$Group)


# remove these from the combined_df main study population

combined_smr_thrombo_removed <- combined_df %>% 
  filter(!ppid %in% prior_thrombotic_smr$ppid)

length(unique(combined_df$ppid)) #
length(unique(combined_smr_thrombo_removed$ppid)) #

#-------------------------------------------------------------------------------#
#check the number in GP records with prior PE/DVT

#clear environment keep combined_smr_thrombo_removed, study_pop datasets
rm(additional_eventdate, check_decimals_CP, cardio_pheno, SMR_CP, all_thrombotic_events, study_pop_thrombotic_smr, prior_thrombotic_smr, combined_df, thrombotic)

#open GP records
#Open GP Read codes for phenotypes
gp_regC <-read.delim(".tsv")
glimpse(gp_regC)


cardio_pheno <- gp_regC %>% 
  filter(PhenotypeGroup == "Cardiovascular") %>% 
  mutate(event_date = as.Date(EventDate))

thrombotic <- cardio_pheno %>% 
  filter(PhenotypeName == "Pulmonary embolism" |
           PhenotypeName == "Venous thromboembolic disease (Excl PE)")


all_thrombotic_events <- thrombotic %>% 
  select(ppid, EventDate, PhenotypeName, CodeDesc) %>% 
  distinct()


#only keep the earliest date 

event_earliest_date_all <- all_thrombotic_events %>% 
  mutate(event_date = as.Date(EventDate)) %>% 
  group_by(ppid) %>% 
  slice(which.min(event_date))

#join datasets with GP records
study_pop_thrombotic_gp <- study_pop %>% 
  left_join(event_earliest_date_all, by = "ppid")

prior_thrombotic_gp <- study_pop_thrombotic_gp %>% 
  filter(event_date <= index_date)

length(unique(prior_thrombotic_gp$ppid)) # some to of these individuals would have already been removed

# Check how many remain in the combined_smr_thrombo_removed dataset

gp_thrombotic <- prior_thrombotic_gp %>% 
  filter(ppid %in% combined_smr_thrombo_removed$ppid)

length(unique(gp_thrombotic$ppid))# additional number to be removed
table(gp_thrombotic$Group)
#
combined_all_exclusions <- combined_smr_thrombo_removed %>% 
  filter(!ppid %in% prior_thrombotic_gp$ppid)

length(unique(combined_all_exclusions$ppid)) # study population with all exclusions now applied

#------------------------------------------
#tidy environment and now find those with PE/DVT outcome using only ICD 10 codes HDR Phenotype DVT/PE.

#tidy environment
rm(study_pop, event_earliest_date_all, gp_regC, gp_thrombotic, prior_thrombotic_gp, thrombotic, study_pop_thrombotic_gp, cardio_pheno, combined_smr_thrombo_removed, all_thrombotic_events)

#check future events in those who did not have prior record only including those 6 weeks after surgery. Use event_earliest_date_all_smr as these are ICD outcome

#tidy thrombotic events keeping ppid, event_date, PhenotypeName
thrombo_events <- event_earliest_date_all_smr %>% 
  select(ppid, event_date, PhenotypeName) %>% 
  distinct()

#only keep those with future events in the study population
future_thrombotic <- thrombo_events %>% 
  filter(ppid %in% combined_all_exclusions$ppid)


# now need to check these thrombotic events >6 weeks post surgery

future_thrombotic_events <- future_thrombotic %>% 
  left_join(combined_all_exclusions, by = "ppid")

#find those with less than 6 weeks between index date and event date
future_thrombotic_events <- future_thrombotic_events %>% 
  mutate(time_to_event = event_date - index_date)

#remove the small number who had DVT/PE after surgery
throm_outcomes <- future_thrombotic_events %>% 
  filter(time_to_event > 2)

length(unique(throm_outcomes$ppid)) #

#check whether these already have cvd_events

check_outcome_dates <- throm_outcomes %>% 
  filter(!is.na(cvd_date)) 

#X had IHD before DVT so do not recorded new event as will already by censored prior (censoring occurs at first CVD event)
remove_id <- check_outcome_dates %>% 
  filter(cvd_date < event_date)

throm_outcomes_clean <- throm_outcomes %>% 
  filter(!ppid %in% remove_id$ppid)

#X had PE before IHD so remove cvd_date and cvd_event for that ppid "" in the dataset

tidy_outcomes_combined <- combined_all_exclusions %>% 
  mutate(cvd_date = ifelse(ppid == "",
                           NA, cvd_date),
         cvd_event = ifelse(ppid == "",
                            NA, cvd_event)
)

check <- tidy_outcomes_combined %>% 
  filter(ppid == "")


#tidy these and create new cvd_date and cvd_event
length(unique(throm_outcomes_clean$ppid)) #

throm_outcomes_clean <- throm_outcomes_clean %>% 
  select(ppid, event_date, Group) %>% 
  distinct()

table(throm_outcomes_clean$Group) # 

#change event_date to cvd_date and create thrombo for cvd_event
throm_outcomes_clean <- throm_outcomes_clean %>% 
  select(-Group) %>% 
  rename(cvd_date = event_date) %>% 
  mutate(cvd_event = "thrombo")

#join the dataset with exclusions use coalesc so new cvd_date (cvd_date.y) is taken when cvd_date.x is NA and new cvd_event.y is taken when cvd_event (cvd_event.x) is NA

final_outcomes <- combined_all_exclusions %>% 
  left_join(throm_outcomes_clean, by = "ppid") %>% 
  mutate(
    cvd_date = coalesce(cvd_date.y, cvd_date.x),
    cvd_event = coalesce(cvd_event.y, cvd_event.x)
  ) 

#check below was done prior to removing other cvd dates

#check <- final_outcomes %>% 
#  filter(!is.na(cvd_date.x) | 
#           !is.na(cvd_date.y) | 
#           !is.na(cvd_date)) %>% 
#  select(ppid, cvd_date.x, cvd_date.y, cvd_date)

final_outcomes <- final_outcomes %>% 
  select(-c(cvd_date.x, cvd_date.y, cvd_event.x, cvd_event.y)) %>% 
  distinct()

length(unique(final_outcomes$ppid))

#check number of outcomes
check <- final_outcomes %>% 
  filter(!is.na(cvd_event)) %>% 
  select(ppid, Group, cvd_event) %>% 
  distinct()

length(unique(check$ppid))
table(check$cvd_event, check$Group)

save(final_outcomes, file = "final_outcomes.RData")
