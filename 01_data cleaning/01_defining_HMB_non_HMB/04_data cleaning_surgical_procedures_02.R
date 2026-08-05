#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease


# All relevant surgical events are now coded with dates linking codes with a operation dates. 
#(1) Make sure each person has one event date for each surgical procedure which will be first time it is coded with an operation date.
# (2) This script does this for ablation but the same steps are done for sterilization, hysterectomy and bilateral salphino-oophrectomy to every individual has one event and one date for each of these procedures. Each procedure is then saves as ablation_dates etc.
#(3) Hysterectomy must be linked to HMB code and then saved so once each individual has one date for hysterectomy this is linked to ICD codes for HMB. Those that have an ICD code 2 years before to 2 weeks after hysterectomy date are included. This is to allow time for the patient pathway from referral to clinic to surgery.


#Clear global environment, restart R session
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)


# Open file with surgical cohorts 
load("Surgical_group.RData")

length(unique(Surgical_group_3$ppid)) # check correct number of individuals

#-----------------------------------------------------------------------------#

#(1) Filter into each surgical procedure, then find those with more than one event date. Note that an individual ppid can be in more than one group. For example if someone had an ablation and hysterectomy that ppid will be in the ablation and hysterectomy group. 

table(Surgical_group_3$event_flag) # this gives the number of times it is coded in the dataset not the number of individuals as some may have more than one event coded.

#The following code is for ablation but this same code is then applied to sterilisation.

Ablation <- Surgical_group_3 %>% 
  filter(event_flag == "Ablation")

#------------------------------------------------------------------------------------
# Starting with ablation, explore this group.
  
length(unique(Ablation$ppid)) # number with ablation

# Find out if anyone has more than 1 distinct event date

Ablation %>% 
  group_by(ppid) %>% 
  summarise(n_event_dates = n_distinct(event_date), .groups = "drop") %>% 
  summarise(n_people_more_than_one_event = sum (n_event_dates > 1))

# Examine those with more than one event date

Multiple_ablation_events <- Ablation %>% 
  group_by(ppid) %>% 
  filter(n_distinct(event_date) > 1) %>% 
  ungroup()

#Check how many additional dates are given.

Number_additional_ablations <- Multiple_ablation_events %>% 
  group_by(ppid) %>% 
  summarise(n_event_dates = n_distinct(event_date), .groups = "drop")

#check time interval

Multiple_ablation_events <- Multiple_ablation_events %>% 
  group_by(ppid) %>% 
  mutate(first_ablation = min(event_date),
         second_ablation = max(event_date))

Multiple_ablation_events <- Multiple_ablation_events %>% 
  mutate(time_interval = as.numeric(second_ablation - first_ablation)/365)

time_intervals_ablation <- Multiple_ablation_events %>% 
  mutate(period = case_when(
    time_interval <= 1 ~ "less than or 1 year",
    time_interval > 1 ~ "more than a year"
  )) %>% 
  distinct(ppid, period) %>% 
  group_by(period) %>% 
  summarise(count = n())

#Number very small, it is likely the procedure was coded in notes at subsequent admissions take first date.

# Check to see if event_date matches date of admission date and if not see how close dates are 

Check_dates <- Ablation %>% 
  mutate(same_date = event_date == admission_date)

table(Check_dates$same_date) # check number that have different dates

Diff_dates <- Check_dates %>% 
  filter(same_date == FALSE) # 

View(Diff_dates) #All event_dates are 1 day later than the admission date which is fine. They can remain in the dataset - some people are admitted the date before operation.

# For Ablation group create new column called "Ablation date" which is either the event_date if only one event_date or for the people with more than 1 ablation code it is the earliest date. 

Ablation_final <- Ablation %>% 
  group_by(ppid) %>% 
  mutate( "Ablation_date" = min(event_date)) %>% 
  ungroup()

length(unique(Ablation_final$ppid)) #

# Remove unwanted columns.

Ablation_final_1 <- Ablation_final %>% 
  select(ppid, Ablation_date)


#There are people who have multiple rows examine why

Repeated_events <- Ablation_final_1 %>% 
  group_by(ppid) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(ppid)

# these are now duplicated rows. Remove these.

Ablation_dates <- Ablation_final_1 %>% 
  distinct()

save(Ablation_dates, file = "Ablation_dates.RData")

#same process is repeated for sterilisation date resulting in data set called "Sterilisation_dates", then for Hysterectomy and Oophrectomy so each operation has a dataframe with PPid, event and date.

#Hysterectomy group need to be linked to HMB codes prior to saving. HMB 2 years before hysterectomy to 2 weeks after. 

# ---------------------------------------------------------------------------------
# Open dataset with HMB codes
load("Hmb_group.RData")

#hysterectomy dates file called Hysterectomy_1 has already been created in the above code.

length(unique(Hmb_group$ppid)) #check number of people

# Left join Hmb to Hysterectomy_1 will only keep those with Hysterectomy date. event_date here is the hmb date

Hmb_hysterectomy <- Hysterectomy_1 %>% 
  left_join(Hmb_group, by = "ppid")

summary(Hmb_hysterectomy) # 
length(unique(Hmb_hysterectomy$ppid))# check number of people
any(is.na(Hmb_hysterectomy$Hysterectomy_date)) # check NA
any(is.na(Hmb_hysterectomy$event_date)) # check NA

# examine people with NA for Hmb then remove these people from the dataset
No_hmb <- Hmb_hysterectomy %>% 
  filter(is.na(event_date))

View(No_hmb)

Hmb_hysterectomy <- Hmb_hysterectomy %>% 
  filter(!is.na(event_date)) %>% 
  select(ppid, Hysterectomy_date, event_date)
#

any(duplicated(Hmb_hysterectomy)) # look for any duplicates
duplicated <- Hmb_hysterectomy %>% 
  group_by(ppid) %>% 
  filter(duplicated(across(everything())))

Hmb_hysterectomy <- Hmb_hysterectomy %>% 
  distinct() # remove any duplicates

length(unique(Hmb_hysterectomy$ppid)) # 
any(duplicated(Hmb_hysterectomy)) # check for duplicates

# For each person find the event_date (Hmb date) which is closest to the Hysterectomy date

Hmb_hysterectomy_linked <- Hmb_hysterectomy %>% 
  mutate(diff_days = as.numeric(Hysterectomy_date - event_date)) %>% 
  group_by(ppid) %>% 
  mutate(closest_date = event_date[which.min(diff_days)]
  ) %>% 
  ungroup()

length(unique(Hmb_hysterectomy_linked$ppid)) # check number of people

# tidy dataset keeping only ppid, Hysterectomy date and closest date but remain closest date tp Hmb_date. Also remove duplicates.

Hmb_hysterectomy_linked <- Hmb_hysterectomy_linked %>% 
  select(ppid, Hysterectomy_date, closest_date) %>% 
  rename(Hmb_date = closest_date) %>% 
  distinct()

# calculate the interval between hysterectomy date and hmb date (this is the closest date to hysterectomy). 

Hmb_hysterectomy_linked <- Hmb_hysterectomy_linked %>% 
  mutate(diff_days = as.numeric(Hysterectomy_date - Hmb_date)) %>% 
  mutate(time_span = case_when(
    diff_days < -15 ~ "more than 2 weeks after",
    diff_days >= -14 & diff_days <= -1 ~ "2 weeks after",
    diff_days == 0 ~ "same date",
    diff_days > 0 & diff_days <= 90 ~"3 months before",
    diff_days > 90 & diff_days <= 182.5 ~ " 6 months before ",
    diff_days > 182.5 & diff_days <= 365 ~ "12 months before ",
    diff_days > 365 & diff_days <= 547.5 ~ "18 months before ",
    diff_days > 547.5 & diff_days <= 730 ~ "2 years before",
    diff_days > 730 ~ "more than 2 years before"
  ))

table(Hmb_hysterectomy_linked$time_span)
any(is.na(Hmb_hysterectomy_linked$time_span)) # Check for any NA

#Study criteria is Hmb recorded within 2 years before and 2 weeks after hysterectomy for broad criteria

Criteria_applied_broad <- Hmb_hysterectomy_linked %>% 
  filter(diff_days >= -14 & diff_days <= 730)

length(unique(Criteria_applied_broad$ppid)) # check number of people

#Tidy and save this dataset. Index date will be hysterectomy date.
Broad_criteria_Hmb_hysterectomy <- Criteria_applied_broad %>% 
  select(ppid, Hysterectomy_date, Hmb_date)

save(Broad_criteria_Hmb_hysterectomy, file = ".RData")


