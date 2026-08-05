###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Cleaning tranexamic acid from prescribing information. Prescribing information includes paid date, formulation, quantity of tablets, BNF codes and description.

#Steps
#(1) Identify prescriptions of tranexamic acid
#(2)Link data set of prescribing information to data sets with date of birth and deaths by ppid which is person ID.
#(3) remove any prescriptions before the age of 18 years so all index dates (start date) begin once individuals are 18 years or over.
#(4) calculate the quantity of tablets for each paid date.Each prescription has a paid date for individuals will have one or more paid dates (rows in the data set)
#(5) calculate time since first paid date (index date) for every script and interval between paid dates
#(6) calculate monthly quantity of tablets

#####################################################################################
#Clear global environment
#rm(list = ls())

# Load Packages 

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load dataset into R from the RData files for date of birth and death and prescription dataset
load("dob_cohort.RData") #date of birth file
load("deaths.RData") # deaths file
load("pis.RData") # prescription data

# link date of birth and death data. 

hmb_demog_death <- merge(dob_cohort, deaths, by = "ppid", all = TRUE)
head(hmb_demog_death)

# Only include date of death, date of birth, ethnicity and person_ID for now

hmb_demog_death <- hmb_demog_death %>% 
  select(person_ID, date_of_birth, death_date, ethnicity) 


#(1) Use prescription data to identify those with tranexamic prescription then add only this information to the combined demographic and death data. 

#First identify tranexamic acid using BNF codes
df_tranexamic_acid <- pis[grep("0211000P0AAACAC|0211000P0AABCBC|0211000P0BBAAAC", pis$PI.BNF.Item.Code, ignore.case = T), ]

length(unique(df_tranexamic_acid$ppid)) # check number of individuals

#include only those that have tablets and remove those with solution
table(df_tranexamic_acid$PI.Drug.Formulation) 
solutions <- df_tranexamic_acid %>% 
  filter(PI.Drug.Formulation == "SOLN")

View(solutions) #check drug description of solutions.

df_tranexamic_acid <- df_tranexamic_acid %>% 
  filter(PI.Drug.Formulation == "TABS")

#check only tablets included
table(df_tranexamic_acid$PI.Drug.Formulation) # 

# check drug strength information
table(df_tranexamic_acid$PI.Item.Strength...UOM) # check strength
any(is.na(df_tranexamic_acid$PI.Item.Strength...UOM )) # check for NA
any(df_tranexamic_acid$PI.Item.Strength...UOM == "") # check for blanks 

#check description required
table(df_tranexamic_acid$PI.BNF.Item.Description)
# some are cyklokapron tabs all others as tranexamic acid. Cyclokapron is the brand name. Check for missing values or blank if non then no need to keep
any(is.na(df_tranexamic_acid$PI.BNF.Item.Description)) # check for NA
any(df_tranexamic_acid$PI.BNF.Item.Description == "") # check for blanks 


#----------------------------------------------------------------------------------------------------#
#(2) Combine this tranexamic acid information to the cohort
hmb_tranexamic_acid <- merge(df_tranexamic_acid, hmb_demog_death, by = "ppid", all = T)

# remove previous datasets to keep global environment organised
rm(pis, deaths, dob_cohort, hmb_demog_death,df_tranexamic_acid, solutions)

# Tidy dataset
# rename variables
hmb_tranexamic_acid_1 <- hmb_tranexamic_acid %>% 
  rename(paid_date = 'Paid.Date', 
         paid_quantity = 'Paid.Quantity',
         date_of_birth = "date_of_birth_updated"
        ) %>% 
  select(ppid, date_of_birth, death_date, ethnicity,  
         paid_date, paid_quantity)
                                                           
glimpse(hmb_tranexamic_acid_1)

 
# changes paid date to a date, all other dates in correct format.
hmb_tranexamic_acid_1$paid_date <- hmb_tranexamic_acid_1$paid_date %>% as.Date()

#-----------------------------------------------------------------------------------------------------------------------------#
#(3) Remove prescription before the age of 18 years

# cohort criteria are aged >= 18 years old, hence:
# remove all prescriptions before the 18th birthday of individuals

hmb_tranexamic_acid_1 <-hmb_tranexamic_acid_1 %>% 
  mutate(age_at_prescription = as.numeric(paid_date - date_of_birth)/365.25)

table(hmb_tranexamic_acid_1$age_at_prescription >= 18)


# remove scripts issued before age 18 years. This step will also remove NA's so people who were not issued with tranexamic acid
df_tranexamic_acid <- hmb_tranexamic_acid_1 %>% 
  filter(age_at_prescription >= 18)

table(df_tranexamic_acid$age_at_prescription >= 18) 

# check prescription after 2023-04-30 (this is the end of study period so should be zero)
table(df_tranexamic_acid$paid_date > "2023-04-30") # 

#check number of people remaining
length(unique(df_tranexamic_acid$ppid)) 

# for everyone find the first paid_date for tranexamic acid and create new variable index_date keeping all paid dates
df_tranexamic_acid <-  df_tranexamic_acid %>% 
  group_by(ppid) %>% 
  mutate(index_date = min(paid_date)) %>% 
  ungroup()

length(unique(df_tranexamic_acid$ppid))
#----------------------------------------------------------------------------------
#(4) calculate total quantity per paid_date by sum

df_tranexamic_acid <- df_tranexamic_acid %>% 
  group_by(ppid, paid_date) %>% 
  mutate(total_paid_quantity = sum(paid_quantity)) %>% 
  as.data.frame()

View(df_tranexamic_acid)
glimpse(df_tranexamic_acid)
length(unique(df_tranexamic_acid$ppid)) # 

# check for any duplicates within dataset
any(duplicated(df_tranexamic_acid)) # false 

# save ID's for anyone who had a tranexamic acid script paid date from age 18 years onwards

save(df_tranexamic_acid, file = ".RData")

#---------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#In the following section the following criteria is applied to identify regular users.

# Definition regular (intermittent/during periods) tranexamic acid use:
# - at least 1 repeat prescription in the first year
# - tabs per months should be within the required range (see below) for the first half year for (1) any scripts and for (2) all scripts as sensitivity analysis

# required range:
# 2 tabs (~2x500mg) 2-4 times daily, 2-5 days per month
# this is equivalent to 4-8 tabs daily, 2-5 times per month
# this is equivalent to 8-40 tabs per month (or 48-240 tabs per 6 months or 96-480 tabs per year)
# if the minimum would be 3 times daily, the lower limit of tabs would be: 12 tabs/month

# (5) calculating time intervals and identifying those that have another paid date within a year 

# calculate time_since_index_date which is time since first paid date i.e index date. For rows of the index date it will be 0.
df_tranexamic_acid <- df_tranexamic_acid %>% 
  mutate(time_since_index_date = as.numeric(paid_date - index_date))


# calculate prescription interval with lead (time to next prescription)
df_tranexamic_acid <- df_tranexamic_acid %>% 
  group_by(ppid) %>% 
  arrange(ppid, paid_date) %>%
  mutate(time_to_next_prescription = as.numeric(lead(paid_date) - paid_date)) %>% 
  as.data.frame()

# calculate prescription interval lag (time since last prescription)
df_tranexamic_acid <- df_tranexamic_acid %>% 
  group_by(ppid) %>% 
  arrange(ppid, paid_date) %>%
  mutate(time_since_last_prescription = as.numeric(paid_date - lag(paid_date))) %>% 
  as.data.frame()

View(df_tranexamic_acid)

# filter individuals with at least 1 repeat prescription within the first year
ids_with_repeat_prescription_within_1_year <- df_tranexamic_acid %>%
  filter(!is.na(time_since_index_date) & 
           time_since_index_date < 365.25 & 
           time_since_index_date != 0)

length(unique(ids_with_repeat_prescription_within_1_year$ppid)) 
View(ids_with_repeat_prescription_within_1_year)

#save these IDs_for those that had another script within a year - satisfying one criterion.
save(ids_with_repeat_prescription_within_1_year, file = ".RData")


#----------------------------------------------------------------------------------

# (6) individuals in which we could calculate dosage during the first half year
# this means individuals should have a prescription after the first half year, otherwise we don't know how long the time interval is during which they could use their tabs. We need this to calculate the monthly quantity of tablets

ids_with_repeat_prescription_after_6_months <- df_tranexamic_acid %>% 
  filter(!is.na(time_since_index_date) & 
           time_since_index_date > 182.6) 

length(unique(ids_with_repeat_prescription_after_6_months$ppid)) # this will include people who had a second script over a year after the first but these will be excluded once we have calculated the monthly number of tablets as the monthly number of tablets needs to have been received in the first 6 months.


# overlap between the those who have another paid date within a year and who will have a calculation of number of monthly tablets (because they have a second paid date after 6months which allows calculation of monthly quantity)
intersect(ids_with_repeat_prescription_after_6_months$ppid, ids_with_repeat_prescription_within_1_year$ppid) %>% length() # 


# calculate tabs per month - should be able to calculate this for people who fit criterion 1 therefore both criteria
df_tranexamic_acid <- df_tranexamic_acid %>% 
  mutate(tabs_per_month = total_paid_quantity/(time_to_next_prescription/30.44))

View(df_tranexamic_acid)


#----------------------------------------------------------------------------#
# Primary analysis will use a broader criteria which includes everyone who has at least one tabs per month within the range and meets the criteria of a regular user
# (1) include those with paid date within a year
# (2) include those with repeat prescription after 6 months
# (3) include those whose time since index less than 6 months (so that monthly amount can be applied for 6 month period)
# (4) filter ids that have the quantity within the range required for at least one paid date, so there may be tabs per month outside these cut off as well as at least one within the range.

ids_meeting_all_criteria <- df_tranexamic_acid %>% 
  filter(
    ppid %in% ids_with_repeat_prescription_within_1_year$ppid &
      ppid %in% ids_with_repeat_prescription_after_6_months$ppid,
    time_since_index_date <=  182.6, 
    tabs_per_month >= 8 & tabs_per_month <= 40 
  )

View(ids_meeting_all_criteria) # all tabs in correct range, 
any(is.na(ids_meeting_all_criteria$tabs_per_month)) # no NAs for tabs per month
length(unique(ids_meeting_all_criteria$ppid)) # 

# Use ids meeting all criteria for the main analysis 

TA_exposure <- df_tranexamic_acid %>% 
  filter(ppid %in% ids_meeting_all_criteria$ppid )

length(unique(TA_exposure$ppid))# 

View(TA_exposure) 

# save this dataset
save(TA_exposure, file = ".RData")

#----------------------------------------------------------------------------#

# For sensitiviy analysis will use another method which identifies everyone with criteria for regular user and within range to calculate tabs per month but who do not have monthly quantity within range. By excluding these people will identify those whose tabs per month are always within the range

# include those with paid date within a year
# include those with repeat prescription after 6 months
# include those whose time since index less than 6 months (so that monthly amount can be applied)
# filter ids that do NOT have the quantity within the range required but have other criterion

ids_not_meeting_tab_criteria <- df_tranexamic_acid %>% 
  filter(
    ppid %in% ids_with_repeat_prescription_within_1_year$ppid &
      ppid %in% ids_with_repeat_prescription_after_6_months$ppid,
    time_since_index_date <= 182.6,
    tabs_per_month < 8 | tabs_per_month > 40)

length(unique(ids_not_meeting_tab_criteria$ppid)) #
View(ids_not_meeting_tab_criteria) # correct tabs are less than 8 and more than 40
any(is.na(ids_not_meeting_tab_criteria$tabs_per_month)) # no NAs for tabs per month

# These do not have numbers within criteria. Change code to only include those within quantity range

ids_meeting_all_criteria_with_tabs_exclusion <- df_tranexamic_acid %>% 
  filter(
    ppid %in% ids_with_repeat_prescription_within_1_year$ppid &      
      ppid %in% ids_with_repeat_prescription_after_6_months$ppid &
      time_since_index_date < 182.6,
    !ppid %in% ids_not_meeting_tab_criteria$ppid
  )

any(is.na(ids_meeting_all_criteria_with_tabs_exclusion$tabs_per_month)) # check for NA
length(unique(ids_meeting_all_criteria_with_tabs_exclusion$ppid)) # number of people

# save this stricter definition for sensitivity analysis

TA_exposure_strict <- df_tranexamic_acid %>% 
  filter(ppid %in% ids_meeting_all_criteria_with_tabs_exclusion$ppid )

length(unique(TA_exposure_strict$ppid))# 

View(TA_exposure_strict) 

# save this dataset
save(TA_exposure_strict, file = ".RData")


#-----------------------------------------------------------------------#
# Exploration of the difference between these groups - TA strict and TA exposure.

#check if there are people in both groups
intersect(ids_not_meeting_tab_criteria$ppid, ids_meeting_all_criteria$ppid) %>% length() #

# examine those people would be included for the all criteria and excluded in the criteria with the inverse of the tab_exclusion was applied

both_included_excluded <- df_tranexamic_acid %>% 
  filter(
    ppid %in% ids_not_meeting_tab_criteria$ppid &
      ppid %in% ids_meeting_all_criteria$ppid,
    time_since_index_date <= 182.6)

length(unique(both_included_excluded$ppid)) #
any(is.na(both_included_excluded$tabs_per_month))
View(both_included_excluded) # individual can have multiple rows so have some rows excluded and some rows included. If we use the exclusion of IDs with tabs outside criteria we are excluding people who at some point might have tabs within the range and outside as well. If we use the range method we will include those that every had tabs within range.
# As soon as someone is outside that range then they can not be in the cohort. Intermittent users for at least first 6 months. Strict with definition of intermittent use don't include those people that had script outside the range. Use of the IDs that do not meet tab criteria as will exclude anyone who has a row that is outside the definition. The stricter definition used for sensitivity analysis.

#--------------------------------------------------------------------------------#
#Final steps 
# (1) identifying those that died within the first year without 2nd script to check whether they should be included in the cohort
# (2) check if any deaths after paid dates

# load those that ever received a script for tranexamic acid 
load("df_tranexamic_acid.RData")

#load those that had another script within a year of the index date
load("IDs_with_repeat_tranexamic_acid_within_year.RData")


#---------------------------------------------------------------------------------#
# To check we have not missed any individuals who have died before second prescription as they could have been using Tranexamic acid meeting all criteria until death but because they did not have a second paid date within a year where excluded. Those that died after a year and did not receive a script a year after the index date would be excluded in anycase. Therefore use dataset of everyone who had paid date for TA to find those that had follow up of a year.

All_follow_up_death <- df_tranexamic_acid %>% 
  filter(!is.na(death_date)) %>% 
  mutate(follow_up_period = death_date - index_date)

ids_die_within_first_year <- All_follow_up_death %>% 
  filter(follow_up_period < 365.25)

length(unique(ids_die_within_first_year$ppid)) # number who died in first year


#From these how many did not have another script within a year of index date. Exclude ppids of those that had a script within a year. 

ids_die_within_first_year_no_second_script <- ids_die_within_first_year %>% 
  filter(!ppid %in% ids_with_repeat_prescription_within_1_year$ppid)

length(unique(ids_die_within_first_year_no_second_script$ppid)) # number of  people tabs per month is NA.

#for this group add another paid date which corresponds to death date. This can then be used to calculate number of tabs per month to see if they would be included into the cohort, if they were taking medications up until death.

new_dates <- ids_die_within_first_year_no_second_script %>% 
  group_by(ppid) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(paid_date = death_date)

#combine this new date with the ids that die dataset to create another paid date row.

new_paid_date_firstyear_deaths <- ids_die_within_first_year_no_second_script %>% 
  bind_rows(new_dates)

View(new_paid_date_firstyear_deaths)

# re-calculate prescription interval (time to next prescription), time_since_index_date, and tabs per months

# calculate time_since_index_date which is time since first paid date i.e index date. For rows of the index date it will be 0.
new_paid_date_firstyear_deaths<-new_paid_date_firstyear_deaths %>% 
  mutate(time_since_index_date = as.numeric(paid_date - index_date))


# calculate prescription interval with lead (time to next prescription)
new_paid_date_firstyear_deaths <- new_paid_date_firstyear_deaths %>% 
  group_by(ppid) %>% 
  arrange(ppid, paid_date) %>%
  mutate(time_to_next_prescription = as.numeric(lead(paid_date) - paid_date)) %>% 
  as.data.frame()

# calculate prescription interval lag (time since last prescription)
new_paid_date_firstyear_deaths <- new_paid_date_firstyear_deaths %>% 
  group_by(ppid) %>% 
  arrange(ppid, paid_date) %>%
  mutate(time_since_last_prescription = as.numeric(paid_date - lag(paid_date))) %>% 
  as.data.frame()

View(new_paid_date_firstyear_deaths)
length(unique(new_paid_date_firstyear_deaths$ppid)) #check number of people

# calculate tabs per month
new_paid_date_firstyear_deaths <- new_paid_date_firstyear_deaths %>% 
  mutate(tabs_per_month = total_paid_quantity/(time_to_next_prescription/30.44))


# First use Main criteria inclusion steps then use sensitivity criteria
# filter ids that have the quantity within the range required for at least one paid date, so there may be tabs per month outside these cut off as well as at least one within the range.

death_ids_meeting_all_criteria <- new_paid_date_firstyear_deaths %>% 
  filter(time_since_index_date <=  182.6, 
         tabs_per_month >= 8 & tabs_per_month <= 40 
  )

View(death_ids_meeting_all_criteria) # all tabs in correct range, 
any(is.na(death_ids_meeting_all_criteria$tabs_per_month)) # check for NA
length(unique(death_ids_meeting_all_criteria$ppid)) # 

# Open the TA group created from those that died not die saved in line 253 TA_exposure

load("TA_exposure.RData")

length(unique(TA_exposure$ppid)) 

# merge ids into the TA_exposure group
TA_exposure_final <- TA_exposure %>% 
  bind_rows(death_ids_meeting_all_criteria)

length(unique(TA_exposure_final$ppid))
#save this final dataset
save(TA_exposure_final, file = ".RData")

#----------------------------------------------------------------------------------# Now use sensitivity check approach
# all tabs per months during those first six months should be within the criteria range.
# thus, those individuals with at least 1 prescription outside the range should be excluded


ids_dead_not_meeting_tab_criteria <- new_paid_date_firstyear_deaths %>% 
  filter(!is.na(time_since_index_date),
         time_since_index_date <= 182.6,
         tabs_per_month < 8 | tabs_per_month > 40)

length(unique(ids_dead_not_meeting_tab_criteria$ppid)) #

# remove ids where paid date/new index date is after death date
# this happens because paid date is artificial date (always last day of the months), so if someone dies at the beginning of the months
# their paid date can be the last day of the month


ids_dead_meeting_all_criteria_with_tabs_exclusion <- new_paid_date_firstyear_deaths %>% 
  filter(!ppid %in% ids_dead_not_meeting_tab_criteria$ppid)

length(unique(ids_dead_meeting_all_criteria_with_tabs_exclusion$ppid)) 

View(ids_dead_meeting_all_criteria_with_tabs_exclusion)

deaths <- ids_dead_meeting_all_criteria_with_tabs_exclusion %>% 
  group_by(ppid) %>% 
  filter(death_date < paid_date) # check there are no death_dates before paid_dates

# Open the strict criteria for TA
load("TA_exposure_strict.RData")


# merge ids into the TA_exposure group
TA_exposure_strict_final <- TA_exposure_strict %>% 
  bind_rows(ids_dead_meeting_all_criteria_with_tabs_exclusion)

length(unique(TA_exposure_strict$ppid)) # 
length(unique(TA_exposure_strict_final$ppid)) #This is final number using strict definition for TA exposure.

# save this dataset
save(TA_exposure_strict, file = "TA_strict_exposed_group.RData")


