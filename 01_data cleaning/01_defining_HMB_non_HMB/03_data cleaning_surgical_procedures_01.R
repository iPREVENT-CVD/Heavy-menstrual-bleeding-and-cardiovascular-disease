
#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Data cleaning for HMB study creating the exposure groups (ablation, hysterectomy + hmb) and comparator  (lapascopic sterilisation). SMR_C are Scottish Morbidity Records 01 with ICD codes, there are 7 positions included on the record.The SMR01 dataset includes the admission, operation and discharge dates. 

# This script creates: 
#(i) Code lists for all surgical exposures of interest and creates flag for each event within dataset.  As only those with Hysterectomy require HMB code dataset for HMB events will be saved separately. This will be used in later R script when defining Hysterectomy. 
#(ii) Keeps only people with events of interest (surgical procedures only)
#(iii) Links main operation date and admission dates from SMR01 to coded surgical procedures SMR_C.

###################################################################################
#Clear global environment
#rm(list = ls())

# Load Packages 

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate) 

#set working directory to linked data as using PIS
setwd("")

SMR_C <-read.delim("SMR_C.tsv")
Surgery_dates <-read.delim("SMR01.tsv")

#=============================================================================#
### (1) First section of code tidies SMR_C file and creates code for each surgical procedure of interest.

## Exploration and tidying of SMR_C 

glimpse(SMR_C)
length(unique(SMR_C$ppid)) # 
table(SMR_C$Source) # all SMRO1
table(SMR_C$CodeType) # ICD and OPCS4
table(SMR_C$Position) # 7 different positions and number of events and position on record

#explore codes of interest in SMR01:
codes <- SMR_C %>%
  group_by(CodeType, Code ) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))
View(codes)

# Some codes include blank spaces around so first need to trim the code variable to text required.

SMR_C <- SMR_C %>% 
  mutate(code_trimmed = str_trim(Code))

# check whether there are any . in the codes
check_decimals <- SMR_C %>% 
  mutate(has_dot = grepl("\\.", Code))

table(check_decimals$has_dot)# no decimals in Codes

#change date to as.Date
SMR_C <- SMR_C %>% 
  mutate(EventDate = as.Date(EventDate)) %>% 
  rename(event_date = "EventDate")

## Following steps identifies those that have had procedures of interest.

#(i) create code lists for all exposures and the comparison group and create list of IDs that have those codes needed for exclusions. 

#Endometrial ablation codes from OPCS
ablation_codes <- c("Q162", "Q163", "Q164", "Q165", "Q166", "Q176", "Q177")

# Lap sterilisation codes
sterilisation_codes <- c("Z302", "V252", "Q351", "Q352", "Q353", "Q354", "Q358", "Q359")

#codes for hysterectomy
hysterectomy_codes <- c("Q074", "Q075", "Q078", "Q079", "Q081", "Q082", "Q083", "Q088", "Q089")

#codes for oophrectomy only
with_oophrectomy_code <- c("Q221", "Q223") # also checked Q222 but there are no people with this code in the dataset.

# HMB ICD codes
hmb_codes <- c("N92", "N920", "N921", "N922", "N924", "626", "6262", "6263", "6266", "6268", "6269")

# (ii) For each person create an event flag for each of the procedures above
Surgical_group <- SMR_C %>% 
  mutate(
    event_flag = case_when(
      code_trimmed %in% ablation_codes ~ "Ablation",
      code_trimmed %in% sterilisation_codes ~ "Sterilisation",
      code_trimmed %in% hysterectomy_codes ~ "Hysterectomy",
      code_trimmed %in% with_oophrectomy_code ~ "Oophrectomy",
      TRUE ~ NA_character_
    ) 
  )


table(Surgical_group$event_flag) # checking all event_flags are included

length(unique(Surgical_group$ppid)) # number of people
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#there will be NA in event_flag column for people who do not have these events

sum(is.na(Surgical_group$event_flag)) # These need to removed from the event_flags as these are not procedures.They may have heavy menstrual bleeding (HMB) codes but these will be identified and saved separately at end of this script.


# (iii) explore those with NA to see if they have other events
None <- Surgical_group %>% 
  filter(is.na(event_flag))

table(None$Code)

View(None)

None_codes <- None %>%
  group_by(CodeType, Code ) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))
View(None_codes)

Surgical_group <- Surgical_group %>% 
  filter(!is.na(event_flag))

sum(is.na(Surgical_group$event_flag))

# (iv) People may have more than one code used on the same event date such as an ICD 10 or OPCS code for the same indication on the same date. 
#Apply this distinct function to whole dataset, to remove any duplicate events (flagged because of codes) on the same event date for each person. 
Surgical_group <- Surgical_group %>% 
  distinct(ppid, event_date, event_flag, .keep_all = TRUE)

length(unique(Surgical_group$ppid))  #number with surgical procedure of interest.

#remove unwanted columns keep Position for now
Surgical_group <- Surgical_group %>% 
  select(ppid, event_date, event_flag, Position)

table(Surgical_group$event_flag)

#==============================================================================#

### (2) Open the file with surgical dates and combine with the dataset created of surgical procedures. This will remove multiple dates for procedures as it will allow the date of the operation to be used as the event-date. Multiple dates may be created in phenotype as past operations may be added to hospital records.

# explore and tidy smro1
glimpse(Surgery_dates)

# remove capitals letters from columns and keep columns with operation dates (up to
Surgery_dates <-Surgery_dates %>% 
  rename_with(tolower)

#only keep dates of main operation and admission date, with ppid

Surgery_dates_1 <- Surgery_dates %>% 
  select(ppid, admission_date,date_of_main_operation)

#change all to date format

Surgery_dates_1 <- Surgery_dates_1 %>% 
  mutate(across
         (c(admission_date, date_of_main_operation),
           as.Date))

# Combine datasets (phenotypes and dates) 
# left join with only ppid, keeps every event date, admission date and main operation date. However people can be admitted for factors unrelated to operation or due to post-operative complication and this leads to a dataset with lots of row with different admission and dates of main operation which suggests multiple operations. Will need to join using main operation date as well as ppid.

#Therefore left join with ppid and when event_date matches main operation date

Surgical_group_3 <- Surgical_group %>% 
  left_join(Surgery_dates_1, by = c("ppid", "event_date" = "date_of_main_operation"),
             relationship = "many-to-many")

length(unique(Surgical_group_3$ppid)) #number of people

# check if there are any duplicated rows with this method
duplicated_rows_1 <- Surgical_group_3[duplicated(Surgical_group_3),] # number of duplicates

#Remove the duplicates
Surgical_group_3 <- Surgical_group_3 %>% distinct(.keep_all = TRUE)

# Save this datatset for creating all exposure groups.
save(Surgical_group_3, file = "/Surgical_group.RData")


#--------------------------------------------------------------------------------#

# (3) Now save dataset of people with HMB codes. 
Surgical_group_for_HMB <- SMR_C %>% 
  mutate(
    event_flag = case_when(
      code_trimmed %in% hmb_codes ~ "Hmb",
      TRUE ~ NA_character_
    ) 
  )

Hmb_group <- Surgical_group_for_HMB %>% 
  filter(event_flag == "Hmb")

length(unique(Hmb_group$ppid)) #

Hmb_group <- Hmb_group %>% 
  select(ppid, event_date, event_flag, Position)

#Save this Hmb dataset with dates to link to Hysterectomy dataset later
save(Hmb_group, file = "Hmb_group.RData")

#=====================================================================================
