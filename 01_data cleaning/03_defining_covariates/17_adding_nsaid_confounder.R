#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease


# Addition of (1) mefenemic acid and nsaids as confounder to main analysis with all outcomes (2) create composite iron deficiency variable

# clear environment
rm(list = ls())

# Load Packages 
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
#------------------------------------------------------------------------------------
#open dataset with all outcomes
load("~/final_outcomes.RData")

#have smaller dataset with ID and index dates
study_pop <- final_outcomes %>% 
  select(ppid, index_date, Group) %>% 
  distinct()

#open prescription data
setwd("")
pis <-read.delim(".tsv")


# identify only chapters of interest in PIS then tidying data to only required information
table(pis$PI.BNF.Chapter.Description)

#explore MS chapter for mefenemic acid
MS <- pis %>% 
  filter(PI.BNF.Chapter.Description == "MUSCULOSKELETAL AND JOINT DISEASES")

colnames(MS)
MS_drugs <- MS %>% 
  select(ppid, Paid.Date, PI.BNF.Item.Code, PI.BNF.Item.Description, PI.BNF.Root.Drug.Description, PI.Approved.Name, PI.Drug.Formulation, PI.Prescribable.Item.Name) %>% 
  distinct()

table(MS_drugs$PI.BNF.Root.Drug.Description)

#includes NSAIDs and mefenemic acid ? include as covariate check numbers. 
 

#-------------------------------------------------------------------------#
#save MS drugs in the study population for discussion

MS_drugs_study <- study_pop %>% 
  left_join(MS_drugs, by = "ppid")

#Only include tablets rather than gels or sprays

MS_drugs_study <- MS_drugs_study %>% 
  filter(PI.Drug.Formulation %in% c("TABS", "CAPS"))

table(MS_drugs_study$PI.Approved.Name)

#save list of drugs in this population to decide which ones could be included
list_ms_drugs <- MS_drugs_study %>% 
  filter(!is.na(Paid.Date)) %>% 
  group_by(PI.Approved.Name, PI.Drug.Formulation) %>% 
  summarise(Counts = n()) %>% 
  arrange(desc(Counts))

write.csv(list_ms_drugs, file = "ms_drugs_types.csv")

#check number of mefenemic acid, mefenemic plus non selective nsaids and mefenemic plus selective and non selective nsaid and add confounder variable

mef_only <- c("MEFENAMIC ACID")

nsaids <- c("DICLOFENAC", "NAPROXEN", "IBUPROFEN")

other_nsaids <- c("CELECOXIB", "ETORICOXIB", "MELOXICAM", "INDOMETACIN",
                  "NABUMETONE",
                "ETODOLAC", "SULINDAC", "FENOPROFEN", "PIROXICAM", "KETOPROFEN",
                "PIROXICAM", "ACECLOFENAC", "DEXIBUPROFEN", "TENOXICAM",
                "FLURBIPROFEN")


#(1) create flags in dataset for each type of nsaid

nsaid_drug_flag <- MS_drugs_study %>% 
  mutate(
    nsaid_drug = case_when(
      PI.Approved.Name %in% mef_only ~ "mef_only",
      PI.Approved.Name %in% nsaids ~"nsaids",
      PI.Approved.Name %in% other_nsaids ~ "other_nsaids",
      TRUE ~ NA_character_
    ) 
  )

table(nsaid_drug_flag$nsaid_drug) # 

length(unique(nsaid_drug_flag$ppid)) # 

#All scripts need to be prior to index date

prior_nsaids <- nsaid_drug_flag %>% 
  filter(!is.na(nsaid_drug)) %>% 
  mutate(paid_date = as.Date(Paid.Date)) %>% 
  filter(paid_date <= index_date)


#only include those who had paid date atleast 12m before index date,

prior_nsaids <- prior_nsaids %>% 
  filter(between(paid_date, index_date - 365.25, index_date))

length(unique(prior_nsaids$ppid)) #

#tidy dataset with only ppid, index and gynae_drug 

prior_nsaid_drugs <- prior_nsaids %>% 
  select(ppid, Group, nsaid_drug) %>% 
  distinct() # 

#check numbers to add to descriptive summary 
table(prior_nsaid_drugs$nsaid_drug, prior_nsaid_drugs$Group)

#create confounding variable nsaids + mefenemic

nsaid_covariates <- prior_nsaid_drugs %>%
  filter(nsaid_drug == "mef_only" |
           nsaid_drug == "nsaids") %>% 
  mutate(nsaid = "Y") %>% 
  select(ppid, Group, nsaid) %>% 
  distinct()

table(nsaid_covariates$nsaid, nsaid_covariates$Group)

#now save to the combined dataset for multiple imputations then modelling
nsaid_covariates <- nsaid_covariates %>% 
  select(-Group) %>% 
  distinct()

final_confounders <- final_outcomes %>% 
  left_join(nsaid_covariates, by = "ppid")


#--------------------------------------------------------------------------#
#adding iron tablets to definition of iron deficiency anaemia

#tidy environment
rm(nsaid_covariates, MS_drugs, MS_drugs_study, prior_nsaids, prior_nsaid_drugs, nsaid_drug_flag, MS)

#check iron tablets search for "Ferrous" within nutrition and blood
iron <- pis %>% 
  filter(PI.BNF.Chapter.Code == 9) %>% 
  filter(str_detect(PI.Approved.Name, "FERROUS"))

iron <- iron %>% 
  select(ppid, PI.Approved.Name, PI.Drug.Formulation, Paid.Date) %>% 
  distinct()

#join datasets and only keep those with paid-date for iron
study_iron <- study_pop %>% 
  left_join(iron, by = "ppid") %>% 
  filter(!is.na(Paid.Date))

table(study_iron$PI.Approved.Name)


#All scripts need to be prior to index date
prior_iron <- study_iron %>% 
  mutate(paid_date = as.Date(Paid.Date)) %>% 
  filter(paid_date <= index_date) %>% 
  distinct()

length(unique(prior_iron$ppid)) 

#atleast 12m before index date
prior_iron_1 <- prior_iron %>% 
  filter(between(paid_date, index_date - 365.25, index_date))

length(unique(prior_iron_1$ppid)) #

#add Fe_medication column to dataset to find those with composite for iron deficiency anaemia.

ID_prior_iron <- prior_iron_1 %>% 
  mutate(iron_tab = "Y") %>% 
  select(ppid, iron_tab) %>% 
  distinct()

#create composite iron tablet code to measure iron deficiency
final_complete <- final_confounders %>% 
  left_join(ID_prior_iron, by = "ppid")

#create anaemia_composite, first rename iron deficiency and change NA to A
final_complete <- final_complete %>% 
  rename(read_anaemia = `Iron deficiency anaemia`) %>% 
  mutate(iron_tab = replace_na(iron_tab, "N"),
         read_anaemia = replace_na(read_anaemia, "N")
)

final_complete <- final_complete %>% 
  mutate(iron_deficiency = case_when(
    read_anaemia == "Y" & iron_tab == "Y" ~ "Y",
    read_anaemia == "N" & iron_tab == "Y" ~ "Y",
    read_anaemia == "Y" & iron_tab == "N" ~ "Y",
    read_anaemia == "N" & iron_tab == "N" ~ "N",
    TRUE ~ NA_character_
  ))

sum(is.na(final_complete$iron_deficiency))
table(final_complete$Group, final_complete$iron_deficiency)

#tidy and save for descriptive analysis
glimpse(final_complete)

final_complete <- final_complete %>% 
  select(-c(iron_tab, read_anaemia)) %>% 
  distinct()
 

save(final_complete, file = "/combined_descriptive.RData")
