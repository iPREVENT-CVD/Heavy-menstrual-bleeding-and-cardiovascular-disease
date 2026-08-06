###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease
#R script for descriptive analysis and crude incidence calculations. These call functions within the function script.

#The included covariates have already been created in the dataset. Those that may not be clear are described further below.
#simd is Scottish index of multiple deprivation
#FH is family history
#gynae_drug
#end_drug
#prior_anti_HTN are prior anti_hypertensive drugs
#prior_lipid_Rx is prior lipid lowering agents
#nsaid are non steroidal anti-inflammatory drugs


#--------------------------------------------------------------------------------------#
#open libraries

library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)


#change working directory to same as functions script
source("functions.R")


# load  data - the following is for RData file
load("model_1.RData")

#this descriptive analysis is for the exposed group which are those with heavy menstrual bleeding
exposed <- model_1 %>% 
  filter(group == "exposed")


#------------------------------------------------------------------------------#

#Descriptive statistics for exposed group
# summarise categorical variables but make ethnicity and simd into larger subgroups

exposed <- exposed %>% 
  mutate(ethnicity_summarised = case_when(
    ethnicity %in% c("Asian", "Black", "Mixed", "Other") ~ "Non_white",
    ethnicity == "White" ~ "White",
    ethnicity == "Not Stated" ~ "Not Stated"
  ))


exposed <- exposed %>% 
  mutate(simd_summarised = case_when(
    simd %in% c("1","2") ~ "1",
    simd %in% c("3", "4") ~ "2",
    simd %in% c("5", "6") ~ "3",
    simd %in% c("7", "8") ~ "4",
    simd %in% c("9", "10") ~ "5"
  ))


#now calulate summary statistic 
total <-  # add the total number in group (demominator)
variables_to_summarise <- c("ethnicity_summarised", "simd_summarised", 
                            "smoking_status", "Hypertension", "Obesity", 
                            "Diabetes", "iron_deficiency", 
                            "FH","gynae_drug", "endo_drug", "prior_anti_HTN",
                            "prior_lipid_Rx", "nsaid")

summary <- map(variables_to_summarise, function(variable) summarise_categorical(exposed, variable))

#Summary includes all variables.
#create df for each variable
ethnicity_df <-summary[[1]] %>% 
  rename(ethnicity = 1, ethnicity_counts =2, ethnicity_prop = 3)

simd_df <-summary[[2]] %>% 
  rename(simd = 1, simd_counts =2, simd_prop = 3)

smoking_df <-summary[[3]] %>% 
  rename(smoking_status = 1, smoking_counts = 2, smoking_prop = 3)

hypertension_df <-summary[[4]] %>% 
  rename(hypertension = 1, hypertension_counts =2, hypertension_prop =3)

obesity_df <-summary[[5]] %>% 
  rename(obesity = 1, obesity_counts =2, obesity_prop = 3)

diabetes_df <-summary[[6]] %>% 
  rename(diabetes = 1, diabetes_counts =2, diabetes_prop =3)

anaemia_df <-summary[[7]] %>% 
  rename(anaemia = 1, anaemia_counts =2, anaemia_prop = 3)

fh_df <-summary[[8]] %>% 
  rename(FH = 1, fh_counts =2, fh_prop =3)

gynae_drug_df <- summary[[9]] %>% 
  rename(gynae_drug = 1, gynae_drug_counts = 2, gynae_drug_prop = 3)

endo_drug_df <- summary[[10]] %>% 
  rename(endo_drug = 1, endo_drug_counts = 2, endo_drug_prop = 3)

prior_anti_HTN_df <- summary[[11]] %>% 
  rename(prior_HTN = 1, prior_HTN_counts =2, prior_HTN_prop =3)

prior_lipid_df <- summary[[12]] %>% 
  rename(prior_lipid = 1, prior_lipid_counts =2, prior_lipid_prop =3)

nsaid_df <- summary[[13]] %>% 
  rename(nsaid = 1, nsaid_counts =2, nsaid_prop =3)

#Combine into one dataset of variable and other of conditions to save.

combined_categorical <- bind_rows(simd_df, smoking_df, ethnicity_df)
combined_conditions <- bind_rows(hypertension_df, diabetes_df, obesity_df, anaemia_df, fh_df)
combined_drugs <- bind_rows(gynae_drug_df, endo_drug_df, prior_anti_HTN_df, prior_lipid_df, nsaid_df)


#save as csv file
write.csv(combined_categorical, file ="")

write.csv(combined_conditions,file ="")

write.csv(combined_drugs,file ="")



#---------------------------------------------------------------------------------#
#(2) Use summarise_continuous function to calculate summary statistics of age.

#tidy environment
rm(anaemia_df, diabetes_df, ethnicity_df, simd_df, hypertension_df, obesity_df, smoking_df, fh_df, combined_categorical, combined_conditions, combined_drugs, gynae_drug_df, endo_drug_df, prior_lipid_df, prior_anti_HTN_df, future_anti_HTN_df, future_lipid_df, future_cardio_drug_df,summary)


#use function to summarise continuous variables
age <- summarise_continuous(exposed$age_at_index)
age <- age %>% 
  mutate(variable = "age")

#save as cvs file
write.csv(age, file ="")

#check distribution age
ggplot(exposed, aes(x = age_at_index)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "lightblue", colour = "white") +
  geom_density(colour = "red", linewidth = 1)

#skewed so use median

#----------------------------------------------------------------------------------------------------------------------------#
# Calculate crude incidence and  median time to event using model_1 which includes the exposed and comparison within the variable group

#follow up variable is the time to event which has already to calculate for each individual at data cleaning stage.
#events is the binary outcome variable where 1 means an event and 0 no event. 

#summarise time to event
model_1 %>% 
  group_by(group) %>% 
  summarise(
    median = median(follow_up, na.rm = TRUE),
    Q1 = quantile(follow_up, 0.25, na.rm = TRUE),
    Q3 = quantile(follow_up, 0.75, na.rm = TRUE)
  )


#overall incidence by group

overall <- model_1 %>% 
  group_by(group) %>% 
  summarise(
    events = sum(events, na.rm = TRUE),
    person_years = sum(follow_up, na.rm = TRUE)
  ) %>% 
  mutate(incidence_py(events, person_years))


#save
write.csv(overall,file ="")



