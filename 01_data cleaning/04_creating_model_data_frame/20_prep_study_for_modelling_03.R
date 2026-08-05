

# Preparation combined datasets join exposed and comparison groups including nsaids

#Model 1 outcomes - CVD event and CVD death
#Model 2 outcomes - CVD event and all cause death
#Model 3 outcomes - CVD event, CVD death, cardiac medication
# Model 4 outcomes - CVD event, all cause death, cardiac medication

#####################################################################################
# General information
#####################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 23.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#join the tidied exposed and comparison groups to create the model for multiple imputations

#Open libraries
library(dplyr)
library(forcats)


#open exposed group 1 and comparison group 1
load("/exposed_model_1_data.RData")

load("comparison_model_1_data.RData")


# Now join datasets
list_com_model_1 <- list(model_1_exposed, model_1_comparison)

combined_model_1 <- bind_rows(list_com_model_1)

glimpse(combined_model_1)

# change SIMD, nsaid, htn_composite, outcome into a factor and rename
combined_model_1 <- combined_model_1 %>% 
  mutate(simd = as.factor(simd),
         nsaid = as.factor(nsaid),
         hypertension = as.factor(htn_composite),
         cvd_outcome = as.factor(outcome_1)
         )

#r remove outcome_1 and htn_composite
combined_model_1 <- combined_model_1 %>% 
  select(-c(outcome_1, htn_composite)) %>% 
  distinct()

#set reference for each level of categorical variables. Use map function to apply to each categorical variables
combined_model_1 <- combined_model_1 %>% 
  mutate(
    group = fct_relevel(group, "comparison"),
    simd = fct_relevel(simd, "10"),
    obesity = fct_relevel(obesity, "n"),
    hypertension = fct_relevel(hypertension, "n"),
    diabetes = fct_relevel(diabetes, "n"),
    smoking_status = fct_relevel(smoking_status, "non smoker"),
    chcp = fct_relevel(chcp, "n"),
    nsaid = fct_relevel(nsaid, "n")
  )

#check reference levels are correct 
summary(combined_model_1) #order of simd is not correct but NA correct simd 730, smoking NA 1016

levels(combined_model_1$simd) <- c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1")

levels(combined_model_1$simd)
#last check for any duplicates
has_duplicates <- any(duplicated(combined_model_1))
sum(duplicated(combined_model_1$ppid))

save(combined_model_1, file = "/combined_model_1.RData")

#-------------------------------------------------------------------------------------------#
#Create model 2 dataset

#clear environment
rm(list = ls())

#load exposed and comparison model 2 groups
load("/exposed_model_2_data.RData")

load("comparison_model_2_data.RData")


# Now join datasets
list_com_model_2 <- list(model_2_exposed, model_2_comparison)

combined_model_2 <- bind_rows(list_com_model_2)

glimpse(combined_model_2)

# change SIMD, htn_composite, nsaid and outcome into a factor
combined_model_2 <- combined_model_2 %>% 
  mutate(simd = as.factor(simd),
         nsaid = as.factor(nsaid),
         cvd_outcome = as.factor(outcome_2),
         hypertension = as.factor(htn_composite)
  )

#remove outcome_2 and htn_composite
combined_model_2 <- combined_model_2 %>% 
  select(-c(outcome_2, htn_composite)) %>% 
  distinct()

#set reference for each level of categorical variables. Use map function to apply to each categorical variables
combined_model_2 <- combined_model_2 %>% 
  mutate(
    group = fct_relevel(group, "comparison"),
    simd = fct_relevel(simd, "10"),
    obesity = fct_relevel(obesity, "n"),
    hypertension = fct_relevel(hypertension, "n"),
    diabetes = fct_relevel(diabetes, "n"),
    smoking_status = fct_relevel(smoking_status, "non smoker"),
    chcp = fct_relevel(chcp, "n"),
    nsaid = fct_relevel(nsaid, "n")
  )

#check reference levels are correct 
summary(combined_model_2) #order of simd is not correct but NA correct simd 730, smoking NA 1016

levels(combined_model_2$simd) <- c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1")

levels(combined_model_2$simd)
#last check for any duplicates
has_duplicates <- any(duplicated(combined_model_2))
sum(duplicated(combined_model_2$ppid))

save(combined_model_2, file = "combined_model_2.RData")

#--------------------------------------------------------------------------------------------#
#Create model 3 dataset

#clear environment
rm(list = ls())

#load exposed and comparison model 3
load("exposed_model_3_data.RData")

load("comparison_model_3_data.RData")

# Now join datasets
list_com_model_3 <- list(model_3_exposed, model_3_comparison)

combined_model_3 <- bind_rows(list_com_model_3)

glimpse(combined_model_3)

# change SIMD and outcome into a factor
combined_model_3 <- combined_model_3 %>% 
  mutate(simd = as.factor(simd),
         nsaid = as.factor(nsaid),
         cvd_outcome = as.factor(outcome_3)
  )

# outcome_3
combined_model_3 <- combined_model_3 %>% 
  select(-outcome_3) %>% 
  distinct()

#set reference for each level of categorical variables. Use map function to apply to each categorical variables
combined_model_3 <- combined_model_3 %>% 
  mutate(
    group = fct_relevel(group, "comparison"),
    simd = fct_relevel(simd, "10"),
    obesity = fct_relevel(obesity, "n"),
    diabetes = fct_relevel(diabetes, "n"),
    smoking_status = fct_relevel(smoking_status, "non smoker"),
    chcp = fct_relevel(chcp, "n"),
    nsaid = fct_relevel(nsaid, "n")
  )

#check reference levels are correct 
summary(combined_model_3) #order of simd is not correct - NA's correct

levels(combined_model_3$simd) <- c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1")

levels(combined_model_3$simd)
#last check for any duplicates
has_duplicates <- any(duplicated(combined_model_3))
sum(duplicated(combined_model_3$ppid))

save(combined_model_3, file = "combined_model_3.RData")


#----------------------------------------------------------------------------------------#

#Create model 4 dataset

#clear environment
rm(list = ls())

#load exposed and comparison model 4
load("model_4_data.RData")

load("model_4_data.RData")


# Now join datasets
list_com_model_4 <- list(model_4_exposed, model_4_comparison)

combined_model_4 <- bind_rows(list_com_model_4)

glimpse(combined_model_4)

# change SIMD,htn_composite, nsaid and outcome into a factor
combined_model_4 <- combined_model_4 %>% 
  mutate(simd = as.factor(simd),
         nsaid = as.factor(nsaid),
         cvd_outcome = as.factor(outcome_4)
  )

#rename htn_composite to hypertension and remove outcome_4
combined_model_4 <- combined_model_4 %>% 
  select(-c(outcome_4)) %>% 
  distinct()

#set reference for each level of categorical variables. Use map function to apply to each categorical variables
combined_model_4 <- combined_model_4 %>% 
  mutate(
    group = fct_relevel(group, "comparison"),
    simd = fct_relevel(simd, "10"),
    obesity = fct_relevel(obesity, "n"),
    diabetes = fct_relevel(diabetes, "n"),
    smoking_status = fct_relevel(smoking_status, "non smoker"),
    chcp = fct_relevel(chcp, "n"),
    nsaid = fct_relevel(nsaid, "n")
  )

#check reference levels are correct 
summary(combined_model_4) #order of simd is not correct

levels(combined_model_4$simd) <- c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1")

levels(combined_model_4$simd)
#last check for any duplicates
has_duplicates <- any(duplicated(combined_model_4))
sum(duplicated(combined_model_4$ppid))

save(combined_model_4, file = "/combined_model_4.RData")



