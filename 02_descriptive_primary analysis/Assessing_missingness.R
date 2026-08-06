###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#R script used to explore missingness in data set

#Steps
#(1) exploring those with missing variables
#(2) comparison of those with complete and missing data
#(3) for each variable with missing data calculate the odds of missingness in that variable due to another variable in the data set.
#(4) use MICE package to explore patterns of missingness

#-----------------------------------------------------------------------------------
#clear environment

#load libraries
library(dplyr)
library(lubridate)
library(finalfit)


#load data - RData file
load("study.RData")

#-------------------------------------------------------------------------------#
#(1) First start with variable that is missing. This section of the script investigates those where smoking is missing

missing_smoking <- study %>% 
  filter(is.na(smoking_status))

summary(missing_smoking) # summarise the characteristic in those with missing smoking status

smoking_present <- study_B %>% 
  filter(!is.na(smoking_status))

summary(smoking_present) #summarise the characteristic in those with no missing smoking status

#--------------------------------------------------------------------------------------#
#(2) Create table to compare those in study with complete data and those with missing data

#create table of missing values as percentages for all columns
missing_data_table <- sapply(study, function(x) (sum(is.na(x))/length(x)) * 100)

study_missing_summary <- as.data.frame(missing_data_table)

# 
#create complete case indicator
study$complete <- factor(complete.cases(study_B),
                           levels = c(TRUE, FALSE),
                           labels = c("Yes", "No"))

#compare those with complete data with those that have missing data in any variable. The explanatory variables are the all the covariates that will be included in the analysis.
complete_case_comparison <- summary_factorlist(study, 
                                               dependent = "complete", 
                                               explanatory = c("age", "ethnicity", 
                                                               "smoking_status",
                                                               "diabetes", 
                                                               "hypertension", 
                                                               "obesity", 
                                                               "simd", 
                                                               "chcp", 
                                                               "cvd_outcome", 
                                                               "group",
                                                               "follow_up",
                                                               "year"),
                                               column = TRUE, p= TRUE)


# save
write.csv(study_missing_summary,file = ".csv")


#--------------------------------------------------------------------------------#

# (3) Explore mechanism of missingness by looking at association of missing variable to others in data set 

#In the code below the association between smoking and other variable are examined using logistic regression

analysis_smoking <- finalfit.glm(study, dependent = "smoking_status",
                                 explanatory = c("ethnicity", "age", "diabetes", 
                                                 "hypertension", "group", 
                                                 "follow_up",
                                                 "obesity","simd", "chcp",
                                                 "year", "cvd_outcome")
)


#save results

write.csv(analysis_smoking, file = ".csv")

#------------------------------------------------------------------------------------------------#
#(4) Using mice package to visualise missingness patterns in study

#clear environment

#open packages
library(lattice)
library(dplyr)
library(mice)
library(ggplot2)
library(visdat) 

#open data set here called model
load("model.RData")

#First start with visualising the missingness and deciding on variables to include for multiple imputations

#check correct format of variables
summary(model)

#rename variables in study so easier to visualise
study <- model %>% 
  rename(Time = follow_up,
         PPID = ppid,
         Group = group,
         Age = age,
         Year = year,
         SIMD = simd,
         CHCP = chcp,
         DM = diabetes,
         Obese = obesity,
         Smoke = smoking_status,
         CVD = cvd_outcome)


md.pattern(study) # checks pattern of missing values


#calculate influx and outflux of multivariate missing data patterns 

#Influx depends on the proportion of missing data of the variable. Outflux is an indicator of the potential usefulness for imputing other variables. Outflux depends on the proportion of missing data of the variable. Intepretation based on guidance by van Buuren, S. (2018) Flexible Imputation of Missing Data, Second Edition (2nd Ed). Chapman and Hall/CRC http://doi.org/10.1201/9780429492259. "In practice variables that are closer to the subdiagonal are typically better connect than those father away. Variable located in lower regions (esp near lower left corner) and that are uninteresting for later analysis are better removed". Clustering variables are high influx and low outflux so are far right low. 

flux(study, local = names(study))

fluxplot(
  study,
  local = names(study),
  plot = TRUE,
  labels = TRUE,
  xlim = c(0,1),
  ylim = c(0,1),
  las = 1,
  xlab = "Influx",
  ylab = "Outflux",
  main = paste("Influx-outflux pattern for", deparse(substitute(study))),
  eqscplot = FALSE,
  pty = "s",
  lwd = 1,
)  

# Good Practice - check
#continuous variables that are not normal can be transformed if these are missing but as age and year not missing so no need to change format for imputation.

## Good Practice - check correlation between variables
predicted_by <- c("age", "cvd_outcome", "simd", "obesity", "diabetes", "smoking_status", "year", "group", "follow_up", "chcp")

df_correlation <- combined_model_3[, predicted_by]
df_correlation[,predicted_by] <-lapply(df_correlation[, predicted_by], as.numeric)

correlation_matrix <- round(cor(df_correlation, use = "pairwise.complete.obs"), 3)

write.csv(correlation_matrix, file = ".csv")

#create visual correlation matrix
cor_matrix <- as.data.frame(correlation_matrix)
vis_cor(cor_matrix)

