###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Checking whether using different methods for multiple imputations of missing data has an effect on final estimates of hazard ratio. Imputing categorical variables so exploring predictive mean matching PMM, polytomous (multi-nominal) logistic regression POLREG and   proportional odds logistic regression POLR. Recent paper by Austin and van Buuren (2025) says PMM is quicker and can be used for ordinal and nominal categorical data: AUstin PC, van Buuren S (2025). Imputation of incomplete ordinal and nominal data by predictive mean matching. Stat Methods Med Res. 2025 Aug 17:34 (11): 9622802251362642.

#steps
#(1) load packages and open 
#(2) ensure person_ID not used and methods set (PMM, then polreg for all categorical then polyreg for categorical and polr for ordinal)
#(3) run CBPS and Cox regression models on each imputated data set to see the effect of different imputation methods. Save iteration plots for comparison

#simd means Scottish Index of Multiple Deprivation a categorical variable

#load packages
library(dplyr)
library(mice)
library(ggplot2)
library(survival)
library(tidyr)
library(forcats)
library(purrr)
library(miceadds)
library(cobalt) #include packages for Covariate balancing - e.g., love plots
library(WeightIt)
library(mitools)
library(forcats)
library(performance)



#change working directory  to same as functions script which is needed for recoding variables after imputation prior to covariate balancing
setwd("")
source("functions.R")


#open dataset
load("model.RData")


#first make sure person_ID is not used for mice by changing to null
pred <- make.predictorMatrix(model)
meth <- make.method(model)

pred[, "person_ID"] <-0 # do not use as a predictor
pred["person_ID",] <- 0 # not predicted by others
meth["person_ID"] <- "" #do not impute

#specify methods - use predicted mean matching first for each missing variable - here simd and smoking_status are missing variables
meth["simd"] <- "pmm" 
meth["smoking_status"] <- "pmm"

#use 40 iterations and 10 imputations for checking all methods
set.seed(123) #set seed prior to running each imputation
imp10_pmm <- mice(model,
                  maxit = 40, #number of iterations
                  m = 10, #number of imputations
                  predictorMatrix = pred,
                  method = meth,
                  print = TRUE)

summary(imp10_pmm)
plot(imp10_pmm)

#save this original imputation model
saveRDS(imp10_pmm, file = ".rds")

#Now run Covariate balancing on each imputated data set (10) and save diagnostics. 
#define the covariates
covariates <- c("age", "simd", "obesity", "diabetes", "year", "smoking_status", "chcp")

# Apply this cbps function across all imputations - here you will call functions within the functions.R script.

cbps_list <- map(1:10, function(i) {
  d <- complete(imp10_pmm, i)
  d <- recode_vars_simd5(d)
  create_weights(d, covariates)
})

#extract diagnostics for cbps model
cbps_diagnostic_df <- map_df(1:10, function(i){
  ess <- cbps_list[[i]]$ess
  ws <- cbps_list[[i]]$weight_summary
  
  tibble(
    imputation = i,
    ess = ess,
    w_mean = ws["mean"],
    w_sd = ws["sd"],
    w_min = ws["min"],
    w_max = ws["max"]
  )
})

#save diagnostics
write.csv(cbps_diagnostic_df, file = "diagnostics.csv")

#to extract Standarised mean difference after balancing for each imputation, 
smd_df <- map_dfr(1:10, function(i) {
  bal <- cbps_list[[i]]$balance$Balance
  tibble(
    variable = rownames(bal),
    SMD_adj = bal$Diff.Adj,
    imputation = i
  )
})

#include the range of SMD to show uncertainty
range_smd <- smd_df %>% 
  group_by(variable) %>% 
  summarise(
    min_smd = min(SMD_adj, na.rm = TRUE),
    max_smd = max(SMD_adj, na.rm = TRUE),
    mean_smd = mean(SMD_adj, na.rm = TRUE),
    .groups = "drop"
  )

#save to compare imputation methods
write.csv(range_smd, file = "csv")

#now fit cox model to each imputed dataset with covariate balancing. This will call function within the functions.R script
cox_list <- map(1:10, function(i) {
  d <- complete(imp10_pmm, i) #use the 10 imputed dataset
  d <- recode_vars_simd5(d) #recode variables
  w <- cbps_list[[i]]$weights$weights #apply covariate balancing propensity scores
  coxph(
    Surv(follow_up, cvd_outcome == "y") ~ group + age + smoking_status + simd +
      + obesity + diabetes + year + chcp, # run cox regression model
    data = d,
    weights = w
  )
})

#Next steps for obtaining HR for each model
#extract pooled estimates (logHR and standard errors from pooled (list)
pooled <- MIcombine(cox_list)
summary_results <- summary(pooled)

coef <- pooled$coefficients
coef_se <- sqrt(diag(pooled$variance))

#build HR tibble for each model
HR_table <- tibble(
  variable = names(coef),
  HR = exp(coef),
  CI_lower = exp(coef - 1.96* coef_se),
  CI_upper = exp(coef + 1.96* coef_se)
) 

write.csv(HR_table, file = "")

#--------------------------------------------------------------------------------------------#
#Repeat with polyreg smoking and polyr for simd

rm(range_smd, pooled, imp10_pmm, HR_table, smd_df, summary_results, cbps_list, cbps_diagnostic_df, cox_list)

#re-run check to make sure person_ID isn't used
pred[, "person_ID"] <-0 # do not use as a predictor
pred["person_ID",] <- 0 # not predicted by others
meth["person_ID"] <- "" #do not impute

#specify methods for unordered categorical variables as use polyr for nominal and polr for ordinal
meth["simd"] <- "polr" 
meth["smoking_status"] <- "polyreg"

#use 40 iterations and 10 imputations 
set.seed(123) #set seed again
imp10_polyreg_polr <- mice(model,
                      maxit = 40,
                      m = 10,
                      predictorMatrix = pred,
                      method = meth,
                      print = TRUE)

summary(imp10_polyreg_polr)
plot(imp10_polyreg_polr)

#save this original imputation model
saveRDS(imp10_polyreg_polr, file = ".rds")


#Now run CBPS and diagnostics using same covariates, weights and recoding

cbps_list <- map(1:10, function(i) {
  d <- complete(imp10_polyreg_polr, i)
  d <- recode_vars_simd5(d)
  create_weights(d, covariates)
})

cbps_diagnostic_df <- map_df(1:10, function(i){
  ess <- cbps_list[[i]]$ess
  ws <- cbps_list[[i]]$weight_summary
  
  tibble(
    imputation = i,
    ess = ess,
    w_mean = ws["mean"],
    w_sd = ws["sd"],
    w_min = ws["min"],
    w_max = ws["max"]
  )
})

#save diagnostics
write.csv(cbps_diagnostic_df, file = ".csv")

#to extract SMD for each imputation, 
smd_df <- map_dfr(1:10, function(i) {
  bal <- cbps_list[[i]]$balance$Balance
  tibble(
    variable = rownames(bal),
    SMD_adj = bal$Diff.Adj,
    imputation = i
  )
})

#include the range of SMD to show uncertainty
range_smd <- smd_df %>% 
  group_by(variable) %>% 
  summarise(
    min_smd = min(SMD_adj, na.rm = TRUE),
    max_smd = max(SMD_adj, na.rm = TRUE),
    mean_smd = mean(SMD_adj, na.rm = TRUE),
    .groups = "drop"
  )

#save to compare imputation methods
write.csv(range_smd, file = ".csv")

#now fit cox model
cox_list <- map(1:10, function(i) {
  d <- complete(imp10_polyreg_polr, i)
  d <- recode_vars_simd5(d)
  w <- cbps_list[[i]]$weights$weights
  coxph(
    Surv(follow_up, cvd_outcome == "y") ~ group + age + smoking_status + simd +
      chcp + obesity + diabetes + year,
    data = d,
    weights = w
  )
})

#Next steps for obtaining HR for each model
#extract pooled estimates (logHR and standard errors from pooled (list)
pooled <- MIcombine(cox_list)
summary_results <- summary(pooled)

coef <- pooled$coefficients
coef_se <- sqrt(diag(pooled$variance))

#build HR tibble for each model
HR_table <- tibble(
  variable = names(coef),
  HR = exp(coef),
  CI_lower = exp(coef - 1.96* coef_se),
  CI_upper = exp(coef + 1.96* coef_se)
) 

write.csv(HR_table, file = ".csv")

