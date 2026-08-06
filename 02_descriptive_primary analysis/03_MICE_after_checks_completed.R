###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease


#Prior scripts have checked (1) imputation method (2) number of iterations checked for convergence and (3) number of imputations.Note as the number of events increases across the models the amount of uncertainty related to missing data falls - the missing data contributes a smaller proportion of total uncertainty. 


#------------------------------------------------------------------------------------------#
#load libraries
library(dplyr)
library(mice)
library(miceadds)
library(ggplot2)
library(survival)
library(tidyr)
library(purrr)
library(forcats)

#open dataset model 1
load("model_1.RData")


#-----------------------------------------------------------------------------------
#first make sure person_ID is not used for mice by changing to null
pred <- make.predictorMatrix(model)
meth <- make.method(model)

pred[, "person_ID"] <-0 # do not use as a predictor
pred["person_ID",] <- 0 # not predicted by others
meth["person_ID"] <- "" #do not impute
#specify methods use pmm
meth["simd"] <- "pmm"
meth["smoking_status"] <- "pmm"


# 40 iterations and 10 imputations
set.seed(123) #set seed so each time you run imputations is starts from here
imp_mod1 <- mice(model_1,
                maxit = 40,
                m = 10,
                predictorMatrix = pred,
                method = meth,
                print = TRUE)

summary(imp_mod1)
plot(imp_mod1)


fit <- with(imp_mod1, coxph(Surv(follow_up, cvd_outcome == "y") ~ group + age + 
                         simd + obesity + chcp + hypertension + 
                          + diabetes + smoking_status + year))

pooled <- pool(fit)
summary10 <- summary(pooled)
fmi10 <- as.data.frame(pooled[[3]]) #FMI save this information for model diagnostics

fmi10_only <- fmi10 %>% 
  select(term, estimate, fmi)
summary10_combined <- summary10 %>% 
  left_join(fmi10_only, by = c("term", "estimate"))

#save this original imputation model
saveRDS(imp_mod1, file = "model1_imputions_pmm.rds")

#save model diagnostics
write.csv(summary10_combined, file = "estimate_summary.csv")

