###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

# Multiple imputations for study to obtain fraction of missing information over different numbers of imputations and to decide number of iterations. Based on methods by van Buuren, S (2018). Flexible Imputation of Missing Data, second Edition.

#Steps - use PMM method as faster but similar results to polyreg/polr for categorical and nominal variables see different imputation methods script
#(1) test number of iterations between 20-50 looking at iteration plots to decide on number to use in final method
#(2) start with 10 imputations and increase to 30, in 10 increments. This number chosen as the amount of missing data is around 10% usually the number of imputations required is approximate to proportion missing. 
#(3) save summary of the imputed model for each imputation - estimate and fraction of missing information FMI. Choose number of imputations based on lowest FMI

#simd means Scottish Index of Multiple Deprivation a categorical variable

#clear environment

#load packages
library(dplyr)
library(mice)
library(miceadds)
library(ggplot2)
library(survival)
library(tidyr)
library(purrr)
library(forcats)

#open dataset
load("model.RData")


#-----------------------------------------------------------------------------------
#first make sure person_ID is not used for mice by changing to null
pred <- make.predictorMatrix(model)
meth <- make.method(model)

pred[, "person_ID"] <-0 # do not use as a predictor
pred["person_ID",] <- 0 # not predicted by others
meth["person_ID"] <- "" #do not impute


#specify methods use pmm for missing variables - simd and smoking_status
meth["simd"] <- "pmm"
meth["smoking_status"] <- "pmm"


# first decide on number of iterations start with 20 at 10 imputations.
set.seed(123) #set seed so each time you run imputations is starts from here
imp10_2 <- mice(model,
                  maxit = 20, #number of iterations
                  m = 10, # number of imputations
                  predictorMatrix = pred,
                  method = meth,
                  print = TRUE)

summary(imp10_2)
plot(imp10_2)

# MICE updates the imputed values multiple times to stabilize imputations.
# A convergence plot shows the trajectory of summary of imputed values. For categorical variables they are changed into 0 and 1 to give mean and sd however it is how the values change over imputations - there should be convergence toward the end of imputation.


#increase to iterations to 30 and 50 (40 iterations already done when examining different methods for imputations (Assessing_different_imputation_methods.R)
set.seed(123) #set seed so each time you run imputations is starts from here
imp10_3 <- mice(model,
                maxit = 30,
                m = 10,
                predictorMatrix = pred,
                method = meth,
                print = TRUE)

set.seed(123) #set seed so each time you run imputations is starts from here
imp10_5 <- mice(model,
                maxit = 50,
                m = 10,
                predictorMatrix = pred,
                method = meth,
                print = TRUE)


summary(imp10_3)
summary(imp10_5)

plot(imp10_3) # 
plot(imp10_5)

#For this data set 40 iterations were chosen
#save imputed dataset
imp <- readRDS("pmm.rds")
#--------------------------------------------------------------------------------#
#clear environment
rm(imp10_2, imp10_3, imp10_5)

#Decide number of imputations for model. For each number of imputations run cox model and obtain model summary data include estimate, standard error and fraction of missing information (fmi). When testing methods for imputations 40 iterations and 10 imputations were already run so open this data set and run Cox model

imp <- readRDS("pmm.rds")

#check imputed data for the iterations and 10imputations
md.pattern(complete(imp, 1)) # all complete missing values 0 - not need to repeat for other imputations as no missing data

#create cox model with all imputed datasets
fit <- with(imp, coxph(Surv(follow_up, cvd_outcome == "y") ~ group + age + 
                         simd + obesity + chcp +
                          + diabetes + smoking_status + year))

pooled <- pool(fit)
summary10 <- summary(pooled)

#checks to make sure imputations worked
fit$call #checking models were created from imputed dataset
length(fit$analyses) #ran all 10
imp$loggedEvents # 0 logged events of concern, 
sapply(fit$analyses, function(x) coef(x)) # check variability across all imputations can be difficult to read as gives you list of all the coefficients from each imputation.
#checking if there are any imputed models with NULL entries

#check if any problems with pooling
str(summary(pooled))


# FMI can be extracted using
fmi10 <- as.data.frame(pooled[[3]])

#Now run 20 imputations repeating the steps as above
set.seed(123) #set seed so each time you run imputations is starts from here
imp20 <- mice(model,
                maxit = 40,
                m = 20,
                predictorMatrix = pred,
                method = meth,
                print = TRUE)


summary(imp20)

fit <- with(imp20, coxph(Surv(follow_up, cvd_outcome == "y") ~ group + age + 
                           simd + obesity + chcp + 
                           + diabetes + smoking_status + year))

pooled <- pool(fit)
summary20 <- summary(pooled)

#checks to make sure imputations worked
fit$call #checking models were created from imputed dataset
length(fit$analyses) #ran all 20
imp20$loggedEvents # 0 logged events
sapply(fit$analyses, function(x) coef(x)) # check variability across all imputations can be difficult to read as gives you list of all the coefficients from each imputation.
#checking if there are any imputed models with NULL entries

#check if any problems with pooling
str(summary(pooled))


fmi20 <- as.data.frame(pooled[[3]])


# run 30 imputations
set.seed(123) #set seed so each time you run imputations is starts from here
imp30 <- mice(combined_model_3,
                maxit = 40,
                m = 30,
                predictorMatrix = pred,
                method = meth,
                print = TRUE)


summary(imp30)

fit <- with(imp30, coxph(Surv(follow_up, cvd_outcome == "y") ~ group + age + 
                           simd + obesity + chcp +
                           + diabetes + smoking_status + year))

pooled <- pool(fit)
summary30 <- summary(pooled)

#checks to make sure imputations worked
fit$call #checking models were created from imputed dataset
length(fit$analyses) #ran all 30
imp30$loggedEvents # 
sapply(fit$analyses, function(x) coef(x))

#check if any problems with pooling
str(summary(pooled))

fmi30 <- as.data.frame(pooled[[3]])

#------------------------------------------------------------------------------------------#
#tidy environment
rm(fit, pooled)

#extract fmi from fmi10/20/30/40

fmi10_only <- fmi10 %>% 
  select(term, estimate, fmi)

fmi20_only <- fmi20 %>% 
  select(term, estimate, fmi)

fmi30_only <- fmi30 %>% 
  select(term, estimate, fmi)



#combine with summary10/20/30/40

summary10_combined <- summary10 %>% 
  left_join(fmi10_only, by = c("term", "estimate"))

summary20_combined <- summary20 %>% 
  left_join(fmi20_only, by = c("term", "estimate"))

summary30_combined <- summary30 %>% 
  left_join(fmi30_only, by = c("term", "estimate"))



#keep all summary data from each imputation 
write.csv(summary10_combined, file = "imputations_10.csv")

write.csv(summary20_combined, file = "imputations_20.csv")

write.csv(summary30_combined, file = "imputations_30.csv")


#Final decision to have 40 iterations and 10 imputations. Use PMM imputation model created in script on different imputation methods

#------------------------------------------------------------------------------------#
