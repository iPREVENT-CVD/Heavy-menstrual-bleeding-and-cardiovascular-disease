###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy mesntrual bleeding and cardiovascular disease


# The following R script are function that were created for (1) descriptive analysis, (2) re-coding the categorical variables socioeconomic status, ethnicity and creating age categories and (3) create weights for covariate balancing propensity scores using different approaches - only weights after re-coding variables, weights including age splines, weights including quadratic year and weights including year splines.

#The R script for descriptive analysis and the script for covariate balancing propensity scores will call these functions. For covariate balancing you need to choose which recoding option you want to use and which weight method to apply. Different coding and weight approaches were developed. The optimum approach that reduces standard mean difference to below 0.1 should be chosen. Diagnostic for covariate balancing assessment are included in the R Script for CBPS_cox model.

#To run these functions the necessary libraries need to be loaded in the R script which are calling these functions

#--------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------#
#(1) Functions for descriptive analysis

# person_ID means de-identified number
# variable need to be assigned in the R script that is calling these functions

# create a function to summarise the categorical variables
summarise_categorical <- function(df, variable) {
  df %>% 
    distinct(person_ID, !!sym(variable)) %>% 
    group_by(!!sym(variable)) %>% 
    summarise(count = n_distinct(person_ID)) %>% 
    mutate(proportion = round(((count/total)*100),2))
}

#calculate summary statistics for continuous variables
summarise_continuous <- function(x){
  data.frame(
    min = round(min(x, na.rm = TRUE),2),
    q1 = round(quantile(x, 0.25, na.rm = TRUE), 2),
    median = round(median(x, na.rm = TRUE), 2),
    mean = round(mean(x, na.rm = TRUE),2),
    q3 = round(quantile(x, 0.75, na.rm = TRUE),2),
    max = round(max(x, na.rm = TRUE),2),
    sd = round(sd(x, na.rm = TRUE),2)
  )
}

#create function to calculate person years and confidence intervals

incidence_py <- function(events, py, mult = 1000) {
  rate <- events / py
  
  lower <- ifelse(
    events == 0,
    0,
    qchisq(0.025, 2* events)/ (2* py)
  )
  upper <- qchisq(0.975, 2* (events + 1)) / (2* py) 
  tibble(
    events = events,
    person_years = py,
    ir = rate *mult,
    lower_ci = lower * mult,
    upper_ci = upper *mult
  )
}

#-------------------------------------------------------------------------------------------------------------------------------#
#Functions for re-coding socioeconomic status. ethnicity and creating age categories for covariate balancing propensity scores

#simd means scottish index of multiple deprivation. It was provided in deciles and then coded into quintiles and tertiles.
#ethnicity categories simplified into White, NOn White and Not stated
#age category was created using continuous age.

#(1) recode simd into 5 categories and ethnicity
#(2) recode simd into 3 categories and ethnicity
#(3) recode simd into 3 categories and ethnicity and age categories
#(4) recode simd into 5 categories, ethnicity and age categories


#(1) create a function to recode simd into quintiles and simplify ethnicity 
recode_vars_simd5 <- function(d) {
  d$simd <- fct_recode(d$simd,
                       "1" = "1",
                       "1" = "2",
                       "2" = "3",
                       "2" = "4",
                       "3" = "5",
                       "3" = "6",
                       "4" = "7",
                       "4" = "8",
                       "5" = "9",
                       "5" = "10")
  d$simd <- factor(d$simd, levels = c("5", "4", "3", "2", "1"))
  
  d$ethnicity <- fct_recode(d$ethnicity,
                            white = "white",
                            non_white = "asian",
                            non_white = "black",
                            non_white = "mixed",
                            non_white = "other",
                            not_stated = "not stated")
  d$ethnicity <- factor(d$ethnicity, levels = c("white", "non_white", "not_stated"))
  d
}
#--------------------------------------------------------------------------------------------#
#(2) create recoded categories with SIMD in 3 categories, lowest 20%, highest 20% and middle 60% with ethnicity
recode_vars_simd3 <- function(d) {
  d$simd <- fct_recode(d$simd,
                       "1" = "1",
                       "1" = "2",
                       "2" = "3",
                       "2" = "4",
                       "2" = "5",
                       "2" = "6",
                       "2" = "7",
                       "2" = "8",
                       "3" = "9",
                       "3" = "10")
  d$simd <- factor(d$simd, levels = c("3", "2", "1"))
  
  d$ethnicity <- fct_recode(d$ethnicity,
                            white = "white",
                            non_white = "asian",
                            non_white = "black",
                            non_white = "mixed",
                            non_white = "other",
                            not_stated = "not stated")
  d$ethnicity <- factor(d$ethnicity, levels = c("white", "non_white", "not_stated"))
  d
}

#------------------------------------------------------------------------------------------#
#(3) recode simd into tertiles and ethnicity and create age categories
recode_vars_age_simd3 <- function(d) {
  d$simd <- fct_recode(d$simd,
                       "1" = "1",
                       "1" = "2",
                       "2" = "3",
                       "2" = "4",
                       "2" = "5",
                       "2" = "6",
                       "2" = "7",
                       "2" = "8",
                       "3" = "9",
                       "3" = "10")
  d$simd <- factor(d$simd, levels = c("3", "2", "1"))
  
  d$ethnicity <- fct_recode(d$ethnicity,
                            white = "white",
                            non_white = "asian",
                            non_white = "black",
                            non_white = "mixed",
                            non_white = "other",
                            not_stated = "not stated")
  d$ethnicity <- factor(d$ethnicity, levels = c("white", "non_white", "not_stated"))
  
  d$age_cat <- case_when(
    d$age >= 18 & d$age < 30 ~ "18-29",
    d$age >= 30 & d$age < 40 ~ "30-39",
    d$age >= 40 & d$age <= 50 ~ "40-50",
    TRUE ~ "40-50"
  )
  d$age_cat <- factor(d$age_cat, levels = c("18-29", "30-39", "40-50"))
  d
}
#-----------------------------------------------------------------------------------------#
#(4) recode simd into quintiles, simplfy ethnicity and add age categories

recode_vars_age_simd5 <- function(d) {
  d$simd <- fct_recode(d$simd,
                       "1" = "1",
                       "1" = "2",
                       "2" = "3",
                       "2" = "4",
                       "3" = "5",
                       "3" = "6",
                       "4" = "7",
                       "4" = "8",
                       "5" = "9",
                       "5" = "10")
  d$simd <- factor(d$simd, levels = c("5", "4", "3", "2", "1"))
  
  d$ethnicity <- fct_recode(d$ethnicity,
                            white = "white",
                            non_white = "asian",
                            non_white = "black",
                            non_white = "mixed",
                            non_white = "other",
                            not_stated = "not stated")
  d$ethnicity <- factor(d$ethnicity, levels = c("white", "non_white", "not_stated"))
  d$age_cat <- case_when(
    d$age >= 18 & d$age < 30 ~ "18-29",
    d$age >= 30 & d$age < 40 ~ "30-39",
    d$age >= 40 & d$age <= 50 ~ "40-50",
    TRUE ~ "40-50"
  )
  d$age_cat <- factor(d$age_cat, levels = c("18-29", "30-39", "40-50"))
  
  d
}

#---------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------#
# (3) Functions for covariate balancing propensity score weights including four approaches

#(1) weight 
#(2) weights including age splines
#(3) weights including quadratic year
#(4) weights including year splines

#covariates need to be assigned in the R script calling the function

#(1) create function for weights - this is basic R script to create the weights for covariate balancing. 

#First build the formula with the covariates to be used for balancing
create_weights <- function(d, covariates) {
  if(!all(covariates %in% names(d))) {
    stop("Covariates missing: ",
         paste(covariates[!covariates %in% names(d)], collapse = ", "))
  }
  trt_form <- as.formula(
    paste("group ~", paste(covariates, collapse = "+"))
  )
  #then create the weights. The method is covariate balancing propensity scores "cbps". This script will extract balancing diagnostics - standised mean difference for each covariate (in the output this is Diff.Adj), ess (effective sample size, mean/max/min weights)
  w <- weightit(
    formula = trt_form,
    method = "cbps",
    over = TRUE, 
    data = d, 
    estimad = "ATT"
  )
  bal <- bal.tab(w, un = TRUE, stats = "m")
  ess = (sum(w$weights)^2)/sum(w$weights^2)
  list(
    weights = w, 
    balance = bal,
    ess = ess,
    weight_summary = c(
      mean = mean(w$weights),
      sd = sd(w$weights),
      min = min(w$weights),
      max = max(w$weights)
    ))
}


#The following additional approaches add the to basic function above. These including adding age and year splines. There were not used in the final analysis but were run to check the effect on the standardised mean difference (SMD), effective sample size and the weights (mean/min/max) applied to each covariate. These approaches did not reduce SMD sufficiently and weights were too extreme. For the final analysis the basic weight function was used with recoding of simd, ethnicity and age categories. 

#(2) function for weights including age splines
create_weights_age_splines <- function(d, knots_age) {
  spline_matrix <- splines::ns(d$age, knots = knots_age)
  spline_matrix <- as.data.frame(spline_matrix)
  colnames(spline_matrix) <- paste0("age_s", seq_len(ncol(spline_matrix)))
  d <- cbind(d, spline_matrix)
  
  trt_form <- as.formula(
    paste("group ~",
          paste(c(colnames(spline_matrix), covariates),
                collapse = " + "))
  )
  w <- weightit(trt_form,
                method = "cbps",
                over = TRUE, 
                data = d, 
                estimand = "ATT")
  bal <- bal.tab(w)
  ess = (sum(w$weights)^2)/sum(w$weights^2)
  list(
    weights = w, 
    balance = bal,
    ess = ess,
    weight_summary = c(
      mean = mean(w$weights),
      sd = sd(w$weights),
      min = min(w$weights),
      max = max(w$weights)
    ))
}

#(3) weights including quadratic year after centering year before using quadratic term

create_weights_year_quadratic <- function(d) {
  d$year_c <- scale(d$year, center = TRUE, scale = FALSE)
  d$year_c2 <- d$year_c^2
  trt_form <- as.formula(
    paste("group ~",
          paste(c("year_c", "year_c2", covariates),
                collapse = " + "))
  )
  w <- weightit(trt_form,
                method = "cbps",
                over = TRUE, 
                data = d, 
                estimand = "ATT")
  bal <- bal.tab(w)
  ess = (sum(w$weights)^2)/sum(w$weights^2)
  list(
    weights = w, 
    balance = bal,
    ess = ess,
    weight_summary = c(
      mean = mean(w$weights),
      sd = sd(w$weights),
      min = min(w$weights),
      max = max(w$weights)
    ))
}

#(4) weights with year splines
create_weights_year_splines <- function(d, knots_year) {
  spline_matrix <- splines::ns(d$year, knots = knots_year)
  spline_matrix <- as.data.frame(spline_matrix)
  colnames(spline_matrix) <- paste0("year_s", seq_len(ncol(spline_matrix)))
  d <- cbind(d, spline_matrix)
  
  trt_form <- as.formula(
    paste("group ~",
          paste(c(colnames(spline_matrix), covariates),
                collapse = " + "))
  )
  w <- weightit(trt_form,
                method = "cbps",
                over = TRUE, 
                data = d, 
                estimand = "ATT")
  bal <- bal.tab(w)
  ess = (sum(w$weights)^2)/sum(w$weights^2)
  list(
    weights = w, 
    balance = bal,
    ess = ess,
    weight_summary = c(
      mean = mean(w$weights),
      sd = sd(w$weights),
      min = min(w$weights),
      max = max(w$weights)
    ))
}