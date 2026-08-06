###################################################################################################
# General information
###################################################################################################

# Author: Thulani Ashcroft
# Date created for extraction: 22.07.26
# Project: Heavy menstrual bleeding and cardiovascular disease

#Using 10 imputed dataset created using MICE, run covariate balance propensity scores (CBPS) on each of the 10 imputed datasets, then run cox regression on each imputed dataset and finally pool results

#steps
#(1) Load imputation model with PMM
#(2) Use recode variables after imputations - SIMD into quintiles, ethnicity simplied and create age categories for covariate balancing propensity score as this decreases the standardised mean difference between the groups (exposed and comparison)
#(3) save CBPS diagnostics and cox model outputs for each. 
#(4) save standardised mean difference before and after weighting to allow plots, save overlap of propensity scores between exposed and comparison group
#(5) save data for Kaplan Meier plots and risk table


#load libraries
library(mice)
library(cobalt) #include packages for Covariate balancing - e.g., love plots
library(purrr)
library(survival)
library(WeightIt)
library(dplyr)
library(forcats)
library(mitools)
library(tibble)
library(ggplot2)
library(survminer)
library(broom)
library(patchwork)


#change wd to same as functions script for cbps and recoding variables after imputation
setwd("")
source("functions.R")


#load imputation model with ppm
imp <- readRDS("pmm.rds")

#check age ranges within each imputed data set
imp_list <- complete(imp, "all")
map(imp_list, ~summary(.x$age))

#-----------------------------------------------------------------------------------------#
#Run recoding of simd, ethnicity and create age categories

#redefine the covariates with age categories
covariates <- c("age_cat", "simd", "hypertension", "obesity", "diabetes", "year", "smoking_status", "chcp", "nsaid")

#use recode_vars_age_simd5 to get age categories

cbps_list <- map(1:10, function(i) {
  d <- complete(imp, i)
  d <- recode_vars_age_simd5(d)
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

#to extract SMD for each imputation before and after balancing 
smd_df <- map_dfr(1:10, function(i) {
  bal <- cbps_list[[i]]$balance$Balance %>% 
    as.data.frame()
  
  tibble(
    variable = rownames(bal),
    SMD_before = bal$Diff.Un,
    SMD_after = bal$Diff.Adj,
    imputation = i
  )
})

#include the range of SMD to show uncertainty
range_smd <- smd_df %>% 
  group_by(variable) %>% 
  summarise(
    min_smd_before = min(SMD_before, na.rm = TRUE),
    max_smd_before = max(SMD_before, na.rm = TRUE),
    mean_smd_before = mean(SMD_before, na.rm = TRUE),
    
    min_smd_after = min(SMD_after, na.rm = TRUE),
    max_smd_after = max(SMD_after, na.rm = TRUE),
    mean_smd_after = mean(SMD_after, na.rm = TRUE),
    .groups = "drop"
  )


#To create plot for publication tidy variable descriptors
smd_tidy <- range_smd %>% 
  mutate(
    variable = dplyr::recode(variable,
                             "diabetes_y" = "Diabetes (present)",
                             "obesity_y" = "Obesity (present)",
                             "hypertension_y" = "Hypertension (present)",
                             "chcp_y" = "CHCP (prescription)",
                             "nsaid_y" = "NSAID (prescription)",
                             "age_cat_18-29" = "Age group (18-29 years)",
                             "age_cat_30-39" = "Age group (30-39 years)",
                             "age_cat_40-50" = "Age group (40-50 years)",
                             "simd_1" = "SIMD (quintile 1)",
                             "simd_2" = "SIMD (quintile 2)",
                             "simd_3" = "SIMD (quintile 3)",
                             "simd_4" = "SIMD (quintile 4)",
                             "simd_5" = "SIMD (quintile 5)",
                             "year" = "Year",
                             "smoking_status_current smoker" = "Smoking status (current)",
                             "smoking_status_non smoker" = "Smoking status (non)",
                             "smoking_status_ex smoker" = "Smoking status (ex-smoker)",
                             "prop.score" = "Propensity score"
    )
  )

#create a long plot
smd_long <- bind_rows(
  smd_tidy %>% 
    transmute(
      variable,
      time = "Before weighting",
      mean = mean_smd_before,
      min = min_smd_before,
      max = max_smd_before
    ),
  smd_tidy %>% 
    transmute(
      variable,
      time = "After weighting",
      mean = mean_smd_after,
      min = min_smd_after,
      max = max_smd_after
    )
)

#order by mean SMD
smd_long <- smd_long %>% 
  mutate(variable = reorder(variable, mean))

write.csv(smd_long, file = "/SMD_for_plots.csv")

#check plot
ggplot(smd_long, aes(x = mean, y = variable, color = time)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    height = 0.25,
    linewidth = 1.1,
    alpha = 0.9,
    position = position_dodge(width = 0.6)
  ) +
  geom_vline(xintercept = 0.1, linetype = "dashed", colour = "orange") +
  scale_color_manual(values = c(
    "Before weighting" = "grey40",
    "After weighting" = "steelblue"
  )) +
  theme_minimal() +
  labs(
    x = "Standardised Mean Difference",
    y = NULL,
    color = "",
    title = "Covariate balance before after after CBPS weighting"
  ) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10)
  )


#------------------------------------------------------------------------------------#
#Create plot for overlap of weighted propensity scores for exposed and comparison groups
weighted_ps_df <- map_dfr(1:10, function(i) {
  d <- complete(imp, i)
  w <- cbps_list[[i]]$weights
  data.frame(
    ps = w$ps,
    treat = d$group,
    imputation = i,
    weight = as.numeric(w$weights)
  )
})


#plot over all 10 imputations
ggplot(weighted_ps_df, aes(x = ps, fill = factor(treat), weight = weight)) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Propensity score",
    y = "Weighted density",
    fill = "Group",
    title = "Weighted PS overlap across imputations"
  ) +
  theme_minimal()

write.csv(weighted_ps_df, file = "ps_weights_for_overlap_plot.csv")

#run cox models and save for each 10 imputated datasets
cox_list <- map(1:10, function(i) {
  d <- complete(imp, i) #use 10 datasets
  d <- recode_vars_simd5(d) #recode variables
  w <- cbps_list[[i]]$weights$weights #apply cbps weights
  coxph(
    Surv(follow_up, cvd_outcome == "y") ~ group + age + smoking_status + simd +
       obesity + hypertension + diabetes + year + chcp + nsaid, #run cox model
    data = d,
    weights = w
  )
})

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

#save HR
write.csv(HR_table, file = "/HR.csv")

#-----------------------------------------------------------------------#
#Create kaplan meier plot and risk table

#create function to clean group names
clean_group <- function(x){
  x <- sub("group=", "", x)
  x <- tolower(x)
  ifelse(x == "comparison", "Comparison",
         ifelse(x == "exposed", "Exposed", x)
  )
}

#extract first dataset from 10 imputed datasets
data_1 <- complete(imp, 1)

#use the first imputed dataset to create Kaplan Meier (KM) plot
kap_meir <- survfit(Surv(follow_up, cvd_outcome == "y") ~ group, data = data_1)

#tidy KM and create new group column
km_df <- tidy(kap_meir)

#create risk table to go beneath KM plot - need to know longest follow up
summary(data_1$follow_up) # longest is 13 years however plot and table look better if stops at 12
times_seq <- c(0, 4,8,12) # create markers

risk_summary <- summary(kap_meir, times = times_seq)

risk_df <- data.frame(
  time = risk_summary$time,
  n.risk = risk_summary$n.risk,
  strata = risk_summary$strata)

#clean exposed and comparison names
km_df <- km_df %>% 
  mutate(Group = clean_group(strata))

risk_df <- risk_df %>% 
  mutate(Group = clean_group(strata))

#check correct order of exposed and comparison groups
km_df$Group <- factor(km_df$Group, levels = c("Exposed", "Comparison"))
risk_df$Group <- factor(risk_df$Group, levels = c("Exposed", "Comparison"))

#save the data so can create figures 
write.csv(km_df, file = ".csv")

write.csv(risk_df, file = "risk_plot.csv")

#run plots
km_plot <- ggplot(km_df, 
                  aes(x = time, y = estimate, colour = Group, fill = Group)) +
  geom_step(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2,
              inherit.aes = TRUE) +
  ylim(0,1) +
  labs(x = "Time (years)", y = "Survival Probability") +
  scale_color_manual(values = c("Comparison" = "#E8873A", "Exposed" ="#4472A8")) +
  scale_fill_manual(values = c("Comparison" = "#E8873A", "Exposed" ="#4472A8")) +
  scale_x_continuous(breaks = times_seq, limits = c(0,12)) +
  theme_minimal(base_size = 14)

#plot table
r_table <- ggplot(risk_df, aes(x = time, y = Group, label = n.risk)
)+
  geom_text(size =3.5) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(breaks = times_seq, limits = c(0,12)) +
  labs( x= "Time (years)", y = NULL)


#combine plots
final_plot <- km_plot / r_table +
  plot_layout(heights = c(3,1))

print(final_plot)
