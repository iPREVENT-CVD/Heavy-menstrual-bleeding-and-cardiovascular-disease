#R script to identify minimal covariates for adjustment and all adjustments for the association between heavy menstrual bleeding and cardiovascular outcomes
# Reference for use of dagitty:	Textor J, van der Zander B, Gilthorpe MS, Liśkiewicz M, Ellison GT. 
#Robust causal inference using directed acyclic graphs: the R package ‘dagitty’. 
#International Journal of Epidemiology. 2016;45(6):1887-1894. doi:10.1093/ije/dyw341

library(dagitty)

#define expose and outcome and which covariates need to be included within the DAG.

#HMB = heavy menstrual bleeding 
#CVD = cardiovascular disease
#CHCP = combined hormonal contraceptive pill
#HRT_prior = HRT given prior to index date
#HRT_post = HRT given after index date

dag <- dagitty('dag {
  HMB          [exposure, pos="0,0"]
  CVD          [outcome, pos="6,0"]
  Age          [pos="3,3"]
  Obesity      [pos="1,2"]
  Hypertension [pos="4,2"]
  Diabetes     [pos="2,2"]
  Smoking      [pos="5,2"]
  Deprivation  [pos="3,4"]
  CHCP         [pos="-1,1"]
  NSAIDs       [pos="-1,-1"]
  HRT_prior    [pos="5,1"]
  HRT_post     [pos="3,-2"]
  Year         [pos="1,-2"]

  Age          -> HMB
  Age          -> CVD
  Age          -> Hypertension
  Age          -> Diabetes
  Age          -> HRT_prior
  Age          -> HRT_post
  Obesity      -> HMB
  Obesity      -> CVD
  Obesity      -> Hypertension
  Obesity      -> Diabetes
  Hypertension -> CVD
  Diabetes     -> CVD
  Smoking      -> CVD
  Deprivation  -> HMB
  Deprivation  -> CVD
  Deprivation  -> Smoking
  Deprivation  -> Obesity
  CHCP         -> HMB
  CHCP         -> CVD
  NSAIDs       -> HMB
  NSAIDs       -> CVD
  HRT_prior    -> CVD
  HRT_post     -> CVD
  Year         -> HMB
  Year         -> CVD
  HMB          -> CVD
}')


# This gives the minimal sufficient adjustment sets needed 
adjustmentSets(dag, 
               exposure = "HMB", 
               outcome  = "CVD",
               type     = "minimal")


# All possible sufficient adjustment sets that could be included
adjustmentSets(dag, 
               exposure = "HMB", 
               outcome  = "CVD",
               type     = "all")
