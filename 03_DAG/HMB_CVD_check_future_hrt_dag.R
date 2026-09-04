#R script with for heavy menstrual bleeding and cardiovascular outcome. This script checks whether future HRT (HRT_post) after exposed needs to be adjusted for.

#HMB = heavy menstrual bleeding 
#CVD = cardiovascular disease
#CHCP = combined hormonal contraceptive pill
#HRT_prior = HRT given prior to index date
#HRT_post = HRT given after index date

library(dagitty)

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


# Check if adjustment set without future HRT is sufficient. So whether adjustment for age blocks the backdoor path to HRT_post.
# if output is TRUE do not need to adjust for HRT_post.
isAdjustmentSet(dag,
                Z = c("Age", "Obesity", "Deprivation",
                      "CHCP", "NSAIDs", "Year",
                      "Hypertension", "Diabetes", "Smoking"),
                exposure = "HMB",
                outcome  = "CVD")
