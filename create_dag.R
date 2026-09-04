#R script to create Directed acyclic graph for heavy menstrual bleeding and cardiovascular disease

#install.packages("dagitty")
#install.packages("ggdag")
library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)

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

# Check adjustment sets
adjustmentSets(dag, exposure = "HMB", outcome = "CVD")


#==============================================================#
#Visualise - draw DAG
# Create a status variable to colour nodes differently
dag_tidy <- dag %>%
  tidy_dagitty() %>%
  mutate(
    node_type = case_when(
      name == "HMB" ~ "Exposure",
      name == "CVD" ~ "Outcome",
      TRUE          ~ "Covariate"
    )
  )

# Plot with different colours per node type
ggplot(dag_tidy, aes(x = x, y = y, 
                     xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour = "black") +
  geom_dag_point(colour = "black", size = 21.5) +
  geom_dag_point(aes(fill = node_type),   
                 size   = 20,
                 shape  = 21,           
                 colour = "white",    
                 stroke = 1.2) +     
  geom_dag_text(colour = "black", 
                size   = 2.5,
                fontface = "bold") +
  scale_fill_manual(
    name = "Variable Type",
    values = c(
      "Exposure"  = "darkred",
      "Outcome"   = "darkgreen",
      "Covariate" = "steelblue"
    )
  ) +
  theme_dag() +
  labs(title  = "DAG: HMB and Cardiovascular Events",
       fill = "Variable Type")

ggsave(
  filename = "dag_plot.png",
  width    = 14,
  height   = 9,
  units    = "in",
  dpi      = 300,      # 300 dpi for publication quality
  bg       = "white"
)
