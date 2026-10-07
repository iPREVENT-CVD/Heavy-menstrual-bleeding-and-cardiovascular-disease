# Cardiovascular outcomes in females with heavy menstrual bleeding
This project was a retrospective cohort study using electronic health care records of females aged 18 to 50 years with and without heavy menstrual bleeding within Southeast Scotland (Health board NHS Lothian).   The aim of the study was to determine whether those with heavy menstrual bleeding had a higher risk of cardiovascular disease than those without.

## Table of Contents
- [About](#about)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Dependencies](#dependencies)
- [Authors](#authors)
- [Acknowledgements](#acknowledgements)
- [Citation](#citation)
- [Licence](#licence)

## About
- Problem: Studies have suggested that females with heavy menstrual bleeding maybe at risk of developing cardiovascular disease. We used a data-drive approach using electronic healthcare records  to design a cohort study to improve understanding of cardiovascular risk in those with heavy menstrual bleeding.
- Exposures: Heavy menstrual bleeding was defined by (i) a prescription record of tranexamic acid, (ii) hysterectomy with hospital record of heavy menstrual bleeding code and/or (iii) operation record of endometrial ablation. Those without heavy menstrual bleeding had a prescription record of copper intrauterine device and/or operation record of laparascopic sterilisation.
- Outcomes: (i) Non-fatal cardiovascular diseases - ischaemic heart disease (coronary heart disease and myocardial infarction), cerebrovascular disease (stroke, transient ischaemic attack) and venous thromboembolism (pulmonary and other venous thromboembolism), (ii) fatal cardiovascular disease (iii) all cause death and (iv) initiation of anti-hypertensive and/or lipid lowering agents 
- Data sources: primary care (Vision GP system), secondary care (TrakCare), national Scottish hospital records (Scottish Morbidity Records 01), death records (National Records of Scotland) and prescribing data (Prescribing Information System).
- Coding of health conditions: primary care - GP Read Codes, hospital and national records - ICD 10 codes, medications - British National Formulary
- Methods:
  - Causal inference for covariate adjustment
  - Multiple imputations were used for missing data as mechanism for missingness was found to be missing at random
  - Covariate balanced propensity scores (CBPS) were used to address confounding by indication
  - Double adjusted approach used CBPS and cox proportional hazards models to estimate risk of cardiovascular disease in those with heavy menstrual bleeding compared to those without.
- Details of definitions used for coding and methods see manuscript and supplementary materials

## Installation
Step by step instructions to get your code running:
1. Clone the repository
git clone https://github.com/iPREVENT-CVD/Heavy-menstrual-bleeding-and-cardiovascular-disease.git

2. Install R
Download and install R from https://www.r-project.org/
Recommended version: R 4.0+

3. Install RStudio (Recommended)
Download from https://www.rstudio.com/

4. Install required packages. At the start of the script required packages are stated. If these have not already been downloaded follow these steps to install packages prior to running scripts: (1) Open R or RStudio (2) run install.packages(c("package1", "package2"))

## Usage
The R scripts provided in "01_data cleaning" are steps then can be used to clean and link datasets to create a data frame to run the code for the study. The R scripts in "02_descriptive_primary analysis" includes methods used in the study for descriptive summary and creation of cox proportional hazard model with covariate balance propensity scores. No data is provided with the R scripts.


## Project Structure
```
├── 01_data cleaning/   # R scripts for data cleaning and tidying

├── 02_descriptive_primary analysis/   # R scripts for descriptive and primary analsysis

├── 03_DAG  # R scripts for directed acyclic graphs for causal inference

└── README.md    # This file
```

## Dependencies
- R 4.0+
- RStudio (recommended)
- Packages required:
   - tidyverse (ggplot2, dplyr, purr, tibble, forcats, lubridate, stringr)
   - mice, mitools
   - survival, survminer
   - cobalt, WeightIt
   - broom, patchwork
   - dagitty
  

## Author
- Thulani Ashcroft - University of Edinburgh
- Collaborators: Marie de Bakker, Dorien Kimenai, Dave Yeung, Peter Gallacher - University of Edinburgh


## Acknowledgements
This work was supported by the Medical Research Council, Precision Medicine Grant (MR/W006804/1) and Health Data Research UK which receives its funding from HDR UK Ltd (HDR-5012) funded by the UK Medical Research Council, Engineering and Physical Sciences Research Council, Economic and Social Research Council, Department of Health and Social Care (England), Chief Scientist Office of the Scottish Government Health and Social Care Directorates, Health and Social Care Research and Development Division (Welsh Government), Public Health Agency (Northern Ireland), British Heart Foundation and the Wellcome Trust. JAM received salary support from Wellcome Fellowship 209589/Z/17/Z and DMK is supported by an Intermediate Basic Science Research Fellowship and Research Excellence Award from the British Heart Foundation (FS/IBSRF/23/25161, RE/24/130012). TA and DMK had full access to all the data in the study and takes responsibility for the integrity of the data and the accuracy of the data analysis.


## Citation
If you use this code in your research please cite:
Copy Code
(placeholder)


## Licence
This project is licensed under the [MIT Licence](LICENSE)

## Contact
For questions or issues please contact:
- Thulani Ashcroft - t.k.ashcroft@sms.ed.ac.uk

