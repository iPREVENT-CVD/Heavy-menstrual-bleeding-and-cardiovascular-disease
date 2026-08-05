# Cardiovascular outcomes in females with heavy menstrual bleeding
This project was a retrospective cohort study using electronic health care records of females aged 18 to 50 years with and without heavy menstrual bleeding within Southeast Scotland (Health board NHS Lothian).   The aim of the study was to determine whether those with heavy menstrual bleeding had higher risk of cardiovascular disease than those without.

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
- Problem: Cardiovascular disease is leading cause of morbidity and mortality in females. Studies have suggested that females with heavy menstrual bleeding may have higher risk of future cardiovascular than those without heavy menstrual bleeding. We used a data-drive approach using electronic healthcare records  to design a cohort study to improve understanding of cardiovascular risk in those with heavy menstrual bleeding.
- Exposures: Heavy menstrual bleeding was defined by (i) a prescription record of tranexamic acid, (ii) hysterectomy with hospital record of heavy menstrual bleeding code and/or (iii) operation record of endometrial ablation. Those without heavy menstrual bleeding had a prescription record of copper intrauterine device and/or operation record of laparascopic sterilisation.
- Outcomes: (i) non-fatal cardiovascular diseases - ischaemic heart disease (coronary heart disease and myocardial infarction), cerebrovascular disease (stroke, transient ischaemic attack) and venous thromboembolism (pulmonary and other venous thromboembolism), (ii) fatal cardiovascular disease (iii) all cause death and (iv) initiation of anti-hypertensive and/or lipid lowering agents 
- Data sources: primary care (Vision GP system), secondary care (TrakCare), national Scottish hospital records (Scottish Morbidity Records 01), death records (National Records of Scotland) and prescribing data (Prescribing Information System).
- Coding of health conditions: primary care - GP Read Codes, hospital and national records - ICD 10 codes, medications - British National Formulary
- Methods: 
- Details of definitions used for coding and methods see paper ""

## Installation
Step by step instructions to get your code running:
1. Clone the repository
git clone https://github.com/yourorganisation/yourrepository.git

2. Install R
Download and install R from https://www.r-project.org/
Recommended version: R 4.0+

3. Install RStudio (Recommended)
Download from https://www.rstudio.com/

4. Install required packages. At the start of the script required packages are stated. If these have not already been downloaded follow these steps to install packages prior to running scripts: (1) Open R or RStudio (2) run install.packages(c("package1", "package2"))

## Usage
How to use the code with examples:
"example code or commands here"


## Project Structure
```
├── data/        # Input data files - if applicable

├── src/         # R scripts

├── tests/       # Test files - if applicable

├── docs/        # Documentation - if applicable

└── README.md    # This file
```

## Dependencies
- R 4.0+
- RStudio (recommended)
- Packages required:
   - tidyverse
   - ggplot2

## Authors
- Your Name - University of Edinburgh
- Collaborator Name - University of Edinburgh

## Acknowledgements
- Funding source (e.g. MRC, Wellcome Trust)
- Any collaborators or contributors
- Related projects that inspired this work

## Citation
If you use this code in your research please cite:
Copy Code
Author(s), Year, Project Name, University of Edinburgh DOI or URL if available


## Licence
This project is licensed under the [MIT Licence](LICENSE)

## Contact
For questions or issues please contact:
- Your Name - your.email@ed.ac.uk

