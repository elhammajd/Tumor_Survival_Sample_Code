# Predicting Tumor Response and Overall Survival

This repository contains R scripts for processing clinical trial data to predict the Best Overall Response (BOR) and overall survival (OS) using two approaches:
1. **Ensemble Modeling with Cross-Validation** (using logistic regression, random forest, and SVM)
2. **Standard Modeling without Cross-Validation** (GLM only)

The scripts also merge OS data with prediction results, segment patients, perform survival analysis (including Cox regression and Kaplan–Meier analysis), generate forest plots for C-index, and conduct Wilcoxon tests.

## Raw Datasets and Links

All raw datasets are in the subdirectory **Datasets** at `[Tumor_Survival]`. The repository includes a figure that shows the 23 datasets and a **Links.xlsx** file that contains the link between every dataset number and its download link.  
You can download the datasets from [Project Data Sphere](https://data.projectdatasphere.org/projectdatasphere/html/home). Open the page and in the search bar paste the Study ID as mentioned in **Links.xlsx**.

## Repository Structure

. ├── README.md ├── R │ ├── 6CV_ensemble.R # Ensemble model with 10-fold CV for Dataset 6 │ ├── 6WithoutCV_ensemble.R # Standard GLM (non-ensemble) for Dataset 6 │ ├── 7CV_ensemble.R # Ensemble model with 10-fold CV for Dataset 7 │ ├── 7WithoutCV_ensemble.R # Standard GLM (non-ensemble) for Dataset 7 │ └── OS_and_Tests.R # OS integration, segmentation, survival analysis, and test results ├── cvresults # Folder for CSV output files ├── Figures # Folder for saved figures (e.g., boxplots, forest plots) └── Datasets # Raw datasets and supplemental files ├── Links.xlsx # Excel file linking dataset numbers with download URLs ├── [Tumor_Survival] # Contains 23 datasets (e.g., 6, 7, etc.) ├── 6 │ ├── demog.sas7bdat │ ├── measur.sas7bdat │ └── ae.sas7bdat └── 7 ├── demog.sas7bdat ├── measur.sas7bdat ├── ae.sas7bdat └── resples.sas7bdat

scss
Copy

## Requirements

Install R (version 3.5 or above) and the following packages:

- dplyr  
- haven  
- ggplot2  
- ggpubr  
- lubridate  
- caret  
- Matrix  
- glmnet  
- pROC  
- broom  
- randomForest  
- e1071  
- survival  
- survminer  
- rstatix  
- ggstatsplot  
- forestplot  
- gtsummary  

Install missing packages with:
```r
install.packages(c("dplyr", "haven", "ggplot2", "ggpubr", "lubridate", "caret", 
                   "Matrix", "glmnet", "pROC", "broom", "randomForest", "e1071",
                   "survival", "survminer", "rstatix", "ggstatsplot", "forestplot", "gtsummary"))
How to Run
Download the datasets as described above and place the SAS files in the appropriate folders (e.g., Datasets/6/, Datasets/7/).

Open and run the R scripts from the R folder (the order is not critical; each script saves its output in the cvresults folder and figures in the Figures folder).

Review the output CSV files for performance metrics and generated figures for survival analysis and statistical tests.

Overview of Scripts
6CV_ensemble.R
Processes Dataset 6 using an ensemble modeling approach with 10-fold cross-validation.

6WithoutCV_ensemble.R
Processes Dataset 6 using a standard GLM approach (without ensemble/CV).

7CV_ensemble.R
Processes Dataset 7 using an ensemble modeling approach with 10-fold cross-validation.

7WithoutCV_ensemble.R
Processes Dataset 7 using a standard GLM approach (without ensemble/CV).

OS_and_Tests.R
Merges OS data with prediction outputs, segments patients based on model scores, performs survival analysis (Cox regression, Kaplan–Meier), generates forest plots (C-index), and conducts Wilcoxon tests on performance metrics.
