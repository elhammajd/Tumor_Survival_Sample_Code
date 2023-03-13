
 # Predicting Tumor Response and Overall Survival Using Early Outcomes and Baseline Characteristics
---

## About this Project
It is critical for cancer treatment to have early decision-making in clinical development and subsequently assess the clinical trials. Undoubtedly, it is worth having a predictive model that can be practical in the early stages of different tumors with various kinds of treatment to predict the best overall response and segment patients into responders and non-responders.
This project aims to identify whether there is a big difference in early efficacy metrics, conducted to predict the best overall response for clinical trials, with subsequent efficacy metrics. In addition to proposing different scoring systems made by the collected scores from the predictive model to segment the patients into responders (good group) and non-responders (bad group).

---
## Directory Layout
![image](https://github.com/elhammajd/Tumor_Survival/blob/master/Illustration1.png)

We assume the user set the default directory at **Narval** at Compute Canada
~~~
    Tumor_Survival 
~~~
All the R codes are in the subdirectory at **code** 
~~~
    Tumor_Survival/code  
~~~
All the .sh files that run the R files are in the subdirectory directory at **sh** 
~~~
    Tumor_Survival/sh  
~~~
All the log files are in the subdirectory directory at **rout** 
~~~
    Tumor_Survival/rout  
~~~
All the final prediction results/intermediate results are in **Tumor_Survival**.
The name of subdirectories are deteremined in the following parts. 
One example of the subdirectory is:
~~~
    Tumor_Survival/Osmethod1
~~~
All the graphs in the paper are in the subdirectory named **Figures** 
~~~
    Tumor_Survival/Figures  
~~~
All the raw **Datasets** from Project Data Sphere's Data Sharing Platform are accessible in subdirectory named **Datasets**. 
The link between each dataset number and the downloaded link is defined in **Links.xlsx** at the subdirectory **Datasets**. You can use the [https://data.projectdatasphere.org/projectdatasphere/html/home] to download the raw datasets.
~~~
    Tumor_Survival/Datasets 
~~~

<details><summary>code</summary>
 
    ├── Code  
    │ 	 ├── 6CV.R		  # 10-fold cross validation and prediction BOR for dataset No.6
    │ 	 ├── 7CV.R		  # 10-fold cross validation and prediction BOR for dataset No.7			
    │ 	 ├── 8CV.R		  # 10-fold cross validation and prediction BOR for dataset No.8
    │ 	 ├── 9CV.R	 		# 10-fold cross validation and prediction BOR for dataset No.9	 			
    │ 	 ├── 11CV.R		 # 10-fold cross validation and prediction BOR for dataset No.11
    │ 	 ├── 12CV.R		 # 10-fold cross validation and prediction BOR for dataset No.12			
    │ 	 ├── 16CV.R		 # 10-fold cross validation and prediction BOR for dataset No.16	
    │ 	 ├── 17CV.R		 # 10-fold cross validation and prediction BOR for dataset No.17			
    │ 	 ├── 18CV.R		 # 10-fold cross validation and prediction BOR for dataset No.18
    │ 	 ├── 22CV.R		 # 10-fold cross validation and prediction BOR for dataset No.22	
    │ 	 ├── 23CV.R		 # 10-fold cross validation and prediction BOR for dataset No.23
    │ 	 ├── 31CV.R		 # 10-fold cross validation and prediction BOR for dataset No.31			
    │ 	 ├── 32CV.R		 # 10-fold cross validation and prediction BOR for dataset No.32
    │ 	 ├── 34CV.R		 # 10-fold cross validation and prediction BOR for dataset No.34				
    │ 	 ├── 37CV.R		 # 10-fold cross validation and prediction BOR for dataset No.37
    │ 	 ├── 41CV.R		 # 10-fold cross validation and prediction BOR for dataset No.41		
    │ 	 ├── 42CV.R		 # 10-fold cross validation and prediction BOR for dataset No.42	
    │ 	 ├── 53CV.R		 # 10-fold cross validation and prediction BOR for dataset No.53 				
    │ 	 ├── 54CV.R		 # 10-fold cross validation and prediction BOR for dataset No.54
    │ 	 ├── 55CV.R		 # 10-fold cross validation and prediction BOR for dataset No.55			
    │ 	 ├── 58CV.R		 # 10-fold cross validation and prediction BOR for dataset No.58
    │ 	 ├── 59CV.R		 # 10-fold cross validation and prediction BOR for dataset No.59			
    │ 	 ├── 6WithoutCV.R		  # Prediction BOR without 10-fold cross validation for dataset No.6
    │ 	 ├── 7WithoutCV.R		  # Prediction BOR without 10-fold cross validation for dataset No.7			
    │ 	 ├── 8WithoutCV.R		  # Prediction BOR without 10-fold cross validation for dataset No.8
    │ 	 ├── 9WithoutCV.R	 		# Prediction BOR without 10-fold cross validation for dataset No.9				
    │ 	 ├── 11WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.11
    │ 	 ├── 12WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.12			
    │ 	 ├── 16WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.16	
    │ 	 ├── 17WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.17			
    │ 	 ├── 18WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.18
    │ 	 ├── 22WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.22	
    │ 	 ├── 23WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.23
    │ 	 ├── 31WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.31			
    │ 	 ├── 32WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.32
    │ 	 ├── 34WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.34				
    │ 	 ├── 37WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.37
    │ 	 ├── 41WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.41		
    │ 	 ├── 42WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.42	
    │ 	 ├── 53WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.53 				
    │ 	 ├── 54WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.54
    │ 	 ├── 55WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.55			
    │ 	 ├── 58WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.58
    │ 	 ├── 59WithoutCV.R		 # Prediction BOR without 10-fold cross validation for dataset No.59	
    │ 	 ├── Mean-coeffs.R		  # Mean of coefficients of each matrics computed through prediction BOR
    │ 	 ├── OS-method1.R 		 # Segmentation of the patients by 10-fold cross validation and Testing on the datasets 				
    │ 	 ├── Elasticnet-weights.R	 	 # Getting the weight of each dataset by suing Elasticnet model	
    │ 	 ├── segmentation-method2.R	   # Segmentation of the patients by weighted datasets			
    │ 	 ├── OS-method2.R		  # Testing the scoresystsems on datasets 	
    │ 	 ├── RegressionTree-Rates.R	   # Rating the patients in each dataset by suing RegressionTree 				
    │ 	 ├── segmentation-method3.R	   # Segmenting the patients by rating the patients
    │ 	 ├── OS-method3.R	   # Testing the scoresystsems on datasets			
    │ 	 ├── WilcoxonTests_Fig3.R	   # Comparing the misclassification rates between 3 & 4 visits with 10-foldCV
    │ 	 ├── WilcoxonTests_Fig4.R	   # Comparing the misclassification rates between 3 & 4 visits without 10-foldCV			
    │ 	 ├── Cindex_Fig9.R		 # Building a forest plot for C-Index results via method 3
    │ 	 ├── Cindex_Appendix1.R		 # Building a forest plot for C-Index results	via method 2
    │ 	 └── Cindex_Appendix2.R		 # Building a forest plot for C-Index results via method 1
 
</details>
<details><summary>sh</summary>

    ├── sh file
    │ 	 ├── 10-foldCV.sh		
    │ 	 └── Rscripts.sh			
   			
</details>
<details><summary>rout</summary>

    ├── log files after submitting jobs
    │ 	 ├── 10-foldCV.rout
    │ 	 └── Rscripts.rout			
   			   
 
</details>
<details><summary>results (final & intermediate results)</summary>
 
     ├── intermediate results
     │   ├── cvresults # prediction of BOR using 10-foldCV and without 10-foldCV
     │   ├── coef_v3 # coefficients of metrics using 10-foldCV.R
     │   ├── val_v3 # coefficients of metrics using 10-foldCV.R
     │   ├── coef_v4 # coefficients of metrics using 10-foldCV.R
     │   ├── val_v4 # coefficients of metrics using 10-foldCV.R   
     │   ├── method2results # Elasticnet-weights  
     │   ├── csvlist # Elasticnet-weights
     │   ├── read # Elasticnet-weights
     │   ├── scoremethod2 # segmentation patients using Elasticnet-weights
     │   ├── finalmethod2 # segmentation patients using Elasticnet-weights
     │   ├── finalresultmethod2 # segmentation patients using Elasticnet-weights
     │   ├── method3results # RegressionTree-Rates 
     │   ├── csvlist2 # RegressionTree-Rates 
     │   ├── readtree # RegressionTree-Rates
     │   ├── scoremethod3 # segmentation patients using RegressionTree-Rates
     │   ├── finalmethod3 # segmentation patients using RegressionTree-Rates
     │   ├── finalresultmethod3 # segmentation patients using RegressionTree-Rates 
     │   ├── rate # segmentation patients using RegressionTree-Rates 
     ├── final results
     │   ├── Resultmethod1 # prediction of BOR using 10-foldCV
     │   ├── Resultmethod2 # prediction of BOR without 10-foldCV
     │   ├── Osmethod1  # Cox test results and evaluation performance for method 1
     │   ├── Osmethod2  # Cox test results and evaluation performance for method 2
     │   └── Osmethod3  # Cox test results and evaluation performance for method 3
 
</details>

<details><summary>figures</summary>

    ├── figure    
    │ 	 ├── wilcoxontest-m1.png (Figure3.png)
    │ 	 ├── wilcoxontest-m2&3.png (Figure4.png)
    │ 	 ├── surplotm1-model58.png (Figure5.png)
    │ 	 ├── surplotm2-model58.png (Figure6.png)
    │ 	 ├── surplotm3-model58.png (Figure7.png)
    │ 	 ├── cindex-method1.png (Appendix.png)
    │ 	 ├── cindex-method2.png (Appendix.png)
    │ 	 └── cindex-method3.png (Figure9.pdf)	
 
</details>

<details><summary>raw data</summary>
    
     ├── Clinical Trials Datasets 
     │ 	├── 6		 # https://clinicaltrials.gov/ct2/show/NCT00272051 **STUDY ID: NCT00272051, Summary: A Multicenter Randomized Dble-Blind Placebo Controlled Phase III Study of the Efficacy of Xaliproden in Reducing the Neurotoxicity of the Oxaliplatin and 5-FU/LV Combination in First-Line Treatment of Patients With Metastatic Colorectal Carcinoma(MCRC)**
     │ 	├── 7	  # https://clinicaltrials.gov/ct2/show/NCT00305188 **STUDY ID: NCT00305188, SUMMARY: A Multicenter, Randomized Double-blind Placebo Controlled Phase III Study of the Efficacy of Xaliproden in Preventing the Neurotoxicity of Oxaliplatin in First-line Treatment of Patients With Metastatic Colorectal Cancer Treated With Oxaliplatin / 5-FU/LV**
     │ 	├── 8		  # https://clinicaltrials.gov/ct2/show/NCT00574275 **STUDY ID: NCT00574275, SUMMARY: A Multinational, Randomized, Double-blind Study, Comparing the Efficacy of Aflibercept Once Every 2 Weeks Versus Placebo in Patients Treated With Gemcitabine for Metastatic Pancreatic Cancer**
     │ 	├── 9		 # https://clinicaltrials.gov/ct2/show/NCT00532155 **STUDY ID: NCT00532155, SUMMARY: A Multinational, Randomized, Double-Blind Study Comparing Aflibercept Versus Placebo in Patients Treated With Second-Line Docetaxel After Failure of One Platinum Based Therapy for Locally Advanced or Metastatic Non-Small-Cell Lung Cancer**
     │ 	├── 11		 # https://clinicaltrials.gov/ct2/show/NCT00364013 **STUDY ID: NCT00364013, SUMMARY: A Randomized, Multicenter, Phase 3 Study to Compare the Efficacy of Panitumumab in Combination With Oxaliplatin/ 5-fluorouracil/ Leucovorin to the Efficacy of Oxaliplatin/ 5-fluorouracil/ Leucovorin Alone in Patients With Previously Untreated Metastatic Colorectal Cancer**
     │ 	├── 12		 # https://clinicaltrials.gov/ct2/show/NCT00460265 **STUDY ID: NCT00460265, SUMMARY: A Phase 3 Randomized Trial of Chemotherapy With or Without Panitumumab in Patients With Metastatic and/or Recurrent Squamous Cell Carcinoma of the Head and Neck (SCCHN)**
     │ 	├── 16	  # https://clinicaltrials.gov/ct2/show/NCT00384176 **STUDY ID: NCT00384176, A Randomised, Double-blind, Multicentre Phase II/III Study to Compare the Efficacy of Cediranib (RECENTIN™, AZD2171) in Combination With 5-fluorouracil, Leucovorin, and Oxaliplatin (FOLFOX), to the Efficacy of Bevacizumab in Combination With FOLFOX in Patients With Previously Untreated Metastatic Colorectal Cancer**
     │ 	├── 17	  # https://clinicaltrials.gov/ct2/show/NCT02514447 **STUDY ID: NCT02514447, SUMMARY: Safety and Pharmacokinetic Study of G1T28 in Patients With Previously Treated Extensive Stage Small Cell Lung Cancer (SCLC) Receiving Topotecan Chemotherapy**
     │ 	├── 18	  # https://clinicaltrials.gov/ct2/show/NCT01439568 **STUDY ID: NCT01439568, SUMMARY: A Randomized Study of LY2510924 and Carboplatin/Etoposide Versus Carboplatin/Etoposide in Extensive-Stage Small Cell Lung Carcinoma**
     │ 	├── 22	  # https://clinicaltrials.gov/ct2/show/NCT03041311 **STUDY ID: NCT03041311, SUMMARY: Study of Carboplatin, Etoposide, and Atezolizumab With or Without Trilaciclib in Patients With Untreated Extensive Stage Small Cell Lung Cancer**
     │ 	├── 23		 # https://clinicaltrials.gov/ct2/show/NCT02499770 **STUDY ID: NCT02499770, SUMMARY: Safety and Pharmacokinetic Study of G1T28 in Patients With Extensive Stage Small Cell Lung Cancer (SCLC) Receiving Etoposide and Carboplatin**
     │ 	├── 31		 # https://clinicaltrials.gov/ct2/show/NCT00094081 **STUDY ID: NCT00094081, SUMMARY: Phase III Randomized Trial of Concomitant Radiation, Cisplatin, and Tirapazamine Versus Concomitant Radiation and Cisplatin in Patients With Advanced Head and Neck Cancer**
     │ 	├── 32		 # https://clinicaltrials.gov/ct2/show/NCT00073307 **STUDY ID: NCT00073307, SUMMARY: A Phase III Randomized Study of BAY43-9006 in Patients With Unresectable and/or Metastatic Renal Cell Cancer**
     │ 	├── 34	  # https://clinicaltrials.gov/ct2/show/NCT00519285 **STUDY ID: NCT00519285, SUMMARY: A Multicenter, Randomized, Double-Blind Study Comparing the Efficacy and Safety of Aflibercept Versus Placebo Administered Every 3 Weeks in Patients Treated with Docetaxel / Prednisone for Metastatic Androgen-Independent Prostate Cancer**
     │ 	├── 35	  # https://clinicaltrials.gov/ct2/show/NCT00417079 **STUDY ID: NCT00417079, SUMMARY: A Randomized, Open Label Multi-Center Study of XRP6258 at 25 mg/m^2 in Combination With Prednisone Every 3 Weeks Compared to Mitoxantrone in Combination With Prednisone For The Treatment of Hormone Refractory Metastatic Prostate Cancer Previously Treated With A Taxotere®-Containing Regimen**
     │ 	├── 37	  # https://clinicaltrials.gov/ct2/show/NCT00988208 **STUDY ID: NCT00988208, SUMMARY:A Phase 3 Study to Evaluate the Efficacy and Safety of Docetaxel and Prednisone With or Without Lenalidomide in Subjects With Castrate-Resistant Prostate Cancer**
     │ 	├── 41	  # https://clinicaltrials.gov/ct2/show/NCT00115765 **STUDY ID: NCT00115765, SUMMARY: A Randomized, Open-Label, Controlled, Clinical Trial of Chemotherapy and Bevacizumab With and Without Panitumumab in the First-Line Treatment of Subjects With Metastatic Colorectal Cancer**
     │ 	├── 42		 # https://clinicaltrials.gov/ct2/show/NCT00339183 **STUDY ID: NCT00339183, SUMMARY: A Randomized, Multicenter Phase 3 Study to Compare the Efficacy of Panitumumab in Combination With Chemotherapy to the Efficacy of Chemotherapy Alone in Patients With Previously Treated Metastatic Colorectal Canc**
     │ 	├── 53		 # https://clinicaltrials.gov/ct2/show/NCT00113763 **STUDY ID: NCT00113763, SUMMARY: An Open-label, Randomized, Phase 3 Clinical Trial of ABX-EGF Plus Best Supportive Care Versus Best Supportive Care in Subjects With Metastatic Colorectal Cancer**
     │ 	├── 54		 # https://clinicaltrials.gov/ct2/show/NCT00522834 **STUDY ID: NCT00522834, SUMMARY: A Randomized, Double-blind, Phase 3 Trial of Elesclomol (STA-4783) in Combination With Paclitaxel Versus Paclitaxel Alone for Treatment of Chemotherapy-Naïve Subjects With Stage IV Metastatic Melanoma (SYMMETRY)**
     │ 	├── 55	 	# https://clinicaltrials.gov/ct2/show/NCT00290966 **STUDY ID: NCT00290966, Open Label, Randomized Multicentre Phase II/III Study of Docetaxel in Combination With Cisplatin (CDDP) or Docetaxel in Combination With 5-Fluorouracil (5-FU) and CDDP Compared to the Combination of CDDP and 5-FU in Patients With Metastatic or Locally Recurrent Gastric Cancer Previously Untreated With Chemotherapy for Advanced Disease**
     │ 	├── 58	  # https://clinicaltrials.gov/ct2/show/NCT01016483 **STUDY ID: NCT01016483, SUMMARY: Phase II Randomized Trial of MEK Inhibitor MSC1936369B or Placebo Combined With Gemcitabine in Metastatic Pancreas Cancer Subjects**
     │ 	└── 59		 # https://clinicaltrials.gov/ct2/show/NCT00981058 **STUDY ID: NCT00981058, A Randomized, Multicenter, Open-Label Phase 3 Study of Gemcitabine-Cisplatin Chemotherapy Plus Necitumumab Versus Gemcitabine-Cisplatin Chemotherapy Alone in the First-Line Treatment of Patients With Stage IV Squamous Non-Small Cell Lung Cancer**
 
</details>

---
## Notice

As all the processes are conducted using the relative path, it's very important to set up [Tumor_Survival] and use it correctly. 
[Tumor_Survival] should be consisted of three parts: part 1 is ```project/6003581/``` to ensure all the files can run on Compute Canada; part 2 is your ```user name``` at Compute Canada; part 3 is your ```folder's name```. For example, the writer's directory is as follows:

~~~
/project/6003581/elhma/Tumor_Survival

~~~

If you are not sure about the path of your working folder, try to type in 'pwd' command in linux or 'getwd()' in R language for reference. 

---
## Before you start
1. Decide the path of [Tumor_Survival] to replicate our results;

2. All raw datasets are in subdirectory **Datasets** at [Tumor_Survival]. The following figure shows the 23 datasets and the **Links.xlsx** contains the link between every dataset number and its downloaded link. You can download the datasets from [https://data.projectdatasphere.org/projectdatasphere/html/home], open the page and in in search bar paste the Study ID as mentioned in the **Links.xlsx**. The following figure shows the subdirectories **Datasets**:

![image](https://github.com/elhammajd/Tumor_Survival/blob/master/Illustration4.png)

3. Create the subdirectories **code**, **rate**, **read**, **readtree**, **Osmethod1**, **Osmethod2**, **Osmethod3**, **finalresultmethod2**, **finalresultmethod3**, **finalmethod2**, **finalmethod3**, **scoremethod1**, **scoremethod2**, **scoremethod3**, **csvlist**, **csvlist2**, **Resultmethod1**, **Resultmethod2**, **Resultmethod3**, **cvresults**, **coef_v3**, **coef_v4**, **val_v3**, **val_v4**, **sh**, **rout** and **Figures** at [Tumor_Survival]. 
** To create all directories and subdirectories use the **Create-Directories.sh** at **sh** folder.** 
The following figure shows the subdirectories at [Tumor_Survival]:

![image](https://github.com/elhammajd/Tumor_Survival/blob/master/Illustration3.png)

4. Allocate all .sh files and Rscripts into each subdirectory, other files can be generated by program. All files in **rout**, and **sh** folders should be copy from GitHub and paste in **rout**, and **sh**  which created through **Create-Directories.sh** .

5. In the **Tumor_Survival**, use the following commands to load R/4.1.0 language in Compute Canada (make sure to check and use their latest settings):

~~~
module load gcc/9.3.0 r/4.1.0
mkdir -p ~/.local/R/$EBVERSIONR/
export R_LIBS=~/.local/R/$EBVERSIONR/
~~~

6. Before we run the .sh files, we use the commands in R (version 4.1.0) to install some R packages needed for the task, as below:

~~~
 install.packages(c('dbplyr','haven','glmnet','ggplot2','ggpubr','caret','lubridate', 'Matrix','pROC', 'broom', 'ggstatsplot', 'forestplot', 'fs', 'purrr', 'fastmatch', 'gtsummary', 'rpart', 'rpart.plot', 'ISLR', 'survminer', 'survival', 'qpcR', 'rlist', 'reshape2'))
 
 pkgs <- c('dbplyr','haven','glmnet','ggplot2','ggpubr','caret','lubridate', 'Matrix','pROC', 'broom', 'ggstatsplot', 'forestplot', 'fs', 'purrr', 'fastmatch', 'gtsummary', 'rpart', 'rpart.plot', 'ISLR', 'survminer', 'survival','qpcR', 'rlist', 'reshape2')
 
lapply(pkgs, require, character.only = TRUE)

sessionInfo()
~~~
 
7. Becuase of Append lines to a file, some files be clear at the begining of running. According to deleting those files and run the .sh files, all of the files will be created with accurate outcomes. 
------

## Running files with estimated time
      
- It is not essential to use the .sh file becuase the estimated time per file is less than one hour. 
 
- If you want to use the .sh file, always submit your job under [Tumor_Survival] instead of any of the subdirectory.
---

<details><summary>1. 10-foldCV.R estimated time 00:20:00</summary>

 - Reading the raw datasets from [Tumor_Survival/Datasets];
 
 - Cleaning the data;
 
 - Making the collapse data frames from dynamic tumor size for every dataset; 
 
 - Calculating PCA for every dataset;
 
 - Finding the best cut for every dataset;
 
 - Predicting BOR for each dataset;
 
 - Comuting the auc, misclassification, sensitivity, specificity, and coefficients;
 
 - Results will be saved in the following folders:
 
  </details>
 
 ~~~
   ├── intermediate and final results are saved in following folders:
   │ 	├── cvresults # intermediate results
   │ 	├── coef_v3 # intermediate results 
   │ 	├── val_v3 # intermediate results
   │ 	├── coef_v4 # intermediate results 
   │ 	├── val_v4 # intermediate results 
   │ 	└── Resultmethod1 # final results 
~~~

*To Run the step one with estimated time 00:20:00*
 
  Open the file **Parallel-jobs.sh** at **sh** folder. First, you can clear the related folders if you run the codes before, and then all jobs can be run    parallelly. 
 
---
<details><summary>2.1. Prediction-without-10foldCV.R with estimated time 00:10:00</summary>
 
 - Reading the raw datasets from [Tumor_Survival/Datasets];
 
 - Using the alldata dataframes which was built via 10-foldCV.R;
 
 - Predicting BOR for each dataset;
 
 - Comuting the auc, misclassification, sensitivity, specificity, and coefficients;
 
 - Results will be saved in the following folders:
 
  </details>
  
 ~~~
    ├── intermediate and final results are saved in following folders:
    │ 	├── cvresults # intermediate results
    │ 	└── Resultmethod2 # final results
 ~~~

<details><summary>2.2. Mean-coeffs.R with estimated time 00:00:07</summary>
 
  - Reading the the csv files from [Tumor_Survival/Main_Directory/Resultmethod1] and [Tumor_Survival/Main_Directory/Resultmethod2];
 
  - Calculating the mean of coefficients computed by Prediction-without-10foldCV.R and 10-foldCV.R;
 
  - Results will be saved in the following folders:
 
  </details> 
  
  ~~~ 
    ├── intermediate and final results are saved in following folders: 
    │ 	├── Resultmethod1 # final results 
    │ 	└── Resultmethod2 # final results   
  ~~~
  
  <details><summary>2.3. WilcoxonTests.R with estimated time 00:00:09</summary>
   
  - Reading the the csv files from [Tumor_Survival/Main_Directory/Resultmethod1] and [Tumor_Survival/Main_Directory/Resultmethod2];
 
  - Calculating the Wilcoxon tests for missclassification rates of prediction with and without 10-foldCV, respectively;
 
 
  - Results will be saved in the following folders:
 
  </details>
  
 ~~~
    ├── intermediate and final results are saved in following folders: 
    │ 	 ├── wilcoxontest-m1.png (Figure3.png)
    │ 	 └── wilcoxontest-m2&3.png (Figure4.png)
 ~~~
  
  <details><summary>2.4. OS-method1.R with estimated time 00:00:52</summary>
   
   - Reading the raw datasets from [Tumor_Survival/Datasets] and also **alldata** data frames;
   
   - Making the OS dataframes for each dataset;
   
  - Reading the mean of coefficients from folder Resultmethod1
   
  - Evaluating the score of each patients;
   
  - Segmenting the patients into bad and god group using best cut and score of each patiens;
 
  - Results will be saved in the following folders:
 
  </details>
  
 ~~~
    ├── intermediate and final results are saved in following folders:
    │ 	 └── Osmethod1 # final results
 ~~~
  
  <details><summary>2.5. Elasticnet-weights.R with estimated time 00:01:25</summary>
 
   Note that this job will be repeated 100 times with random number of dataset numbers
   
  - Reading the scores and groups from [Tumor_Survival/Main_Directory/Osmethod1] and also **alldata** data frames;
   
  - Building the Elasticnet model;
   
  - Getting the weigt for each model;
   
  - Repeating for 100 times;   
 
  - Results will be saved in the following folders:
 
   
   </details>
   
 ~~~ 
    ├── intermediate and final results are saved in following folders:
    │ 	├── method2results # intermediate results  
    │ 	├── csvlist # intermediate results 
    │ 	└── read # intermediate results 
 ~~~
    
<details><summary>2.6 RegressionTree-Rates.R with estimated time 00:03:11</summary>
 
   Note that this job will be repeated 100 times with random number of dataset numbers
   
  - Reading the scores and groups from [Tumor_Survival/Main_Directory/Osmethod1] and also **alldata** data frames;
   
  - Building the Regresion Tree on every dataset;
   
  - Getting the rate of each patient;
   
  - Repeating for 100 times;   
 
  - Results will be saved in the following folders:
 
   
   </details>
   
 ~~~ 
    ├── intermediate and final results are saved in following folders:
    │ 	├── method3results # intermediate results  
    │ 	├── csvlist2 # intermediate results 
    │ 	└── readtree # intermediate results 
 ~~~
 
  <details><summary>2.7. Segmentation-method2.R with estimated time 00:00:52</summary>
   
  - Reading the scores and groups from [Tumor_Survival/Main_Directory/method2results] and [Tumor_Survival/Main_Directory/OS-method1.R];
   
  - Evaluating the score of each patients;
 
  - Results will be saved in the following folders:
 
   
   </details>
   
 ~~~
    ├── intermediate and final results are saved in following folders:
    │ 	├── scoremethod2 # intermediate results 
    │ 	├── finalmethod2 # intermediate results  
    │ 	└── finalresultmethod2 # intermediate results   
 ~~~
 
  <details><summary>2.8. Segmentation-method3.R with estimated time 00:01:35</summary>
   
  - Reading the scores and groups from [Tumor_Survival/Main_Directory/method2results] and [Tumor_Survival/Main_Directory/OS-method1.R];
   
  - Evaluating the score of each patients;
 
  - Results will be saved in the following folders:
 
   
   </details>
   
 ~~~
    ├── intermediate and final results are saved in following folders:
    │ 	├── scoremethod3 # intermediate results 
    │ 	├── finalmethod3 # intermediate results 
    │ 	├── rate # intermediate results  
    │ 	└── finalresultmethod3 # intermediate results   
 ~~~
 
 <details><summary>2.9. OS-method2.R with estimated time 00:03:24</summary>
   
   - Reading the raw datasets from [Tumor_Survival/Datasets] and also **alldata** data frames;
   
   - Making the OS dataframes for each dataset;
   
  - Reading the mean of coefficients from folder Resultmethod2
   
  - Evaluating the score of each patients;
   
  - Segmenting the patients into bad and god group using best cut and score of each patiens;
 
  - Results will be saved in the following folders:
 
  </details>
  
 ~~~
    ├── intermediate and final results are saved in following folders: 
    │ 	 └── Osmethod2 # final results
 ~~~
 
 <details><summary>2.10 OS-method3.R with estimated time 00:03:22</summary>
   
  - Reading the raw datasets from [Tumor_Survival/Datsets] and also **alldata** data frames;
   
  - Making the OS dataframes for each dataset;
   
  - Reading the mean of coefficients from folder Resultmethod2
   
  - Evaluating the score of each patients;
   
  - Segmenting the patients into bad and god group using best cut and score of each patiens;
 
  - Results will be saved in the following folders:
 
  </details>
  
 ~~~
    ├── intermediate and final results are saved in following folders: 
    │ 	 └── Osmethod3 # final results
 ~~~
 
  <details><summary>2.11. C-index.R with estimated time 00:00:05</summary>
   
  - Reading the the csv files from [Tumor_Survival/Main_Directory/Osmethod1], [Tumor_Survival/Main_Directory/Osmethod2], and [Tumor_Survival/Main_Directory/Osmethod3];
   
  - Making the forest plots with C-index, lower, upper and mean results in each method;
 
  - Results will be saved in the following folders:
 
  </details>
  
 ~~~
    ├── intermediate and final results are saved in following folders: 
    │ 	 ├── cindex-method1.png (Figure9.png)
    │ 	 ├── cindex-method2.png (Figure S1.png)
    │ 	 └── cindex-method3.png (Figure S2.png)	
 ~~~
 
 *To Run the step two with the estimated time 00:18:00 using the following*
 ~~~
    sbatch ./sh/Rscripts.sh
 ~~~
 
