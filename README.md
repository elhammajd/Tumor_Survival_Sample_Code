
 # Predicting Tumor Response and Overall Survival 
---

## About this Project

The scripts are not for implementing the project. Scripts are sample presented codes to compute CV and OS in R.

---
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

2. In the **Tumor_Survival**, use the following commands to load R/4.1.0 language in Compute Canada (make sure to check and use their latest settings):

~~~
module load gcc/9.3.0 r/4.1.0
mkdir -p ~/.local/R/$EBVERSIONR/
export R_LIBS=~/.local/R/$EBVERSIONR/
~~~

3. Before we run the .sh files, we use the commands in R (version 4.1.0) to install some R packages needed for the task, as below:

~~~
 install.packages(c('dbplyr','haven','glmnet','ggplot2','ggpubr','caret','lubridate', 'Matrix','pROC', 'broom', 'ggstatsplot', 'forestplot', 'fs', 'purrr', 'fastmatch', 'gtsummary', 'rpart', 'rpart.plot', 'ISLR', 'survminer', 'survival', 'qpcR', 'rlist', 'reshape2'))
 
 pkgs <- c('dbplyr','haven','glmnet','ggplot2','ggpubr','caret','lubridate', 'Matrix','pROC', 'broom', 'ggstatsplot', 'forestplot', 'fs', 'purrr', 'fastmatch', 'gtsummary', 'rpart', 'rpart.plot', 'ISLR', 'survminer', 'survival','qpcR', 'rlist', 'reshape2')
 
lapply(pkgs, require, character.only = TRUE)

sessionInfo()
~~~
 
5. Becuase of Append lines to a file, some files be clear at the begining of running. According to deleting those files and run the .sh files, all of the files will be created with accurate outcomes. 
------

## Running files with estimated time
      
- It is not essential to use the .sh file becuase the estimated time per file is less than one hour. 
 
- If you want to use the .sh file, always submit your job under [Tumor_Survival] instead of any of the subdirectory.
---

<details><summary> 10-foldCV.R estimated time 00:20:00</summary>

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
    │ 	 └── CV # intermediate results

 ~~~
  
  <details><summary> OS-method1.R with estimated time 00:00:52</summary>
   
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
 

 
