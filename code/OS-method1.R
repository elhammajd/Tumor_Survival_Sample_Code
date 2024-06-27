# ==================================================================================================
#    DESCRIPTION: 
#    This script computes the 10-fold cross-validation and predicts the Best Overall Response
#    based on baseline characteristics and tumor size at different visits.
# ==================================================================================================
#    INPUTS:
#    - Clinical Trial Dataset containing Age, Sex, Tumor Size, Visit Number, Best Overall Response, and Overall Survival
# ==================================================================================================
#    OUTPUTS:
#    - Coefficients of each variable for three and four visit numbers.
#    - Best cutoff of 10-fold cross-validation.
#    - AUC, Accuracy, Misclassification rate, Sensitivity, Specificity.
# ==================================================================================================
#    Codes produced by: 
#    - Elham Majd
# ==================================================================================================
#    HISTORY:
#    - Creation: Feb/2022
# ==================================================================================================
#    STATEMENT:
#    This file is part of a project entitled "Predicting Tumor Response and 
#    Overall Survival Using Early Outcomes and Baseline Characteristics"
# ==================================================================================================

# Load required packages
install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("haven", repos = "http://cran.us.r-project.org")
install.packages("survival", repos = "http://cran.us.r-project.org")
install.packages("survminer", repos = "http://cran.us.r-project.org")
install.packages("caret", repos = "http://cran.us.r-project.org")
install.packages("lubridate", repos = "http://cran.us.r-project.org")
install.packages("Matrix", repos = "http://cran.us.r-project.org")
install.packages("glmnet", repos = "http://cran.us.r-project.org")
install.packages("gtsummary", repos = "http://cran.us.r-project.org")

library(dplyr)
library(haven)
library(survival)
library(survminer)
library(lubridate)
library(caret)
library(Matrix)
library(glmnet)
library(gtsummary)

# Load mean and coefficient values
meanval <- read.csv("./Resultmethod1/meanval.csv")
coefvisit3 <- read.csv("./Resultmethod1/coefvisit3.csv")

datasets_dir <- "./Datasets" 

# Define function to compute PCA model with mean and segmentation
compute_pca_model <- function(data_path, meanval, coefvisit3, model_name) {
  alldata <- read.csv(data_path)
  alldata$pc11 <- meanval$V2 * (alldata$v2 - mean(alldata$v2)) + meanval$V3 * (alldata$v3 - mean(alldata$v3))
  alldata$score <- coefvisit3$X2 + coefvisit3$X3 * alldata$SEX + coefvisit3$X4 * alldata$AGE + coefvisit3$X5 * alldata$pc11
  alldata$group0 <- alldata$score > coefvisit3$X1

  write.table(alldata$score, paste0("./scoremethod1/score", model_name, "method1.csv"), append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = model_name)
  write.table(alldata$group0, paste0("./scoremethod1/group", model_name, "method1.csv"), append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = model_name)

  return(alldata)
}

# Define function to compute survival analysis and Cox proportional hazards model
compute_survival_analysis <- function(alldata, datasets_dir, model_name, ae_file) {
  ae_path <- paste0(datasets_dir, "/", ae_file)
  os_data <- read_sas(ae_path)
  
  os <- unique(data.frame(USUBJID = os_data$RUSUBJID, event = os_data$AEOUT, time = os_data$CODDY))
  os$event <- recode(os$event, "FATAL" = 1, "NOT RECOVERED" = 1, "RECOVERED" = 0, "RECOVERING" = 0, "UNKNOWN" = NA)
  os$event <- as.numeric(os$event)
  os <- na.omit(os)
  
  alldata <- merge(os, alldata, by = "USUBJID")

  fit <- survfit(Surv(time, event) ~ group0, data = alldata)
  cox_model <- coxph(Surv(time, event) ~ group0, data = alldata)
  res <- summary(cox_model)

  coxresults <- cbind(C.index = res$concordance, HR = res$conf.int, p.value = res$waldtest["pvalue"])
  write.table(coxresults, file = "./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = model_name, col.names = FALSE)

  CoxphResult <- cox_model %>% gtsummary::tbl_regression(exp = TRUE)
  ff <- survfit(Surv(time, event) ~ group0, data = alldata, conf.type = "log-log")

  return(list(fit = fit, cox_model = cox_model, CoxphResult = CoxphResult, ff = ff))
}

# Process each model and compute survival analysis
models <- list(
  list(data_path = "./cvresults/6-alldata6.csv", ae_file = "6/ae", model_name = "6"),
  list(data_path = "./cvresults/7-alldata7.csv", ae_file = "7/ae", model_name = "7"),
  list(data_path = "./cvresults/8-alldata8.csv", ae_file = "8/ae", model_name = "8"),
  list(data_path = "./cvresults/9-alldata9.csv", ae_file = "9/ae", model_name = "9"),
  list(data_path = "./cvresults/11-alldata11.csv", ae_file = "11/a_eendpt", model_name = "11"),
  list(data_path = "./cvresults/12-alldata12.csv", ae_file = "12/aeendpt", model_name = "12"),
  list(data_path = "./cvresults/16-alldata16.csv", ae_file = "16/rdpaefig", model_name = "16"),
  list(data_path = "./cvresults/17-alldata17.csv", ae_file = "17/adtte", model_name = "17"),
  list(data_path = "./cvresults/18-alldata18.csv", ae_file = "18/os", model_name = "18"),
  list(data_path = "./cvresults/22-alldata22.csv", ae_file = "22/adtte", model_name = "22"),
  list(data_path = "./cvresults/23-alldata23.csv", ae_file = "23/adtte", model_name = "23"),
  list(data_path = "./cvresults/31-alldata31.csv", ae_file = "31/ae", model_name = "31"),
  list(data_path = "./cvresults/32-alldata32.csv", ae_file = "32/ae", model_name = "32"),
  list(data_path = "./cvresults/34-alldata34.csv", ae_file = "34/adae", model_name = "34"),
  list(data_path = "./cvresults/35-alldata35.csv", ae_file = "35/ds", model_name = "35"),
  list(data_path = "./cvresults/37-alldata37.csv", ae_file = "37/survive", model_name = "37"),
  list(data_path = "./cvresults/41-alldata41.csv", ae_file = "41/a_eendpt", model_name = "41"),
  list(data_path = "./cvresults/42-alldata42.csv", ae_file = "42/a_endpt", model_name = "42"),
  list(data_path = "./cvresults/53-alldata53.csv", ae_file = "53/adsl_pds2019", model_name = "53"),
  list(data_path = "./cvresults/54-alldata54.csv", ae_file = "54/vdt", model_name = "54"),
  list(data_path = "./cvresults/55-alldata55.csv", ae_file = "55/uae", model_name = "55"),
  list(data_path = "./cvresults/58-alldata58.csv", ae_file = "58/ae", model_name = "58"),
  list(data_path = "./cvresults/59-alldata59.csv", ae_file = "59/adtte", model_name = "59")
)

for (model in models) {
  alldata <- compute_pca_model(model$data_path, meanval, coefvisit3, model$model_name)
  survival_results <- compute_survival_analysis(alldata, datasets_dir, model$model_name, model$ae_file)
  
  fit <- survival_results$fit
  ff <- survival_results$ff

  # Kaplan Meier curves
  surv_plot <- ggsurvplot(
    ff,
    pval = TRUE,
    conf.int = TRUE,
    conf.int.style = "step",
    xlab = "TIME",
    ggtheme = theme_light(),
    surv.median.line = "hv",
    legend.main = "  ",
    legend.lab = c("Bad group", "Good group"),
    legend.lab.location = "bottomleft",
    palette = c("#FF0066", "#AF6FDF")
  )
  ggsave(file = paste0("./Figures/surplotm1-model", model$model_name, ".png"), surv_plot$plot)

  surv_plot_full <- ggsurvplot(
    ff, data = alldata,
    legend.title = "Group",
    legend.labs = c("Bad group", "Good group"),
    pval = TRUE,
    risk.table = TRUE,
    cumcensor = TRUE,
    xlab = "TIME",
    palette = c("#FF0066", "#AF6FDF"),
    ggtheme = theme_bw()
  )
  ggsave(file = paste0("./Figures/surplotm1-risk-censor-model", model$model_name, ".png"), surv_plot_full$plot)
}

summary(ff)
# ===============================================================================
