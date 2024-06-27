# ==================================================================================================
#    DESCRIPTION: 
#    These codes compute the 10-fold cross-validation and predict the Best Overall Response
#    based on baseline characteristics and tumor size in different visits.
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

# Load required libraries
library(dplyr)
library(haven)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(caret)
library(Matrix)
library(glmnet)
library(pROC)
library(broom)

# ==========================================================================
# Load Baseline Characteristics Data
# ==========================================================================
datasets_dir <- "./Datasets"
demog_file <- paste0(datasets_dir, "/6/demog.sas7bdat")
demog_data <- read_sas(demog_file)

baseline_data <- unique(demog_data[, c("RUSUBJID", "SEX", "AGE")])
names(baseline_data) <- c("USUBJID", "SEX", "AGE")
baseline_data$SEX <- recode(baseline_data$SEX, "FEMALE" = "2", "MALE" = "1")
baseline_data[baseline_data == ""] <- NA
baseline_data[baseline_data == "Unknown"] <- NA
baseline_data <- na.omit(baseline_data)

write.csv(baseline_data, "./cvresults/6-base.csv")

# ==========================================================================
# Load Tumor Size Data
# ==========================================================================
measur_file <- paste0(datasets_dir, "/6/measur.sas7bdat")
tumor_size_data <- read_sas(measur_file)

tumor_size <- data.frame(USUBJID = tumor_size_data$RUSUBJID, 
                         val = tumor_size_data$MSDIM1, 
                         visitN = tumor_size_data$VNUM)
tumor_size <- na.omit(tumor_size)

# Function to collapse data
collapse_data <- function(data, variable, fun = c("min", "max", "mean")) {
  res <- NULL
  for (subject_id in unique(data$USUBJID)) {
    tmp_data <- data[data$USUBJID == subject_id, , drop = FALSE]
    if (nrow(tmp_data) == 0) next
    for (visit in unique(tmp_data$visitN)) {
      visit_data <- tmp_data[tmp_data$visitN == visit, , drop = FALSE]
      if (nrow(visit_data) == 0) next
      row_data <- visit_data[1,]
      row_data[variable] <- switch(fun, 
                                   min = min(visit_data[, variable]), 
                                   max = max(visit_data[, variable]), 
                                   mean = mean(visit_data[, variable]))
      res <- rbind(res, row_data)
    }
  }
  res <- reshape(res, idvar = "USUBJID", timevar = "visitN", direction = "wide")
  res <- res[rowMeans(is.na(res[, -1])) != 1, ] # Remove records with all missing values
  return(res)
}

tumor_size <- collapse_data(tumor_size, "val", "mean")
tumor_size[is.na(tumor_size)] <- 0
tumor_change <- ((tumor_size[2:ncol(tumor_size)] - tumor_size[, 2]) / tumor_size[, 2]) * 100

pct_tumor_change <- data.frame(USUBJID = tumor_size$USUBJID, tumor_change)
pct_tumor_change[pct_tumor_change == "Inf"] <- NA
pct_tumor_change <- na.omit(pct_tumor_change)

write.csv(pct_tumor_change, "./cvresults/6-FullTumorSz.csv")

keep_columns <- c("USUBJID", "val.31", "val.32", "val.33")
final_tumor_size <- pct_tumor_change[keep_columns]
names(final_tumor_size) <- c("USUBJID", "v2", "v3", "v4")

write.csv(final_tumor_size, "./cvresults/6-TumorSz.csv")

# ==========================================================================
# PCA Model
# ==========================================================================
pca_data <- read.csv("./cvresults/6-TumorSz.csv")

# Function for PCA
perform_pca <- function(data, num_components) {
  set.seed(1230)
  idx <- 1:num_components
  pca_result <- princomp(data[, idx], cor = FALSE, scores = TRUE)
  return(pca_result)
}

# PCA for 3 visits
pca_3_visits <- perform_pca(pca_data[, c("v2", "v3")], 2)
write.csv(matrix(pca_3_visits$loadings), "./val_v3/6.csv")
write.csv(cbind(USUBJID = final_tumor_size$USUBJID, pca_3_visits$scores), "./cvresults/6-pca3.csv")

# PCA for 4 visits
pca_4_visits <- perform_pca(pca_data[, c("v2", "v3", "v4")], 3)
write.csv(matrix(pca_4_visits$loadings), "./val_v4/6.csv")
write.csv(cbind(USUBJID = final_tumor_size$USUBJID, pca_4_visits$scores), "./cvresults/6-pca4.csv")

# ==========================================================================
# Load Overall Survival Data
# ==========================================================================
os_file <- paste0(datasets_dir, "/6/ae.sas7bdat")
os_data <- read_sas(os_file)

overall_survival <- unique(data.frame(USUBJID = os_data$RUSUBJID, 
                                      event = os_data$AEOUT, 
                                      time = os_data$AEOUTDY))
overall_survival$event <- recode(overall_survival$event, 
                                 "FATAL" = "0", 
                                 "NOT RECOVERED" = "0", 
                                 "RECOVERED" = "1", 
                                 "RECOVERING" = "1", 
                                 "UNKNOWN" = NA)
overall_survival$event <- as.numeric(overall_survival$event)
overall_survival <- na.omit(overall_survival)

write.csv(overall_survival, "./cvresults/6-os.csv")

# ==========================================================================
# Load Best Overall Response Data
# ==========================================================================
resples_file <- paste0(datasets_dir, "/6/resples.sas7bdat")
resples_data <- read_sas(resples_file)

best_response <- resples_data %>% 
  filter(RLNTL == "COMPLETE RESPONSE") %>% 
  transmute(USUBJID = RUSUBJID, 
            BORval = recode(RLINT, 
                            "COMPLETE RESPONSE" = "1", 
                            "PARTIAL RESPONSE" = "1", 
                            "STABLE DISEASE" = "0", 
                            "PROGRESSIVE DISEASE" = "0", 
                            "UNKNOWN" = NA, 
                            "NA" = NA, 
                            "" = NA)) %>% 
  group_by(USUBJID) %>% 
  summarise(BORval = sum(as.numeric(BORval), na.rm = TRUE)) %>% 
  mutate(BORval = ifelse(BORval > 1, 1, BORval)) %>% 
  na.omit()

write.csv(best_response, "./cvresults/6-BestResp.csv")

# ==========================================================================
# Merge All Data
# ==========================================================================
base <- read.csv("./cvresults/6-base.csv")
os <- read.csv("./cvresults/6-os.csv")
best_response <- read.csv("./cvresults/6-BestResp.csv")
pca_3 <- read.csv("./cvresults/6-pca3.csv")
pca_4 <- read.csv("./cvresults/6-pca4.csv")

merged_data <- base %>% 
  merge(best_response, by = "USUBJID") %>% 
  merge(pca_3, by = "USUBJID") %>% 
  merge(pca_4, by = "USUBJID") %>% 
  merge(final_tumor_size, by = "USUBJID") %>% 
  merge(os, by = "USUBJID")

write.csv(merged_data, "./cvresults/6-alldata.csv")

# ==========================================================================
# Prediction / Cross-Validation for PCA Model
# ==========================================================================
perform_glmnet <- function(data, predictors, response = "BORval", lambda = c("lambda.min", "lambda.1se")) {
  set.seed(1230)
  lambda <- match.arg(lambda)
  data <- data %>% mutate(across(all_of(predictors), as.numeric))
  x <- data.matrix(data[predictors])
  y <- as.matrix(data[[response]])
  
  cv_fit <- cv.glmnet(x, y, family = "binomial")
  best_cutoff <- find_best_cutoff(cv_fit, x, y)
  
  return(list(cv_fit = cv_fit, best_cutoff = best_cutoff))
}

find_best_cutoff <- function(cv_fit, x, y) {
  predicted_prob <- predict(cv_fit, newx = x, s = "lambda.min", type = "response")
  cutoffs <- seq(0.1, 0.9, length = 1000)
  best_cutoff <- cutoffs[which.max(sapply(cutoffs, function(cutoff) mean((predicted_prob > cutoff) == y)))]
  return(best_cutoff)
}

predict_glmnet <- function(cv_fit, x, best_cutoff) {
  predicted_prob <- predict(cv_fit, newx = x, s = "lambda.min", type = "response")
  predicted_classes <- ifelse(predicted_prob > best_cutoff, 1, 0)
  return(predicted_classes)
}

# Example usage for PCA with 3 visits
predictors_3 <- c("SEX", "AGE", "Comp.1")
glmnet_result_3 <- perform_glmnet(merged_data, predictors_3)
predicted_classes_3 <- predict_glmnet(glmnet_result_3$cv_fit, merged_data[predictors_3], glmnet_result_3$best_cutoff)

# Evaluate model performance
evaluate_model_performance <- function(actual, predicted) {
  conf_matrix <- table(predicted = predicted, actual = actual)
  auc_value <- auc(actual, predicted)
  misclassification_rate <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
  accuracy <- 1 - misclassification_rate
  sensitivity_value <- sensitivity(conf_matrix)
  specificity_value <- specificity(conf_matrix)
  return(list(auc = auc_value, misclassification_rate = misclassification_rate, 
              accuracy = accuracy, sensitivity = sensitivity_value, specificity = specificity_value))
}

performance_3 <- evaluate_model_performance(merged_data$BORval, predicted_classes_3)

write.csv(performance_3, "./cvresults/6-PCA3Performance.csv")

# Similarly, perform for PCA with 4 visits
predictors_4 <- c("SEX", "AGE", "Comp.1", "Comp.2")
glmnet_result_4 <- perform_glmnet(merged_data, predictors_4)
predicted_classes_4 <- predict_glmnet(glmnet_result_4$cv_fit, merged_data[predictors_4], glmnet_result_4$best_cutoff)
performance_4 <- evaluate_model_performance(merged_data$BORval, predicted_classes_4)

write.csv(performance_4, "./cvresults/6-PCA4Performance.csv")
