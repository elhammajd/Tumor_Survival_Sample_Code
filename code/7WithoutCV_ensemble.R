################################################################################
# 7WithoutCV_ensemble.R
# DESCRIPTION:
#   Process Dataset 7 to predict BOR using a standard GLM (without ensemble and CV).
#   Saves performance metrics.
################################################################################

library(dplyr)
library(haven)
library(caret)
library(Matrix)
library(glmnet)
library(pROC)

# Load preprocessed Dataset 7
alldata <- read.csv("./cvresults/7-alldata.csv")
alldata <- as.data.frame(alldata)
alldata$BORval <- as.factor(alldata$BORval)

# Standard GLM modeling
set.seed(2000)
fit <- glm(BORval ~ SEX + AGE + pc1, data = alldata, family = "binomial")
coef_fit <- coef(fit)
write.table(coef_fit, "./Resultmethod2/coef_V3_7.csv", quote = FALSE, row.names = FALSE)
predictions <- predict(fit, type = "response")
write.table(predictions, "./Resultmethod2/predictions7.csv", quote = FALSE, row.names = FALSE)
tab <- table(predicted = predictions >= 0.6, references = alldata$BORval)
misclassificationrate <- 1 - sum(diag(tab)) / sum(tab)
accuracy <- 1 - misclassificationrate
auc_val <- auc(alldata$BORval, as.numeric(predictions >= 0.6))
metrics <- data.frame(misclassificationrate = misclassificationrate,
                      accuracy = accuracy,
                      auc = auc_val)
write.csv(metrics, "./cvresults/7-GLM-Metrics.csv", row.names = FALSE)
