################################################################################
# 6WithoutCV_ensemble.R
# DESCRIPTION:
#   Process Dataset 6 to predict BOR using a standard GLM (without ensemble and CV).
#   Saves performance metrics.
################################################################################

library(dplyr)
library(haven)
library(caret)
library(Matrix)
library(glmnet)
library(pROC)

# Load preprocessed Dataset 6
alldata <- read.csv("./cvresults/6-alldata6.csv")
alldata <- as.data.frame(alldata)
alldata$BORval <- as.factor(alldata$BORval)

# Standard GLM modeling
set.seed(0)
fit <- glm(BORval ~ SEX + AGE + pc1, data = alldata, family = "binomial")
coef_fit <- coef(fit)
write.table(coef_fit, "./Resultmethod2/coef_V3_6.csv", quote = FALSE, row.names = FALSE)
predictions <- predict(fit, type = "response")
write.table(predictions, "./Resultmethod2/predictions6.csv", quote = FALSE, row.names = FALSE)
tab <- table(predicted = predictions >= 0.4, references = alldata$BORval)
misclassificationrate <- 1 - sum(diag(tab)) / sum(tab)
accuracy <- 1 - misclassificationrate
auc_val <- auc(alldata$BORval, as.numeric(predictions >= 0.4))
metrics <- data.frame(misclassificationrate = misclassificationrate,
                      accuracy = accuracy,
                      auc = auc_val)
write.csv(metrics, "./cvresults/6-GLM-Metrics.csv", row.names = FALSE)
