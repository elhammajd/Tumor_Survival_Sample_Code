# ===================================================================================================================== 
#  DESCRIPTION: 
#  The codes compute the prediction of BOR in each dataset
# ===================================================================================================================== 
#  INPUTS:
#  - Clinical Trial Data set contains Age, Sex, TumorSize, VisitNumber, Best Overall Response, and Overall Survival
# ===================================================================================================================== 
#  OUTPUTS:
#  - AUC, Accuracy, Misclassification rate, Sensitivity, Specificity.  
# ===================================================================================================================== 
#  Codes produced by: 
#  - Elham Majd
# ===================================================================================================================== 
#  HISTORY:
#  - Creation: Feb/2022
# ===================================================================================================================== 
# STATEMENT:
#   This file is part of a project entitled "Predicting Tumor Response and 
#   Overall Survival Using Early Outcomes and Baseline Characteristics"
# =====================================================================================================================
# Libraries 
# =====================================================================================================================
# load package
library(dplyr)
require(haven)
library(caret)
library(Matrix)
library(glmnet)
library(caret)
library(pROC)
# ==============================================================================================
alldata<-read.csv("./cvresults/37-alldata.csv")

alldata<- as.data.frame(alldata)


alldata$BORval<- as.factor(alldata$BORval)

set.seed(2000)
fit <- glm(alldata$BORval~ SEX+AGE+pc1, data= alldata, family="binomial")
coef<- coef(fit)
write.table(coef, "./Resultmethod2/coef_V3_37.csv", append = FALSE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")

summary(fit)

varImp(fit)


predictions<- predict(fit,type="response") 




write.table(predictions, "./Resultmethod2/predictions37.csv", append = FALSE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")


# length(alldata$BORval)
# contrasts(alldata$BORval)
# 
# glm.pred <- rep ("0", 260)
# glm.pred[predictions > .5] = "1"
# table (glm.pred ,alldata$BORval)
# mean (glm.pred == alldata$BORval)

tab<-table(predicted= predictions>=0.4, references= alldata$BORval)

summary(tab)

res3<- as.numeric(predictions>=0.4)


misclassificationrate<- 1-sum(diag(tab))/sum(tab)

accuracy= 1-misclassificationrate

warnings()

retrieved <- sum(res3==alldata$BORval)
actual<- alldata$BORval
predicted<- as.numeric(res3)
auc<- auc(actual, res3)

warnings()

predicted_values<-as.factor(res3)
actual_values<-as.factor(actual)
conf_matrix<-table(predicted_values,actual_values)
conf_matrix
sensitivity<- sensitivity(conf_matrix)
specificity<- specificity(conf_matrix)
conf_matrix<- as.numeric(conf_matrix)

PCAV3Prediction<- cbind(misclassificationrate= misclassificationrate,  accuracy=accuracy, 
                        auc=auc, sensitivity=sensitivity, specificity= specificity)

write.csv(PCAV3Prediction, "./cvresults/37-PCAV3Prediction.csv")
write.table(misclassificationrate, "./Resultmethod2/misclassificationrate_V3.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
write.table(auc, "./Resultmethod2/auc_V3.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
write.table(sensitivity, "./Resultmethod2/sensitivity_V3.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
write.table(specificity, "./Resultmethod2/specificity_V3.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
# ===============================================================================

alldata<- as.data.frame(alldata)


alldata$BORval<- as.factor(alldata$BORval)

set.seed(2000)
fit <- glm(alldata$BORval~ SEX+AGE+pc1+pc2, data= alldata,family="binomial")
coef<- coef(fit)
write.table(coef, "./Resultmethod2/coef_V4_37.csv", append = FALSE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")

summary(fit)

varImp(fit)


predictions<- predict(fit,type="response") 




# length(alldata$BORval)
# contrasts(alldata$BORval)
# 
# glm.pred <- rep ("0", 260)
# glm.pred[predictions > .5] = "1"
# table (glm.pred ,alldata$BORval)
# mean (glm.pred == alldata$BORval)


tab<-table(predicted= predictions>=0.4, references= alldata$BORval)

summary(tab)

res3<- as.numeric(predictions>=0.4)


misclassificationrate<- 1-sum(diag(tab))/sum(tab)

accuracy= 1-misclassificationrate

warnings()

retrieved <- sum(res3==alldata$BORval)
actual<- alldata$BORval
predicted<- as.numeric(res3)
auc<- auc(actual, res3)

warnings()

predicted_values<-as.factor(res3)
actual_values<-as.factor(actual)
conf_matrix<-table(predicted_values,actual_values)
conf_matrix
sensitivity<- sensitivity(conf_matrix)
specificity<- specificity(conf_matrix)
conf_matrix<- as.numeric(conf_matrix)

PCAV4Prediction<- cbind(misclassificationrate= misclassificationrate,  accuracy=accuracy, 
                        auc=auc, sensitivity=sensitivity, specificity= specificity)


write.csv(PCAV3Prediction, "./cvresults/37-PCAV4Prediction.csv")
write.table(misclassificationrate, "./Resultmethod2/misclassificationrate_V4.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
write.table(auc, "./Resultmethod2/auc_V4.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
write.table(sensitivity, "./Resultmethod2/sensitivity_V4.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
write.table(specificity, "./Resultmethod2/specificity_V4.csv", append = TRUE, quote = FALSE, sep = "", row.names = FALSE, col.names = "model37")
# ==============================================================================================