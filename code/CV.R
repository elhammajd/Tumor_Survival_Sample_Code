# load package
library(dplyr)
require(haven)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(caret)
library(Matrix)
library(glmnet)
library(pROC)
library(broom)
library(randomForest)
library(e1071)
# ============================================================================================
# DATASET 11
# =========================================================================
# Baseline Characteristics dataframe
# =========================================================================
Datasets= "./Datasets"  
filename0=paste0(Datasets,"/11/lab")
wd<- paste0(Datasets,"/11/lab.sas7bdat")
dm<- assign(filename0, read_sas(wd))
base <- unique(dm[,c("SUBJID" , "SEX", "AGE")])
names(base)[names(base) == "SUBJID"] <- "USUBJID"
base$SEX[base$SEX=="Female"]<- "2"
base$SEX[base$SEX=="Male"]<- "1"
Male<-sum(base$SEX=="1")
Female<-sum(base$SEX=="2")
base[base == ""] <- NA
base[base == "Unknown"] <- NA
base<- na.omit(base)
write.csv(base,"./cvresults/11-base.csv")
# ==========================================================================
# Actual Tumor Size
# ==========================================================================
filename0=paste0(Datasets,"/11/lesion")
wd<- paste0(Datasets,"/11/lesion.sas7bdat")
dtr<- assign(filename0, read_sas(wd))
TumorSz <- data.frame( USUBJID=dtr$SUBJID, val=dtr$LSSLDPCH , visitN = dtr$VISITCD)
TumorSz<- na.omit(TumorSz)
dup0 <- which(rowSums((table(TumorSz[,c(1,3)])>1))==1 )
subset(TumorSz, TumorSz$USUBJID %in% names(dup0))
collapseData <- function(dat,variable, fun=c("min","max","mean")) {
  fun <- match.arg(fun)
  res <- NULL
  for(ii in unique(dat$USUBJID)){
    tmp0 <- dat[dat$USUBJID==ii,,drop=F]
    if (nrow(tmp0)==0) next
    for(jj in unique(tmp0$visitN)){
      tmp2 <- tmp0[tmp0$visitN==jj,,drop=F]
      if (nrow(tmp2)==0) next
      tmp1 <- tmp2[1,]
      if(fun=="min") tmp1[variable] <- min(tmp2[,variable])
      if(fun=="max") tmp1[variable] <- max(tmp2[,variable])
      if(fun=="mean") tmp1[variable] <- mean(tmp2[,variable])
      res <- rbind(res, tmp1)
    }
  }
  res <- reshape(res, idvar="USUBJID", timevar= "visitN", direction = "wide")
  res <- res[rowMeans(is.na(res[,-1]))!=1,]
  return(res)
}
TumorSz <- collapseData(TumorSz, "val", "mean")
str(TumorSz)
TumorSz[is.na(TumorSz)] <- 0
write.csv(TumorSz, "./cvresults/11-FullTumorSz.csv")
keeps <- c("USUBJID","val.W8", "val.W16", "val.W24")
TumorSzfinal = TumorSz[keeps]
names(TumorSzfinal)[names(TumorSzfinal)=="val.W8"] <- "v2"
names(TumorSzfinal)[names(TumorSzfinal)=="val.W16"]<- "v3"
names(TumorSzfinal)[names(TumorSzfinal)=="val.W24"]<- "v4"
write.csv(TumorSzfinal, "./cvresults/11-TumorSz.csv")
# ========================================================================
# PCA MODEL
# ========================================================================
TumorSzV3<-read.csv("./cvresults/11-TumorSz.csv")
keeps <- c("v2","v3")
tumorsize2 = TumorSzV3[keeps]
set.seed(1230) 
idx2 <- 1:2
pca3 <- princomp(tumorsize2[,idx2], cor = FALSE, scores = TRUE)
summary(pca3)
scorevisit3<- pca3$scores
a<- pca3$loadings
a<- matrix(a)
colnames(a)<- paste0("model11")
write.csv(a,"./val_v3/11.csv", append = FALSE, quote = FALSE, row.names = FALSE, col.names = TRUE)
scorevisit3<- cbind(USUBJID=TumorSzfinal$USUBJID, scorevisit3)
write.csv(scorevisit3,"./cvresults/11-pca3.csv")
keeps <- c("v2", "v3", "v4")
tumorsize3 = TumorSzV3[keeps]
set.seed(1230) 
idx1 <- 1:3
pca4 <- princomp(tumorsize3[,idx1], cor = FALSE, scores = TRUE)
summary(pca4)
scorevisit4<- pca4$scores 
a<- pca4$loadings
a<- matrix(a)
colnames(a)<- paste0("model11")
write.csv(a,"./val_v4/11.csv", append = FALSE, quote = FALSE, row.names = FALSE, col.names = TRUE)
scorevisit4<- cbind(USUBJID=TumorSzfinal$USUBJID, scorevisit4)
write.csv(scorevisit4,"./cvresults/11-pca4.csv")
# =========================================================================
# Overall survival
# =========================================================================
filename0=paste0(Datasets,"/11/a_eendpt")
wd<- paste0(Datasets,"/11/a_eendpt.sas7bdat")
os1<- assign(filename0, read_sas(wd))
os <- unique(data.frame(USUBJID=os1$SUBJID, event=1-(os1$PFSCRS), time=os1$PFSDYCRS))
os<- na.omit(os)
write.csv(os, "./cvresults/11-os.csv")
# =================================================================
# best overall response T=CR, T=PR, F=SD, F=PD, NE -> NA
# ================================================================= 
filename0=paste0(Datasets,"/11/respeval")
wd<- paste0(Datasets,"/11/respeval.sas7bdat")
adrs<- assign(filename0, read_sas(wd))
BestResp <- unique(data.frame(USUBJID=adrs$SUBJID, BORval=adrs$RSBORCD))
BestResp<- BestResp %>% group_by(USUBJID)
BestResp<- na.omit(BestResp)
BestResp$BORval[BestResp$BORval=="2"]<- TRUE
BestResp$BORval[BestResp$BORval=="3"]<- TRUE
BestResp$BORval[BestResp$BORval=="6"]<- NA
BestResp$BORval[BestResp$BORval=="4"]<- FALSE
BestResp$BORval[BestResp$BORval=="5"]<- FALSE
BestResp$BORval[BestResp$BORval=="77"]<- NA
BestResp<- na.omit(BestResp)
BestResp$BORval<- as.numeric(BestResp$BORval)
BestResp<- BestResp %>% group_by(USUBJID)%>% summarise(BORval= sum(BORval))
BestResp$BORval[BestResp$BORval>1]<- 1
BestRespf<- unique(BestResp)
write.csv(BestResp, "./cvresults/11-BestResp.csv")
# ==================================================================
# Merge all data  visits 
# ==================================================================
TumorSz<-read.csv("./cvresults/11-TumorSz.csv")
base<-read.csv("./cvresults/11-base.csv")
os<-read.csv("./cvresults/11-os.csv")
BestResp<-read.csv("./cvresults/11-BestResp.csv")
pca3<-read.csv("./cvresults/11-pca3.csv")
pca4<-read.csv("./cvresults/11-pca4.csv")
alldata <- merge(BestResp, base, by="USUBJID")
alldata$"val..1" <- NULL
alldata <- merge(pca3, alldata, by="USUBJID")
alldata <- merge(pca4, alldata, by="USUBJID")
names(alldata)[names(alldata) == "Comp.1.y"] <- "pc1"
names(alldata)[names(alldata) == "Comp.2.x"] <- "pc2"
drop <- c("X.x","X.y")
write.csv(alldata, "./cvresults/11-alldata0.csv")
alldata<- merge(TumorSzfinal, alldata, by="USUBJID")
write.csv(alldata, "./cvresults/11-alldata11.csv")
alldata <- merge(os, alldata, by="USUBJID")
alldata = alldata[,!(names(alldata) %in% drop)]
write.csv(alldata, "./cvresults/11-alldata.csv")
ensemble.bin <- function(data, idx.tr=NULL, base.covariates, lambda="lambda.min") {
  if (is.null(idx.tr)) idx.tr <- 1:nrow(data)
  yy <- as.matrix(data[,c("BORval")])
  xx <- data[,base.covariates]
  xx$SEX <- as.numeric(xx$SEX)
  xx$AGE <- as.numeric(xx$AGE)
  for(i in 1:length(base.covariates)) {
    if(grepl("pc", base.covariates[i])) {
      xx[,base.covariates[i]] <- as.numeric(xx[,base.covariates[i]])
    }
  }
  for(ii in which(sapply(xx, is.factor))) xx[,ii] <- as.numeric(xx[,ii]) - 1
  xx <- data.matrix(xx)
  xx.tr <- xx[idx.tr, , drop=F]
  yy.tr <- yy[idx.tr, , drop=F]
  idx0 <- sample(1:10, nrow(xx.tr), replace=TRUE)
  pred0 <- rep(NA, nrow(xx.tr))
  for (jj in 1:10) {
    lr_fit <- cv.glmnet(xx.tr[idx0 != jj, , drop=F], yy.tr[idx0 != jj, , drop=F], family="binomial")
    pred_lr <- predict(lr_fit, newx=xx.tr[idx0==jj, , drop=F], s=lambda, type="response")
    rf_fit <- randomForest(x=xx.tr[idx0 != jj, , drop=F], y=as.factor(yy.tr[idx0 != jj,1]))
    pred_rf <- predict(rf_fit, newdata=xx.tr[idx0==jj, , drop=F], type="prob")[,2]
    svm_fit <- svm(x=xx.tr[idx0 != jj, , drop=F], y=as.factor(yy.tr[idx0 != jj,1]), probability=TRUE)
    pred_svm <- attr(predict(svm_fit, newdata=xx.tr[idx0==jj, , drop=F], probability=TRUE), "probabilities")[,2]
    pred0[idx0==jj] <- (pred_lr + pred_rf + pred_svm) / 3
  }
  cuts <- seq(0.1, 0.9, length=1000)
  bestcut <- cuts[which.max(sapply(cuts, function(x) mean((pred0 > x) == yy.tr)))]
  lr_full <- cv.glmnet(xx.tr, yy.tr, family="binomial")
  rf_full <- randomForest(x=xx.tr, y=as.factor(yy.tr[,1]))
  svm_full <- svm(x=xx.tr, y=as.factor(yy.tr[,1]), probability=TRUE)
  if (nrow(xx.tr) == nrow(data)) {
    pp <- rep(NA, nrow(data))
  } else {
    xx.test <- xx[-idx.tr, , drop=F]
    pred_lr_test <- predict(lr_full, newx=xx.test, s=lambda, type="response")
    pred_rf_test <- predict(rf_full, newdata=xx.test, type="prob")[,2]
    pred_svm_test <- attr(predict(svm_full, newdata=xx.test, probability=TRUE), "probabilities")[,2]
    ensemble_pred <- (pred_lr_test + pred_rf_test + pred_svm_test) / 3
    pp <- ensemble_pred > bestcut
  }
  return(list(predicted=pp, bestcut=bestcut, lr_fit=lr_full, rf_fit=rf_full, svm_fit=svm_full, predictor=xx))
}
# ============================================================================
# Prediction / Cross validation for PCA Model 3 Visits
# ============================================================================
alldata<-read.csv("./cvresults/11-alldata.csv")
alldata<- as.data.frame(alldata)
adds<- list(c())
baseCov= c("SEX", "AGE", "pc1")
result <- ensemble.bin(data=alldata, base.covariates=baseCov)
result
bestcut<- result$bestcut
NN <- nrow(alldata)
nmodel <- length(adds)
res1 <- matrix(NA, NN, nmodel)
colnames(res1) <- paste0("model11")
set.seed(0)
BB <- 10
index <- sample(1:BB, NN, replace=TRUE)
for(ii in 1:BB) {
  for(jj in 1:length(adds)) {
    res1[which(index==ii), jj] <- ensemble.bin(alldata, idx.tr=(which(index!=ii)), base.covariates=baseCov)$predicted
  }
}
result <- ensemble.bin(data=alldata, base.covariates=baseCov)
bestcut<- result$bestcut
allrate<- colMeans(res1==alldata$BORval)
hitrate<- colSums(res1==alldata$BORval & res1==1)/sum(alldata$BORval==1)
retrieved <- sum(res1==alldata$BORval)
tab<- table(predicted= res1, references= alldata$BORval)
auc<- auc(res1, alldata$BORval)
misclassificationrate<- 1-sum(diag(tab))/sum(tab)
accuracy= 1- misclassificationrate
alldata$BORval[alldata$BORval== 1]<- "TRUE"
alldata$BORval[alldata$BORval== 0]<- "FALSE"
predicted_values<- as.factor(res1)
actual_values<- as.factor(alldata$BORval)
conf_matrix<- table(predicted_values, actual_values)
sensitivity<- sensitivity(conf_matrix)
specificity<- specificity(conf_matrix)
conf_matrix<- as.numeric(conf_matrix)
write.csv(conf_matrix, "./cvresults/11-PCA2_conf_matrix.csv")
PCAV2Prediction<- cbind(alldata=allrate, hitrate=hitrate, misclassificationrate= misclassificationrate, accuracy=accuracy, auc= auc, sensitivity= sensitivity, specificity= specificity)
write.table(bestcut, "./cvresults/11bestcut_V3.csv")
write.csv(PCAV2Prediction, "./cvresults/11PCAV2Prediction_V3.csv")
write.table(misclassificationrate, "./cvresults/11misclassificationrate_V3.csv")
write.table(auc, "./cvresults/11auc_V3.csv")
write.table(sensitivity, "./cvresults/11sensitivity_V3.csv")
write.table(specificity, "./cvresults/11specificity_V3.csv")
# ============================================================================
# Prediction / Cross validation for PCA Model 4 Visits
# ============================================================================
alldata<-read.csv("./cvresults/11-alldata.csv")
alldata<- as.data.frame(alldata)
adds<- list(c())
baseCov= c("SEX", "AGE", "pc1", "pc2")
result <- ensemble.bin(data=alldata, base.covariates=baseCov)
result
bestcut<- result$bestcut
NN <- nrow(alldata)
nmodel <- length(adds)
res3 <- matrix(NA, NN, nmodel)
colnames(res3) <- paste0("model11")
set.seed(0)
BB <- 10
index <- sample(1:BB, NN, replace=TRUE)
for(ii in 1:BB) {
  for(jj in 1:length(adds)) {
    res3[which(index==ii), jj] <- ensemble.bin(alldata, idx.tr=(which(index!=ii)), base.covariates=baseCov)$predicted
  }
}
result <- ensemble.bin(data=alldata, base.covariates=baseCov)
bestcut<- result$bestcut
tab<- table(predicted= res3, references= alldata$BORval)
auc<- auc(res3, alldata$BORval)
misclassificationrate<- 1-sum(diag(tab))/sum(tab)
accuracy= 1-misclassificationrate
alldata$BORval[alldata$BORval== 1]<- "TRUE"
alldata$BORval[alldata$BORval== 0]<- "FALSE"
predicted_values<- as.factor(res3)
actual_values<- as.factor(alldata$BORval)
conf_matrix<- table(predicted_values, actual_values)
sensitivity<- sensitivity(conf_matrix)
specificity<- specificity(conf_matrix)
conf_matrix<- as.numeric(conf_matrix)
write.csv(conf_matrix, "./cvresults/11-PCA3_conf_matrix.csv")
PCAV3Prediction<- cbind(alldata=colMeans(res3==alldata$BORval), hitrate= colSums(res3==alldata$BORval & res3==1)/sum(alldata$BORval==1),
                        misclassificationrate= misclassificationrate, accuracy=accuracy, auc=auc, sensitivity=sensitivity, specificity=specificity)
write.table(bestcut, "./cvresults/11bestcut_V4.csv")
write.csv(PCAV3Prediction, "./cvresults/11PCAV3Prediction_V4.csv")
write.table(misclassificationrate, "./cvresults/11misclassificationrate_V4.csv")
write.table(auc, "./cvresults/11auc_V4.csv")
write.table(sensitivity, "./cvresults/11sensitivity_V4.csv")
write.table(specificity, "./cvresults/11specificity_V4.csv")
