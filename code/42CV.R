# =====================================================================================================================
#    DESCRIPTION: 
#    These codes compute the 10-fold cross-validation and prediction of the Best Overall Response
#  based on baseline characteristics and tumor size in different visits
# =====================================================================================================================
#    INPUTS:
#    - Clinical Trial Data set contains Age, Sex, TumorSize, VisitNumber, Best Overall Response, and Overall Survival
# =====================================================================================================================
#    OUTPUTS:
#    - Coefficients of each variable in three and four visit numbers. 
#    - Best cut-off of 10-fold cross-validation. 
#    - AUC, Accuracy, Misclassification rate, Sensitivity, Specificity.  
# =====================================================================================================================
#    Codes produced by: 
#    - Elham Majd
# =====================================================================================================================
#    HISTORY:
#    - Creation: Feb/2022
# =====================================================================================================================
#    STATEMENT:
#    This file is part of a project entitled "Predicting Tumor Response and 
#    Overall Survival Using Early Outcomes and Baseline Characteristics"
# =====================================================================================================================
# Libraries 
# =====================================================================================================================
# install.packages("dplyr",repos = "http:/cran.us.r-project.org")
# install.packages("haven",repos = "http:/cran.us.r-project.org")
# install.packages("ggplot2",repos = "http:/cran.us.r-project.org")
# install.packages("ggpubr",repos = "http:/cran.us.r-project.org")
# install.packages("caret",repos = "http:/cran.us.r-project.org")
# install.packages("lubridate",repos = "http:/cran.us.r-project.org")
# install.packages("Matrix",repos = "http:/cran.us.r-project.org")
# install.packages("glmnet",repos = "http:/cran.us.r-project.org")
# install.packages("pROC",repos = "http:/cran.us.r-project.org")
# install.packages("broom",repos = "http:/cran.us.r-project.org")

# load package
library(dplyr)
require(haven)
library (ggplot2)
library(ggpubr)
library(lubridate)
library(caret)
library(Matrix)
library(glmnet)
library(pROC)
library(broom)
# =====================================================================================
# Dataset 42
# =========================================================================
# Baseline Characteristics dataframe
# =========================================================================
Datasets= "./Datasets"  
filename0=paste0(Datasets,"/42/corevar")
wd<- paste0(Datasets,"/42/corevar.sas7bdat")
dm<- assign(filename0, read_sas(wd))


base <- unique(dm[,c("SUBJID" , "SEX", "AGE")])
names(base)[names(base) == "SUBJID"] <- "USUBJID"
base["SEX"][base["SEX"]=="Female"]<- "2"
base["SEX"][base["SEX"]=="Male"]<-"1"
base[base == ""] <- NA
base[base == "Unknown"] <- NA

base<- na.omit(base)


write.csv(base,"./cvresults/42-base.csv")
# ==========================================================================
# Actual Tumor Size
# ==========================================================================
filename0=paste0(Datasets,"/42/lesion")
wd<- paste0(Datasets,"/42/lesion.sas7bdat")
dtr<- assign(filename0, read_sas(wd))


TumorSz <- data.frame( USUBJID=dtr$SUBJID, val=dtr$LSSLD, visitN = dtr$VISITCD)
TumorSz<- na.omit(TumorSz)


#list ID of patients with duplicated observations
dup0 <- which(rowSums((table(TumorSz[,c(1,3)])>1))==1 )


subset(TumorSz, TumorSz$USUBJID %in% names(dup0))

collapseData <- function(dat,variable, fun=c("min","max","mean")) {
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
  res <- res[rowMeans(is.na(res[,-1]))!=1,] # if all missing, remove record
  return(res)
}

TumorSz <- collapseData(TumorSz, "val", "mean")
str(TumorSz)

TumorSz[is.na(TumorSz)] <- 0


TumorCh<- ((TumorSz[2:ncol(TumorSz)]-TumorSz[,2])/ TumorSz[,2])*100  


PTumorch<- data.frame(USUBJID=TumorSz$USUBJID, TumorCh)
PTumorch[PTumorch == "Inf"] <- NA
PTumorch<- na.omit(PTumorch)

write.csv(PTumorch, "./cvresults/42-FullTumorSz.csv")


keeps <- c("USUBJID","val.W8","val.W16","val.W24")

TumorSzfinal =PTumorch[keeps]
names(TumorSzfinal)[names(TumorSzfinal)=="val.W8"] <-"v2"
names(TumorSzfinal)[names(TumorSzfinal)=="val.W16"]<- "v3"
names(TumorSzfinal)[names(TumorSzfinal)=="val.W24"]<- "v4"

write.csv(TumorSzfinal, "./cvresults/42-TumorSz.csv")
# ========================================================================
# PCA MODEL
# ========================================================================
TumorSzV3<-read.csv("./cvresults/42-TumorSz.csv")
keeps <-c("v2","v3")
tumorsize2 = TumorSzV3[keeps]

set.seed(1230) 
idx2 <- 1:2
pca3 <- princomp(tumorsize2[,idx2], cor = FALSE, scores = TRUE)
summary(pca3)

scorevisit3<-pca3$scores
a<-pca3$loadings

a<- matrix(a)
colnames(a)<-paste0("model42")
write.csv(a,"./val_v3/42.csv", append = FALSE, quote = FALSE, sep = "", row.names = FALSE, col.names = TRUE)
scorevisit3<- cbind(USUBJID=TumorSzfinal$USUBJID, scorevisit3)


write.csv(scorevisit3,"./cvresults/42-pca3.csv")
# ========================================================================
keeps <- c("v2", "v3", "v4")
tumorsize3 = TumorSzV3[keeps]

set.seed(1230) 
idx1 <- 1:3
pca4 <- princomp(tumorsize3[,idx1], cor = FALSE, scores = TRUE)
summary(pca4)

scorevisit4<-pca4$scores 
a<-pca4$loadings

a<- matrix(a)
colnames(a)<-paste0("model42")
write.csv(a,"./val_v4/42.csv", append = FALSE, quote = FALSE, sep = "", row.names = FALSE, col.names = TRUE)
scorevisit4<- cbind(USUBJID=TumorSzfinal$USUBJID, scorevisit4)


write.csv(scorevisit4,"./cvresults/42-pca4.csv")
# =========================================================================
# Overall survival
# =========================================================================
filename0=paste0(Datasets,"/42/a_endpt")
wd<- paste0(Datasets,"/42/a_endpt.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$SUBJID, event=os$PFSCR, time=os$PFSDYCR))
os<- na.omit(os)


write.csv(os, "./cvresults/42-os.csv")
# =================================================================
# best overall response T=CR, T=PR, F=SD, F=PD, NE -> NA
# ================================================================= 
filename0=paste0(Datasets,"/42/respeval")
wd<- paste0(Datasets,"/42/respeval.sas7bdat")
adrs<- assign(filename0, read_sas(wd))


BestResp <- unique(data.frame(USUBJID=adrs$SUBJID, BORval=adrs$RSBR))


BestResp<- BestResp %>% group_by(USUBJID)
BestResp<- na.omit(BestResp)

BestResp["BORval"][BestResp["BORval"]=="Complete response"]<-"1"
BestResp["BORval"][BestResp["BORval"]=="Partial response"]<-"1"
BestResp["BORval"][BestResp["BORval"]=="Progressive disease"]<-"0"
BestResp["BORval"][BestResp["BORval"]=="Stable disease"]<-"0"
BestResp["BORval"][BestResp["BORval"]=="Unable to evaluate"]<-NA
BestResp["BORval"][BestResp["BORval"]==""]<-NA
BestResp<- na.omit(BestResp)
BestResp$BORval<- as.numeric(BestResp$BORval)
BestResp<- BestResp %>% group_by(USUBJID)%>% summarise(BORval= sum(BORval))
BestResp["BORval"][BestResp["BORval"]>1]<-1
BestResp<- unique(BestResp)




write.csv(BestResp, "./cvresults/42-BestResp.csv")

# ==================================================================
# Merge all data  visits 
# ==================================================================
base<-read.csv("./cvresults/42-base.csv")
os<-read.csv("./cvresults/42-os.csv")
BestResp<-read.csv("./cvresults/42-BestResp.csv")
pca3<-read.csv("./cvresults/42-pca3.csv")
pca4<-read.csv("./cvresults/42-pca4.csv")



alldata <- merge(BestResp, base, by="USUBJID")
alldata$"val..1" <- NULL
alldata <- merge(pca3, alldata, by="USUBJID")
alldata <- merge(pca4, alldata, by="USUBJID")
names(alldata)[names(alldata) == "Comp.2.y"] <- "pc1"
names(alldata)[names(alldata) == "Comp.3"] <- "pc2"
drop <- c("X.x","X.y")
write.csv(alldata, "./cvresults/42-alldata0.csv")
alldata<- merge(TumorSzfinal, alldata, by="USUBJID")
write.csv(alldata, "./cvresults/42-alldata42.csv")
alldata <- merge(os, alldata, by="USUBJID")
alldata = alldata[,!(names(alldata) %in% drop)]


write.csv(alldata, "./cvresults/42-alldata.csv")

# ============================================================================
# Prediction / Cross validation for PCA Model 3 Visits
# ============================================================================
alldata<-read.csv("./cvresults/42-alldata.csv")

alldata<-as.data.frame(alldata)

adds<- list(c())
baseCov= c("SEX", "AGE", "pc1")
glmnet.bin <- function(data,  idx.tr=NULL, base.covariates= baseCov, lambda=c("lambda.min", "lambda.1se")){
  lambda <- match.arg(lambda)
  if (is.null(idx.tr)) idx.tr <- 1:nrow(data)
  yy <- as.matrix(data[,c("BORval")])
  preds <- c( base.covariates)
  xx <- (data[,preds])
  
  xx$SEX <- as.numeric(xx$SEX)
  xx$AGE <- as.numeric(xx$AGE)
  xx$pc1 <- as.numeric(xx$pc1)
  
  
  for(ii in which(sapply(xx,is.factor))) xx[,ii] <- as.numeric(xx[,ii]) - 1
  xx <- data.matrix(xx)
  xx.tr <- xx[idx.tr,, drop=F]
  yy.tr <- yy[idx.tr,, drop=F]
  pred0 <- yy.tr
  
  #using 10-fold CV to select best cutoff predicted probability for classification
  idx0 <- sample(1:10, nrow(xx.tr),replace=T)
  for (jj in 1:10) {
    cv.fit0 <- cv.glmnet(xx.tr[idx0!=jj,,drop=F], yy.tr[idx0!=jj,,drop=F], family="binomial")
    pred0[idx0==jj,] <- predict(cv.fit0, newx=xx.tr[idx0==jj,], s=c(lambda), type="response")
  }
  cuts <- seq(0.1,0.9, length=1000)
  bestcut <- cuts[which.max(sapply(cuts, function(x) mean((pred0>x) == yy.tr)))]
  cv.fit <- cv.glmnet(xx.tr, yy.tr, family="binomial")
  
  {if (nrow(xx.tr) == nrow(data)) 
    pp <- rep(NA, nrow(data))
    
    else pp <- predict(cv.fit, newx=xx[-c(idx.tr),, drop=F],s=c(lambda),type="response") > bestcut}
  # tidy(bestcut)
  # write.csv( tidy(bestcut),  "./cvresults/42-bestcut_V3.csv")
  coef.exactt <- coef(cv.fit, s = c(lambda))
  
  # tidy(coef.exactt)
  # write.csv( tidy(coef.exactt),  "./cvresults/42-coef_V3.csv")
  
  return(list(predicted=pp, bestcut=bestcut, coef=coef.exactt, cvfit=cv.fit, predictor=xx))
  
}
result <- glmnet.bin(data=alldata)
result
bestcut<- result$bestcut
coeff<- result$coef
coef<-tidy(coeff)
coef_intercept<- coef$value[coef$row=="(Intercept)"] 
coef_sex<- coef$value[coef$row=="SEX"] 
coef_age<- coef$value[coef$row=="AGE"] 
coef_pc1<- coef$value[coef$row=="pc1"] 

NN <- nrow(alldata)
nmodel <- length(adds)

res <- matrix(NA, NN, nmodel)
colnames(res) <- paste0("model42")
set.seed(0)
BB <- 10 #ceiling(NN/10)
index <- sample(1:BB, NN, replace=T)

res1 <- res
for(ii in 1:BB) {
  for( jj in 1:length(adds)) res1[which(index==ii),jj] <- glmnet.bin(alldata,  idx.tr=(which(index!=ii)))$predicted
}

#misclassification rate
allrate<- colMeans(res1==alldata$BORval)
# hit rate
hitrate<- colSums(res1==alldata$BORval & res1==1)/sum(alldata$BORval==1)
retrieved <- sum(res1==alldata$BORval)
tab<-table(predicted= res1, references= alldata$BORval)

auc<- auc(res1, alldata$BORval)
misclassificationrate<- 1-sum(diag(tab))/sum(tab)
accuracy= 1- misclassificationrate
warnings()

alldata["BORval"][alldata["BORval"]== 1]<-"TRUE"
alldata["BORval"][alldata["BORval"]== 0]<-"FALSE"
predicted_values<-as.factor(res1)
actual_values<-as.factor(alldata$BORval)
conf_matrix<-table(predicted_values,actual_values)

sensitivity<- sensitivity(conf_matrix)
specificity<- specificity(conf_matrix)
conf_matrix<- as.numeric(conf_matrix)
write.csv(conf_matrix, "./cvresults/42-PCA2_conf_matrix.csv")

PCAV2Prediction<- cbind(alldata=allrate, hitrate=hitrate, 
                        misclassificationrate= misclassificationrate,  accuracy=accuracy, 
                        auc= auc, sensitivity=sensitivity, specificity= specificity)


write.table( bestcut,  "./cvresults/42bestcut_V3.csv")
write.table(coef_intercept, "./cvresults/42Intercept_V3.csv")
write.table(coef_sex, "./cvresults/42SEX_V3.csv")
write.table(coef_age, "./cvresults/42AGE_V3.csv")
write.table(coef_pc1, "./cvresults/42PC1_V3.csv")
write.csv(PCAV2Prediction, "./cvresults/42PCAV2Prediction_V3.csv")
write.table(misclassificationrate, "./cvresults/42misclassificationrate_V3.csv")
write.table(auc, "./cvresults/42auc_V3.csv")
write.table(sensitivity, "./cvresults/42sensitivity_V3.csv")
write.table(specificity, "./cvresults/42specificity_V3.csv")
# ============================================================================
# Prediction / Cross validation for PCA Model 4 Visits
# ============================================================================
alldata<-read.csv("./cvresults/42-alldata.csv")

alldata<-as.data.frame(alldata)

adds<- list(c())
baseCov= c("SEX", "AGE", "pc1","pc2")
glmnet.bin <- function(data, idx.tr=NULL, base.covariates= baseCov, lambda=c("lambda.min", "lambda.1se")){
  lambda <- match.arg(lambda)
  if (is.null(idx.tr)) idx.tr <- 1:nrow(data)
  yy <- as.matrix(data[,c("BORval")])
  preds <- c( base.covariates)
  xx <- (data[,preds])
  
  xx$SEX <- as.numeric(xx$SEX)
  xx$AGE <- as.numeric(xx$AGE)
  xx$pc1 <- as.numeric(xx$pc1)
  xx$pc2 <- as.numeric(xx$pc2)
  
  for(ii in which(sapply(xx,is.factor))) xx[,ii] <- as.numeric(xx[,ii]) - 1
  xx <- data.matrix(xx)
  xx.tr <- xx[idx.tr,, drop=F]
  yy.tr <- yy[idx.tr,, drop=F]
  pred0 <- yy.tr
  
  #using 10-fold CV to select best cutoff predicted probability for classification
  idx0 <- sample(1:10, nrow(xx.tr),replace=T)
  for (jj in 1:10) {
    cv.fit0 <- cv.glmnet(xx.tr[idx0!=jj,,drop=F], yy.tr[idx0!=jj,,drop=F], family="binomial")
    pred0[idx0==jj,] <- predict(cv.fit0, newx=xx.tr[idx0==jj,], s=c(lambda), type="response")
  }
  cuts <- seq(0.1,0.9, length=1000)
  bestcut <- cuts[which.max(sapply(cuts, function(x) mean((pred0>x) == yy.tr)))]
  cv.fit <- cv.glmnet(xx.tr, yy.tr, family="binomial")
  
  {if (nrow(xx.tr) == nrow(data)) 
    pp <- rep(NA, nrow(data))
    
    else pp <- predict(cv.fit, newx=xx[-c(idx.tr),, drop=F],s=c(lambda),type="response") > bestcut}
  # tidy(bestcut)
  # write.csv( tidy(bestcut),  "./cvresults/42-bestcut_V4.csv")
  coef.exactt <- coef(cv.fit, s = c(lambda))
  
  # tidy(coef.exactt)
  # write.csv( tidy(coef.exactt),  "./cvresults/42-coef_V4.csv")
  
  return(list(predicted=pp, bestcut=bestcut, coef=coef.exactt, cvfit=cv.fit, predictor=xx))
  
}
result <- glmnet.bin(data=alldata)
result
bestcut<- result$bestcut
coeff<- result$coef
coef<-tidy(coeff)
coef_intercept<- coef$value[coef$row=="(Intercept)"] 
coef_sex<- coef$value[coef$row=="SEX"] 
coef_age<- coef$value[coef$row=="AGE"] 
coef_pc1<- coef$value[coef$row=="pc1"] 
coef_pc2<- coef$value[coef$row=="pc2"]

NN <- nrow(alldata)
nmodel <- length(adds)

res <- matrix(NA, NN, nmodel)
colnames(res) <- paste0("model42")
set.seed(0)
BB <- 10 #ceiling(NN/10)
index <- sample(1:BB, NN, replace=T)

res3 <- res
for(ii in 1:BB) {
  for( jj in 1:length(adds)) res3[which(index==ii),jj] <- glmnet.bin(alldata, idx.tr=(which(index!=ii)))$predicted
}

#misclassification rate
allrate<- colMeans(res3==alldata$BORval)
# hit rate
hitrate<- colSums(res3==alldata$BORval & res3==1)/sum(alldata$BORval==1)
retrieved <- sum(res3==alldata$BORval)
tab<-table(predicted= res3, references= alldata$BORval)

auc<- auc(res3, alldata$BORval)
misclassificationrate<- 1-sum(diag(tab))/sum(tab)
accuracy= 1-misclassificationrate
warnings()

alldata["BORval"][alldata["BORval"]== 1]<-"TRUE"
alldata["BORval"][alldata["BORval"]== 0]<-"FALSE"
predicted_values<-as.factor(res3)
actual_values<-as.factor(alldata$BORval)
conf_matrix<-table(predicted_values,actual_values)

sensitivity<- sensitivity(conf_matrix)
specificity<- specificity(conf_matrix)
conf_matrix<- as.numeric(conf_matrix)
write.csv(conf_matrix, "./cvresults/42-PCA3_conf_matrix.csv")

PCAV3Prediction<- cbind(alldata=allrate, hitrate=hitrate, 
                        misclassificationrate= misclassificationrate,  accuracy=accuracy, 
                        auc=auc, sensitivity=sensitivity, specificity= specificity)



write.table( bestcut,  "./cvresults/42bestcut_V4.csv")
write.table(coef_intercept, "./cvresults/42Intercept_V4.csv")
write.table(coef_sex, "./cvresults/42SEX_V4.csv")
write.table(coef_age, "./cvresults/42AGE_V4.csv")
write.table(coef_pc1, "./cvresults/42PC1_V4.csv")
write.table(coef_pc2, "./cvresults/42PC2_V4.csv")
write.csv(PCAV3Prediction, "./cvresults/42PCAV3Prediction_V4.csv")
write.table(misclassificationrate, "./cvresults/42misclassificationrate_V4.csv")
write.table(auc, "./cvresults/42auc_V4.csv")
write.table(sensitivity, "./cvresults/42sensitivity_V4.csv")
write.table(specificity, "./cvresults/42specificity_V4.csv")
# ===================================================================================