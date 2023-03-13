# =====================================================================================================================
# DESCRIPTION: 
# The codes aggregate the coefficients collected from cv.glmnet
# =====================================================================================================================
# INPUTS:
# - coefficients of baseline characteristics
# - coefficients of tumor size
# - prediction features' results
# =====================================================================================================================
# OUTPUTS:
# - Mean of coefficients 
# - Mean of prediction features
# =====================================================================================================================
# Codes produced by: 
# - Elham Majd
# =====================================================================================================================
# HISTORY:
# - Creation: Feb/2022
# =====================================================================================================================
# STATEMENT:
#  This file is part of a project entitled "Predicting Tumor Response and 
#  Overall Survival Using Early Outcomes and Baseline Characteristics"
# =====================================================================================================================
# load package
library(dplyr)

bestcut<-read.csv("./Resultmethod1/bestcut_V3.csv")
INTERCEPT<-read.csv("./coef_v3/Intercept.csv")
AGE<-read.csv("./coef_v3/AGE.csv")
SEX<-read.csv("./coef_v3/SEX.csv")
pc1<-read.csv("./coef_v3/PC1.csv")
bestcutf<- (bestcut[!grepl("model", bestcut[,1]),])
X1 <- mean(as.numeric(bestcutf), na.rm=TRUE)
INTERCEPTf<- (INTERCEPT[!grepl("model", INTERCEPT[,1]),])
X2 <- mean(as.numeric(INTERCEPTf))
SEXf<- (SEX[!grepl("model", SEX[,1]),])
X3 <- mean(as.numeric(SEXf), na.rm=TRUE)
AGEf<- (AGE[!grepl("model", AGE[,1]),])
X4 <- mean(as.numeric(AGEf), na.rm=TRUE)
pc1f<- (pc1[!grepl("model", pc1[,1]),])
X5 <- mean(as.numeric(pc1f),na.rm=TRUE)

coefvisit3<-cbind(X1, X2, X3, X4, X5)
write.csv(coefvisit3,"./Resultmethod1/coefvisit3.csv")
# =====================================================================================================================

bestcut<-read.csv("./Resultmethod1/bestcut_V4.csv")
INTERCEPT<-read.csv("./coef_v4/Intercept.csv")
AGE<-read.csv("./coef_v4/AGE.csv")
SEX<-read.csv("./coef_v4/SEX.csv")
pc1<-read.csv("./coef_v4/PC1.csv")
pc2<-read.csv("./coef_v4/PC2.csv")
bestcutf<- (bestcut[!grepl("model", bestcut[,1]),])
X1<- mean(as.numeric(bestcutf))
INTERCEPTf<- (INTERCEPT[!grepl("model", INTERCEPT[,1]),])
X2 <- mean(as.numeric(INTERCEPTf))
SEXf<- (SEX[!grepl("model", SEX[,1]),])
X3 <- mean(as.numeric(SEXf))
AGEf<- (AGE[!grepl("model", AGE[,1]),])
X4 <- mean(as.numeric(AGEf))
pc1f<- (pc1[!grepl("model", pc1[,1]),])
X5 <- mean(as.numeric(pc1f))
pc2f<- (pc2[!grepl("model", pc2[,1]),])
X6 <- mean(as.numeric(pc2f))


coefvisit4<-cbind(X1, X2, X3, X4, X5, X6)
write.csv(coefvisit4,"./Resultmethod1/coefvisit4.csv")
# =========================================================================
# Val-Coef-method1
# =========================================================================
val6<- read.csv("./val_v3/6.csv")
val7<- read.csv("./val_v3/7.csv")
val8<- read.csv("./val_v3/8.csv")
val9<- read.csv("./val_v3/9.csv")
val11<- read.csv("./val_v3/11.csv")
val12<- read.csv("./val_v3/12.csv")
val16<- read.csv("./val_v3/16.csv")
val17<- read.csv("./val_v3/17.csv")
val18<- read.csv("./val_v3/18.csv")
val22<- read.csv("./val_v3/22.csv")
val23<- read.csv("./val_v3/23.csv")
val31<- read.csv("./val_v3/31.csv")
val32<- read.csv("./val_v3/32.csv")
val34<- read.csv("./val_v3/34.csv")
val35<- read.csv("./val_v3/35.csv")
val37<- read.csv("./val_v3/37.csv")
val41<- read.csv("./val_v3/41.csv")
val42<- read.csv("./val_v3/42.csv")
val53<- read.csv("./val_v3/53.csv")
val54<- read.csv("./val_v3/54.csv")
val55<- read.csv("./val_v3/55.csv")
val59<- read.csv("./val_v3/59.csv")
coefval<- data.frame(val6, val7, val8, val9, val11, val12, val16, val17, val18, val22, val23, val31, val32, 
                     val34, val35, val37, val41, val42, val53, val54, val55, val59)

write.csv(coefval, "./Resultmethod1/coefval.csv")
meanval<- rowMeans(coefval, na.rm = TRUE)
meanvalt<- t(meanval)
write.csv(meanvalt, "./Resultmethod1/meanval.csv")
# =========================================================================
# metrices-coef-method1
# =========================================================================
meanv3<- read.csv("./Resultmethod1/coefvisit3.csv")
meanv3<- rowMeans(meanv3, na.rm = TRUE)
write.csv(meanv3, "./Resultmethod1/meanvisit3.csv")
# =========================================================================
meanv4<- read.csv("./Resultmethod1/coefvisit4.csv")
meanv4<- rowMeans(meanv4, na.rm = TRUE)
write.csv(meanv4, "./Resultmethod1/meanvisit4.csv")
# =========================================================================
# metrices-coef-method2
# =========================================================================
C6<- read.csv("./Resultmethod2/coef_V3_6.csv")
C7<- read.csv("./Resultmethod2/coef_V3_7.csv")
C8<- read.csv("./Resultmethod2/coef_V3_8.csv")
C9<- read.csv("./Resultmethod2/coef_V3_9.csv")
C11<- read.csv("./Resultmethod2/coef_V3_11.csv")
C12<- read.csv("./Resultmethod2/coef_V3_12.csv")
C16<- read.csv("./Resultmethod2/coef_V3_16.csv")
C17<- read.csv("./Resultmethod2/coef_V3_17.csv")
C18<- read.csv("./Resultmethod2/coef_V3_18.csv")
C22<- read.csv("./Resultmethod2/coef_V3_22.csv")
C23<- read.csv("./Resultmethod2/coef_V3_23.csv")
C31<- read.csv("./Resultmethod2/coef_V3_31.csv")
C32<- read.csv("./Resultmethod2/coef_V3_32.csv")
C34<- read.csv("./Resultmethod2/coef_V3_34.csv")
C35<- read.csv("./Resultmethod2/coef_V3_35.csv")
C37<- read.csv("./Resultmethod2/coef_V3_37.csv")
C41<- read.csv("./Resultmethod2/coef_V3_41.csv")
C42<- read.csv("./Resultmethod2/coef_V3_42.csv")
C53<- read.csv("./Resultmethod2/coef_V3_53.csv")
C54<- read.csv("./Resultmethod2/coef_V3_54.csv")
C55<- read.csv("./Resultmethod2/coef_V3_55.csv")
C59<- read.csv("./Resultmethod2/coef_V3_59.csv")
coefvisit3<- data.frame(C6, C7, C8, C9, C11, C12, C16, C17, C18, C22, C23, C31, C32, C34, C35, C37, C41, C42, C53, C54, C55, C59)

write.csv(coefvisit3, "./Resultmethod2/coefvisit3.csv")
meanvisit3<- rowMeans(coefvisit3, na.rm = TRUE)
write.csv(meanvisit3, "./Resultmethod2/meanvisit3.csv")
# =========================================================================
C6<- read.csv("./Resultmethod2/coef_V4_6.csv")
C7<- read.csv("./Resultmethod2/coef_V4_7.csv")
C8<- read.csv("./Resultmethod2/coef_V4_8.csv")
C9<- read.csv("./Resultmethod2/coef_V4_9.csv")
C11<- read.csv("./Resultmethod2/coef_V4_11.csv")
C12<- read.csv("./Resultmethod2/coef_V4_12.csv")
C16<- read.csv("./Resultmethod2/coef_V4_16.csv")
C17<- read.csv("./Resultmethod2/coef_V4_17.csv")
C18<- read.csv("./Resultmethod2/coef_V4_18.csv")
C22<- read.csv("./Resultmethod2/coef_V4_22.csv")
C23<- read.csv("./Resultmethod2/coef_V4_23.csv")
C31<- read.csv("./Resultmethod2/coef_V4_31.csv")
C32<- read.csv("./Resultmethod2/coef_V4_32.csv")
C34<- read.csv("./Resultmethod2/coef_V4_34.csv")
C35<- read.csv("./Resultmethod2/coef_V4_35.csv")
C37<- read.csv("./Resultmethod2/coef_V4_37.csv")
C41<- read.csv("./Resultmethod2/coef_V4_41.csv")
C42<- read.csv("./Resultmethod2/coef_V4_42.csv")
C53<- read.csv("./Resultmethod2/coef_V4_53.csv")
C54<- read.csv("./Resultmethod2/coef_V4_54.csv")
C55<- read.csv("./Resultmethod2/coef_V4_55.csv")
C59<- read.csv("./Resultmethod2/coef_V4_59.csv")
coefvisit4<- data.frame(C6, C7, C8, C9, C11, C12, C16, C17, C18, C22, C23, C31, C32, C34, C35, C37, C41, C42, C53, C54, C55, C59)

write.csv(coefvisit4, "./Resultmethod2/coefvisit4.csv")
meanvisit4<- rowMeans(coefvisit4, na.rm = TRUE)
write.csv(meanvisit4, "./Resultmethod2/meanvisit4.csv")
# ============================================================================
auc<- read.csv("./Resultmethod1/auc_V3.csv")
auc <- auc[seq(1, nrow(auc), 2),]

sensitivity<- read.csv("./Resultmethod1/sensitivity_V3.csv")
sensitivity<- sensitivity[seq(1, nrow(sensitivity), 2),]
specificity<- read.csv("./Resultmethod1/specificity_V3.csv")
specificity<- specificity[seq(1, nrow(specificity), 2),]
missclassiticationrate<- read.csv("./Resultmethod1/misclassificationrate_V3.csv")
missclassiticationrate<- missclassiticationrate[seq(1,  nrow(missclassiticationrate), 2),]

mean1<- mean(as.numeric(auc, na.rm = TRUE))
mean2<- mean(as.numeric(sensitivity, na.rm = TRUE))
mean3<- mean(as.numeric(specificity, na.rm = TRUE))
mean4<- mean(as.numeric(missclassiticationrate, na.rm = TRUE))
mean<- rbind(AUC=mean1, Sensitivity=mean2, Specificty=mean3, MisclasificationRate=mean4)
write.csv(mean, "./Resultmethod1/meanmethod1_v3.csv")

auc<- read.csv("./Resultmethod1/auc_V4.csv")
auc <- auc[seq(1, nrow(auc), 2),]
sensitivity<- read.csv("./Resultmethod1/sensitivity_V4.csv")
sensitivity<- sensitivity[seq(1, nrow(sensitivity), 2),]
specificity<- read.csv("./Resultmethod1/specificity_V4.csv")
specificity<- specificity[seq(1, nrow(specificity), 2),]
missclassiticationrate<- read.csv("./Resultmethod1/misclassificationrate_V4.csv")
missclassiticationrate<- missclassiticationrate[seq(1, nrow(missclassiticationrate), 2),]

mean1<- mean(as.numeric(auc, na.rm = TRUE))
mean2<- mean(as.numeric(sensitivity, na.rm = TRUE))
mean3<- mean(as.numeric(specificity, na.rm = TRUE))
mean4<- mean(as.numeric(missclassiticationrate, na.rm = TRUE))
mean<- rbind(AUC=mean1, Sensitivity=mean2, Specificty=mean3, MisclasificationRate=mean4)
write.csv(mean, "./Resultmethod1/meanmethod1_v4.csv")

# ============================================================================
auc<- read.csv("./Resultmethod2/auc_V3.csv")
auc <- auc[seq(1, nrow(auc), 2),]

sensitivity<- read.csv("./Resultmethod2/sensitivity_V3.csv")
sensitivity<- sensitivity[seq(1, nrow(sensitivity), 2),]
specificity<- read.csv("./Resultmethod2/specificity_V3.csv")
specificity<- specificity[seq(1, nrow(specificity), 2),]
missclassiticationrate<- read.csv("./Resultmethod2/misclassificationrate_V3.csv")
missclassiticationrate<- missclassiticationrate[seq(1,  nrow(missclassiticationrate), 2),]

mean1<- mean(as.numeric(auc, na.rm = TRUE))
mean2<- mean(as.numeric(sensitivity, na.rm = TRUE))
mean3<- mean(as.numeric(specificity, na.rm = TRUE))
mean4<- mean(as.numeric(missclassiticationrate, na.rm = TRUE))
mean<- rbind(AUC=mean1, Sensitivity=mean2, Specificty=mean3, MisclasificationRate=mean4)
write.csv(mean, "./Resultmethod2/meanmethod2$3_v3.csv")

auc<- read.csv("./Resultmethod2/auc_V4.csv")
auc <- auc[seq(1, nrow(auc), 2),]
sensitivity<- read.csv("./Resultmethod2/sensitivity_V4.csv")
sensitivity<- sensitivity[seq(1, nrow(sensitivity), 2),]
specificity<- read.csv("./Resultmethod2/specificity_V4.csv")
specificity<- specificity[seq(1, nrow(specificity), 2),]
missclassiticationrate<- read.csv("./Resultmethod2/misclassificationrate_V4.csv")
missclassiticationrate<- missclassiticationrate[seq(1, nrow(missclassiticationrate), 2),]

mean1<- mean(as.numeric(auc, na.rm = TRUE))
mean2<- mean(as.numeric(sensitivity, na.rm = TRUE))
mean3<- mean(as.numeric(specificity, na.rm = TRUE))
mean4<- mean(as.numeric(missclassiticationrate, na.rm = TRUE))
mean<- rbind(AUC=mean1, Sensitivity=mean2, Specificty=mean3, MisclasificationRate=mean4)
write.csv(mean, "./Resultmethod2/meanmethod2$3_v4.csv")



