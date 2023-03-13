# install.packages("dplyr",repos = "http://cran.us.r-project.org")
# install.packages("haven",repos = "http://cran.us.r-project.org")
# install.packages("survival",repos = "http://cran.us.r-project.org")
# install.packages("survminer",repos = "http://cran.us.r-project.org")
# install.packages("caret",repos = "http://cran.us.r-project.org")
# install.packages("lubridate",repos = "http://cran.us.r-project.org")
# install.packages("Matrix",repos = "http://cran.us.r-project.org")
# install.packages("glmnet",repos = "http://cran.us.r-project.org")
# install.packages("gtsummary",repos = "http://cran.us.r-project.org")

# load package
library(dplyr)
require(haven)
library(survival)
library(ggpubr)
library(survminer)
library(lubridate)
library(caret)
library(Matrix)
library(glmnet)
library(gtsummary)

meanval<- read.csv("./Resultmethod1/meanval.csv")
coefvisit3<- read.csv("./Resultmethod1/coefvisit3.csv")

Datasets= "./Datasets" 
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata6<-read.csv("./cvresults/6-alldata6.csv")


alldata6$pc11 <- meanval$V2 *(alldata6$v2-mean(alldata6$v2)) + meanval$V3 *(alldata6$v3-mean(alldata6$v3))


# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata6$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata6$SEX  + coefvisit3$X4 * alldata6$AGE +coefvisit3$X5 * alldata6$pc11


write.table(alldata6$score,"./scoremethod1/score6method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model6")

alldata6$group0 <- alldata6$score >  coefvisit3$X1

write.table(alldata6$group0,"./scoremethod1/group6method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model6")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/6/ae")
wd<- paste0(Datasets,"/6/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=os1$AEOUT, time=os1$CODDY))
os["event"][os["event"]=="FATAL"]<- "1"
os["event"][os["event"]=="NOT RECOVERED"]<- "1"
os["event"][os["event"]=="RECOVERED"]<- "0"
os["event"][os["event"]=="RECOVERING"]<- "0"
os["event"][os["event"]=="UNKNOWN"]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata6<- merge(os, alldata6, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata6)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata6))


res<- summary(coxph(Surv(time,event)~group0, data = alldata6))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model6", col.names = TRUE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata6) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata6, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata7<-read.csv("./cvresults/7-alldata7.csv")


alldata7$pc11 <- meanval$V2 *(alldata7$v2-mean(alldata7$v2)) + meanval$V3 *(alldata7$v3-mean(alldata7$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata7$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata7$SEX  + coefvisit3$X4 * alldata7$AGE +coefvisit3$X5 * alldata7$pc11

write.table(alldata7$score,"./scoremethod1/score7method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model7")

alldata7$group0 <- alldata7$score >  coefvisit3$X1

write.table(alldata7$group0,"./scoremethod1/group7method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names ="model7")

# =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/7/ae")
wd<- paste0(Datasets,"/7/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


str(os1$AEOUT)
os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=os1$AEOUT, time=os1$VNUM))
os["event"][os["event"]==""]<- NA
os["event"][os["event"]=="NOT RECOVERED"]<- "0"
os["event"][os["event"]=="RECOVERED"]<- "1"
os["event"][os["event"]=="RECOVERING"]<- "1"
os["event"][os["event"]=="UNKNOWN"]<- NA

os$event<- as.numeric(os$event)

os<- na.omit(os)


alldata7<- merge(os, alldata7, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata7)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata7))


res<- summary(coxph(Surv(time,event)~group0, data = alldata7))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model7", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata7) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata7, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata8<-read.csv("./cvresults/8-alldata8.csv")


alldata8$pc11 <- meanval$V2 *(alldata8$v2-mean(alldata8$v2)) + meanval$V3 *(alldata8$v3-mean(alldata8$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata8$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata8$SEX  + coefvisit3$X4 * alldata8$AGE +coefvisit3$X5 * alldata8$pc11


write.table(alldata8$score,"./scoremethod1/score8method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model8")

alldata8$group0 <- alldata8$score >  coefvisit3$X1


write.table(alldata8$group0 ,"./scoremethod1/group8method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model8")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/8/ae")
wd<- paste0(Datasets,"/8/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=os1$AEOUT, time=os1$VISITNUM))
os["event"][os["event"]=="FATAL"]<- "1"
os["event"][os["event"]=="NOT RECOVERED/NOT RESOLVED"]<- "1"
os["event"][os["event"]=="RECOVERED/RESOLVED"]<- "0"
os["event"][os["event"]=="RECOVERING/RESOLVING"]<- "0"
os["event"][os["event"]=="RECOVERED/RESOLVED WITH SEQUELAE"]<- "0"
os["event"][os["event"]=="UNKNOWN"]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata8<- merge(os, alldata8, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata8)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata8))


res<- summary(coxph(Surv(time,event)~group0, data = alldata8))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model8", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata8) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata8, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata9<-read.csv("./cvresults/9-alldata9.csv")


alldata9$pc11 <- meanval$V2 *(alldata9$v2-mean(alldata9$v2)) + meanval$V3 *(alldata9$v3-mean(alldata9$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata9$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata9$SEX  + coefvisit3$X4 * alldata9$AGE +coefvisit3$X5 * alldata9$pc11


write.table(alldata9$score,"./scoremethod1/score9method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model9")

alldata9$group0 <- alldata9$score >  coefvisit3$X1

write.table(alldata9$group0,"./scoremethod1/group9method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model9")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/9/ae")
wd<- paste0(Datasets,"/9/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os1$duration<- (os1$AEENDY-os1$AESTDY)
os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=(os1$AEOUT), time=os1$VISITNUM))
os["event"][os["event"]=="FATAL"]<- "0"
os["event"][os["event"]== "NOT.RECOVERED/NOT.RESOLVED"]<- "0"
os["event"][os["event"]== "RECOVERED/RESOLVED"]<- "1"
os["event"][os["event"]== "RECOVERING/RESOLVING"]<- "1"
os["event"][os["event"]== "."]<- NA
os["event"][os["event"]=="UNKNOWN"]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata9<- merge(os, alldata9, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata9)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata9))


res<- summary(coxph(Surv(time,event)~group0, data = alldata9))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model9", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata9) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata9, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata11<-read.csv("./cvresults/11-alldata11.csv")


alldata11$pc11 <- meanval$V2 *(alldata11$v2-mean(alldata11$v2)) + meanval$V3 *(alldata11$v3-mean(alldata11$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata11$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata11$SEX  + coefvisit3$X4 * alldata11$AGE +coefvisit3$X5 * alldata11$pc11

write.table(alldata11$score,"./scoremethod1/score11method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model11")

alldata11$group0 <- alldata11$score >  coefvisit3$X1

write.table(alldata11$group0 ,"./scoremethod1/group11method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model11")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/11/a_eendpt")
wd<- paste0(Datasets,"/11/a_eendpt.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$SUBJID, event=1-(os1$PFSCRS), time=os1$PFSDYCRS))
os<- na.omit(os)

alldata11<- merge(os, alldata11, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata11)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata11))


res<- summary(coxph(Surv(time,event)~group0, data = alldata11))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model11", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata11) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata11, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata12<-read.csv("./cvresults/12-alldata12.csv")


alldata12$pc11 <- meanval$V2 *(alldata12$v2-mean(alldata12$v2)) + meanval$V3 *(alldata12$v3-mean(alldata12$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata12$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata12$SEX  + coefvisit3$X4 * alldata12$AGE +coefvisit3$X5 * alldata12$pc11

write.table(alldata12$score,"./scoremethod1/score12method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model12")

alldata12$group0 <- alldata12$score >  coefvisit3$X1

write.table(alldata12$group0,"./scoremethod1/group12method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model12")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/12/aeendpt")
wd<- paste0(Datasets,"/12/aeendpt.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$SUBJID, event=1-(os1$PFSLRE), time=os1$PFSDYLR))
os<- na.omit(os)

alldata12<- merge(os, alldata12, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata12)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata12))


res<- summary(coxph(Surv(time,event)~group0, data = alldata12))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model12", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata12) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata12, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata16<-read.csv("./cvresults/16-alldata16.csv")


alldata16$pc11 <- meanval$V2 *(alldata16$v2-mean(alldata16$v2)) + meanval$V3 *(alldata16$v3-mean(alldata16$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata16$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata16$SEX  + coefvisit3$X4 * alldata16$AGE +coefvisit3$X5 * alldata16$pc11

write.table(alldata16$score,"./scoremethod1/score16method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model16")

alldata16$group0 <- alldata16$score >  coefvisit3$X1

write.table(alldata16$group0,"./scoremethod1/group16method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model16")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/16/rdpaefig")
wd<- paste0(Datasets,"/16/rdpaefig.sas7bdat")
os<- assign(filename0, read_sas(wd))


eventi<- as.numeric(os$CENSOR)
os <- unique(data.frame(USUBJID=os$RANDCODE, event=(eventi), time=os$TIMETEV))
os<- na.omit(os)

alldata16$USUBJID<- as.numeric(alldata16$USUBJID)
os$USUBJID<- as.numeric(os$USUBJID)

alldata16<- merge(os, alldata16, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata16)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata16))

res<- summary(coxph(Surv(time,event)~group0, data = alldata16))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model16", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata16) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata16, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata17<-read.csv("./cvresults/17-alldata17.csv")


alldata17$pc11 <- meanval$V2 *(alldata17$v2-mean(alldata17$v2)) + meanval$V3 *(alldata17$v3-mean(alldata17$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata17$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata17$SEX  + coefvisit3$X4 * alldata17$AGE +coefvisit3$X5 * alldata17$pc11

write.table(alldata17$score,"./scoremethod1/score17method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model17")

alldata17$group0 <- alldata17$score >  coefvisit3$X1

write.table(alldata17$group0,"./scoremethod1/group17method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model17")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/17/adtte")
wd<- paste0(Datasets,"/17/adtte.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$USUBJID, event=1-(os$CNSR) , time=os$ADY))
os<- na.omit(os)


alldata17<- unique(merge(os, alldata17, by="USUBJID"))

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata17)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata17))

res<- summary(coxph(Surv(time,event)~group0, data = alldata17))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model17", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata17) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata17, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata18<-read.csv("./cvresults/18-alldata18.csv")


alldata18$pc11 <- meanval$V2 *(alldata18$v2-mean(alldata18$v2)) + meanval$V3 *(alldata18$v3-mean(alldata18$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata18$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata18$SEX  + coefvisit3$X4 * alldata18$AGE +coefvisit3$X5 * alldata18$pc11

write.table(alldata18$score,"./scoremethod1/score18method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model18")

alldata18$group0 <- alldata18$score >  coefvisit3$X1

write.table(alldata18$group0 ,"./scoremethod1/group18method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model18")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/18/os")
wd<- paste0(Datasets,"/18/os.sas7bdat")
os1<- assign(filename0, read_sas(wd))

os <- unique(data.frame(USUBJID=os1$USUBJID, event=1-(os1$CENS), time= os1$OS_DUR))
os<- na.omit(os)


alldata18<- merge(os, alldata18, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata18)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata18))

res<- summary(coxph(Surv(time,event)~group0, data = alldata18))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model18", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata18) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata18, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata22<-read.csv("./cvresults/22-alldata22.csv")


alldata22$pc11 <- meanval$V2 *(alldata22$v2-mean(alldata22$v2)) + meanval$V3 *(alldata22$v3-mean(alldata22$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata22$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata22$SEX  + coefvisit3$X4 * alldata22$AGE +coefvisit3$X5 * alldata22$pc11

write.table(alldata22$score ,"./scoremethod1/score22method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model22")

alldata22$group0 <- alldata22$score >  coefvisit3$X1

write.table(alldata22$group0,"./scoremethod1/group22method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model22")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/22/adtte")
wd<- paste0(Datasets,"/22/adtte.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$USUBJID, event=1-(os1$CNSR), time=os1$ADY))
os<- na.omit(os)

alldata22<- merge(os, alldata22, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata22)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata22))

res<- summary(coxph(Surv(time,event)~group0, data = alldata22))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model22", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata22) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata22, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata23<-read.csv("./cvresults/23-alldata23.csv")


alldata23$pc11 <- meanval$V2 *(alldata23$v2-mean(alldata23$v2)) + meanval$V3 *(alldata23$v3-mean(alldata23$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata23$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata23$SEX  + coefvisit3$X4 * alldata23$AGE +coefvisit3$X5 * alldata23$pc11


write.table(alldata23$score,"./scoremethod1/score23method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model23")

alldata23$group0 <- alldata23$score >  coefvisit3$X1

write.table(alldata23$group0,"./scoremethod1/group23method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model23")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/23/adtte")
wd<- paste0(Datasets,"/23/adtte.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$USUBJID, event=1-(os1$CNSR), time=os1$ADY))
os<- na.omit(os)


alldata23<- merge(os, alldata23, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata23)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata23))

res<- summary(coxph(Surv(time,event)~group0, data = alldata23))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model23", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata23) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata23, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata31<-read.csv("./cvresults/31-alldata31.csv")

alldata31$pc11 <- meanval$V2 *(alldata31$v2-mean(alldata31$v2)) + meanval$V3 *(alldata31$v3-mean(alldata31$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata31$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata31$SEX  + coefvisit3$X4 * alldata31$AGE +coefvisit3$X5 * alldata31$pc11

write.table(alldata31$score,"./scoremethod1/score31method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model31")

alldata31$group0 <- alldata31$score >  coefvisit3$X1


write.table(alldata31$group0,"./scoremethod1/group31method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model31")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/31/ae")
wd<- paste0(Datasets,"/31/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=(os1$AEOUTCD), time=os1$AESTDY))
os["event"][os["event"]==5]<-0
os["event"][os["event"]==4]<-1
os["event"][os["event"]==3]<-0
os["event"][os["event"]==2]<-NA
os["event"][os["event"]==1]<-1
os["event"][os["event"]==6]<-NA
# 
os<- na.omit(os)

# 
alldata31<- merge(os, alldata31, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata31)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata31))

res<- summary(coxph(Surv(time,event)~group0, data = alldata31))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model31", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata31) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata31, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata32<-read.csv("./cvresults/32-alldata32.csv")


alldata32$pc11 <- meanval$V2 *(alldata32$v2-mean(alldata32$v2)) + meanval$V3 *(alldata32$v3-mean(alldata32$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 


alldata32$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata32$SEX  + coefvisit3$X4 * alldata32$AGE +coefvisit3$X5 * alldata32$pc11

write.table(alldata32$score ,"./scoremethod1/score32method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model32")

alldata32$group0 <- alldata32$score >  coefvisit3$X1


write.table(alldata32$group0,"./scoremethod1/group32method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model32")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/32/ae")
wd<- paste0(Datasets,"/32/ae.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$patid, event=os$AEOUT, time=os$AE_ERS))
os["event"][os["event"]==1]<- 1
os["event"][os["event"]==2]<-1
os["event"][os["event"]==3]<-1
os["event"][os["event"]==4]<- 0
os["event"][os["event"]==6]<-0
os["event"][os["event"]==7]<-0
os[os == ""] <- NA
os[os == "Unknown"] <- NA

os<- na.omit(os)
alldata32 <- merge(os, alldata32, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata32)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata32))

res<- summary(coxph(Surv(time,event)~group0, data = alldata32))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model32", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata32) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata32, conf.type="log-log")
# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata34<-read.csv("./cvresults/34-alldata34.csv")


alldata34$pc11 <- meanval$V2 *(alldata34$v2-mean(alldata34$v2)) + meanval$V3 *(alldata34$v3-mean(alldata34$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata34$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata34$SEX  + coefvisit3$X4 * alldata34$AGE +coefvisit3$X5 * alldata34$pc11


write.table(alldata34$score,"./scoremethod1/score34method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model34")

alldata34$group0 <- alldata34$score >  coefvisit3$X1


write.table(alldata34$group0,"./scoremethod1/group34method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model34")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/34/adae")
wd<- paste0(Datasets,"/34/adae.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$USUBJID, event=os$AEOUTN, time=os$VISITNUM))
os["event"][os["event"]==1]<-1
os["event"][os["event"]==4]<-0
os["event"][os["event"]==2]<- 1
os["event"][os["event"]==3]<-0
os["event"][os["event"]==5]<- 0
os["event"][os["event"]==6]<-NA
os[os == ""] <- NA
os[os == "Unknown"] <- NA

os<- na.omit(os)


alldata34<- merge(os, alldata34, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata34)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata34))

res<- summary(coxph(Surv(time,event)~group0, data = alldata34))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model11", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata34) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata34, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata35<-read.csv("./cvresults/35-alldata35.csv")


alldata35$pc11 <- meanval$V2 *(alldata35$v2-mean(alldata35$v2)) + meanval$V3 *(alldata35$v3-mean(alldata35$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# =====================================================================   
alldata35$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata35$SEX  + coefvisit3$X4 * alldata35$AGE +coefvisit3$X5 * alldata35$pc11

write.table(alldata35$score,"./scoremethod1/score35method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model35")

alldata35$group0 <- alldata35$score >  coefvisit3$X1

write.table(alldata35$group0,"./scoremethod1/group35method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "mpdel35")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/35/ds")
wd<- paste0(Datasets,"/35/ds.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$USUBJID, event=(os$DSSEQ), time=os$DSSTWK))
os["event"][os["event"]==1]<- 1
os["event"][os["event"]==2]<-1
os["event"][os["event"]==3]<-1
os["event"][os["event"]==4]<- 0
os["event"][os["event"]==5]<- 0
os["event"][os["event"]==6]<-0
os["event"][os["event"]==7]<-0
os["event"][os["event"]==8]<-NA
os["event"][os["event"]==9]<-NA
os[os == ""] <- NA
os[os == "Unknown"] <- NA

os<- na.omit(os)


alldata35<- merge(os, alldata35, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata35)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata35))

res<- summary(coxph(Surv(time,event)~group0, data = alldata35))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model11", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata35) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata35, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata37<-read.csv("./cvresults/37-alldata37.csv")


alldata37$pc11 <- meanval$V2 *(alldata37$v2-mean(alldata37$v2)) + meanval$V3 *(alldata37$v3-mean(alldata37$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata37$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata37$SEX  + coefvisit3$X4 * alldata37$AGE +coefvisit3$X5 * alldata37$pc11


write.table(alldata37$score,"./scoremethod1/score37method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model37")

alldata37$group0 <- alldata37$score >  coefvisit3$X1


write.table(alldata37$group0,"./scoremethod1/group37method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model37")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/37/survive")
wd<- paste0(Datasets,"/37/survive.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$RPT, event=os1$STATUSN, time=os1$VISDAY))
os["event"][os["event"]==1]<- 1
os["event"][os["event"]==2]<-0
os["event"][os["event"]==3]<-0
os["event"][os["event"]==""]<-NA
os<- na.omit(os)
os$USUBJID<- as.numeric(os$USUBJID)

alldata37<- merge(os, alldata37, by="USUBJID")
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata37)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata37))

res<- summary(coxph(Surv(time,event)~group0, data = alldata37))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model37", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata37) %>% 
                 gtsummary::tbl_regression(exp = TRUE))


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata37, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata41<-read.csv("./cvresults/41-alldata41.csv")

alldata41$pc11 <- meanval$V2 *(alldata41$v2-mean(alldata41$v2)) + meanval$V3 *(alldata41$v3-mean(alldata41$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata41$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata41$SEX  + coefvisit3$X4 * alldata41$AGE +coefvisit3$X5 * alldata41$pc11

write.table(alldata41$score,"./scoremethod1/score41method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model41")

alldata41$group0 <- alldata41$score >  coefvisit3$X1


write.table(alldata41$group0,"./scoremethod1/group41method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model41")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/41/a_eendpt")
wd<- paste0(Datasets,"/41/a_eendpt.sas7bdat")
os<- assign(filename0, read_sas(wd))

os <- unique(data.frame(USUBJID=os$SUBJID, event=1-(os$DTH30) , time=os$DTHDY30 ))
os<- na.omit(os)


alldata41<- merge(os, alldata41, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata41)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata41))

res<- summary(coxph(Surv(time,event)~group0, data = alldata41))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model41", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata41) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata41, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata42<-read.csv("./cvresults/42-alldata42.csv")

alldata42$pc11 <- meanval$V2 *(alldata42$v2-mean(alldata42$v2)) + meanval$V3 *(alldata42$v3-mean(alldata42$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# =====================================================================

alldata42$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata42$SEX  + coefvisit3$X4 * alldata42$AGE +coefvisit3$X5 * alldata42$pc11

write.table(alldata42$score,"./scoremethod1/score42method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model42")

alldata42$group0 <- alldata42$score >  coefvisit3$X1

write.table(alldata42$group0,"./scoremethod1/group42method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model42")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/42/a_endpt")
wd<- paste0(Datasets,"/42/a_endpt.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$SUBJID, event=os$PFSCR, time=os$PFSDYCR))
os<- na.omit(os)


alldata42<- merge(os, alldata42, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata42)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata42))

res<- summary(coxph(Surv(time,event)~group0, data = alldata42))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model42", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata42) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata42, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata53<-read.csv("./cvresults/53-alldata53.csv")

alldata53$pc11 <- meanval$V2 *(alldata53$v2-mean(alldata53$v2)) + meanval$V3 *(alldata53$v3-mean(alldata53$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata53$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata53$SEX  + coefvisit3$X4 * alldata53$AGE +coefvisit3$X5 * alldata53$pc11

write.table(alldata53$score,"./scoremethod1/score53method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model53")

alldata53$group0 <- alldata53$score >  coefvisit3$X1

write.table(alldata53$group0,"./scoremethod1/group53method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model53")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/53/adsl_pds2019")
wd<- paste0(Datasets,"/53/adsl_pds2019.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=as.numeric(os1$SUBJID), event=os1$PFSCR, time=os1$PFSDYCR))
os<- na.omit(os)


alldata53<- merge(os, alldata53, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata53)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata53))

res<- summary(coxph(Surv(time,event)~group0, data = alldata53))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model53", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata53) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata53, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata54<-read.csv("./cvresults/54-alldata54.csv")


alldata54$pc11 <- meanval$V2 *(alldata54$v2-mean(alldata54$v2)) + meanval$V3 *(alldata54$v3-mean(alldata54$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata54$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata54$SEX  + coefvisit3$X4 * alldata54$AGE +coefvisit3$X5 * alldata54$pc11

write.table(alldata54$score ,"./scoremethod1/score54method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model54")

alldata54$group0 <- alldata54$score >  coefvisit3$X1

write.table(alldata54$group0,"./scoremethod1/group54method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model54")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/54/vdt")
wd<- paste0(Datasets,"/54/vdt.sas7bdat")
os11<- assign(filename0, read_sas(wd))

filename0=paste0(Datasets,"/54/srv")
wd<- paste0(Datasets,"/54/srv.sas7bdat")
os111<- assign(filename0, read_sas(wd))

os1<- merge(os11, os111, by= "PT")

os <- unique(data.frame(USUBJID=os1$PT, event=os1$SRVSTS, time=os1$VISITNUM.x))
os["event"][os["event"]=="ALIVE"]<- "0"
os["event"][os["event"]=="DECEASED"]<- "1"
os["event"][os["event"]=="UNKNOWN"]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)

alldata54<- merge(os, alldata54, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata54)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata54))

res<- summary(coxph(Surv(time,event)~group0, data = alldata54))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model54", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata54) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata54, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata55<-read.csv("./cvresults/55-alldata55.csv")

alldata55$pc11 <- meanval$V2 *(alldata55$v2-mean(alldata55$v2)) + meanval$V3 *(alldata55$v3-mean(alldata55$v3))

# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata55$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata55$SEX  + coefvisit3$X4 * alldata55$AGE +coefvisit3$X5 * alldata55$pc11

write.table(alldata55$score ,"./scoremethod1/score55method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model55")

alldata55$group0 <- alldata55$score >  coefvisit3$X1

write.table(alldata55$group0 ,"./scoremethod1/group55method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model55")

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/55/uae")
wd<- paste0(Datasets,"/55/uae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=(os1$AEOUT), time=os1$CYCLE))
os["event"][os["event"]== "DEATH"]<- "0"
os["event"][os["event"]== "RESOLVED"]<- "1"
os["event"][os["event"]== "ONGOING"]<- "1"
os["event"][os["event"]== ""]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata55<- merge(os, alldata55, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata55)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata55))

res<- summary(coxph(Surv(time,event)~group0, data = alldata55))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model55", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata55) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata55, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
alldata59<-read.csv("./cvresults/59-alldata59.csv")

alldata59$pc11 <- meanval$V2 *(alldata59$v2-mean(alldata59$v2)) + meanval$V3 *(alldata59$v3-mean(alldata59$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata59$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata59$SEX  + coefvisit3$X4 * alldata59$AGE +coefvisit3$X5 * alldata59$pc11

write.table(alldata59$score,"./scoremethod1/score59method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model59")

alldata59$group0 <- alldata59$score >  coefvisit3$X1

write.table(alldata59$group0,"./scoremethod1/group59method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model59")
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/59/adtte")
wd<- paste0(Datasets,"/59/adtte.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$USUBJID, event=(os1$CNSR), time=os1$ADY))
os<- na.omit(os)

alldata59<- merge(os, alldata59, by="USUBJID")

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time, event) ~ group0, data = alldata59)


# Hazard ratio
Hazardratio<- (coxph(Surv(time,event)~group0, data = alldata59))

res<- summary(coxph(Surv(time,event)~group0, data = alldata59))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model59", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata59) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata59, conf.type="log-log")

# ===================================================================================================================================================
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 
alldata58<-read.csv("./cvresults/58-alldata58.csv")

alldata58$pc11 <- meanval$V2 *(alldata58$v2-mean(alldata58$v2)) + meanval$V3 *(alldata58$v3-mean(alldata58$v3))
# ===================================================================== 
# Merge scores of variables and do segmentation 
# ===================================================================== 

alldata58$score <-  coefvisit3$X2 + coefvisit3$X3 * alldata58$SEX  + coefvisit3$X4 * alldata58$AGE +coefvisit3$X5 * alldata58$pc11

write.table(alldata58$score,"./scoremethod1/score58method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model58")

alldata58$group0 <- alldata58$score >  coefvisit3$X1

write.table(alldata58$group0,"./scoremethod1/group58method1.csv", append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = "model58")
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/58/ae")
wd<- paste0(Datasets,"/58/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))

os <- unique(data.frame(USUBJID=os1$USUBJID, event=os1$AEOUT, time=os1$AESTDY))
os["event"][os["event"]=="FATAL"]<- "0"
os["event"][os["event"]=="NOT RECOVERED/NOT RESOLVED"]<- "0"
os["event"][os["event"]=="RECOVERED/RESOLVED"]<- "1"
os["event"][os["event"]=="RECOVERED/RESOLVED WITH SEQUELAE"]<- "1"
os["event"][os["event"]=="UNKNOWN"]<- NA
os["event"][os["event"]==""]<- NA
os$USUBJID<- as.numeric(os$USUBJID)
os$event0<- as.numeric(os$event)
os$time0<- as.numeric(os$time)
os<- na.omit(os)

print(os)

alldata58<- merge(os, alldata58, by="USUBJID")
print(alldata58)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
fit <- survfit(Surv(time0, event0) ~ group0, data = alldata58)


# Hazard ratio
Hazardratio<- (coxph(Surv(time0,event0)~group0, data = alldata58))

res<- summary(coxph(Surv(time0,event0)~group0, data = alldata58))

C.index<-res$concordance
N<- res$nevent
p.value<- res$waldtest["pvalue"]
HR<- res$conf.int
coxresults<- cbind( C.index, HR, p.value)

write.table(coxresults, file="./Osmethod1/coxmethod1.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model58", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time0, event0) ~ group0, data = alldata58) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time0,event0)~group0, data = alldata58, conf.type="log-log")

# Kaplan Meier curves
ggsurvplot(
  ff,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "TIME",   # customize X axis label.
  ggtheme = theme_light(),
  surv.median.line = "hv",# customize plot and risk table with a theme.
  legend.main="  ",
  legend.lab= c("Bad group", "Good group"),
  legend.lab.location= "bottomleft",
  palette = 
    c("#FF0066", "#AF6FDF") # custom color palettes.
)

survp58 <- ggsurvplot(ff, risk.table = FALSE, palette = 
                        c("#FF0066", "#AF6FDF"))
ggsave(file = "./Figures/surplotm1-model58.png", survp58$plot)


surv58<-ggsurvplot(ff, data = alldata58,
 legend.title = "Group",
 legend.labs = c("Bad group", "Good group"),
 pval = TRUE,
 risk.table = TRUE,
 cumcensor = TRUE,
 xlab = "TIME",
 palette = c("#FF0066", "#AF6FDF"),
 ggtheme = theme_bw() # Change ggplot2 theme
)

ggsave(file = "./Figures/surplotm1-risk-censor-model58.png", surv58$plot)

summary(ff)
# ===============================================================================
