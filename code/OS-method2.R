# load package
library(dplyr)
require(haven)
library(survival)
library(ggpubr)
library(survminer)
library(lubridate)
library(caret)
library(reshape2)
library(Matrix)
library(glmnet)

Datasets= "./Datasets" 
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group6<- read.csv("./finalresultmethod2/groupfmodel6.csv")
alldata6<-read.csv("./cvresults/6-alldata.csv")
alldata6<- data.frame(USUBJID=alldata6$USUBJID, group0=group6)
alldata6<- unique(alldata6)
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
names(alldata6)[names(alldata6) == "group0.model6"] <- "group0"
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

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model6", col.names = TRUE)

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
group7<- read.csv("./finalresultmethod2/groupfmodel7.csv")

alldata7<-read.csv("./cvresults/7-alldata.csv")

alldata7<- data.frame(USUBJID=alldata7$USUBJID, group0=group7)
alldata7<- unique(alldata7)

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

os$event<- as.numeric(os$event)

os<- na.omit(os)


alldata7<- merge(os, alldata7, by="USUBJID")
names(alldata7)[names(alldata7) == "group0.model7"] <- "group0"
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
coxresults<- cbind(C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model7", col.names = FALSE)

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
group8<- read.csv("./finalresultmethod2/groupfmodel8.csv")

alldata8<-read.csv("./cvresults/8-alldata.csv")

alldata8<- data.frame(USUBJID=alldata8$USUBJID, group0=group8)
alldata8<- unique(alldata8)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/8/ae")
wd<- paste0(Datasets,"/8/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=os1$AEOUT, time=os1$VISITNUM))
os["event"][os["event"]=="FATAL"]<- "0"
os["event"][os["event"]=="NOT RECOVERED/NOT RESOLVED"]<- "0"
os["event"][os["event"]=="RECOVERED/RESOLVED"]<- "1"
os["event"][os["event"]=="RECOVERING/RESOLVING"]<- "1"
os["event"][os["event"]=="RECOVERED/RESOLVED WITH SEQUELAE"]<- "1"
os["event"][os["event"]=="UNKNOWN"]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata8<- merge(os, alldata8, by="USUBJID")
names(alldata8)[names(alldata8) == "group0.model8"] <- "group0"
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

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model8", col.names = FALSE)

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
group9<- read.csv("./finalresultmethod2/groupfmodel9.csv")

alldata9<-read.csv("./cvresults/9-alldata.csv")

alldata9<- data.frame(USUBJID=alldata9$USUBJID, group0=group9)
alldata9<- unique(alldata9)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/9/ae")
wd<- paste0(Datasets,"/9/ae.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os1$duration<- (os1$AEENDY-os1$AESTDY)
os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=(os1$AEOUT), time=os1$AESTDY))
os["event"][os["event"]=="FATAL"]<- "0"
os["event"][os["event"]== "NOT.RECOVERED/NOT.RESOLVED"]<- NA
os["event"][os["event"]== "RECOVERED/RESOLVED"]<- "1"
os["event"][os["event"]== "RECOVERING/RESOLVING"]<- NA
os["event"][os["event"]== "."]<- NA
os["event"][os["event"]=="UNKNOWN"]<- "1"
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata9<- merge(os, alldata9, by="USUBJID")
names(alldata9)[names(alldata9) == "group0.model9"] <- "group0"
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

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model9", col.names = FALSE)

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
group11<- read.csv("./finalresultmethod2/groupfmodel11.csv")

alldata11<-read.csv("./cvresults/11-alldata.csv")

alldata11<- data.frame(USUBJID=alldata11$USUBJID, group0=group11)
alldata11<- unique(alldata11)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/11/a_eendpt")
wd<- paste0(Datasets,"/11/a_eendpt.sas7bdat")
os<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os$SUBJID, event=1-(os$PFSCRS), time=os$PFSDYCRS))
os<- na.omit(os)


alldata11<- merge(os, alldata11, by="USUBJID")
names(alldata11)[names(alldata11) == "group0.model11"] <- "group0"
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

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model11", col.names = FALSE)

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
group12<- read.csv("./finalresultmethod2/groupfmodel12.csv")

alldata12<-read.csv("./cvresults/12-alldata.csv")

alldata12<- data.frame(USUBJID=alldata12$USUBJID, group0=group12)
alldata12<- unique(alldata12)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/12/aeendpt")
wd<- paste0(Datasets,"/12/aeendpt.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$SUBJID, event=1-(os1$PFSLRE), time=os1$PFSDYLRE))
os<- na.omit(os)

alldata12<- merge(os, alldata12, by="USUBJID")
names(alldata12)[names(alldata12) == "group0.model12"] <- "group0"
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

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model12", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata12) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group16<- read.csv("./finalresultmethod2/groupfmodel16.csv")

alldata16<-read.csv("./cvresults/16-alldata.csv")

alldata16<- data.frame(USUBJID=alldata16$USUBJID, group0=group16)
alldata16<- unique(alldata16)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/16/rdpaefig")
wd<- paste0(Datasets,"/16/rdpaefig.sas7bdat")
os<- assign(filename0, read_sas(wd))


eventi<- as.numeric(os$CENSOR)
os <- unique(data.frame(USUBJID=os$RANDCODE, event=(eventi), time=os$TIMETEV))
os<- na.omit(os)


alldata16<- merge(os, alldata16, by="USUBJID")
names(alldata16)[names(alldata16) == "group0.model16"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model16", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata16) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group17<- read.csv("./finalresultmethod2/groupfmodel17.csv")

alldata17<-read.csv("./cvresults/17-alldata.csv")

alldata17<- data.frame(USUBJID=alldata17$USUBJID, group0=group17)
alldata17<- unique(alldata17)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/17/adtte")
wd<- paste0(Datasets,"/17/adtte.sas7bdat")
os<- assign(filename0, read_sas(wd))

os <- unique(data.frame(USUBJID=os$USUBJID, event=1-(os$CNSR) , time=os$ADY))
os<- na.omit(os)


alldata17<- unique(merge(os, alldata17, by="USUBJID"))
names(alldata17)[names(alldata17) == "group0.model17"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model17", col.names = FALSE)

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
group18<- read.csv("./finalresultmethod2/groupfmodel18.csv")

alldata18<-read.csv("./cvresults/18-alldata.csv")

alldata18<- data.frame(USUBJID=alldata18$USUBJID, group0=group18)
alldata18<- unique(alldata18)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/18/os")
wd<- paste0(Datasets,"/18/os.sas7bdat")
os1<- assign(filename0, read_sas(wd))

os <- unique(data.frame(USUBJID=os1$USUBJID, event=1-(os1$CENS), time= os1$OS_DUR))
os<- na.omit(os)


alldata18<- merge(os, alldata18, by="USUBJID")
names(alldata18)[names(alldata18) == "group0.model18"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model18", col.names = FALSE)

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
group22<- read.csv("./finalresultmethod2/groupfmodel22.csv")

alldata22<-read.csv("./cvresults/22-alldata.csv")

alldata22<- data.frame(USUBJID=alldata22$USUBJID, group0=group22)
alldata22<- unique(alldata22)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
filename0=paste0(Datasets,"/22/adtte")
wd<- paste0(Datasets,"/22/adtte.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$USUBJID, event=(os1$CNSR), time=os1$ADY))
os<- na.omit(os)

alldata22<- merge(os, alldata22, by="USUBJID")
names(alldata22)[names(alldata22) == "group0.model22"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model22", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata22) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group23<- read.csv("./finalresultmethod2/groupfmodel23.csv")

alldata23<-read.csv("./cvresults/23-alldata.csv")

alldata23<- data.frame(USUBJID=alldata23$USUBJID, group0=group23)
alldata23<- unique(alldata23)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
filename0=paste0(Datasets,"/23/adtte")
wd<- paste0(Datasets,"/23/adtte.sas7bdat")
os1<- assign(filename0, read_sas(wd))


os <- unique(data.frame(USUBJID=os1$USUBJID, event=1-(os1$CNSR), time=os1$ADY))
os<- na.omit(os)


alldata23<- merge(os, alldata23, by="USUBJID")
names(alldata23)[names(alldata23) == "group0.model23"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model23", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata23) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group31<- read.csv("./finalresultmethod2/groupfmodel31.csv")

alldata31<-read.csv("./cvresults/31-alldata.csv")

alldata31<- data.frame(USUBJID=alldata31$USUBJID, group0=group31)
alldata31<- unique(alldata31)

# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
os1<-read_sas("./Datasets/31/ae.sas7bdat")


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=(os1$AEOUTCD), time=os1$AESTDY))
os["event"][os["event"]==5]<-0
os["event"][os["event"]==4]<-1
os["event"][os["event"]==3]<-0
os["event"][os["event"]==2]<-1
os["event"][os["event"]==1]<-1
os["event"][os["event"]==6]<-NA
# 
os<- na.omit(os)

# 
alldata31<- merge(os, alldata31, by="USUBJID")
names(alldata31)[names(alldata31) == "group0.model31"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model31", col.names = FALSE)

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
group32<- read.csv("./finalresultmethod2/groupfmodel32.csv")

alldata32<-read.csv("./cvresults/32-alldata.csv")

alldata32<- data.frame(USUBJID=alldata32$USUBJID, group0=group32)
alldata32<- unique(alldata32)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
os<-read_sas("./Datasets/32/ae.sas7bdat")


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
names(alldata32)[names(alldata32) == "group0.model32"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model32", col.names = FALSE)

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
group34<- read.csv("./finalresultmethod2/groupfmodel34.csv")

alldata34<-read.csv("./cvresults/34-alldata.csv")

alldata34<- data.frame(USUBJID=alldata34$USUBJID, group0=group34)
alldata34<- unique(alldata34)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
os<-read_sas("./Datasets/34/adae.sas7bdat") 


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
names(alldata34)[names(alldata34) == "group0.model34"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model34", col.names = FALSE)

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
group35<- read.csv("./finalresultmethod2/groupfmodel35.csv")

alldata35<-read.csv("./cvresults/35-alldata.csv")

alldata35<- data.frame(USUBJID=alldata35$USUBJID, group0=group35)
alldata35<- unique(alldata35)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
os<-read_sas("./Datasets/35/ds.sas7bdat") 


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
names(alldata35)[names(alldata35) == "group0.model35"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model35", col.names = FALSE)

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
group37<- read.csv("./finalresultmethod2/groupfmodel37.csv")

alldata37<-read.csv("./cvresults/37-alldata.csv")

alldata37<- data.frame(USUBJID=alldata37$USUBJID, group0=group37)
alldata37<- unique(alldata37)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
os1<-read_sas("./Datasets/37/survive.sas7bdat") 


os <- unique(data.frame(USUBJID=os1$RPT, event=os1$STATUSN, time=os1$VISDAY))
os["event"][os["event"]==1]<- 1
os["event"][os["event"]==2]<-1
os["event"][os["event"]==3]<-0
os["event"][os["event"]==""]<-NA
os<- na.omit(os)
os$USUBJID<- as.numeric(os$USUBJID)

alldata37<- merge(os, alldata37, by="USUBJID")
names(alldata37)[names(alldata37) == "group0.model37"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model37", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata37) %>% 
                 gtsummary::tbl_regression(exp = TRUE))

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group41<- read.csv("./finalresultmethod2/groupfmodel41.csv")

alldata41<-read.csv("./cvresults/41-alldata.csv")

alldata41<- data.frame(USUBJID=alldata41$USUBJID, group0=group41)
alldata41<- unique(alldata41)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 
os<-read_sas("./Datasets/41/a_eendpt.sas7bdat") 


os <- unique(data.frame(USUBJID=os$SUBJID, event=1-(os$DTH30) , time=os$DTHDY30 ))
os<- na.omit(os)


alldata41<- merge(os, alldata41, by="USUBJID")
names(alldata41)[names(alldata41) == "group0.model41"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model41", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata41) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group42<- read.csv("./finalresultmethod2/groupfmodel42.csv")

alldata42<-read.csv("./cvresults/42-alldata.csv")

alldata42<- data.frame(USUBJID=alldata42$USUBJID, group0=group42)
alldata42<- unique(alldata42)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
os<-read_sas("./Datasets/42/a_endpt.sas7bdat")


os <- unique(data.frame(USUBJID=os$SUBJID, event=os$PFSCR, time=os$PFSDYCR))
os<- na.omit(os)


alldata42<- merge(os, alldata42, by="USUBJID")
names(alldata42)[names(alldata42) == "group0.model42"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model42", col.names = FALSE)

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
group53<- read.csv("./finalresultmethod2/groupfmodel53.csv")

alldata53<-read.csv("./cvresults/53-alldata.csv")

alldata53<- data.frame(USUBJID=alldata53$USUBJID, group0=group53)
alldata53<- unique(alldata53)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
os1<-read_sas("./Datasets/53/adsl_pds2019.sas7bdat")


os <- unique(data.frame(USUBJID=as.numeric(os1$SUBJID), event=os1$PFSCR, time=os1$PFSDYCR))
os<- na.omit(os)


alldata53<- merge(os, alldata53, by="USUBJID")
names(alldata53)[names(alldata53) == "group0.model53"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model53", col.names = FALSE)

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
group54<- read.csv("./finalresultmethod2/groupfmodel54.csv")

alldata54<-read.csv("./cvresults/54-alldata.csv")

alldata54<- data.frame(USUBJID=alldata54$USUBJID, group0=group54)
alldata54<- unique(alldata54)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
os11<-read_sas("./Datasets/54/vdt.sas7bdat")
os111<-read_sas("./Datasets/54/srv.sas7bdat")
os1<- merge(os11, os111, by= "PT")

os <- unique(data.frame(USUBJID=os1$PT, event=os1$SRVSTS, time=os1$VISITNUM.x))
os["event"][os["event"]=="ALIVE"]<- "0"
os["event"][os["event"]=="DECEASED"]<- "1"
os["event"][os["event"]=="UNKNOWN"]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)

alldata54<- merge(os, alldata54, by="USUBJID")
names(alldata54)[names(alldata54) == "group0.model54"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model54", col.names = FALSE)

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
group55<- read.csv("./finalresultmethod2/groupfmodel55.csv")

alldata55<-read.csv("./cvresults/55-alldata.csv")

alldata55<- data.frame(USUBJID=alldata55$USUBJID, group0=group55)
alldata55<- unique(alldata55)
# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
os1<-read_sas("./Datasets/55/uae.sas7bdat")


os <- unique(data.frame(USUBJID=os1$RUSUBJID, event=(os1$AEOUT), time=os1$CYCLE))
os["event"][os["event"]== "DEATH"]<- "0"
os["event"][os["event"]== "RESOLVED"]<- "1"
os["event"][os["event"]== "ONGOING"]<- "1"
os["event"][os["event"]== ""]<- NA
os$event<- as.numeric(os$event)
os<- na.omit(os)


alldata55<- merge(os, alldata55, by="USUBJID")
names(alldata55)[names(alldata55) == "group0.model55"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model55", col.names = FALSE)

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
# ======================================================================
group59<- read.csv("./finalresultmethod2/groupfmodel59.csv")

alldata59<-read.csv("./cvresults/59-alldata.csv")

alldata59<- data.frame(USUBJID=alldata59$USUBJID, group0=group59)
alldata59<- unique(alldata59)

# # =====================================================================
# # Differences between overall Survival (bad and good group)
# # ===================================================================== 
os1<-read_sas("./Datasets/59/adtte.sas7bdat")


os <- unique(data.frame(USUBJID=os1$USUBJID, event=(os1$CNSR), time=os1$ADY))
os<- na.omit(os)


alldata59<- merge(os, alldata59, by="USUBJID")
names(alldata59)[names(alldata59) == "group0.model59"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model59", col.names = FALSE)

# counts of sample size in each group
# show p-value of log-rank test.
CoxphResult<- (coxph(Surv(time, event) ~ group0, data = alldata59) %>% 
                 gtsummary::tbl_regression(exp = TRUE) )


# Median survival
ff <- survfit(Surv(time,event)~group0, data = alldata59, conf.type="log-log")

# ===================================================================================================================================================
# ======================================================================
# PCA MODEL with mean 
# ======================================================================
group58<- read.csv("./finalresultmethod2/groupfmodel58.csv")

alldata58<-read.csv("./cvresults/58-alldata58.csv")

alldata58<- data.frame(USUBJID=alldata58$USUBJID, group0=group58)
alldata58<- unique(alldata58)
# =====================================================================
# Differences between overall Survival (bad and good group)
# ===================================================================== 

os1<-read_sas("./Datasets/58/ae.sas7bdat")

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


alldata58<- merge(os, alldata58, by="USUBJID")
names(alldata58)[names(alldata58) == "group0.model58"] <- "group0"
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
coxresults<- cbind( C.index, HR,  p.value)

write.table(coxresults, file="./Osmethod2/coxmethod2.csv", append = TRUE, quote = FALSE, sep = "      ", row.names = "model58", col.names = FALSE)

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
  # legend.main="  ",
  legend.lab= c("Bad group", "Good group"),
  legend.lab.location= "bottomleft",
  palette = 
    c("#FF0066", "#AF6FDF") # custom color palettes.
)

survp58 <- ggsurvplot(ff, risk.table = FALSE, palette = 
                        c("#FF0066", "#AF6FDF"))
ggsave(file = "./Figures/surplotm2-model58.png", survp58$plot)


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

ggsave(file = "./Figures/surplotm2-risk-censor-model58.png", surv58$plot)

summary(ff)
# ===================================================================================================================================================

