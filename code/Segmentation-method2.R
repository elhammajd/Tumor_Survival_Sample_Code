# =====================================================================================================================
#    DESCRIPTION: 
#    These codes segment patients into a bad and good group
# =====================================================================================================================
#    INPUTS:
#    - Prediction results in each data set
#    - Weights got from Elastic net model
# =====================================================================================================================
#    OUTPUTS:
#    - Score of each patient
#    - Segmentation (bad and good group)
# =====================================================================================================================
#    Codes produced by: 
#    - Elham Majd
# =====================================================================================================================
#    HISTORY:
#    - Creation: Feb/2022
# =====================================================================================================================
#   STATEMENT:
#     This file is part of a project entitled "Predicting Tumor Response and 
#     Overall Survival Using Early Outcomes and Baseline Characteristics"
# =====================================================================================================================
# Libraries 
# =====================================================================================================================
# install.packages("fastmatch",repos = "http://cran.us.r-project.org")

# load package
library(purrr)
library(dplyr)
require(fastmatch)
library(caret)
#library(fs)

filelist <- list.files("./read/")
basename(filelist)

list <-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
for (j in list){
  
  filename0=paste0("./read/resultmodel",j,"")
  filenames <- list.files(filename0, full.names = TRUE)
  ldfj<-lapply(filenames,read.table)
  ldfj<- as.data.frame(ldfj)
  scoremethod2j<- ldfj[!duplicated(as.list(ldfj))]
  filename2=paste("./scoremethod2/scoremodel",j,".csv",sep="")
  write.table(scoremethod2j, filename2,  append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = FALSE)
}

# ======================================================================================
predictions6<- read.csv("./Resultmethod2/predictions6.csv")
predictions7<- read.csv("./Resultmethod2/predictions7.csv")
predictions8<- read.csv("./Resultmethod2/predictions8.csv")
predictions9<- read.csv("./Resultmethod2/predictions9.csv")
predictions11<- read.csv("./Resultmethod2/predictions11.csv")
predictions12<- read.csv("./Resultmethod2/predictions12.csv")
predictions16<- read.csv("./Resultmethod2/predictions16.csv")
predictions17<- read.csv("./Resultmethod2/predictions17.csv")
predictions18<- read.csv("./Resultmethod2/predictions18.csv")
predictions22<- read.csv("./Resultmethod2/predictions22.csv")
predictions23<- read.csv("./Resultmethod2/predictions23.csv")
predictions31<- read.csv("./Resultmethod2/predictions31.csv")
predictions32<- read.csv("./Resultmethod2/predictions32.csv")
predictions34<- read.csv("./Resultmethod2/predictions34.csv")
predictions35<- read.csv("./Resultmethod2/predictions35.csv")
predictions37<- read.csv("./Resultmethod2/predictions37.csv")
predictions41<- read.csv("./Resultmethod2/predictions41.csv")
predictions42<- read.csv("./Resultmethod2/predictions42.csv")
predictions53<- read.csv("./Resultmethod2/predictions53.csv")
predictions54<- read.csv("./Resultmethod2/predictions54.csv")
predictions55<- read.csv("./Resultmethod2/predictions55.csv")
predictions59<- read.csv("./Resultmethod2/predictions59.csv")
predictions58<- read.csv("./Resultmethod2/predictions58.csv")
# ======================================================================================
weights<- read.csv("./method2results/Weightsf.csv")
weights<- as.data.frame(weights)
weights<- na.omit(weights)
colnames(weights)<- c("model", "value")
meanweights<-aggregate(value ~ model, weights, mean)
write.csv( meanweights, "./method2results/Weightsfinal.csv")
W<- data.frame(t(meanweights))
W<- data.frame(W)
colnames(W)<- W[1,]
rownames(W)<- NULL
W<- na.omit(W)
W<- W[-1,]
colnames(W)<-gsub("model","",(colnames(W)))
w<- W[order(as.numeric(colnames(W)))]
write.csv( W, "./method2results/Weightsfinal2.csv")

intercepts<- read.csv("./method2results/Interceptf.csv")
intercepts<- as.data.frame(intercepts)
intercepts<- na.omit(intercepts)
colnames(intercepts)<- c("model", "value")
meanintercepts<-aggregate(value ~ model, intercepts, mean)
write.csv( meanintercepts, "./method2results/intercepts.csv")
intercept<- data.frame(t(meanintercepts))
intercept<- data.frame(intercept)
colnames(intercept)<- intercept[1,]
rownames(intercept)<- NULL
intercept<- na.omit(intercept)
intercept<- intercept[-1,]
colnames(intercept)<-gsub("model","",(colnames(intercept)))
intercept<- intercept[order(as.numeric(colnames(intercept)))]
write.csv( intercept, "./method2results/intercept2.csv")


list <-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
for (i in list){
  
  filename0=paste0("./scoremethod2/scoremodel",i,"")
  wd<- paste0("./scoremethod2/scoremodel",i,".csv")
  scorej<- assign(filename0, read.table(wd))

score<- as.data.frame(scorej)
score
score <- unname(score)
score<-score[!sapply(score,is.null)]

for (i in list){
  i != j
  modeli<- paste("model",i,sep="")
  for (h in 1:length(score)){
   scorei<- NULL
    if(modeli %in% score[1, ])
    {
      scorei<- score[-1,h]
      filename=paste("./finalmethod2/score",i,".csv",sep="")
      write.table(scorei,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = FALSE)
    }
   }
 }

  score<- NULL
  for  (i in list)
    for (i in colnames(W[-1]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal6<- colSums(score)
  write.csv(Scorefinal6, "./finalresultmethod2/scorefmodel6.csv")
  
  normalizeScorefinal6<-  (Scorefinal6 - min(Scorefinal6)) / (max(Scorefinal6) - min(Scorefinal6))
  group6 <- normalizeScorefinal6> predictions6
  write.csv(group6, "./finalresultmethod2/groupfmodel6.csv")

  score<- NULL
  for  (i in colnames(W[-2]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal7<- colSums(score)
  write.csv(Scorefinal7, "./finalresultmethod2/scorefmodel7.csv")
  
  normalizeScorefinal7<-  (Scorefinal7 - min(Scorefinal7)) / (max(Scorefinal7) - min(Scorefinal7))
  group7 <- normalizeScorefinal7> predictions7
  write.csv(group7, "./finalresultmethod2/groupfmodel7.csv")
  
  score<- NULL
  for  (i in colnames(W[-3]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal8<- colSums(score)
  write.csv(Scorefinal8, "./finalresultmethod2/scorefmodel8.csv")
  
  normalizeScorefinal8<-  (Scorefinal8 - min(Scorefinal8)) / (max(Scorefinal8) - min(Scorefinal8))
  group8 <- normalizeScorefinal8> predictions8
  write.csv(group8, "./finalresultmethod2/groupfmodel8.csv")
  
  score<- NULL
  for  (i in colnames(W[-4]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal9<- colSums(score)
  write.csv(Scorefinal9, "./finalresultmethod2/scorefmodel9.csv")
  
  normalizeScorefinal9<-  (Scorefinal9 - min(Scorefinal9)) / (max(Scorefinal9) - min(Scorefinal9))
  group9 <- normalizeScorefinal9> predictions9
  write.csv(group9, "./finalresultmethod2/groupfmodel9.csv")
  
  score<- NULL
  for  (i in colnames(W[-5]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal11<- colSums(score)
  write.csv(Scorefinal11, "./finalresultmethod2/scorefmodel11.csv")
  
  normalizeScorefinal11<-  (Scorefinal11 - min(Scorefinal11)) / (max(Scorefinal11) - min(Scorefinal11))
  group11 <- normalizeScorefinal11> predictions11
  write.csv(group11, "./finalresultmethod2/groupfmodel11.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-6]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal12<- colSums(score)
  write.csv(Scorefinal12, "./finalresultmethod2/scorefmodel12.csv")
  
  normalizeScorefinal12<-  (Scorefinal12 - min(Scorefinal12)) / (max(Scorefinal12) - min(Scorefinal12))
  group12 <- normalizeScorefinal12> predictions12
  write.csv(group12, "./finalresultmethod2/groupfmodel12.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-7]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal16<- colSums(score)
  write.csv(Scorefinal16, "./finalresultmethod2/scorefmodel16.csv")
  
  normalizeScorefinal16<-  (Scorefinal16 - min(Scorefinal16)) / (max(Scorefinal16) - min(Scorefinal16))
  group16 <- normalizeScorefinal16> predictions16
  write.csv(group16, "./finalresultmethod2/groupfmodel16.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-8]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal17<- colSums(score)
  write.csv(Scorefinal17, "./finalresultmethod2/scorefmodel17.csv")
  
  normalizeScorefinal17<-  (Scorefinal17 - min(Scorefinal17)) / (max(Scorefinal17) - min(Scorefinal17))
  group17 <- normalizeScorefinal17> predictions17
  write.csv(group17, "./finalresultmethod2/groupfmodel17.csv")
  
  score<- NULL
  for  (i in colnames(W[-9]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal18<- colSums(score)
  write.csv(Scorefinal18, "./finalresultmethod2/scorefmodel18.csv")
  
  normalizeScorefinal18<-  (Scorefinal18 - min(Scorefinal18)) / (max(Scorefinal18) - min(Scorefinal18))
  group18 <- normalizeScorefinal18> predictions18
  write.csv(group18, "./finalresultmethod2/groupfmodel18.csv")
  
  score<- NULL
  for  (i in colnames(W[-10]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal22<- colSums(score)
  write.csv(Scorefinal22, "./finalresultmethod2/scorefmodel22.csv")
  
  normalizeScorefinal22<-  (Scorefinal22 - min(Scorefinal22)) / (max(Scorefinal22) - min(Scorefinal22))
  group22 <- normalizeScorefinal22> predictions22
  write.csv(group22, "./finalresultmethod2/groupfmodel22.csv")
  
  score<- NULL
  for  (i in colnames(W[-11]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal23<- colSums(score)
  write.csv(Scorefinal23, "./finalresultmethod2/scorefmodel23.csv")
  
  normalizeScorefinal23<-  (Scorefinal23 - min(Scorefinal23)) / (max(Scorefinal23) - min(Scorefinal23))
  group23 <- normalizeScorefinal23> predictions23
  write.csv(group23, "./finalresultmethod2/groupfmodel23.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-12]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal31<- colSums(score)
  write.csv(Scorefinal31, "./finalresultmethod2/scorefmodel31.csv")
  
  normalizeScorefinal31<-  (Scorefinal31 - min(Scorefinal31)) / (max(Scorefinal31) - min(Scorefinal31))
  group31 <- normalizeScorefinal31> predictions31
  write.csv(group31, "./finalresultmethod2/groupfmodel31.csv")
  
  score<- NULL
  for  (i in colnames(W[-13]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal32<- colSums(score)
  write.csv(Scorefinal32, "./finalresultmethod2/scorefmodel32.csv")
  
  normalizeScorefinal32<-  (Scorefinal32 - min(Scorefinal32)) / (max(Scorefinal32) - min(Scorefinal32))
  group32 <- normalizeScorefinal32> predictions32
  write.csv(group32, "./finalresultmethod2/groupfmodel32.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-14]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal34<- colSums(score)
  write.csv(Scorefinal34, "./finalresultmethod2/scorefmodel34.csv")
  
  normalizeScorefinal34<-  (Scorefinal34 - min(Scorefinal34)) / (max(Scorefinal34) - min(Scorefinal34))
  group34 <- normalizeScorefinal34> predictions34
  write.csv(group34, "./finalresultmethod2/groupfmodel34.csv")
  
  score<- NULL
  for  (i in colnames(W[-15]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal35<- colSums(score)
  write.csv(Scorefinal35, "./finalresultmethod2/scorefmodel35.csv")
  
  normalizeScorefinal35<-  (Scorefinal35 - min(Scorefinal35)) / (max(Scorefinal35) - min(Scorefinal35))
  group35 <- normalizeScorefinal35> predictions35
  write.csv(group35, "./finalresultmethod2/groupfmodel35.csv")
  
  score<- NULL
  for  (i in colnames(W[-16]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal37<- colSums(score)
  write.csv(Scorefinal37, "./finalresultmethod2/scorefmodel37.csv")
  
  normalizeScorefinal37<-  (Scorefinal37 - min(Scorefinal37)) / (max(Scorefinal37) - min(Scorefinal37))
  group37 <- normalizeScorefinal37> predictions37
  write.csv(group37, "./finalresultmethod2/groupfmodel37.csv")
  
  score<- NULL
  for  (i in colnames(W[-17]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal41<- colSums(score)
  write.csv(Scorefinal41, "./finalresultmethod2/scorefmodel41.csv")
  
  normalizeScorefinal41<-  (Scorefinal41 - min(Scorefinal41)) / (max(Scorefinal41) - min(Scorefinal41))
  group41 <- normalizeScorefinal41> predictions41
  write.csv(group41, "./finalresultmethod2/groupfmodel41.csv")
  
  score<- NULL
  for  (i in colnames(W[-18]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal42<- colSums(score)
  write.csv(Scorefinal42, "./finalresultmethod2/scorefmodel42.csv")
  
  normalizeScorefinal42<-  (Scorefinal42 - min(Scorefinal42)) / (max(Scorefinal42) - min(Scorefinal42))
  group42 <- normalizeScorefinal42> predictions42
  write.csv(group42, "./finalresultmethod2/groupfmodel42.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-19]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal53<- colSums(score)
  write.csv(Scorefinal53, "./finalresultmethod2/scorefmodel53.csv")
  
  normalizeScorefinal53<-  (Scorefinal53 - min(Scorefinal53)) / (max(Scorefinal53) - min(Scorefinal53))
  group53 <- normalizeScorefinal53> predictions53
  write.csv(group53, "./finalresultmethod2/groupfmodel53.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-20]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal54<- colSums(score)
  write.csv(Scorefinal54, "./finalresultmethod2/scorefmodel54.csv")
  
  normalizeScorefinal54<-  (Scorefinal54 - min(Scorefinal54)) / (max(Scorefinal54) - min(Scorefinal54))
  group54 <- normalizeScorefinal54> predictions54
  write.csv(group54, "./finalresultmethod2/groupfmodel54.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-21]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal55<- colSums(score)
  write.csv(Scorefinal55, "./finalresultmethod2/scorefmodel55.csv")
  
  normalizeScorefinal55<-  (Scorefinal55 - min(Scorefinal55)) / (max(Scorefinal55) - min(Scorefinal55))
  group55 <- normalizeScorefinal55> predictions55
  write.csv(group55, "./finalresultmethod2/groupfmodel55.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-22]))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric (intercept[i])
  score<- rbind(scorei, score)
  Scorefinal59<- colSums(score)
  write.csv(Scorefinal55, "./finalresultmethod2/scorefmodel59.csv")
  
  normalizeScorefinal59<-  (Scorefinal55 - min(Scorefinal59)) / (max(Scorefinal59) - min(Scorefinal59))
  group59 <- normalizeScorefinal59> predictions59
  write.csv(group59, "./finalresultmethod2/groupfmodel59.csv")
  
}

# ==========================================================================================
# Score and Group Test Dataset 58
# ==========================================================================================
predictions58<- read.csv("./Resultmethod2/predictions58.csv")
W<- read.csv("./method2results/Weightsfinal2.csv")
colnames(W)<- c(0, 6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
intercept<- read.csv("./method2results/intercept2.csv")
colnames(intercept)<- c(0, 6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)


list <-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
for (i in list){ 
  modeli<- "model"[i]
  scorei<- score[i]

  score<- NULL
  for  (i in colnames(W))
    filename0=paste0("./finalmethod2/score",i,"")
  wd2<- paste0("./finalmethod2/score",i,".csv")
  scorei<- assign(filename0, read.table(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  scorei<- (as.numeric(W[i])*scorei)+as.numeric(intercept[i])
  score<- rbind(scorei, score)
  Scorefinal58<- colSums(score)
  
  write.csv(Scorefinal58, "./finalresultmethod2/scorefmodel58.csv")
}
  normalizeScorefinal58<-  (Scorefinal58 - min(Scorefinal58)) / (max(Scorefinal58) - min(Scorefinal58))
  group58 <- normalizeScorefinal58> predictions58
  write.csv(group58, "./finalresultmethod2/groupfmodel58.csv")


