# ===================================================================================================================== 
#  DESCRIPTION: 
#  These codes segment patients into a bad and good group
# ===================================================================================================================== 
#  INPUTS:
#  - Prediction results in each data set
#  - Results from Regression Tree
# ===================================================================================================================== 
#  OUTPUTS:
#  - Score of each patient
#  - Segmentation (bad and good group)
# ===================================================================================================================== 
#  Codes produced by: 
#  - Elham Majd
# ===================================================================================================================== 
#  HISTORY:
#  - Creation: Feb/2022
# ===================================================================================================================== 
#  STATEMENT:
#   This file is part of a project entitled "Predicting Tumor Response and 
#   Overall Survival Using Early Outcomes and Baseline Characteristics"
# =====================================================================================================================
# Libraries 
# =====================================================================================================================
 install.packages("qpcR",repos = "http://cran.us.r-project.org")
 install.packages("rlist",repos = "http://cran.us.r-project.org")

# load package
library(purrr)
library(dplyr)
require(fastmatch)
library(caret)
library(qpcR) 
library(fs)
library(rlist)

filelist <- list.files("./readtree/")
basename(filelist)

list <-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
for (j in list){
  
  filename0=paste0("./readtree/resultmodel",j,"")
  filenames <- list.files(filename0, full.names = TRUE)
  ldfj<-lapply(filenames,read.table)
  ldfj<- as.data.frame(ldfj)
  scoremethod3j<- ldfj[!duplicated(as.list(ldfj))]
  filename0=paste0("./scoremethod3/scoremodel",j,".table")
  write.table(scoremethod3j, filename0,  append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = FALSE)
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
predictions58<- read.csv("./Resultmethod2/predictions58.csv")
predictions59<- read.csv("./Resultmethod2/predictions59.csv")
# ======================================================================================
Weight<- readLines("./method3results/PredDT.csv")
Weights<-strsplit(Weight, ",")
length(Weights)

for( i in 1:length(Weights)){
  
  if(list.filter("model6" %in% Weights[[i]][1])){
    W6<- mean(as.numeric(Weights[[i]][-1]))
    print(W6)
    write.csv(W6, "./rate/W6.csv")
  }
  if (list.filter("model7" %in% Weights[[i]][1])){
    W7<- mean(as.numeric(Weights[[i]][-1]))
    print(W7)
    write.csv(W7, "./rate/W7.csv")
  }
  if (list.filter("model8" %in% Weights[[i]][1])){
    W8<- mean(as.numeric(Weights[[i]][-1]))
    print(W8)
    write.csv(W8, "./rate/W8.csv")
  }
  
  if(list.filter("model9" %in% Weights[[i]][1])){
    W9<- mean(as.numeric(Weights[[i]][-1]))
    print(W9)
    write.csv(W9, "./rate/W9.csv")
  }
  if (list.filter("model11" %in% Weights[[i]][1])){
    W11<- mean(as.numeric(Weights[[i]][-1]))
    print(W11)
    write.csv(W11, "./rate/W11.csv")
  }
  if (list.filter("model12" %in% Weights[[i]][1])){
    W12<- mean(as.numeric(Weights[[i]][-1]))
    print(W12)
    write.csv(W12, "./rate/W12.csv")
  }
  if(list.filter("model16" %in% Weights[[i]][1])){
    W16<- mean(as.numeric(Weights[[i]][-1]))
    print(W16)
    write.csv(W16, "./rate/W16.csv")
  }
  if (list.filter("model17" %in% Weights[[i]][1])){
    W17<- mean(as.numeric(Weights[[i]][-1]))
    print(W17)
    write.csv(W17, "./rate/W17.csv")
  }
  if (list.filter("model18" %in% Weights[[i]][1])){
    W18<- mean(as.numeric(Weights[[i]][-1]))
    print(W18)
    write.csv(W18, "./rate/W18.csv")
  }
  if (list.filter("model22" %in% Weights[[i]][1])){
    W22<- mean(as.numeric(Weights[[i]][-1]))
    print(W22)
    write.csv(W22, "./rate/W22.csv")
  }
  if (list.filter("model23" %in% Weights[[i]][1])){
    W23<- mean(as.numeric(Weights[[i]][-1]))
    print(W23)
    write.csv(W23, "./rate/W23.csv")
  }
  if (list.filter("model31" %in% Weights[[i]][1])){
    W31<- mean(as.numeric(Weights[[i]][-1]))
    print(W31)
    write.csv(W31, "./rate/W31.csv")
  }
  if(list.filter("model32" %in% Weights[[i]][1])){
    W32<- mean(as.numeric(Weights[[i]][-1]))
    print(W32)
    write.csv(W32, "./rate/W32.csv")
  }
  if (list.filter("model34" %in% Weights[[i]][1])){
    W34<- mean(as.numeric(Weights[[i]][-1]))
    print(W34)
    write.csv(W34, "./rate/W34.csv")
  }
  if (list.filter("model35" %in% Weights[[i]][1])){
    W35<- mean(as.numeric(Weights[[i]][-1]))
    write.csv(W35, "./rate//W35.csv")
  }
  if(list.filter("model37" %in% Weights[[i]][1])){
    W37<- mean(as.numeric(Weights[[i]][-1]))
    print(W37)
    write.csv(W37, "./rate//W37.csv")
  }
  if (list.filter("model41" %in% Weights[[i]][1])){
    W41<- mean(as.numeric(Weights[[i]][-1]))
    print(W41)
    write.csv(W41, "./rate//W41.csv")
  }
  if (list.filter("model42" %in% Weights[[i]][1])){
    W42<- mean(as.numeric(Weights[[i]][-1]))
    write.csv(W42, "./rate//W42.csv")
  }
  if (list.filter("model53" %in% Weights[[i]][1])){
    W53<- mean(as.numeric(Weights[[i]][-1]))
    print(W53)
    write.csv(W53, "./rate//W53.csv")
  }
  if (list.filter("model54" %in% Weights[[i]][1])){
    W54<- mean(as.numeric(Weights[[i]][-1]))
    write.csv(W54, "./rate//W54.csv")
  }
  if (list.filter("model55" %in% Weights[[i]][1])){
    W55<- mean(as.numeric(Weights[[i]][-1]))
    print(W55)
    write.csv(W55, "./rate//W55.csv")
  }
  if (list.filter("model59" %in% Weights[[i]][1])){
    W59<- mean(as.numeric(Weights[[i]][-1]))
    write.csv(W59, "./rate//W59.csv")
  }
}

csv_file_list<-dir_ls("./rate/")
csv_file_list
df_list=map(csv_file_list,read.csv)
W<- qpcR:::cbind.na(W6, W7, W8, W9, W11, W12, W16, W17, W18, W22, W23,
                    W31, W32, W34, W35, W37, W41, W42, W53, W54, W55, W59)
W<- data.frame(W)
write.csv(W, "./method3results/rates.csv")
colnames(W)<-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)

list <-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
for (j in list){
  
  filename0=paste0("./scoremethod3/scoremodel",j,"")
  wd<- paste0("./scoremethod3/scoremodel",j,".table")
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
        filename=paste("./finalmethod3/score",i,".csv",sep="")
        write.table(scorei,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = FALSE)
      }
    }
  }
  
  score<- NULL
  for  (i in list)
  for  (i in colnames(W[-1]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei<- na.omit(scorei)
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal6<- colSums(score)

  write.csv(Scorefinal6, "./finalresultmethod3/scorefmodel6.csv")

  normalizeScorefinal6<-  (Scorefinal6 - min(Scorefinal6)) / (max(Scorefinal6) - min(Scorefinal6))
  group6 <- normalizeScorefinal6> predictions6
  write.csv(group6, "./finalresultmethod3/groupfmodel6.csv")

  score<- NULL
  for  (i in colnames(W[-2]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal7<- colSums(score)
  write.csv(Scorefinal7, "./finalresultmethod3/scorefmodel7.csv")

  normalizeScorefinal7<-  (Scorefinal7 - min(Scorefinal7)) / (max(Scorefinal7) - min(Scorefinal7))
  group7 <- normalizeScorefinal7> predictions7
  write.csv(group7, "./finalresultmethod3/groupfmodel7.csv")

  score<- NULL
  for  (i in colnames(W[-3]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.table(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal8<- colSums(score)
  write.csv(Scorefinal8, "./finalresultmethod3/scorefmodel8.csv")

  normalizeScorefinal8<-  (Scorefinal8 - min(Scorefinal8)) / (max(Scorefinal8) - min(Scorefinal8))
  group8 <- normalizeScorefinal8> predictions8
  write.csv(group8, "./finalresultmethod3/groupfmodel8.csv")


  score<- NULL
  for  (i in colnames(W[-4]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal9<- colSums(score)
  write.csv(Scorefinal9, "./finalresultmethod3/scorefmodel9.csv")

  normalizeScorefinal9<-  (Scorefinal9 - min(Scorefinal9)) / (max(Scorefinal9) - min(Scorefinal9))
  group9 <- normalizeScorefinal9> predictions9
  write.csv(group9, "./finalresultmethod3/groupfmodel9.csv")

  score<- NULL
  for  (i in colnames(W[-5]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal11<- colSums(score)
  write.csv(Scorefinal11, "./finalresultmethod3/scorefmodel11.csv")

normalizeScorefinal11<-  (Scorefinal11 - min(Scorefinal11)) / (max(Scorefinal11) - min(Scorefinal11))
group11 <- normalizeScorefinal11> predictions11
write.csv(group11, "./finalresultmethod3/groupfmodel11.csv")


score<- NULL
for  (i in colnames(W[-6]))
  filename0=paste0("./finalmethod3/score",i,"")
wd2<- paste0("./finalmethod3/score",i,".csv")
scorei<- assign(filename0, read.csv(wd2))
scorei <- scorei[-1,]
scorei<- as.numeric(unlist(scorei))
W[i]<- (unlist(W[i]))
Wi<- W[i][!is.na(W[i])]
Wi<- as.numeric(Wi)
scorei<- (Wi*scorei)
score<- rbind(scorei, score)
Scorefinal12<- colSums(score)
write.csv(Scorefinal12, "./finalresultmethod3/scorefmodel12.csv")

normalizeScorefinal12<-  (Scorefinal12 - min(Scorefinal12)) / (max(Scorefinal12) - min(Scorefinal12))
group12 <- normalizeScorefinal12> predictions12
write.csv(group12, "./finalresultmethod3/groupfmodel12.csv")


score<- NULL
for  (i in colnames(W[-7]))
  filename0=paste0("./finalmethod3/score",i,"")
wd2<- paste0("./finalmethod3/score",i,".csv")
scorei<- assign(filename0, read.csv(wd2))
scorei <- scorei[-1,]
scorei<- as.numeric(unlist(scorei))
W[i]<- (unlist(W[i]))
Wi<- W[i][!is.na(W[i])]
Wi<- as.numeric(Wi)
scorei<- (Wi*scorei)
score<- rbind(scorei, score)
Scorefinal16<- colSums(score)
write.csv(Scorefinal16, "./finalresultmethod3/scorefmodel16.csv")

  normalizeScorefinal16<-  (Scorefinal16 - min(Scorefinal16)) / (max(Scorefinal16) - min(Scorefinal16))
  group16 <- normalizeScorefinal16> predictions16
  write.csv(group16, "./finalresultmethod3/groupfmodel16.csv")


  score<- NULL
  for  (i in colnames(W[-8]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.table(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal17<- colSums(score)
  write.csv(Scorefinal17, "./finalresultmethod3/scorefmodel17.csv")

  normalizeScorefinal17<-  (Scorefinal17 - min(Scorefinal17)) / (max(Scorefinal17) - min(Scorefinal17))
  group17 <- normalizeScorefinal17> predictions17
  write.csv(group17, "./finalresultmethod3/groupfmodel17.csv")

  score<- NULL
  for  (i in colnames(W[-9]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal18<- colSums(score)
  write.csv(Scorefinal18, "./finalresultmethod3/scorefmodel18.csv")

  normalizeScorefinal18<-  (Scorefinal18 - min(Scorefinal18)) / (max(Scorefinal18) - min(Scorefinal18))
  group18 <- normalizeScorefinal18> predictions18
  write.csv(group18, "./finalresultmethod3/groupfmodel18.csv")

  score<- NULL
  for  (i in colnames(W[-10]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal22<- colSums(score)
  write.csv(Scorefinal22, "./finalresultmethod3/scorefmodel22.csv")

  normalizeScorefinal22<-  (Scorefinal22 - min(Scorefinal22)) / (max(Scorefinal22) - min(Scorefinal22))
  group22 <- normalizeScorefinal22> predictions22
  write.csv(group22, "./finalresultmethod3/groupfmodel22.csv")

  score<- NULL
  for  (i in colnames(W[-11]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal23<- colSums(score)
  write.csv(Scorefinal23, "./finalresultmethod3/scorefmodel23.csv")

  normalizeScorefinal23<-  (Scorefinal23 - min(Scorefinal23)) / (max(Scorefinal23) - min(Scorefinal23))
  group23 <- normalizeScorefinal23> predictions23
  write.csv(group23, "./finalresultmethod3/groupfmodel23.csv")

  score<- NULL
  for  (i in colnames(W[-12]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal31<- colSums(score)
  write.csv(Scorefinal31, "./finalresultmethod3/scorefmodel31.csv")

  normalizeScorefinal31<-  (Scorefinal31 - min(Scorefinal31)) / (max(Scorefinal31) - min(Scorefinal31))
  group31 <- normalizeScorefinal31> predictions31
  write.csv(group31, "./finalresultmethod3/groupfmodel31.csv")

  score<- NULL
  for  (i in colnames(W[-13]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal32<- colSums(score)
  write.csv(Scorefinal32, "./finalresultmethod3/scorefmodel32.csv")

  normalizeScorefinal32<-  (Scorefinal32 - min(Scorefinal32)) / (max(Scorefinal32) - min(Scorefinal32))
  group32 <- normalizeScorefinal32> predictions32
  write.csv(group32, "./finalresultmethod3/groupfmodel32.csv")


  score<- NULL
  for  (i in colnames(W[-14]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal34<- colSums(score)
  write.csv(Scorefinal34, "./finalresultmethod3/scorefmodel34.csv")

  normalizeScorefinal34<-  (Scorefinal34 - min(Scorefinal34)) / (max(Scorefinal34) - min(Scorefinal34))
  group34 <- normalizeScorefinal34> predictions34
  write.csv(group34, "./finalresultmethod3/groupfmodel34.csv")

  score<- NULL
  for  (i in colnames(W[-15]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.table(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal35<- colSums(score)
  write.csv(Scorefinal35, "./finalresultmethod3/scorefmodel35.csv")

  normalizeScorefinal35<-  (Scorefinal35 - min(Scorefinal35)) / (max(Scorefinal35) - min(Scorefinal35))
  group35 <- normalizeScorefinal35> predictions35
  write.csv(group35, "./finalresultmethod3/groupfmodel35.csv")

  score<- NULL
  for  (i in colnames(W[-16]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal37<- colSums(score)
  write.csv(Scorefinal37, "./finalresultmethod3/scorefmodel37.csv")

  normalizeScorefinal37<-  (Scorefinal37 - min(Scorefinal37)) / (max(Scorefinal37) - min(Scorefinal37))
  group37 <- normalizeScorefinal37> predictions37
  write.csv(group37, "./finalresultmethod3/groupfmodel37.csv")

  score<- NULL
  for  (i in colnames(W[-17]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal41<- colSums(score)
  write.csv(Scorefinal41, "./finalresultmethod3/scorefmodel41.csv")

  normalizeScorefinal41<-  (Scorefinal41 - min(Scorefinal41)) / (max(Scorefinal41) - min(Scorefinal41))
  group41 <- normalizeScorefinal41> predictions41
  write.csv(group41, "./finalresultmethod3/groupfmodel41.csv")

  score<- NULL
  for  (i in colnames(W[-18]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal42<- colSums(score)
  write.csv(Scorefinal42, "./finalresultmethod3/scorefmodel42.csv")

  normalizeScorefinal42<-  (Scorefinal42 - min(Scorefinal42)) / (max(Scorefinal42) - min(Scorefinal42))
  group42 <- normalizeScorefinal42> predictions42
  write.csv(group42, "./finalresultmethod3/groupfmodel42.csv")

  score<- NULL
  for  (i in colnames(W[-19]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal53<- colSums(score)
  write.csv(Scorefinal53, "./finalresultmethod3/scorefmodel53.csv")

  normalizeScorefinal53<-  (Scorefinal53 - min(Scorefinal53)) / (max(Scorefinal53) - min(Scorefinal53))
  group53 <- normalizeScorefinal53> predictions53
  write.csv(group53, "./finalresultmethod3/groupfmodel53.csv")


  score<- NULL
  for  (i in colnames(W[-20]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal54<- colSums(score)
  write.csv(Scorefinal54, "./finalresultmethod3/scorefmodel54.csv")
  
  normalizeScorefinal54<-  (Scorefinal54 - min(Scorefinal54)) / (max(Scorefinal54) - min(Scorefinal54))
  group54 <- normalizeScorefinal54> predictions54
  write.csv(group54, "./finalresultmethod3/groupfmodel54.csv")

  score<- NULL
  for  (i in colnames(W[-21]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.table(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal55<- colSums(score)
  write.csv(Scorefinal55, "./finalresultmethod3/scorefmodel55.csv")
  
  normalizeScorefinal55<-  (Scorefinal55 - min(Scorefinal55)) / (max(Scorefinal55) - min(Scorefinal55))
  group55 <- normalizeScorefinal55> predictions55
  write.csv(group55, "./finalresultmethod3/groupfmodel55.csv")
  
  
  score<- NULL
  for  (i in colnames(W[-22]))
    filename0=paste0("./finalmethod3/score",i,"")
  wd2<- paste0("./finalmethod3/score",i,".csv")
  scorei<- assign(filename0, read.csv(wd2))
  scorei <- scorei[-1,]
  scorei<- as.numeric(unlist(scorei))
  W[i]<- (unlist(W[i]))
  Wi<- W[i][!is.na(W[i])]
  Wi<- as.numeric(Wi)
  scorei<- (Wi*scorei)
  score<- rbind(scorei, score)
  Scorefinal59<- colSums(score)
  write.csv(Scorefinal59, "./finalresultmethod3/scorefmodel59.csv")
  
  normalizeScorefinal59<-  (Scorefinal59 - min(Scorefinal59)) / (max(Scorefinal59) - min(Scorefinal59))
  group59 <- normalizeScorefinal59> predictions59
  write.csv(group59, "./finalresultmethod3/groupfmodel59.csv")
 
}
# ==========================================================================================
# Score and Group Test Dataset 58
# ==========================================================================================
 W2<- read.csv("./method3results/rates.csv")
 colnames(W2)<- c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
 
 list <-c(6, 7, 8, 9, 11, 12, 16, 17, 18, 22, 23, 31, 32, 34, 35, 37, 41, 42, 53, 54, 55, 59)
 
 for (i in list){ 
   modeli<- "model"[i]
   scorei<- score[i]
     score<- NULL
     for  (i in colnames(W))
       filename0=paste0("./finalmethod3/score",i,"")
     wd2<- paste0("./finalmethod3/score",i,".csv")
     scorei<- assign(filename0, read.table(wd2))
     scorei <- scorei[-1,]
     scorei<- as.numeric(unlist(scorei))
     W[i]<- (unlist(W[i]))
     Wi<- W[i][!is.na(W[i])]
     Wi<- as.numeric(Wi)
     scorei<- (Wi*scorei)
     score<- rbind(scorei, score)
     Scorefinal58<- colSums(score)
 
     write.csv(Scorefinal58, "./finalresultmethod3/scorefmodel58.csv")
 
     normalizeScorefinal58<-  (Scorefinal58 - min(Scorefinal58)) / (max(Scorefinal58) - min(Scorefinal58))
     group58 <- normalizeScorefinal58> predictions58
     write.csv(group58, "./finalresultmethod3/groupfmodel58.csv")
 
 }
# ==========================================================================================
