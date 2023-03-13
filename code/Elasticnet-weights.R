# =====================================================================================================================
#    DESCRIPTION: 
#    These codes give the weight for each data set in the prediction
# =====================================================================================================================
#   INPUTS:
#  - Segmentation
#  - Score of each patient
#  - All data frames ccontainbaseline characteristics, Principal component scores, BOR, OS
# =====================================================================================================================
#   OUTPUTS:
#   - Weight of each data set
# =====================================================================================================================
#   Codes produced by: 
#   - Elham Majd
# =====================================================================================================================
#   HISTORY:
#   - Creation: Feb/2022
# =====================================================================================================================
#   STATEMENT:
#   This file is part of a project entitled "Predicting Tumor Response and 
#   Overall Survival Using Early Outcomes and Baseline Characteristics"
# =====================================================================================================================
# install.packages("dplyr",repos = "http://cran.us.r-project.org")
# install.packages("ggpubr",repos = "http://cran.us.r-project.org")
# install.packages("Matrix",repos = "http://cran.us.r-project.org")
# install.packages("glmnet",repos = "http://cran.us.r-project.org")
# install.packages("fs",repos = "http://cran.us.r-project.org")
# install.packages("purrr",repos = "http://cran.us.r-project.org")


# load package
library(dplyr)
library(ggpubr)
library(Matrix)
library(glmnet)
library(fs)
library(purrr)


group6<- read.csv("./scoremethod1/group6method1.csv")
group7<- read.csv("./scoremethod1/group7method1.csv")
group8<- read.csv("./scoremethod1/group8method1.csv")
group9<- read.csv("./scoremethod1/group9method1.csv")
group11<- read.csv("./scoremethod1/group11method1.csv")
group12<- read.csv("./scoremethod1/group12method1.csv")
group16<- read.csv("./scoremethod1/group16method1.csv")
group17<- read.csv("./scoremethod1/group17method1.csv")
group18<- read.csv("./scoremethod1/group18method1.csv")
group22<- read.csv("./scoremethod1/group22method1.csv")
group23<- read.csv("./scoremethod1/group23method1.csv")
group31<- read.csv("./scoremethod1/group31method1.csv")
group32<- read.csv("./scoremethod1/group32method1.csv")
group34<- read.csv("./scoremethod1/group34method1.csv")
group35<- read.csv("./scoremethod1/group35method1.csv")
group37<- read.csv("./scoremethod1/group37method1.csv")
group41<- read.csv("./scoremethod1/group41method1.csv")
group42<- read.csv("./scoremethod1/group42method1.csv")
group53<- read.csv("./scoremethod1/group53method1.csv")
group54<- read.csv("./scoremethod1/group54method1.csv")
group55<- read.csv("./scoremethod1/group55method1.csv")
group59<- read.csv("./scoremethod1/group59method1.csv")
group<- list(model6=c(group6), model7=c(group7), model8= c(group8), model9= c(group9), model11= c(group11), 
             model12= c(group12), model16= c(group16), model17= c(group17), model18= c(group18), model22= c(group22), 
             model23= c(group23), model31= c(group31), model32= c(group32),model34= c(group34),model35= c(group35), 
             model37= c(group37),  model41= c(group41), model42= c(group42),model53= c(group53), model54= c(group54), 
             model55= c(group55), model59=c(group59))
# ============================================================================
score6<- read.csv("./scoremethod1/score6method1.csv")
score7<- read.csv("./scoremethod1/score7method1.csv")
score8<- read.csv("./scoremethod1/score8method1.csv")
score9<- read.csv("./scoremethod1/score9method1.csv")
score11<- read.csv("./scoremethod1/score11method1.csv")
score12<- read.csv("./scoremethod1/score12method1.csv")
score16<- read.csv("./scoremethod1/score16method1.csv")
score17<- read.csv("./scoremethod1/score17method1.csv")
score18<- read.csv("./scoremethod1/score18method1.csv")
score22<- read.csv("./scoremethod1/score22method1.csv")
score23<- read.csv("./scoremethod1/score23method1.csv")
score31<- read.csv("./scoremethod1/score31method1.csv")
score32<- read.csv("./scoremethod1/score32method1.csv")
score34<- read.csv("./scoremethod1/score34method1.csv")
score35<- read.csv("./scoremethod1/score35method1.csv")
score37<- read.csv("./scoremethod1/score37method1.csv")
score41<- read.csv("./scoremethod1/score41method1.csv")
score42<- read.csv("./scoremethod1/score42method1.csv")
score53<- read.csv("./scoremethod1/score53method1.csv")
score54<- read.csv("./scoremethod1/score54method1.csv")
score55<- read.csv("./scoremethod1/score55method1.csv")
score59<- read.csv("./scoremethod1/score59method1.csv")
score<- list(model6=c(score6), model7=c(score7), model8= c(score8), model9= c(score9), model11= c(score11), 
             model12= c(score12), model16= c(score16), model17= c(score17), model18= c(score18), model22= c(score22), 
             model23= c(score23), model31= c(score31), model32= c(score32),model34= c(score34),model35= c(score35), 
             model37= c(score37),  model41= c(score41), model42= c(score42),model53= c(score53), model54= c(score54), 
             model55= c(score55), model59=c(score59))
# ============================================================================
D<- list((model6= read.csv("./cvresults/6-alldata6.csv")),
         (model7= read.csv("./cvresults/7-alldata7.csv")),
         (model8= read.csv("./cvresults/8-alldata8.csv")),
         (model9= read.csv("./cvresults/9-alldata9.csv")),
         (model11= read.csv("./cvresults/11-alldata11.csv")),
         (model12= read.csv("./cvresults/12-alldata12.csv")),
         (model16= read.csv("./cvresults/16-alldata16.csv")),
         (model17= read.csv("./cvresults/17-alldata17.csv")),
         (model18= read.csv("./cvresults/18-alldata18.csv")),
         (model22= read.csv("./cvresults/22-alldata22.csv")),
         (model23= read.csv("./cvresults/23-alldata23.csv")),
         (model31= read.csv("./cvresults/31-alldata31.csv")),
         (model32= read.csv("./cvresults/32-alldata32.csv")),
         (model34= read.csv("./cvresults/34-alldata34.csv")),
         (model35= read.csv("./cvresults/35-alldata35.csv")),
         (model37= read.csv("./cvresults/37-alldata37.csv")),
         (model41= read.csv("./cvresults/41-alldata41.csv")),
         (model42= read.csv("./cvresults/42-alldata42.csv")),
         (model53= read.csv("./cvresults/53-alldata53.csv")),
         (model54= read.csv("./cvresults/54-alldata54.csv")),
         (model55= read.csv("./cvresults/55-alldata55.csv")),
         (model59= read.csv("./cvresults/59-alldata59.csv")))
names(D)<-c("model6","model7","model8","model9","model11","model12","model16","model17","model18",
            "model22","model23","model31","model32","model34","model35","model37","model41","model42",
            "model53","model54","model55","model59") 

for(i in 1:100)
{
  s<- sample(D, 5, replace=FALSE)
  k<- D[!(D %in% s)]
  names(k)
  coefval<- read.csv("./Resultmethod1/coefval.csv")
  coefval<- (coefval[,-1])
  coefvisit<- read.csv("./Resultmethod2/coefvisit3.csv")
  do.call(file.remove,list(list.files("./csvlist",full.names = TRUE)))
  
  for (h in names (s[1]))
  {
    for (j in names (k))
    {
      coefval[j]
      coefvisit[j]
      if (is.na(coefvisit[j][2,])) coefvisit[j][2,]<- 0
      
      
      pc1<- coefval[j][1,]*((D[[h]]$v2)-mean((D[[h]]$v2))) +coefval[j][3,]*((D[[h]]$v3)-mean((D[[h]]$v3)))
      D[[h]][is.na(D[[h]])]<- 0
      scorej<- coefvisit[j][1,] + coefvisit[j][2,]*D[[h]]$SEX + coefvisit[j][3,]*D[[h]]$AGE + coefvisit[j][4,]*pc1
      scorej[is.na(scorej)]<- 0
      filename=paste("./csvlist/",j,".csv",sep="")
      
      write.table(scorej,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = j)
      
    }
    csv_file_list<-dir_ls("./csvlist")
    csv_file_list
    df_list=map(csv_file_list,read.csv)
    bind_rows(df_list,.id='idd')
    df_list
    df_list[is.na(df_list)]<- 0
    df_list<- data.frame(df_list)
    filename2=paste("./read/result",h,"/result[1]",i,"",h,".table",sep="")
    write.table(df_list,filename2, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = TRUE)
    
    score[h]
    group[h]
    datasetfinal<- data.frame(score[h])
    actual<- data.frame(group[h])
    
    
    dat <- data.frame(df_list, datasetfinal=datasetfinal, actual=actual)
   
    # dat[is.na(dat)]= 0
    names(dat)[c(18)]<-"datasetfinal"
    names(dat)[c(19)]<-"actual"
    
    
    fit <- glmnet(dat[c(1:17)], dat$datasetfinal)
    
    weights <- coef(fit,  s=0.1)
    row.names<- h
    Weightsf<-weights[-1,]
    Intercept<- weights[1,]
    
    # weights<- data.frame(t(weights))
    write.table(
      Weightsf,
      "./method2results/Weightsf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = TRUE,
      col.names = FALSE)
    
    
    write.table(
      Intercept,
      "./method2results/Interceptf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = row.names,
      col.names = FALSE)
  } 
  # ==========================================================================================
  for (h in names (s[2]))
  {
    for (j in names (k))
    {
      coefval[j]
      coefvisit[j]
      if (is.na(coefvisit[j][2,])) coefvisit[j][2,]<- 0
      
      
      pc1<- coefval[j][1,]*((D[[h]]$v2)-mean((D[[h]]$v2))) +coefval[j][3,]*((D[[h]]$v3)-mean((D[[h]]$v3)))
      D[[h]][is.na(D[[h]])]<- 0
      scorej<- coefvisit[j][1,] + coefvisit[j][2,]**D[[h]]$SEX + coefvisit[j][3,]*D[[h]]$AGE + coefvisit[j][4,]*pc1
      scorej[is.na(scorej)]<- 0
      filename=paste("./csvlist/",j,".csv",sep="")
      
      write.table(scorej,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = j)
      
    }
    csv_file_list<-dir_ls("./csvlist")
    csv_file_list
    df_list=map(csv_file_list,read.csv)
    bind_rows(df_list,.id='idd')
    df_list
    df_list[is.na(df_list)]<- 0
    df_list<- data.frame(df_list)
    filename2=paste("./read/result",h,"/result[1]",i,"",h,".table",sep="")
    write.table(df_list,filename2, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = TRUE)
    
    score[h]
    group[h]
    datasetfinal<- data.frame(score[h])
    actual<- data.frame(group[h])
    
    
    dat <- data.frame(df_list, datasetfinal=datasetfinal, actual=actual)
   
    # dat[is.na(dat)]= 0
    names(dat)[c(18)]<-"datasetfinal"
    names(dat)[c(19)]<-"actual"
    
    fit <- glmnet(dat[c(1:17)], dat$datasetfinal)
    
    weights <- coef(fit,  s=0.1)
    row.names<- h
    Weightsf<-weights[-1,]
    Intercept<- weights[1,]
    
    # weights<- data.frame(t(weights))
    write.table(
      Weightsf,
      "./method2results/Weightsf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = TRUE,
      col.names = FALSE)
    
    
    write.table(
      Intercept,
      "./method2results/Interceptf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = row.names,
      col.names = FALSE)
  }
  # =================================================================================================
  
  for (h in names (s[3]))
  {
    for (j in names (k))
    {
      coefval[j]
      coefvisit[j]
      if (is.na(coefvisit[j][2,])) coefvisit[j][2,]<- 0
      
      
      pc1<- coefval[j][1,]*((D[[h]]$v2)-mean((D[[h]]$v2))) +coefval[j][3, ]*((D[[h]]$v3)-mean((D[[h]]$v3)))
      D[[h]][is.na(D[[h]])]<- 0
      scorej<- coefvisit[j][1,] + coefvisit[j][2,]**D[[h]]$SEX + coefvisit[j][3,]*D[[h]]$AGE + coefvisit[j][4,]*pc1
      scorej[is.na(scorej)]<- 0
      filename=paste("./csvlist/",j,".csv",sep="")
      
      write.table(scorej,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = j)
      
    }
    csv_file_list<-dir_ls("./csvlist")
    csv_file_list
    df_list=map(csv_file_list,read.csv)
    bind_rows(df_list,.id='idd')
    df_list
    df_list[is.na(df_list)]<- 0
    df_list<- data.frame(df_list)
    filename2=paste("./read/result",h,"/result[1]",i,"",h,".table",sep="")
    write.table(df_list,filename2, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = TRUE)
    
    score[h]
    group[h]
    datasetfinal<- data.frame(score[h])
    actual<- data.frame(group[h])
    
    
    dat <- data.frame(df_list, datasetfinal=datasetfinal, actual=actual)
   
    # dat[is.na(dat)]= 0
    names(dat)[c(18)]<-"datasetfinal"
    names(dat)[c(19)]<-"actual"
    
    fit <- glmnet(dat[c(1:17)], dat$datasetfinal)
    
    weights <- coef(fit,  s=0.1)
    row.names<- h
    Weightsf<-weights[-1,]
    Intercept<- weights[1,]
    
    # weights<- data.frame(t(weights))
    write.table(
      Weightsf,
      "./method2results/Weightsf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = TRUE,
      col.names = FALSE)
    
    
    write.table(
      Intercept,
      "./method2results/Interceptf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = row.names,
      col.names = FALSE)
  }
  # ==========================================================================================
  for (h in names (s[4]))
  {
    for (j in names (k))
    {
      coefval[j]
      coefvisit[j]
      if (is.na(coefvisit[j][2,])) coefvisit[j][2,]<- 0
      
      
      pc1<- coefval[j][1,]*((D[[h]]$v2)-mean((D[[h]]$v2))) +coefval[j][3, ]*((D[[h]]$v3)-mean((D[[h]]$v3)))
      D[[h]][is.na(D[[h]])]<- 0
      scorej<- coefvisit[j][1,] + coefvisit[j][2,]**D[[h]]$SEX + coefvisit[j][3,]*D[[h]]$AGE + coefvisit[j][4,]*pc1
      scorej[is.na(scorej)]<- 0
      filename=paste("./csvlist/",j,".csv",sep="")
      
      write.table(scorej,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = j)
      
    }
    csv_file_list<-dir_ls("./csvlist")
    csv_file_list
    df_list=map(csv_file_list,read.csv)
    bind_rows(df_list,.id='idd')
    df_list
    df_list[is.na(df_list)]<- 0
    df_list<- data.frame(df_list)
    filename2=paste("./read/result",h,"/result[1]",i,"",h,".table",sep="")
    write.table(df_list,filename2, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = TRUE)
    
    score[h]
    group[h]
    datasetfinal<- data.frame(score[h])
    actual<- data.frame(group[h])
    
    
    dat <- data.frame(df_list, datasetfinal=datasetfinal, actual=actual)
   
    # dat[is.na(dat)]= 0
    names(dat)[c(18)]<-"datasetfinal"
    names(dat)[c(19)]<-"actual"
    
    fit <- glmnet(dat[c(1:17)], dat$datasetfinal)
    
    weights <- coef(fit,  s=0.1)
    row.names<- h
    Weightsf<-weights[-1,]
    Intercept<- weights[1,]
    
    # weights<- data.frame(t(weights))
    write.table(
      Weightsf,
      "./method2results/Weightsf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = TRUE,
      col.names = FALSE)
    
    
    write.table(
      Intercept,
      "./method2results/Interceptf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = row.names,
      col.names = FALSE)
  }
  # ==========================================================================================   
  
  for (h in names (s[5]))
  {
    for (j in names (k))
    {
      coefval[j]
      coefvisit[j]
      if (is.na(coefvisit[j][2,])) coefvisit[j][2,]<- 0
      
      
      pc1<- coefval[j][1,]*((D[[h]]$v2)-mean((D[[h]]$v2))) +coefval[j][3, ]*((D[[h]]$v3)-mean((D[[h]]$v3)))
      D[[h]][is.na(D[[h]])]<- 0
      scorej<- coefvisit[j][1,] + coefvisit[j][2,]**D[[h]]$SEX + coefvisit[j][3,]*D[[h]]$AGE + coefvisit[j][4,]*pc1
      scorej[is.na(scorej)]<- 0
      filename=paste("./csvlist/",j,".csv",sep="")
      
      write.table(scorej,filename, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = j)
      
    }
    csv_file_list<-dir_ls("./csvlist")
    csv_file_list
    df_list=map(csv_file_list,read.csv)
    bind_rows(df_list,.id='idd')
    df_list
    df_list[is.na(df_list)]<- 0
    df_list<- data.frame(df_list)
    filename2=paste("./read/result",h,"/result[1]",i,"",h,".table",sep="")
    write.table(df_list,filename2, append = FALSE, quote = FALSE, sep = "    ", row.names = FALSE, col.names = TRUE)
    
    score[h]
    group[h]
    datasetfinal<- data.frame(score[h])
    actual<- data.frame(group[h])
    
    
    dat <- data.frame(df_list, datasetfinal=datasetfinal, actual=actual)
   
    # dat[is.na(dat)]= 0
    names(dat)[c(18)]<-"datasetfinal"
    names(dat)[c(19)]<-"actual"
    
    fit <- glmnet(dat[c(1:17)], dat$datasetfinal)
    
    weights <- coef(fit,  s=0.1)
    row.names<- h
    Weightsf<-weights[-1,]
    Intercept<- weights[1,]
    
    # weights<- data.frame(t(weights))
    write.table(
      Weightsf,
      "./method2results/Weightsf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = TRUE,
      col.names = FALSE)
    
    
    write.table(
      Intercept,
      "./method2results/Interceptf.csv",
      append = TRUE,
      quote = FALSE,
      sep = ",",
      row.names = row.names,
      col.names = FALSE)
    
  }
}
warnings()
