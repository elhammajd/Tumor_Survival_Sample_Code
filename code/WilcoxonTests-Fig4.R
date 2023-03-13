# ===================================================================================================================== 
#  DESCRIPTION: 
#  The codes evaluate the pairwise comparisons of misclassification rates of predictive models between three and four visits
# ===================================================================================================================== 
#  INPUTS:
#  - The Misclassification rates for visit three and four in method one
#  - The Misclassification rates for visit three and four in method two and three
# ===================================================================================================================== 
#  OUTPUTS:
#  - Result of Wilcoxon test for three and four visits in method one
#  - Result of Wilcoxon test for three and four visits in method two and three
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
# install.packages("ggstatsplot",repos = "http://cran.us.r-project.org")
# load package
library(ggplot2)
library(ggpubr)
library(rstatix)
library(ggstatsplot)
# =========================================================================  
#   paired Wilcoxon test for pairwise comparisons between groups 
# =========================================================================
methodtwoandthree1<-read.csv("./Resultmethod2/misclassificationrate_V3.csv")
methodtwoandthree11<- methodtwoandthree1[!grepl('model', methodtwoandthree1[,1]),]
methodtwoandthree2<-read.csv("./Resultmethod2/misclassificationrate_V4.csv")
methodtwoandthree22<- methodtwoandthree2[!grepl('model', methodtwoandthree2[,1]),]
methodtwoandthreeA<- data.frame(weight= as.numeric(methodtwoandthree11))
methodtwoandthreeA$group<- c("visit3")
methodtwoandthreeB<- data.frame(weight= as.numeric(methodtwoandthree22))
methodtwoandthreeB$group<- c("visit4")
methodtwoandthree<-  rbind.data.frame(methodtwoandthreeA, methodtwoandthreeB)
rate<-methodtwoandthree
rate.long<- rate %>%
  gather(key = "group", value = "weight")
head(rate.long)

rate.long%>%
  group_by(group) %>%
  get_summary_stats(weight, type = "median_iqr")
bxp <- ggboxplot(rate.long, x = "group", y = "weight", color = "group", ylim = c(0, 1), add = "jitter", ylab = "Missclassification Rate", xlab = "Visit",
                 width = 0.7, font.label = list(size = 13, color = "black"))+ stat_compare_means(paired = TRUE)
bxp
ggsave(file = "./Figures/wilcoxontest-m2&3.png")





