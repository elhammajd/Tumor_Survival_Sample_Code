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
methodone1<-read.csv("./Resultmethod1/misclassificationrate_V3.csv")
methodone11<- methodone1[!grepl('model', methodone1[,1]),]
methodone2<-read.csv("./Resultmethod1/misclassificationrate_V4.csv")
methodone22<- methodone2[!grepl('model', methodone2[,1]),]
methodoneA<- data.frame(weight= as.numeric(methodone11))
methodoneA$group<- c("visit3")
methodoneB<- data.frame(weight= as.numeric(methodone22))
methodoneB$group<- c("visit4")
methodone<-  rbind.data.frame(methodoneA, methodoneB)
rate<-methodone
rate.long<- rate %>%
  gather(key = "group", value = "weight")
head(rate.long)

rate.long%>%
  group_by(group) %>%
  get_summary_stats(weight, type = "median_iqr")
bxp <- ggboxplot(rate.long, x = "group", y = "weight", color = "group", ylim = c(0, 0.7), add = "jitter", ylab = "Missclassification Rate", xlab = "Visit",
                 width = 0.7, font.label = list(size = 13, color = "black"))+ stat_compare_means(paired = TRUE)
bxp
ggsave(file = "./Figures/wilcoxontest-m1.png")