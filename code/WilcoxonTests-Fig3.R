# ==================================================================================================
# DESCRIPTION:
# This script evaluates the pairwise comparisons of misclassification rates of predictive models 
# between three and four visits using Wilcoxon tests.
# ==================================================================================================
# INPUTS:
# - Misclassification rates for visit three and four in method one.
# - Misclassification rates for visit three and four in method two and three.
# ==================================================================================================
# OUTPUTS:
# - Result of Wilcoxon test for three and four visits in method one.
# - Result of Wilcoxon test for three and four visits in method two and three.
# ==================================================================================================
# Codes produced by:
# - Elham Majd
# ==================================================================================================
# HISTORY:
# - Creation: Feb/2022
# ==================================================================================================
# STATEMENT:
# This file is part of a project entitled "Predicting Tumor Response and 
# Overall Survival Using Early Outcomes and Baseline Characteristics"
# ==================================================================================================

# Load required packages
install.packages("ggstatsplot", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(ggpubr)
library(rstatix)
library(ggstatsplot)

# =========================================================================
# Load misclassification rate data
# =========================================================================
method_one_v3 <- read.csv("./Resultmethod1/misclassificationrate_V3.csv")
method_one_v4 <- read.csv("./Resultmethod1/misclassificationrate_V4.csv")

method_one_v3_clean <- method_one_v3[!grepl('model', method_one_v3[,1]), ]
method_one_v4_clean <- method_one_v4[!grepl('model', method_one_v4[,1]), ]

# Prepare data for plotting and analysis
method_one_v3_df <- data.frame(weight = as.numeric(method_one_v3_clean), group = "visit3")
method_one_v4_df <- data.frame(weight = as.numeric(method_one_v4_clean), group = "visit4")
combined_data <- rbind(method_one_v3_df, method_one_v4_df)

# =========================================================================
# Perform Wilcoxon test and create boxplot
# =========================================================================
# Summary statistics
summary_stats <- combined_data %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "median_iqr")

print(summary_stats)

# Create boxplot with jitter and Wilcoxon test results
boxplot <- ggboxplot(
  combined_data, 
  x = "group", 
  y = "weight", 
  color = "group", 
  ylim = c(0, 0.7), 
  add = "jitter", 
  ylab = "Misclassification Rate", 
  xlab = "Visit", 
  width = 0.7, 
  font.label = list(size = 13, color = "black")
) + stat_compare_means(paired = TRUE)

# Save the plot
ggsave(filename = "./Figures/wilcoxontest-m1.png", plot = boxplot)

# Display the boxplot
print(boxplot)
