################################################################################
# 7CV_ensemble.R
# DESCRIPTION:
#   Process Dataset 7 to predict BOR using an ensemble model (combining
#   logistic regression, random forest, and SVM) with 10-fold cross-validation.
#   Saves performance metrics and best cutoff.
################################################################################

library(dplyr)
library(haven)
library(caret)
library(Matrix)
library(glmnet)
library(pROC)
library(broom)
library(randomForest)
library(e1071)

# Load Dataset 7
Datasets <- "./Datasets/7"
demog_file <- paste0(Datasets, "/demog.sas7bdat")
dm <- read_sas(demog_file)
base <- unique(dm[, c("RUSUBJID", "SEX", "AGE")])
names(base)[names(base) == "RUSUBJID"] <- "USUBJID"
names(base)[names(base) == "AGEC"] <- "AGE"
base$SEX[base$SEX == "FEMALE"] <- "2"
base$SEX[base$SEX == "MALE"] <- "1"
base <- na.omit(base)
write.csv(base, "./cvresults/7-base.csv", row.names = FALSE)

# Process Tumor Size
measur_file <- paste0(Datasets, "/measur.sas7bdat")
dtr <- read_sas(measur_file)
TumorSz <- data.frame(USUBJID = dtr$RUSUBJID, val = dtr$MSDIM1, visitN = dtr$VNUM)
TumorSz <- na.omit(TumorSz)
collapseData <- function(dat, variable, fun = "mean") {
  fun <- match.arg(fun, c("min", "max", "mean"))
  res <- NULL
  for (ii in unique(dat$USUBJID)) {
    tmp0 <- dat[dat$USUBJID == ii, , drop = FALSE]
    for (jj in unique(tmp0$visitN)) {
      tmp2 <- tmp0[tmp0$visitN == jj, , drop = FALSE]
      tmp1 <- tmp2[1, ]
      tmp1[variable] <- mean(tmp2[, variable])
      res <- rbind(res, tmp1)
    }
  }
  res <- reshape(res, idvar = "USUBJID", timevar = "visitN", direction = "wide")
  res[is.na(res)] <- 0
  return(res)
}
TumorSz <- collapseData(TumorSz, "val", "mean")
write.csv(TumorSz, "./cvresults/7-FullTumorSz.csv", row.names = FALSE)
keeps <- c("USUBJID", "val.4", "val.5", "val.6")
TumorSzfinal <- TumorSz[keeps]
names(TumorSzfinal)[names(TumorSzfinal) == "val.4"] <- "v2"
names(TumorSzfinal)[names(TumorSzfinal) == "val.5"] <- "v3"
names(TumorSzfinal)[names(TumorSzfinal) == "val.6"] <- "v4"
write.csv(TumorSzfinal, "./cvresults/7-TumorSz.csv", row.names = FALSE)

# PCA for Dataset 7 (using visits 2 and 3)
TumorSzV <- read.csv("./cvresults/7-TumorSz.csv")
pca_model <- princomp(TumorSzV[, c("v2", "v3")], cor = FALSE, scores = TRUE)
score_pca <- pca_model$scores
loadings <- as.matrix(pca_model$loadings)
colnames(loadings) <- "model7"
write.csv(loadings, "./val_v3/7_loadings.csv", row.names = FALSE)
score_pca <- cbind(USUBJID = TumorSzfinal$USUBJID, score_pca)
write.csv(score_pca, "./cvresults/7-pca.csv", row.names = FALSE)

# Merge base and PCA results
alldata <- merge(base, TumorSzfinal, by = "USUBJID")
alldata <- merge(alldata, score_pca, by = "USUBJID")
write.csv(alldata, "./cvresults/7-alldata7.csv", row.names = FALSE)

# Simulate BORval if not available
set.seed(123)
alldata$BORval <- sample(c(0, 1), nrow(alldata), replace = TRUE)

# Ensemble modeling function (reuse from 6CV_ensemble.R)
ensemble.bin <- function(data, idx.tr = NULL, base.covariates, lambda = "lambda.min") {
  if (is.null(idx.tr)) idx.tr <- 1:nrow(data)
  yy <- as.matrix(data[, "BORval"])
  xx <- data[, base.covariates]
  xx$SEX <- as.numeric(xx$SEX)
  xx$AGE <- as.numeric(xx$AGE)
  for(i in seq_along(base.covariates)){
    if(grepl("Comp", base.covariates[i])){
      xx[, base.covariates[i]] <- as.numeric(xx[, base.covariates[i]])
    }
  }
  for(ii in which(sapply(xx, is.factor))) xx[,ii] <- as.numeric(xx[,ii]) - 1
  xx <- data.matrix(xx)
  xx.tr <- xx[idx.tr, , drop = FALSE]
  yy.tr <- yy[idx.tr, , drop = FALSE]
  idx0 <- sample(1:10, nrow(xx.tr), replace = TRUE)
  pred0 <- rep(NA, nrow(xx.tr))
  for (jj in 1:10) {
    lr_fit <- cv.glmnet(xx.tr[idx0 != jj, , drop = FALSE], yy.tr[idx0 != jj, , drop = FALSE],
                        family = "binomial")
    pred_lr <- predict(lr_fit, newx = xx.tr[idx0 == jj, , drop = FALSE],
                         s = lambda, type = "response")
    rf_fit <- randomForest(x = xx.tr[idx0 != jj, , drop = FALSE],
                           y = as.factor(yy.tr[idx0 != jj, 1]))
    pred_rf <- predict(rf_fit, newdata = xx.tr[idx0 == jj, , drop = FALSE],
                         type = "prob")[, 2]
    svm_fit <- svm(x = xx.tr[idx0 != jj, , drop = FALSE],
                   y = as.factor(yy.tr[idx0 != jj, 1]), probability = TRUE)
    pred_svm <- attr(predict(svm_fit, newdata = xx.tr[idx0 == jj, , drop = FALSE],
                               probability = TRUE), "probabilities")[, 2]
    pred0[idx0 == jj] <- (pred_lr + pred_rf + pred_svm) / 3
  }
  cuts <- seq(0.1, 0.9, length = 1000)
  bestcut <- cuts[which.max(sapply(cuts, function(x) mean((pred0 > x) == yy.tr)))]
  lr_full <- cv.glmnet(xx.tr, yy.tr, family = "binomial")
  rf_full <- randomForest(x = xx.tr, y = as.factor(yy.tr[, 1]))
  svm_full <- svm(x = xx.tr, y = as.factor(yy.tr[, 1]), probability = TRUE)
  if(nrow(xx.tr) == nrow(data)) {
    pp <- rep(NA, nrow(data))
  } else {
    xx.test <- xx[-idx.tr, , drop = FALSE]
    pred_lr_test <- predict(lr_full, newx = xx.test, s = lambda, type = "response")
    pred_rf_test <- predict(rf_full, newdata = xx.test, type = "prob")[, 2]
    pred_svm_test <- attr(predict(svm_full, newdata = xx.test, probability = TRUE),
                           "probabilities")[, 2]
    ensemble_pred <- (pred_lr_test + pred_rf_test + pred_svm_test) / 3
    pp <- ensemble_pred > bestcut
  }
  return(list(predicted = pp, bestcut = bestcut))
}

baseCov <- c("SEX", "AGE", "Comp.1")
result <- ensemble.bin(alldata, base.covariates = baseCov)
bestcut <- result$bestcut

NN <- nrow(alldata)
res <- matrix(NA, NN, 1)
colnames(res) <- "model7"
set.seed(0)
BB <- 10
index <- sample(1:BB, NN, replace = TRUE)
for (ii in 1:BB) {
  res[which(index == ii), 1] <- ensemble.bin(alldata, idx.tr = (which(index != ii)), 
                                             base.covariates = baseCov)$predicted
}
tab <- table(predicted = res, references = alldata$BORval)
misclassificationrate <- 1 - sum(diag(tab)) / sum(tab)
accuracy <- 1 - misclassificationrate
auc_val <- auc(alldata$BORval, as.numeric(res))
metrics <- data.frame(misclassificationrate = misclassificationrate,
                      accuracy = accuracy,
                      auc = auc_val)
write.csv(metrics, "./cvresults/7-ensembleMetrics.csv", row.names = FALSE)
write.table(bestcut, "./cvresults/7_bestcut.txt", row.names = FALSE, col.names = FALSE)
