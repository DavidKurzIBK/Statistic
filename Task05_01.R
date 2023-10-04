### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task05")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")


library(zoo)
linz <- readRDS("LinzHoersching_obs_ECMWF_2009-2022.rds")
linz <- as.data.frame(linz)
linz <- cbind.data.frame(datetime = rownames(linz), linz)
rownames(linz) <- 1:nrow(linz)

##############Task 1: #######################################################
##############data preparation##############################################
# Remove NA values from location
linz <- na.omit(linz)

# Add new column wet with binary variables
linz$wet <- ifelse(linz$rr6hObs > 0.1, 1, 0)
linz$drenched <- ifelse(linz$rr6hObs >= 12.0, 1, 0)
linz$model_wet <- ifelse(linz$tp6h > 0.1, 1, 0)
linz$model_drenched <- ifelse(linz$tp6h >= 12.0, 1, 0)


#############Splitting into training and testing part: ########################
# Subset the even mornings for training
trainLinz <- subset(linz,(index(linz) %% 2 == 0))

# Subset the odd mornings for testing
LinzTest <- subset(linz, (index(linz) %% 2 == 1))


# Fit a logistic regression with ECMWF 6-h precip sum as predictor 
train_wet <- glm(wet ~ tp6h, family = binomial, data = trainLinz, method = "glm.fit")
train_wet_multi <- glm(wet ~ tp6h + lsp6h + tcc, family = binomial, data = trainLinz, method = "glm.fit")

test_wet <- glm(wet ~ tp6h, family = binomial, data = LinzTest, method = "glm.fit")
test_wet_multi <- glm(wet ~ tp6h + lsp6h + tcc, family = binomial, data = LinzTest, method = "glm.fit")


# Fit a logistic regression with ECMWF 6-h >= 12.0 precip sum as predictor 
train_drenched <- glm(drenched ~ tp6h, family = binomial, data = trainLinz, method = "glm.fit")
train_drenched_multi <- glm(drenched ~ tp6h + lsp6h + tcc, family = binomial, data = trainLinz, method = "glm.fit")

test_drenched <- glm(drenched ~ tp6h, family = binomial, data = LinzTest, method = "glm.fit")
test_drenched_multi <- glm(drenched ~ tp6h + lsp6h + tcc, family = binomial, data = LinzTest, method = "glm.fit")


##################Predict:#########################################################
# Make predictions on the training data set
predictTrain_wet <- predict(train_wet, newdata = trainLinz, type="response")
predictTrain_wet_multi <- predict(train_wet_multi, newdata = trainLinz, type="response")

# Make predictions on the test data set
predictTest_wet <- predict(test_wet, LinzTest, type="response")
predictTest_wet_multi <- predict(test_wet_multi, LinzTest, type="response")

# Make predictions on the training data set
predictTrain_drenched <- predict(train_drenched, newdata = trainLinz, type="response")
predictTrain_drenched_multi <- predict(train_drenched_multi, newdata = trainLinz, type="response")

# Make predictions on the test data set
predictTest_drenched <- predict(test_drenched, LinzTest, type="response")
predictTest_drenched_multi <- predict(test_drenched_multi, LinzTest, type="response")



########ROC-curve: ###############################################################
library(ROCit)

# create rocit object with empirical method
rocEmp_train_wet <- rocit(method = "empirical", score = coredata(predictTrain_wet), class = trainLinz$wet)
rocEmp_train_wet_multi <- rocit(method = "empirical", score = coredata(predictTrain_wet_multi), class = trainLinz$wet)
rocEmp_train_wet_c <- rocit(score = coredata(predictTrain_wet), class = trainLinz$wet, method = "non")
rocEmp_train_wet_multi_c <- rocit(score = coredata(predictTrain_wet_multi), class = trainLinz$wet, method = "non")

rocEmp_test_wet <- rocit(method = "empirical", score = coredata(predictTest_wet), class = LinzTest$wet)
rocEmp_test_wet_multi <- rocit(method = "empirical", score = coredata(predictTest_wet_multi), class = LinzTest$wet)
rocEmp_test_wet_c <- rocit(score = coredata(predictTest_wet), class = LinzTest$wet, method = "non")
rocEmp_test_wet_multi_c <- rocit(score = coredata(predictTest_wet_multi), class = LinzTest$wet, method = "non")


# create rocit object with empirical method
rocEmp_train_drenched <- rocit(method = "empirical", score = coredata(predictTrain_drenched), class = trainLinz$drenched)
rocEmp_train_drenched_multi <- rocit(method = "empirical", score = coredata(predictTrain_drenched_multi), class = trainLinz$drenched)
rocEmp_train_drenched_c <- rocit(score = coredata(predictTrain_drenched), class = trainLinz$drenched, method = "non")
rocEmp_train_drenched_multi_c <- rocit(score = coredata(predictTrain_drenched_multi), class = trainLinz$drenched, method = "non")


rocEmp_test_drenched <- rocit(method = "empirical", score = coredata(predictTest_drenched), class = LinzTest$drenched)
rocEmp_test_drenched_multi <- rocit(method = "empirical", score = coredata(predictTest_drenched_multi), class = LinzTest$drenched)
rocEmp_test_drenched_c <- rocit(score = coredata(predictTest_drenched), class = LinzTest$drenched, method = "non")
rocEmp_test_drenched_multi_c <- rocit(score = coredata(predictTest_drenched_multi), class = LinzTest$drenched, method = "non")


roc90_train_wet <- ciROC(rocEmp_train_wet, level = 0.9, nboot = 100)
roc90_train_wet_multi <- ciROC(rocEmp_train_wet_multi, nboot = 100, level = 0.9)

roc90_train_drenched <- ciROC(rocEmp_train_drenched, nboot = 100, level = 0.9)
roc90_train_drenched_multi <- ciROC(rocEmp_train_drenched_multi, nboot = 100, level = 0.9)

roc90_test_wet <- ciROC(rocEmp_test_wet, nboot = 100, level = 0.9)
roc90_test_wet_multi <- ciROC(rocEmp_test_wet_multi, nboot = 100, level = 0.9)

roc90_test_drenched <- ciROC(rocEmp_test_drenched, nboot = 100, level = 0.9)
roc90_test_drenched_multi <- ciROC(rocEmp_test_drenched_multi, nboot = 100, level = 0.9)

# plot ROC curves for training and test data
plot(roc90_train_wet, col = "blue", ci.col = "gray", ci.lty = 2, lwd = 2) # main = "ROC Curves of wet days training & test data")
lines(roc90_test_wet$FPR, roc90_test_wet$TPR, col = "red", lwd = 2)

plot(roc90_train_drenched, col = "blue", ci.col = "gray", ci.lty = 2, lwd = 2)#, main = "ROC Curves of drenched days training & test data")
lines(roc90_test_drenched$FPR, roc90_test_drenched$TPR, col = "red", lwd = 2)



library(colorspace)
# Create color palette
colRoc <- sequential_hcl(10, palette = "Hawai")

# Assign colors to each threshold value
rocEmp_test_wet_c$col <- colRoc[findInterval(rocEmp_test_wet_c$Cutoff, c(0, seq(0.1, 0.9, by = 0.1), 1))]
rocEmp_test_drenched_c$col <- colRoc[findInterval(rocEmp_test_drenched_c$Cutoff, c(0, seq(0.1, 0.9, by = 0.1), 1))]

rocEmp_train_wet_c$col <- colRoc[findInterval(rocEmp_train_wet_c$Cutoff, c(0, seq(0.1, 0.9, by = 0.1), 1))]
rocEmp_train_drenched_c$col <- colRoc[findInterval(rocEmp_train_drenched_c$Cutoff, c(0, seq(0.1, 0.9, by = 0.1), 1))]


# Plot ROC curve with color-coded thresholds
# plot ROC curves for training and test data
#par(mfrow = c(2, 2))
plot(rocEmp_test_wet_c$FPR,rocEmp_test_wet_c$TPR ,type = "p", pch = 19, cex = 0.5, col = rocEmp_test_wet_c$col, xlab = "1 ??? Specificity (FPR)", ylab = "Sensitivity (TPR)", main = "ROC colored prob thresh.: test data")
legend("bottomright", legend = c(paste("Test AUC:", round(rocEmp_test_wet_c$AUC, 3))), cex = 0.8)
plot(rocEmp_test_drenched_c$FPR, rocEmp_test_drenched_c$TPR, type = "p", pch = 19, cex = 0.5, col = rocEmp_test_drenched_c$col, xlab = "1 ??? Specificity (FPR)", ylab = "Sensitivity (TPR)", main = "ROC colored prob thresh.: drenched test data")
legend("bottomright", legend = c(paste("Test AUC:", round(rocEmp_test_drenched_c$AUC, 3))), cex = 0.8)
plot(rocEmp_train_wet_c$FPR, rocEmp_train_wet_c$TPR, type = "p", pch = 19, cex = 0.5, col = rocEmp_train_wet_c$col, xlab = "1 ??? Specificity (FPR)", ylab = "Sensitivity (TPR)", main = "ROC colored prob thresh.: train data")
legend("bottomright", legend = c(paste("Train AUC:", round(rocEmp_train_wet_c$AUC, 3))), cex = 0.8)
plot(rocEmp_train_drenched_c$FPR, rocEmp_train_drenched_c$TPR, type = "p", pch = 19, cex = 0.5, col = rocEmp_train_drenched_c$col, xlab = "1 ??? Specificity (FPR)", ylab = "Sensitivity (TPR)", main = "ROC colored prob thresh.: train drechend data")
legend("bottomright", legend = c(paste("Train AUC:", round(rocEmp_train_drenched_c$AUC, 3))), cex = 0.8)

