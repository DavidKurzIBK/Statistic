### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task04")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")


library(zoo)
linz <- readRDS("LinzHoersching_obs_ECMWF_2009-2022.rds")
linz <- as.data.frame(linz)
linz <- cbind.data.frame(datetime = rownames(linz), linz)
rownames(linz) <- 1:nrow(linz)

write.csv(linz, "LinzHoersching_obs_ECMWF_2009-2022.csv")

##############Task 1: #######################################################
##############data preparation##############################################
# Remove NA values from location$rr6hObs column
linz <- na.omit(linz)

# Add new column wet with binary variables
linz$wet <- ifelse(linz$rr6hObs > 0.01, 1, 0)
linz$wet_model <- ifelse(linz$tp6h > 0.01, 1, 0)


##############Base Rate: #####################################################
# Calculate the number of wet mornings
num_wet_mornings <- sum(linz$rr6hObs > 0)
num_wet_mornings_model <- sum(linz$tp6h > 0)


# Calculate the total number of mornings
total_mornings <- length(linz$rr6hObs)
total_mornings_model <- length(linz$tp6h)


# Calculate the base rate of wet mornings
base_rate <- num_wet_mornings / total_mornings
base_rate_model <- num_wet_mornings_model / total_mornings_model


# Print the base rate of wet mornings
print(base_rate)

# base rate: 0.233002


#############Splitting into training and testing part: ########################
# Subset the even mornings for training
trainLinz <- subset(linz,(index(linz) %% 2 == 0))

# Subset the odd mornings for testing
LinzTest <- subset(linz, (index(linz) %% 2 == 1))


###########Classify: ############################################################
# Fit a logistic regression with ECMWF 6-h precip sum as predictor 
train_fit_1 <- glm(wet ~ tp6h, family = binomial, data = trainLinz, method = "glm.fit")
train_fit_multi <- glm(wet ~ tp6h + lsp6h + tcc, family = binomial, data = trainLinz, method = "glm.fit")

test_fit_1 <- glm(wet ~ tp6h, family = binomial, data = LinzTest, method = "glm.fit")
test_fit_multi <- glm(wet ~ tp6h + lsp6h + tcc, family = binomial, data = LinzTest, method = "glm.fit")

# Print the summary of the logistic regression model
summary(train_fit_1)
summary(train_fit_multi)

summary(test_fit_1)
summary(test_fit_multi)


###########goodness of fit: ######################################################
#Deviance Residuals: 
#       Min          1Q      Median          3Q         Max  
#-1.449e-04  -2.100e-08  -2.100e-08  -2.100e-08   1.740e-04  

#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -54.75    1362.74   -0.04    0.968
#rr6hObs       363.78    9143.90    0.04    0.968

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 2.4634e+03  on 2514  degrees of freedom
#Residual deviance: 3.6460e-06  on 2513  degrees of freedom
#AIC: 4

#Number of Fisher Scoring iterations: 25


##################Predict:#########################################################
# Make predictions on the training data set
predictTrain <- predict(train_fit_1, newdata = trainLinz, type="response")
predictTrain_multi <- predict(train_fit_multi, newdata = trainLinz, type="response")

# Make predictions on the test data set
predictTest <- predict(test_fit_1, LinzTest, type="response")
predictTest_multi <- predict(test_fit_multi, LinzTest, type="response")


################Confusion matrix:################################################

#library(cvms)
library(tibble)   # tibble()


# Compute the confusion matrix for predictions on the training data set
# 1 = wet, 0 = not wet
trainPred <- ifelse(predictTrain > 0.5, 1,0)
trainPred_multi <- ifelse(predictTrain_multi > 0.5, 1,0)

#trainTruth <- ifelse(trainLinz$wet == 0,0,0)

trainCM <- table(Predicted=trainPred, Truth=trainLinz$wet, dnn=c("Predicted", "Truth"), deparse.level=2)
trainCM_multi <- table(Predicted=trainPred_multi, Truth=trainLinz$wet, dnn=c("Predicted", "Truth"), deparse.level=2)

cfm_train <- as_tibble(trainCM)
cfm_train_multi <- as_tibble(trainCM_multi)

# Compute the confusion matrix for predictions on the test data set
testPred <- ifelse(predictTest > 0.5, 1,0)
testPred_multi <- ifelse(predictTest_multi > 0.5, 1,0)

#testTruth <- ifelse(LinzTest$wet == 0,0,0)

testCM <- table(Predicted=testPred, Truth=LinzTest$wet, dnn=c("Predicted", "Truth"), deparse.level=2)
testCM_multi <- table(Predicted=testPred_multi, Truth=LinzTest$wet, dnn=c("Predicted", "Truth"), deparse.level=2)

cfm_test <- as_tibble(testCM)
cfm_test_multi <- as_tibble(testCM_multi)

print(cfm_test)
print(cfm_train)
print(cfm_test_multi)
print(cfm_train_multi)



########ROC-curve: ###############################################################
library(ROCit)

# create rocit object with empirical method
rocEmp_train <- rocit(method = "empirical", score = coredata(predictTrain), class = trainLinz$wet)
rocEmp_train_multi <- rocit(method = "empirical", score = coredata(predictTrain_multi), class = trainLinz$wet)

rocEmp_test <- rocit(method = "empirical", score = coredata(predictTest), class = LinzTest$wet)
rocEmp_test_multi <- rocit(method = "empirical", score = coredata(predictTest_multi), class = LinzTest$wet)


# plot ROC curves for training and test data
plot(rocEmp_train$FPR, rocEmp_train$TPR, type = "l", col = "blue", xlab = "False Positive Rate",
     ylab = "True Positive Rate", main = "ROC Curves")
lines(rocEmp_test$FPR, rocEmp_test$TPR, col = "red")
legend("bottomright", legend = c(paste("Test AUC:", round(rocEmp_test$AUC, 3)),
                                 paste("Train AUC:", round(rocEmp_train$AUC, 3))),
       lty = 1, col = c("red", "blue"), cex = 0.8)

# plot ROC curves for training and test data
plot(rocEmp_train_multi$FPR, rocEmp_train_multi$TPR, type = "l", col = "blue", xlab = "False Positive Rate",
     ylab = "True Positive Rate", main = "ROC Curves with multi predictors")
lines(rocEmp_test_multi$FPR, rocEmp_test_multi$TPR, col = "red")
legend("bottomright", legend = c(paste("Test AUC:", round(rocEmp_test_multi$AUC, 3)),
                                 paste("Train AUC:", round(rocEmp_train_multi$AUC, 3))),
       lty = 1, col = c("red", "blue"), cex = 0.8)



################PR-curve: #####################################################
library(PRROC)


# Calculate precision-recall values for test data
pr <- pr.curve(predictTest, predictTrain, curve = T)
pr_mu <- pr.curve(predictTest_multi, predictTrain_multi, curve = T)

# create a new plot with x and y axis labels
plot(pr, xlab = "Recall", ylab = "Precision", main = "PR-Curve")
points(predictTest, type = "p", pch = 19, cex = 1, col = "magenta")
plot(pr_mu, xlab = "Recall", ylab = "Precision", main = "PR-Curve with multi predictors")
points(predictTest_multi, type = "p", pch = 19, cex = 1, col = "magenta")


