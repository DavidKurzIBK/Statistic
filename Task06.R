### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task06")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")


library(zoo)
library(lmSubsets)
library(boot)

data("IbkTemperature", package = "lmSubsets")
?IbkTemperature


# Step 1: Create lagged temperature variable and remove missing values
IbkTemperature$lagTemp <- lag(IbkTemperature$temp, k = 1)
IbkTemperature <- na.omit(IbkTemperature)

# Step 2: Build persistence reference model (lmPers)
lmPers <- lm(temp ~ lagTemp, data = IbkTemperature)

# Step 3: Build one-variable reference model with GEFS predictor (lmT2m)
lmT2m <- lm(temp ~ t2m, data = IbkTemperature)

# Check model summaries
print(summary(lmPers))
print(summary(lmT2m))

# Comment on the results


# Step 4: Fit models with glm() and estimate CV-errors
#library(delta)
glmPers <- glm(temp ~ lagTemp, data = IbkTemperature)
glmT2m <- glm(temp ~ t2m, data = IbkTemperature)

# Estimate CV-errors with cv.glm() using 10-fold cross-validation
cv_errors_glmPers <- cv.glm(IbkTemperature, glmPers, K = 10)$delta
cv_errors_glmT2m <- cv.glm(IbkTemperature, glmT2m, K = 10)$delta

# Add "glm" to model names
names(cv_errors_glmPers) <- paste0(names(cv_errors_glmPers), "_glm")
names(cv_errors_glmT2m) <- paste0(names(cv_errors_glmT2m), "_glm")

# Print CV-errors
cv_errors_glmPers
cv_errors_glmT2m

# Comment on the results


# Step 5: Add time/seasonal terms to the models
#IbkTemperature$time <- 1:nrow(IbkTemperature)
#IbkTemperature$sin <- sin(2 * pi * IbkTemperature$time / 24)
#IbkTemperature$cos <- cos(2 * pi * IbkTemperature$time / 24)
#IbkTemperature$sin2 <- sin(4 * pi * IbkTemperature$time / 24)
#IbkTemperature$cos2 <- cos(4 * pi * IbkTemperature$time / 24)

# Fit models with time/seasonal terms
glmPers_time <- glm(temp ~ lagTemp + time + sin + cos + sin2 + cos2, data = IbkTemperature)
glmT2m_time <- glm(temp ~ t2m + time + sin + cos + sin2 + cos2, data = IbkTemperature)

# Estimate CV-errors with cv.glm() using 10-fold cross-validation
cv_errors_glmPers_time <- cv.glm(IbkTemperature, glmPers_time, K = 10)$delta
cv_errors_glmT2m_time <- cv.glm(IbkTemperature, glmT2m_time, K = 10)$delta

# Add "glm" to model names
names(cv_errors_glmPers_time) <- paste0(names(cv_errors_glmPers_time), "_glm")
names(cv_errors_glmT2m_time) <- paste0(names(cv_errors_glmT2m_time), "_glm")

# Print CV-errors
cv_errors_glmPers_time
cv_errors_glmT2m_time





## fit a simple climatological model for the temperature
## with a linear trend and annual/bi-annual harmonic seasonal pattern
CLIM <- lm(temp ~ time + sin + cos + sin2 + cos2,data = IbkTemperature)

## fit a simple MOS with 2-meter temperature forecast in addition
## to the climatological model
MOS0 <- lm(temp ~ t2m + time + sin + cos + sin2 + cos2, data = IbkTemperature)

## graphical comparison and MOS summary
plot(temp ~ time, data = IbkTemperature, type = "l", col = "darkgray") #orig. ibkTemp
lines(fitted(MOS0) ~ time, data = IbkTemperature, col = "darkred")     #2m temp time fitted
lines(fitted(CLIM) ~ time, data = IbkTemperature, lwd = 2)             #ibkTemp annual/bi-annual harmonic seasonal fitted
#legend()
MOS0


# Use lmSelect() for best subset model selection
MOSPers_best <- lmSelect(temp ~ ., data = IbkTemperature, method = "exhaustive",
                         include = "lagTemp", penalty = "BIC", nbest = 20)
MOSPers_best_all <- lmSelect(temp ~ ., data = IbkTemperature, method = "exhaustive",
                           include = c("lagTemp", "t2m", "time", "sin", "cos", "sin2", "cos2"), penalty = "BIC", nbest = 20)

# Print the predictors included in the best model
#print(MOSPers_best$nbest)

# Visualize the model results using image()
image(MOSPers_best, hilite = 1, lab_hilite = "bold(lab)", pad_size = 2, pad_which = 2)
image(MOSPers_best_all, hilite = 1, lab_hilite = "bold(lab)", pad_size = 2, pad_which = 2)
plot(MOSPers_best) 
plot(MOSPers_best_all) 


# Print the coefficients of the best subset model
coef(MOSPers_best)
coef(MOSPers_best_all)


# Perform exhaustive search through all possible model configurations
MOS_all <- lmSubsets(temp ~ ., data = IbkTemperature, method = "exhaustive",include = c("lagTemp", "t2m", "time", "sin", "cos", "sin2", "cos2"), penalty = "BIC", size = 3:27, hilite_penalty = "BIC")

# Visualize the best subset models with respect to BIC using image()
image(MOS_all, hilite = 1, lab_hilite = "bold(lab)", pad_size = 2, pad_which = 2, hilite_penalty = "BIC")
plot(MOS_all)


# Print the coefficients of the best model
coef(MOS_all)


#install.packages("foreach")
library(glmnet)
library(foreach)

# Create the model matrix and response vector
x <- model.matrix(temp ~ ., data = IbkTemperature)[, -1]
y <- IbkTemperature$temp

# Fit Lasso using glmnet
lasso <- glmnet(x, y, alpha = 1)

# Perform cross-validation using cv.glmnet
cvfit <- cv.glmnet(x, y)


# Get the lambda for the minimal CV error
lambda_min <- cvfit$lambda.min

# Get the lambda for the most regularized model within one standard error
lambda_1se <- cvfit$lambda.1se

# Plot the cross-validation result

xlim = -10
ylim = 100
plot(cvfit)
plot.window(xlim, ylim)



# Get the number of predictors included for lambda_min and lambda_1se
num_predictors_min <- sum(coef(cvfit, s = lambda_min))
num_predictors_1se <- sum(coef(cvfit, s = lambda_1se))

# Print the number of predictors included for lambda_min and lambda_1se
print(num_predictors_min)
print(num_predictors_1se)

# Get the coefficients and names of predictors for lambda_min and lambda_1se
coef_min <- coef(cvfit, s = lambda_min)
coef_1se <- coef(cvfit, s = lambda_1se)
predictors_min <- names(coef_min)[coef_min !=0 ]
predictors_1se <- names(coef_1se)[coef_1se !=0 ]


# Compare the predictors included in Lasso with the ones obtained from best subset selection
print(MOSPers_best$nbest)
print(MOS_all$nbest)

