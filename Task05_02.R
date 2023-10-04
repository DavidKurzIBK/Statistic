### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task05")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")


library(zoo)


############# generating random data set###################################
set.seed(100)
#different random seed for repitition:
#set.seed(123)

x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

############ data scatterplot##############################################
plot(x, y, main="Scatterplot with seed(100) of X against Y", xlab="X values", ylab="Y values", col="red")

############ LLS regression####################################################

########### create data set
#set.seed(100)
#different random seed for repitition:
set.seed(123)

x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
data <- data.frame(x,y)


########### fit models
model1 <- glm(y ~ x, data = data, family = "gaussian")
model2 <- glm(y ~ x + I(x^2), data = data, family = "gaussian")
model3 <- glm(y ~ x + I(x^2) + I(x^3), data = data, family = "gaussian")
model4 <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = data, family = "gaussian")

# Compute LOOCV errors
library(boot)

cv_error1 <- cv.glm(data, model1)$delta[1]
cv_error2 <- cv.glm(data, model2)$delta[1]
cv_error3 <- cv.glm(data, model3)$delta[1]
cv_error4 <- cv.glm(data, model4)$delta[1]

# Print the LOOCV errors
print(paste0("LOOCV error for model 1: ", cv_error1))
print(paste0("LOOCV error for model 2: ", cv_error2))
print(paste0("LOOCV error for model 3: ", cv_error3))
print(paste0("LOOCV error for model 4: ", cv_error4))

# Create a sequence of X values
x_seq <- seq(min(data$x), max(data$x), length.out = 100)

# Compute predicted values for each model using the x_seq sequence()
y_pred1 <- predict(model1, newdata = data.frame(x = x_seq))
y_pred2 <- predict(model2, newdata = data.frame(x = x_seq))
y_pred3 <- predict(model3, newdata = data.frame(x = x_seq))
y_pred4 <- predict(model4, newdata = data.frame(x = x_seq))




##################################Plot the data and the four models######################################
plot(data$x, data$y, pch = 19, main = "4 polynomial regression models from seed123", xlab = "x", ylab = "y")
lines(x_seq, y_pred1, col = "red")
lines(x_seq, y_pred2, col = "green")
lines(x_seq, y_pred3, col = "blue")
lines(x_seq, y_pred4, col = "purple")
legend("bottom", legend = c("Model 1", "Model 2", "Model 3", "Model 4"), lty = 1, col = c("red", "green", "blue", "purple"), cex =  0.7)



