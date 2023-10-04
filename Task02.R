### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task02")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")

##############Exercise 1:#######################################################
library(zoo)
linz <- readRDS("LinzHoersching_obs_ECMWF_2009-2022.rds")
linz <- as.data.frame(linz)
linz <- cbind.data.frame(datetime = rownames(linz), linz)
rownames(linz) <- 1:nrow(linz)

#convert kelvin to celsius with linz[,c("t2m","t850")]
#Select the columns "t2m" and "t850" from the data frame "linz"
temperatures_kelvin <- linz[,c("t2m","t850")]

# Convert the temperatures from Kelvin to Celsius
temperatures_celsius <- temperatures_kelvin - 273.15

# Rename the columns to indicate that the temperatures are in Celsius
colnames(temperatures_celsius) <- c("t2m_celsius", "t850_celsius")

# Add the temperatures in Celsius to the data frame "linz"
linz <- cbind(linz, temperatures_celsius)

##################Exercise 2 - 7:#######################################################

# Fit a simple linear regression
lm.fit <- lm(t2mObs ~ t2m_celsius, data = linz)

# Print the summary of the linear model
print(coef(lm.fit))
summary(lm.fit)

# Intercept       t2m_celsius slope 
# -1.669422                1.037230 

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-9.9010 -0.9523  0.2410  1.2517  7.3054 

# Coefficients:
#             Estimate  Std.-Error   t-value      Pr(>|t|)    
#Intercept   -1.669422    0.051533     -32.4    <2e-16 ***
#t2m_celsius  1.037230    0.003078     337.0    <2e-16 ***

#Residual standard error: 1.907 on 5043 degrees of freedom
#(52 observations deleted due to missingness)


#Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9575

#4 & 5: If the R-squared values is approaching to 1, it means that our predictors explain much #of our data variability (much of our variance). 
#R-squared = 0.9575, so the relationship between predictors and response is very strong #positive!! 


#F-statistic: 1.136e+05 on 1 and 5043 DF,  p-value: < 2.2e-16
#6.The F-statistic quantifies the difference in error between the model, assuming your null #hypothesis (i.e. ??i=0)nand your fitted model. It simply a formal way of determining if a #coefficient belongs to the model. If the p-value recieved from the F-testing statistics is #small then we reject the null (because the probability of occurrence is so small), and say #??i???0 here.
#So assuming ??i=0 and rejecting the null hypothesis here must have to something that we expect #the p-value small.

# confidence interval: 
confint(lm.fit)


confint(lm.fit, level = 0.95)

#                 2.5%    97.5 %
#(Intercept) -1.770450 -1.568394
#t2m_celsius  1.031196  1.043264


intercept_kelvin = -1.568394 - (-1.770450)
slope_change = (1.043264/1.031196)*100

################ Excercise 8: #################################
# create scatterplot with small symbols
plot(linz$t2m_celsius, linz$t2mObs, pch = 20, cex = 0.1, xlab = "t2m", ylab = "t2mObs")

# add red linear regression line
lm.fit <- lm(linz$t2mObs ~ linz$t2m_celsius)
abline(lm.fit, col = "red")

# add pivot point
mean_x <- mean(linz$t2m_celsius, na.rm = TRUE)
mean_y <- mean(linz$t2mObs, na.rm = TRUE)
points(mean_x, mean_y, pch = 20,cex = 2, col = "orange")

################ Excercise 9: #################################

h_stat <- hatvalues(lm.fit)
which.max(h_stat)

h_stat_max_1 <- h_stat[1132]
h_stat_max_2 <- h_stat[1129]
h_stat_mean <- mean(h_stat)
diff_h_stat <- h_stat_max_2 - h_stat_mean

h_stat_table <- as.data.frame(h_stat)
hist(h_stat)

################ Excercise 10: #################################

# create plot of studentized residuals
plot(predict(lm.fit), rstudent(lm.fit), pch = 20, cex = 0.5, xlab = "Fitted Values", ylab = "Studentized Residuals")

# add horizontal lines at y = 3 and y = -3 for checking values lying between +/- 3
abline(h = 3, col = "red", lty = 2)
abline(h = -3, col = "red", lty = 2)


################ Excercise 11: #################################
# set up 2x2 graphics layout
#par(mfrow = c(2,2))

# plot diagnostic plots for lm.fit
plot(lm.fit)

# reset graphics layout to default (1x1)
#par(mfrow = c(1,1))

