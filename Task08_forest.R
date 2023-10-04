setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task08_trees")
#tell R that time is in UTC
Sys.setenv(TZ="GMT")

library("partykit")
data("airquality")


# Step 1: Data preparation
airquality$DayOfYear <- as.POSIXlt(paste(1973, airquality$Month, airquality$Day), format = "%Y %m %d")$yday
airquality$Ozone <- airquality$Ozone * 2.687155923 # Convert Langley to Joule per square meter
airquality$Wind <- airquality$Wind * 0.44704 # Convert miles per hour to meters per second
airquality$Temp <- (airquality$Temp - 32) * 5/9 # Convert Fahrenheit to Celsius
airq <- subset(airquality, !is.na(Ozone), select = -c(Month, Day))
str(airq)

# Step 2: Fit forest
set.seed(2908)
airQcforest <- partykit::cforest(Ozone ~ ., data = airq, ntree = 100)

# Step 3: Examine and plot the resulting forest model
library(stablelearner)

airQcforest_st <- as.stabletree(airQcforest)
summary(airQcforest_st, original = FALSE)  # Interpret the summary


# Plot frequency of predictor selection
barplot(airQcforest_st, original = FALSE, xlab = "Predictor", ylab = "Frequency", cex.names = 0.8)

image(airQcforest_st, original = FALSE, cex.names = 0.8)  # Interpret the image; xlab = "Variables not selected", ylab = "Variables selected", 

plot(airQcforest_st, original = FALSE, select = c("Solar.R", "Wind", "Temp", "DayOfYear"))  # Interpret the plot

