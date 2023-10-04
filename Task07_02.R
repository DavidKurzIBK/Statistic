### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task07")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")
library(zoo)
library(dplyr)

load("Zuers.rda")
ls()


# Convert t2mObs colum for the period 2009-2021 to a data frame
#zP <- as.data.frame(precip$obs)
# Add date information from the zoo-object location to ibkT$date
#index(precip) <- zP$date
#zP$date <- index(precip)

# Compute sequential numbers of each day in the year (Julian days)
precip$yday <- as.POSIXlt(precip$date)$yday

# Filter the dataframe for a specific time period
start_date <- as.POSIXct("2009-01-02 12:00:00")
end_date <- as.POSIXct("2022-12-16 12:00:00")

filtered_precip <- precip %>%
  filter(date >= start_date & date <= end_date)


# Check for any days without precip values
if (any(is.na(filtered_precip$obs))) {
  filtered_precip <- na.omit(filtered_precip)  # Remove days without precip values
}

# Make all years 365 days long by dropping the last day of a leap year
filtered_precip <- subset(filtered_precip, !(as.POSIXlt(date)$isdst == 1 & as.POSIXlt(date)$yday == 365))

# Plot the precip time series as points
plot(filtered_precip$date, filtered_precip$obs, type = "p", xlab = "Date", ylab = "Preciptition", main = "observed precipitation Zuers")

# Compute daily average precip (Pmean) using aggregate()
Pmean <- aggregate(filtered_precip$obs, by = list(filtered_precip$yday), FUN = mean)

# Compute daily median precip (Pmedian) using aggregate()
Pmedian <- aggregate(filtered_precip$obs, by = list(filtered_precip$yday), FUN = median)

# Plot daily precip, Pmean, and Pmedian as points for days 1...365
plot(filtered_precip$yday, filtered_precip$obs, type = "p", xlab = "Day of Year", ylab = "Precipitiation",
     main = "Daily Precippitation")
points(Pmean$Group.1, Pmean$x, type = "p", col = "red")
points(Pmedian$Group.1, Pmedian$x, type = "p", col = "blue")
legend("topleft", legend = c("Daily Precipitation", "Pmean", "Pmedian" ),
       col = c("black", "red", "blue"),pch = c(1, 1, 1))#,
      # pch = c(1, 1, 1), lty = c(NA, NA, NA))


# Compute running mean of daily averages (Pmean) using a 15-day filter
Pmean_running <- rollmean(Pmean$x, k = 15, fill = NA, align = "center")

# Compute running median of daily medians (Pmedian) using a 15-day filter
Pmedian_running <- rollmedian(Pmedian$x, k = 15, fill = NA, align = "center")

# Add running mean and running median curves to the plot
plot(filtered_precip$yday, filtered_precip$obs, type = "p", xlab = "Day of Year", ylab = "Precipitiation",
     main = "Daily Precipitaition")
lines(Pmean$Group.1, Pmean_running, col = "red")
lines(Pmedian$Group.1, Pmedian_running, col = "blue")

# Compute smooth daily average temperatures using loess()
Psmooth_loess <- loess(Pmean$x ~ Pmean$Group.1, span = 1/3)

# Add loess curve to the plot
lines(Pmean$Group.1, predict(Psmooth_loess), col = "green")

legend("topleft", legend = c("Daily Precipitation", "Running Mean","Running Median", "Loess"),
       col = c("black", "red", "blue", "green"),
       pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1))



# Compute smooth daily average temperatures using GAM with cyclic spline
library(mgcv)
fitG <- gam(obs ~ s(yday, bs = "cc"), data = filtered_precip)

# Extract effective degrees of freedom and intercept from the GAM model
edf <- summary(fitG)$s.table[, "edf"]
intercept <- coef(fitG)[1]

# Add GAM curve to the plot
plot(fitG, shift = intercept, col = "orange", ylim = c(0, 30))
points(Pmean$Group.1, Pmean$x, type = "p", col = "red")

# Add a legend to the plot
legend("topleft", legend = c("Pmean", "GAM"),
       col = c("red", "orange"),
       pch = c(1, NA), lty = c(NA, 1))






