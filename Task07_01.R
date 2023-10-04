### Prepare data

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task07")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")
library(zoo)
ibk <- readRDS("ibkFlughafen_obs_ECMWF_2009-2022.rds")


# Convert t2mObs colum for the period 2009-2021 to a data frame
ibkT <- as.data.frame(ibk$t2mObs)


# Add date information from the zoo-object location to ibkT$date
ibkT$date <- index(ibk)

# Compute sequential numbers of each day in the year (Julian days)
ibkT$yday <- as.POSIXlt(ibkT$date)$yday


# Check for any days without temperatures
if (any(is.na(ibkT$ibk))) {
  ibkT <- na.omit(ibkT)  # Remove days without temperatures
}

# Make all years 365 days long by dropping the last day of a leap year
ibkT <- subset(ibkT, !(as.POSIXlt(date)$isdst == 1 & as.POSIXlt(date)$yday == 365))

# Plot the temperature time series as points
plot(ibkT$date, ibkT$ibk, type = "p", xlab = "Date", ylab = "Temperature", main = "2 m observed temperatures IBK")


# Compute daily average temperatures (Tmean) using aggregate()
Tmean <- aggregate(ibkT, by = list(ibkT$yday), FUN = mean)

# Compute daily median temperatures (Tmedian) using aggregate()
Tmedian <- aggregate(ibkT, by = list(ibkT$yday), FUN = median)

# Plot daily temperatures, Tmean, and Tmedian as points for days 1...365
plot(ibkT$yday, ibkT$ibk, type = "p", xlab = "Day of Year", ylab = "Temperature",
     main = "Daily Temperatures")
points(Tmean$Group.1, Tmean$ibk, type = "p", col = "red")
points(Tmedian$Group.1, Tmedian$ibk, type = "p", col = "blue")
legend("bottom", legend = c("Daily Temperatures", "Tmean", "Tmedian" ),
       col = c("black", "red", "blue"),pch = c(1, 1, 1))#,
      # pch = c(1, 1, 1), lty = c(NA, NA, NA))


# Compute running mean of daily averages (Tmean) using a 15-day filter
Tmean_running <- rollmean(Tmean$ibk, k = 15, fill = NA, align = "center")

# Compute running median of daily medians (Tmedian) using a 15-day filter
Tmedian_running <- rollmedian(Tmedian$ibk, k = 15, fill = NA, align = "center")

# Add running mean and running median curves to the plot
plot(ibkT$yday, ibkT$ibk, type = "p", xlab = "Day of Year", ylab = "Temperature",
     main = "Daily Temperatures")
lines(Tmean$Group.1, Tmean_running, col = "red")
lines(Tmedian$Group.1, Tmedian_running, col = "blue")

# Compute smooth daily average temperatures using loess()
Tsmooth_loess <- loess(Tmean$ibk ~ Tmean$Group.1, span = 1/3)

# Add loess curve to the plot
lines(Tmean$Group.1, predict(Tsmooth_loess), col = "green")

legend("bottom", legend = c("Daily Temperatures", "Running Mean","Running Median", "Loess"),
       col = c("black", "red", "blue", "green"),
       pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1))


# rename column: 
ibkT$t2mObs <- ibkT$ibk

# Compute smooth daily average temperatures using GAM with cyclic spline
library(mgcv)
fitG <- gam(t2mObs ~ s(yday, bs = "cc"), data = ibkT)

# Extract effective degrees of freedom and intercept from the GAM model
edf <- summary(fitG)$s.table[, "edf"]
intercept <- coef(fitG)[1]

# Add GAM curve to the plot
plot(fitG, shift = intercept, col = "orange", ylim = c(0, 30))
points(Tmean$Group.1, Tmean$ibk, type = "p", col = "red")

# Add a legend to the plot
legend("topleft", legend = c("Tmean", "GAM"),
       col = c("red", "orange"),
       pch = c(1, NA), lty = c(NA, 1))


# Load required packages
library(mgcv)
Sys.setenv(TZ="GMT")
library(zoo)
ibk <- readRDS("ibkFlughafen_obs_ECMWF_2009-2022.rds")


# Convert colum for the period 2009-2021 to a data frame
ibk <- as.data.frame(ibk)
ibk <- cbind.data.frame(date = rownames(ibk), ibk)


# Compute sequential numbers of each day in the year (Julian days)
ibk$yday <- as.POSIXlt(ibk$date)$yday


# Check for any days without temperatures
if (any(is.na(ibk$t2mObs))) {
  ibk <- na.omit(ibk)  # Remove days without temperatures
}

# Make all years 365 days long by dropping the last day of a leap year
ibk <- subset(ibk, !(as.POSIXlt(date)$isdst == 1 & as.POSIXlt(date)$yday == 365))


# Train and test period
train_start <- as.Date("2009-01-02")
train_end <- as.Date("2019-12-31")
test_start <- as.Date("2020-01-01")
test_end <- as.Date("2022-12-16")

ibk$date <- as.Date(as.character(ibk$date))

# Subset the data for train and test periods
train_data <- ibk[(ibk$date >= train_start & ibk$date <= train_end),]
test_data <- ibk[(ibk$date >= test_start & ibk$date <= test_end),]

# Drop missing values and convert to data frame
train_data <- na.omit(train_data)
train_data <- as.data.frame(train_data)

test_data <- na.omit(test_data)
test_data <- as.data.frame(test_data)

# Compute yday for train and test data
train_data$yday <- as.POSIXlt(train_data$date)$yday
test_data$yday <- as.POSIXlt(test_data$date)$yday


#convert Kelvin to Celsius: 
test_data$t2m <- test_data$t2m - 273.15
train_data$t2m <- train_data$t2m - 273.15


# Train a GAM using mgcv package
gmod <- gam(t2mObs ~ s(t2m, bs = "cr") + s(lcc, bs = "cr") + s(yday, bs = "cc"), data = train_data)

# Plot effects of smooth terms
plot(gmod)

# Predict on test data and plot differences
test_pred <- predict(gmod, newdata = test_data)
differences <- test_pred - test_data$t2mObs

plot(test_data$date, differences, type = "l", xlab = "Date", ylab = "Difference",
     main = "Differences: GAM Forecasted vs. Observed Temperatures")


# Fit a GAM with interaction term
gmod_interaction <- gam(t2mObs ~ s(t2m, bs = "cr") + s(lcc, bs = "cr") + ti(t2m, yday, bs = c("cr", "cc")),
                        data = train_data)

# Plot model and differences for test period
plot(gmod_interaction)

test_pred_interaction <- predict(gmod_interaction, newdata = test_data)
differences_interaction <- test_pred_interaction - test_data$t2mObs

plot(test_data$date, differences_interaction, type = "l", xlab = "Date", ylab = "Difference",
     main = "Differences: GAM Forecasted (with Interaction) vs. Observed Temperatures")




