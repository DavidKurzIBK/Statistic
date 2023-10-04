
# dyad 9: Kurz David & Hildebrandt Malte

# task01, 16.03.2023

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task01")

#tell R that time is in UTC
Sys.setenv(TZ="GMT")

##########################Exercise 1:#######################################################
library(zoo)
linz <- readRDS("selected_linzHoersching36_2009-2022.rds")
linz <- as.data.frame(linz)
linz <- cbind.data.frame(datetime = rownames(linz), linz)
rownames(linz) <- 1:nrow(linz)


##########################Exercise 2:#######################################################
#convert kelvin to celsius with linz[,c("t2m","t850")]
#Select the columns "t2m" and "t850" from the data frame "linz"
temperatures_kelvin <- linz[,c("t2m","t850")]

# Convert the temperatures from Kelvin to Celsius
temperatures_celsius <- temperatures_kelvin - 273.15

# Rename the columns to indicate that the temperatures are in Celsius
colnames(temperatures_celsius) <- c("t2m_celsius", "t850_celsius")

# Add the temperatures in Celsius to the data frame "linz"
linz <- cbind(linz, temperatures_celsius)


##########################Exercise 3:#######################################################
#get tabular overview of data
View(linz)


##########################Exercise 4:#######################################################
summary(linz[,c("cape","sshf6h")])
#   cape (J/kg)        sshf6h (J/m^2)        
#Min.   :   0.0000   Min.   :-5810000  
#1st Qu.:   0.0000   1st Qu.:-2470000  
#Median :   0.1667   Median :-1430000  
#Mean   :  43.8566   Mean   :-1550476  
#3rd Qu.:   4.0000   3rd Qu.: -530000->only positive during special conditions (snowcover, frost and/or no sunshine) 
#Max.   :2471.3333   Max.   : 2880000  


##########################Exercise 5:#######################################################
#pairs will create a scatterplot matrix of all variables in linz;
#with cex size of plotting text and symbols -> cex = 0.1 equal to 1/10 of default size of cex
pairs(linz[, 2:7])  # t2m with t850 and tcwv show linear relationship, t2m with r500 and tcc show no relationship

#not executed cause too computationally expensive
#pairs(linz)  


##########################Exercise 6:#######################################################
#plot temp data:
plot(linz$t2m_celsius, type = "l", main = "2m Temperatures in Linz", xlab = "Time", ylab = "Temperature in 2m")
identify(linz) # fit to below selected max & min values;

#identify coldest and warmest temperatures and save it as variables: 
max_temp_linz <- max(temperatures_celsius$t2m_celsius, na.rm = TRUE) # 35.8 °C;
min_temp_linz <- min(temperatures_celsius$t2m_celsius, na.rm = TRUE) # -8.6 °C;


##########################Exercise 7:#######################################################
#scatterplot of 2m temp and 850 m temp:
plot(linz$t850_celsius, linz$t2m_celsius, main = "Scatterplot", xlab = "850 hPa Temperature", ylab = "2m Temperature")
# clear threshold of -1 °C per 100 m upward (strongest possible atm. lapse rate);
# scattered values with lower lapse rates (thermical structure) of atm. due inversions;


##########################Exercise 8:#######################################################
#plot histogram for every variable:                                                                       # hard lim. values:
hist(linz$t2m_celsius, main = "Histogram of 2m temperatures in Linz", xlab = "in °C")                     # no
hist(linz$t850_celsius, main = "Histogram of 850 hPa Temperatures", xlab = "in °C")                       # no 
hist(linz$dpd2m, main = "Histogram of dewpoint differences", xlab = "in K")                               # yes 0 K
hist(linz$r500[linz$r500 <= 100],  main = "Histogram of relative humidity @ 500 hPa", xlab = "in %")      # yes 0 % min and 100 % max
hist(linz$tcwv,  main = "Histogram of total column water vapor", xlab = "in kg/m^2")                      # yes 0 kg/m^2 lower bound
hist(linz$tcc,  main = "Histogram of total cloud cover", xlab = "in 0-1 values")                          # yes 0 (min) and 1 (max)
hist(linz$lcc,  main = "Histogram of low level cloud cover", xlab = "in 0-1 values")                      # yes 0 (min) and 1 (max) 
hist(linz$pmsl,  main = "Histogram of pressure reduced to mean sea level ", xlab = "in Pa")               # no
hist(linz$ff10m,  main = "Histogram of 10 m wind speed", xlab = "in m/s")                                 # yes 0 m/s
hist(linz$ff700,  main = "Histogram of wind speed at 700 hPa", xlab = "in m/s")                           # yes 0 m/s 
hist(linz$w500,  main = "Histogram of vertical velocity ", xlab = "in Pa/s")                              # no
hist(linz$pv700, main = "Histogram of potential vorticity at 700 hPa ", xlab = "in K m^2 kg^-1 s^-1")     # no 
hist(linz$blh,  main = "Histogram of boundary layer height (above ground level) ", xlab = "in m")         # yes ground-sfc
hist(linz$cape,  main = "Histogram of convective available potential energy", xlab = "in J/kg")           # yes 0 J/m^2 lower lim
hist(linz$ssr6h,  main = "Histogram of surface net solar radiation over previous 6h", xlab = "in J m^-2") # yes 0 J/m^2 lower limit
hist(linz$bld6h,  main = "Histogram of boundary layer dissipation previous 6 h ", xlab = "in J m^-2")     # yes 0 J/m^2 -"-
hist(linz$tp6h,  main = "Histogram of precipitation sum over previous 6h", xlab = "in m")                 # yes with 0 m lower lim.
hist(linz$lsp6h,  main = "Histogram of large scale precipitation sum over previous 6h", xlab = "in m")    # yes -"-


##########################Exercise 9:#######################################################
# create QQ plot for t2m variable
qqplot_t2m <- qqnorm(linz$t2m_celsius, main = "Q-Q Plot for 2m Temp")
qqline(linz$t2m_celsius)

# create QQ plot for t850 variable
qqplot_t850 <- qqnorm(linz$t850_celsius, main = "Q-Q Plot for 850 hPa Temp")
qqline(linz$t850_celsius)

#create a gaussian distribution:
mean_t2m <- mean(temperatures_celsius$t2m_celsius, na.rm = TRUE)
mean_850 <- mean(temperatures_celsius$t850_celsius, na.rm = TRUE)
sd_t2m <- sd(temperatures_celsius$t2m_celsius, na.rm = TRUE)
sd_850 <- sd(temperatures_celsius$t850_celsius, na.rm = TRUE)

y_t2m <- dnorm(temperatures_celsius$t2m_celsius, mean = mean_t2m, sd = sd_t2m)
y_850 <- dnorm(temperatures_celsius$t850_celsius, mean = mean_850, sd = sd_850)

plot(density(temperatures_celsius$t2m_celsius, na.rm = TRUE))
points(temperatures_celsius$t2m_celsius, y_t2m,  col = "red", cex = 0.5)

plot(density(temperatures_celsius$t850_celsius, na.rm = TRUE))
points(temperatures_celsius$t850_celsius, y_850,  col = "red", cex = 0.5)

# the t850 values follow better a gaussian distribution than t2m ones. but t850 
# values are still away from a good shaped gaussian curve. this can be seen in the 
# Q-Q-Plot as well as in density-function plots. For the density plot, as reference 
# for gaussian distribution the red points plot was created. 

##########################Exercise 10:#######################################################
#filter summer and winter months out of data
linz_summer <- subset(linz, subset = (as.POSIXlt(datetime)$mon + 1) %in% c(6,7,8))
linz_winter <- subset(linz, subset = (as.POSIXlt(datetime)$mon + 1) %in% c(12,1,2))

summary(linz_summer)
summary(linz_winter)

# after investigating all values the command summary gives us, we came to the conclusion that only pmsl show similar values for 
# the seasonal median compared summer to winter. Some values very small in their magnitude may look close to each other but with 
# their significance they are far off.
