

setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task08")
#tell R that time is in UTC
Sys.setenv(TZ="GMT")
library("raster")
library("colorspace")
library("mgcv")


# Load the tirolTdaily.rda file
load("tirolTdaily.rda")

# Check the names of the variables
names(tirolTdaily)

# Extract the names of all stations
stations <- unique(tirolTdaily$station)

# Add day of year yday to tirolTdaily
tirolTdaily$yday <- as.numeric(format(tirolTdaily$date, "%j"))

# Load the digital elevation data from dem.rds
dem <- readRDS("dem.rds")

# Load the coordinates of boundaries of Tirol from tirol.gadm.rds
tirol <- readRDS("tirol.gadm.rds")

# Plot the digital elevation data
plot(dem, col = gray.colors(51))

# Plot the state boundaries
plot(tirol, add = TRUE)




?choose.k

# Fit the GAM tModS model
tModS_model <- gam(t ~ s(alt, k=5) + s(yday, k=10, bs="cc"), data=tirolTdaily)

# Print the summary of the model
summary(tModS_model)



# Fit the GAM tModA model for mean temperature
tModA_mean <- gam(t ~ s(alt, k=5) + s(yday, k=10, bs="cc") + ti(yday, alt, bs=c("cc", "cr"), k=c(5,5)) + ti(yday, lat, lon, bs=c("cc", "tp"), d=c(1,2), k=c(8,20)), data=tirolTdaily)


# Fit the GAM tModMax model for maximum temperature
tModMax <- gam(tmax ~ s(alt, k=5) + s(yday, k=10, bs="cc") + ti(yday, alt, bs=c("cc", "cr"), k=c(5,5)) + ti(yday, lat, lon, bs=c("cc", "tp"), d=c(1,2), k=c(8,20)), data=tirolTdaily)


# Fit the GAM tModMin model for minimum temperature
tModMin <- gam(tmin ~ s(alt, k=5) + s(yday, k=10, bs="cc") + ti(yday, alt, bs=c("cc", "cr"), k=c(5,5)) + ti(yday, lat, lon, bs=c("cc", "tp"), d=c(1,2), k=c(8,20)), data=tirolTdaily)



# Plot the effects of the additive terms for mean temperature of altitude 
par(mfrow=c(2,2))
plot(tModS_model,select=1:4, main = "tModS of altitude")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModA_mean, select=1:4, main = "tModA of altitude")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMax, select=1:4, main = "tModMax of altitude")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMin, select=1:4, main = "tModMin of altitude")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)



# Plot the effects of the additive terms for mean temperature of yday
par(mfrow=c(2,2))
plot(tModS_model,select=2:4, main = "tModS of yday")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModA_mean, select=2:4, main = "tModA of yday")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMax, select=2:4, main = "tModMax of yday")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMin, select=2:4, main = "tModMin of yday")
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)



# Plot the effects of the additive terms for mean temperature of yday and altitude

#par(mfrow=c(2,2))
plot(tModA_mean, select=3:4, main = "tModA of yday and altitude", cex.lab = 2, cex.main = 2, cex = 2, cex.sub = 2, cex.axis = 2)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMax, select=3:4, main = "tModMax of yday and altitude",cex.lab = 2, cex.main = 2, cex = 2, cex.sub = 2, cex.axis = 2)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMin, select=3:4, main = "tModMin of yday and altitude", cex.lab = 2, cex.main = 2, cex = 2, cex.sub = 2, cex.axis = 2)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)



# Plot the effects of the additive terms for mean temperature of yday, lat and long
par(mfrow=c(2,2))
plot(tModA_mean, select=4:4,cex = 1)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMax, select=4:4, cex = 1)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)
plot(tModMin, select=4:4, cex = 1)
grid(nx = NULL, ny = NULL,lty = 2,col = "gray",lwd = 1)




# Crop DEM to the extent of Tirol
dem <- crop(dem, extent(tirol))

# Create SpatialPoints object for all DEM pixels
sp <- SpatialPoints(coordinates(dem), proj4string = crs(tirol))

# Find the indices of grid cells in Tirol
take <- which(!is.na(over(sp, tirol)))

# Assign NA to values outside Tirol
values(dem)[!take] <- NA

# Create a newdata data.frame for the prediction for the new data locations
newdata <- as.data.frame(coordinates(dem))

# Give the lat/lon coordinates their proper names "lat" and "lon"
names(newdata) <- c("lon", "lat")

# Add altitude to newdata with values(dem)
newdata$alt <- values(dem)

# Subset newdata to within boundaries of Tirol
newdata <- newdata[take, ]

# Choose a date
date <- as.POSIXlt("2019-12-01")

# Add yday variable to newdata
newdata$yday <- date$yday

# Predict/compute the climatologies for each of the 4 GAMs
tmpA <- predict(tModA_mean, newdata = newdata)
tmpB <- predict(tModS_model, newdata = newdata)
tmpC <- predict(tModMax, newdata = newdata)
tmpD <- predict(tModMin, newdata = newdata)

# Create a RasterStack object
res <- dem
values(res) <- NA #initialize
res <- stack(res, res, res, res)
names(res) <- c("tmean", "tsimple", "tmax", "tmin")
values(res$tmean)[take] <- tmpA
values(res$tsimple)[take] <- tmpB
values(res$tmax)[take] <- tmpC
values(res$tmin)[take] <- tmpD

# Plot the climatology for the chosen date
library(colorspace)
par(mfrow=c(2,2))
col = diverge_hcl(51, h = c(260, 305),power = 1)
plot(res$tmean, main = sprintf("Daily mean temperature %s", strftime(date, "%b %d")), col = col, cex = 1.5)
plot(res$tsimple, main = sprintf("Daily simple temperature %s", strftime(date, "%b %d")), col = col, cex = 1.5)
plot(res$tmax, main = sprintf("Daily max temperature %s", strftime(date, "%b %d")), col = col, cex = 1.5)
plot(res$tmin, main = sprintf("Daily min temperature %s", strftime(date, "%b %d")), col = col, cex = 1.5)

