setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task09")
#tell R that time is in UTC
Sys.setenv(TZ="GMT")

dat <- readRDS("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task09/lightningPCAclustering.rds")

# Subset the data to scenarios without lightning
ms <- scale(dat[dat$scenario %in% c("snol", "wnol"), -1])

# Apply scaling to the whole data set era5
era5 <- scale(dat[, -1], center = attr(ms, "scaled:center"), scale = attr(ms, "scaled:scale"))
era5 <- as.data.frame(era5)

# Set the seed for reproducibility:
set.seed(123)  # Replace 123 with any number of your choice

# Perform k-means clustering on the "era5" data frame:
clK <- kmeans(era5, centers = 5, nstart = 150, algorithm = "MacQueen")

# Assign names to each cluster based on the cluster assignments in clK$cluster. 
# Assuming the "dat" data frame contains the original data:
dat$cluster_new[clK$cluster == 1] <- "wind_field"
dat$cluster_new[clK$cluster == 2] <- "mass_field"
dat$cluster_new[clK$cluster == 3] <- "average"
dat$cluster_new[clK$cluster == 4] <- "cloud_physics_mass"
dat$cluster_new[clK$cluster == 5] <- "cloud_physics_wind"

# Compute the frequencies of cases in each cluster for each scenario 
# using the table() function on dat$scenario and dat$cluster_new:
tab <- table(dat$scenario, dat$cluster_new)
print(tab)

# Compute the relative frequencies of the clusters with respect to rows (scenarios) 
# using the prop.table() function on tab:
relFreq_rows <- prop.table(tab, margin = 1) 
relFreq_cols <- prop.table(tab, margin = 2) 

print(relFreq_rows)
print(relFreq_cols)


# Reorder the frequency table tab to have the rows in the sequence "wl", "wnol", "snol", and "sl", 
# and the columns in the sequence "cloud_physics_wind", "wind_field", "average", "mass_field", 
# and "cloud_physics_mass":
tab <- tab[c("wl", "wnol", "snol", "sl"), c("cloud_physics_wind", "wind_field", "average", "mass_field", "cloud_physics_mass") ]


# Set up the colors for the mosaic plot:
cols_mosaic <- rev(hcl(c(10, 10, 80, 250, 250), c(70, 60, 50, 60, 70), c(20, 60, 90, 60, 20)))

# Plot the distribution using mosaicplot() with the specified options:
?mosaicplot
mosaicplot(tab, off = c(15, 0), color = cols_mosaic, las = 1, cex.axis = 1.2, main = " distribution of clusters scenarios")
