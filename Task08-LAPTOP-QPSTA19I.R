setwd("C:/Users/david/OneDrive - uibk.ac.at/MA/VU Geostatistics/Geostatistics/Task08_trees")
#tell R that time is in UTC
Sys.setenv(TZ="GMT")

library("partykit")
data("airquality")

# month and day to day of year conversation
airquality$DayOfYear <- as.POSIXlt(paste(1973, airquality$Month, airquality$Day), format = "%Y %m %d")$yday


str(airquality)
pairs(airquality)
?airquality


# convert to SI-Units: 
# Langley to Joule per square meter
airquality$Ozone_SI <- airquality$Ozone * 41840

# Miles per hour to meters per second
airquality$Wind_SI <- airquality$Wind * 0.44704

# Fahrenheit to Celsius
airquality$Temp_SI <- (airquality$Temp - 32) * (5/9)

# nan-including an column dropping
airq <- subset(airquality, !is.na(Ozone), select = -c(Month, Day))

# Fit the tree using ctree
airQTree <- partykit::ctree(Ozone_SI ~ ., data = airq)
airq_windQtree <- partykit::ctree(Ozone_SI ~ ,Wind_SI, data = airq)
airq_yday <- partykit::ctree(Ozone_SI ~ DayOfYear + Temp_SI, data = airq)

#Ozone_SI is the response variable, and . is used in the formula to indicate that all other variables in the airq 
#dataset should be included as predictors. The resulting tree is stored in the airQTree object.

#The algorithm works as follows:
  
#It tests the global null hypothesis of independence between any of the input variables and the response. 
#If this hypothesis cannot be rejected, the algorithm stops growing the tree.
#If the global null hypothesis is rejected, the algorithm selects the input variable with the strongest association 
#to the response. The association is measured by a p-value corresponding to a test for the partial null 
#hypothesis of a single input variable and the response.
#It implements a binary split in the selected input variable.
#Steps 1-3 are repeated recursively to grow the tree further.
#By performing statistical tests and making unbiased variable selections, the ctree algorithm helps address 
#overfitting and selection bias commonly associated with other tree-growing algorithms.


airQTree
airq_windQtree
airq_yday


#Fitted party:
#  [1] root
#|   [2] Ozone <= 52
#|   |   [3] Ozone <= 24
#|   |   |   [4] Ozone <= 14
#|   |   |   |   [5] Ozone <= 9: 280328.000 (n = 10, err = 1.01709e+11)
#|   |   |   |   [6] Ozone > 9: 523000.000 (n = 14, err = 41138761600.0)
#|   |   |   [7] Ozone > 14
#|   |   |   |   [8] Ozone <= 19: 720577.778 (n = 9, err = 20228989155.6)
#|   |   |   |   [9] Ozone > 19: 915557.647 (n = 17, err = 59108007905.9)
#|   |   [10] Ozone > 24
#|   |   |   [11] Ozone <= 37: 1341204.444 (n = 18, err = 341266937244.4)
#|   |   |   [12] Ozone > 37: 1877221.333 (n = 15, err = 3.84662e+11)
#|   [13] Ozone > 52
#|   |   [14] Ozone <= 97
#|   |   |   [15] Ozone <= 73: 2731010.909 (n = 11, err = 451969373090.9)
#|   |   |   [16] Ozone > 73: 3553610.667 (n = 15, err = 1.356587e+12)
#|   |   [17] Ozone > 97: 5235977.143 (n = 7, err = 4.588035e+12)

#Number of inner nodes:    8
#Number of terminal nodes: 9


plot(airQTree)
plot(airq_windQtree)
plot(airq_yday)
