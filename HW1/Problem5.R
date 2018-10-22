# MA 543/DS 502 HW1 - Problem 5
# My working directory
setwd("/Users/SHOOP//Documents/WPI graduate year2/DS 502/HW1")

# loading in Auto.csv, and identifying the NA fields with "?" string
Auto = read.csv("Auto.csv", na.strings = "?")
# removing empty/NA fields
Auto = na.omit(Auto)

summary(Auto)

# getting range of values for all quantitative predictors
apply(Auto[,1:7], 2, range)

# mean and std dev of each quantitative predictor
apply(Auto[,1:7], 2, mean)
apply(Auto[,1:7], 2, sd)

# now removing the 10th through 85th observations.
editAuto = Auto[-10:-85,]
# getting range, mean, std dev of all quantitative predictors of this subset
apply(editAuto[,1:7], 2, range)
apply(editAuto[,1:7], 2, mean)
apply(editAuto[,1:7], 2, sd)

# Using full data set, graphical investigation
pairs(Auto)
# Newer model cars have better MPG.
plot(Auto$year, Auto$mpg, xlab = "Automobile Year", ylab = "MPG")
# Lightweight cars have smaller horsepower whereas heavier cars (with probably more gear) have better horsepower
plot(Auto$horsepower, Auto$weight, xlab = "Horsepower", ylab = "Weight")
# Cars with more horsepower don't necessarily have better acceleration.
# In fact, it appears as though those cars with great acceleration have low-mid level of horsepower.
# Heavier cars (with likely more horsepower) don't necessarily have high acceleration.
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "Acceleration")