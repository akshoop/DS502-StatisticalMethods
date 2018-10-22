# MA 543/DS 502 HW1 - Problem 9
# My working directory
setwd("/Users/SHOOP//Documents/WPI graduate year2/DS 502/HW1")

# loading in Auto.csv, and identifying the NA fields with "?" string
Auto = read.csv("Auto.csv", na.strings = "?")
# removing empty/NA fields
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

# linear regression
lm.MPGHP <- lm(Auto$mpg ~ Auto$horsepower)
summary(lm.MPGHP)

# plotting, horsepower needs to be x-axis for abline() least squares regression line to be shown.
plot(Auto$horsepower, Auto$mpg)
abline(lm.MPGHP)