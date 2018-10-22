# MA 543/DS 502 HW1 - Problem 4
# My working directory is set via Preferences.
setwd("/Users/SHOOP//Documents/WPI graduate year2/DS 502/HW1")

# loading in College.csv
college = read.csv("College.csv")

rownames(college) = college[,1]
college = college[,-1]

fix(college)

summary(college)

	

plot(college$Private,college$Outstate, xlab = "Private (Yes/No)", ylab = "Outstate")

# Elite schools
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite (Yes/No)", ylab = "Outstate")

# histograms
par(mfrow = c(2,2))
hist(college$Accept, col = 3)
hist(college$perc.alumni, col = 4)
hist(college$F.Undergrad, breaks = 50)
hist(college$Expend, breaks = 100)

# other analysis
# Elite schools don't necessarily show a high graduation rate.
plot(college$Top10perc, college$Grad.Rate, xlab = "Elite schools", ylab = "Graduate rate")
# Universities with a high cost of tuition have some high Graduation rate.
# Beyond this, one can only observe wide variation in graduation rate for low cost schools.
# There is one very unusual outlier, a college with a graduation rate of about 120%
plot(college$Expend, college$Grad.Rate, xlab = "Tuition", ylab = "Graduation rate")
# Colleges with a low acceptance rate seemed to have higher percentage of alumni who donate.
# This could be the case with top Elite schools, and them only accepting the best-of-best, then those who graduate from these elite schools would participate more in donations.
plot(college$Accept / college$Apps, college$perc.alumni, xlab = "Acceptance rate", ylab = "Percentage of alumni who donate")

