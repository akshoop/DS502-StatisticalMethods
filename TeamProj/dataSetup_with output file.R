# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu

# Importing the CVS raw dataset
rawDF <- read.csv(file="mainCVSdata.csv", header=TRUE, sep=",")

# Setting up the additional column of SPENDSNEXT
newDF <- rawDF
# removing last row since we will have no use for it
newDF <- newDF[-nrow(newDF),]
# getting the next visit's SPENDS for the customer
newDF$SPENDSNEXT <- with(newDF, rawDF$SPENDS[-1])
# loop to make NA the SPENDSNEXT where it's the customer's last visit
# this takes about 1 minute to process
for (i in 1:nrow(newDF)){
  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i+1]){
    newDF$SPENDSNEXT[i] <- NA
  }
}
# removing the rows that have an NA entry
# ie, removing the last visit entries for each customer
analysisDF <- na.omit(newDF)
# output analysis data
write.csv(analysisDF, file = 'analysis_data.csv')