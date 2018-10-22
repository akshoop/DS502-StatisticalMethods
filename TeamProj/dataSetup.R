# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu

# Importing the CVS rdata.csv"aw dataset
newDF <- read.csv("mainCVSdata.csv", header=TRUE, sep=",")

# Setting up the additional column of SPENDSNEXT
newDF <- rawDF
# removing last row since we will have no use for it
newDF <- newDF[-nrow(newDF),]
# getting the next visit's SPENDS for the customer

newDF$SPENDSNEXT <- with(newDF, rawDF$SPENDS[-1])
?with
# loop to make NA the SPENDSNEXT where it's the customer's last visit
# this takes about 1 minute to process
####################################################################
customerList=unique(newDF$ï..XTRA_CARD_NBR)

for(a in 1:dim(newDF)[1]){
  newDF$SPENDSNEXT=0
}
cusvalDf=data.frame()
mainDf=data.frame()
for(j in 1:length(customerList)){
  cusvalDf=subset(newDF, (ï..XTRA_CARD_NBR==customerList[j]))
  #k=1
  for(k in 1:dim(cusvalDf)[1]-1){
    #print(k)
    cusvalDf$SPENDSNEXT[k]=cusvalDf$SPENDS[k+1]
  }
  cusvalDf <- cusvalDf[-nrow(cusvalDf),]
  mainDf=rbind.data.frame(mainDf,cusvalDf)
}
write.csv(mainDf,"trainingdata.csv")
########################################################################
for (i in 1:nrow(newDF)){
  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i+1]){
    newDF$SPENDSNEXT[i] <- NA
  }
}


# removing the rows that have an NA entry
# ie, removing the last visit entries for each customer
analysisDF <- na.omit(newDF)


