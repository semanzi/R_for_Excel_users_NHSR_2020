library(msm)
library(readr)

set.seed(2)
nPat <- 2396

id <- floor(runif(nPat, 100000, 900000)) #Create a unique id of 6 digits

age <- floor(rtnorm(nPat,55,5,18,90)) #Create age vector based on normal dist mean=55, sd=5, lower=18, upper=90

#Randomly sample dates from between date values using a uniform distribution
refDate <- sample(seq(as.Date('2015-01-01'), as.Date('2016-12-31'), by="day"), nPat, replace=TRUE)

s1N <- 1145 #Number of records for Team 1
s2N <- nPat - s1N   #Number of records for Team 2

s1Days <- floor(rlnorm(s1N, 3, 0.4))  #Randomly sample values from log normal distribution with logmean=3 logsd=0.4 and round down
s2Days <- floor(rlnorm(s2N, 3.6, 0.2))  #Randomly sample values from log normal distribution with logmean=3.6 logsd=0.2 and round down
sDays <- c(s1Days,s2Days)
treatDate <- refDate + sDays
treatDate[c(2220,1921,58)] <- refDate[c(2220,1921,58)] - sDays[c(2220,1921,58)]

#Create team name entries then insert missing values
s1Name <- rep('Team1', s1N)
s2Name <- rep('Team2', s2N)
sName <- c(s1Name,s2Name)
nameError <- ""
sName[c(345,756,976,23)] <- nameError

#Create a dataframe from the columns of data points
rawData <- data.frame(id,age,refDate,treatDate,sName)
rawData$treatDate[rawData$treatDate >= as.Date("2017-01-01")] <- NA
colnames(rawData) <- c("ID", "Age", "Referral_Date", "Treat_Date", "Service_Name")

#Order the raw data by ID
orderedRawData <- rawData[order(id),]

write.csv(orderedRawData,"workshopRawData.csv",row.names = FALSE)
