#[p]
#Inititate the required libraries


#[12]
library(psych)
library(plyr)
library(reshape2)
library(ggplot2)


#[F]
#Read in the data


#[4]
data <- read.csv("workshopRawData.csv") 


#[E]
#Convert dates


#[11]
data$Referral_Date <- as.Date(data$Referral_Date)
data$Treat_Date <- as.Date(data$Treat_Date)


#[L]
#Check the data for errors
#Missing data


#[6]
id_na <- sum(is.na(data$ID))
age_na <- sum(is.na(data$Age))
ref_na <- sum(is.na(data$Referral_Date))
trt_na <- sum(is.na(data$Treat_Date))
ser_na <- sum(is.na(data$Service_Name))


#[A]
#Category typos, ommissions and duplications


#[16]
id_uni <- unique(data$ID)
age_uni <- unique(data$Age)
ser_uni <- unique(data$Service_Name)

id_dup <- count(data, 'ID')
id_dup_sub <- subset(id_dup,id_dup$freq > 1)
dups <- id_dup_sub$ID
dupRows <- data[data$ID %in% dups,]


#[C]
#Incorrectly entered date stamps


#[9]
dataTime <- data
dataTime$time_check <- dataTime$Treat_Date - dataTime$Referral_Date
time_sub <- subset(dataTime, dataTime$time_check < 0)


#[M]
#Swap incorrectly entered date stamps


#[2]
time_rows <- as.integer(row.names(time_sub))
for (i in 1:length(time_rows)){
  # print(c("Before ",data[time_rows[i],]))
  a <- data[time_rows[i],3]
  b <- data[time_rows[i],4]
  data[time_rows[i],3] <- b
  data[time_rows[i],4] <- a
  # print(c("After ",data[time_rows[i],]))
}


#[J]
#Remove patients without team entries


#[13]
omiss <- subset(data, data$Service_Name == "")
omiss_rows <- as.integer(row.names(omiss))
dataClean <- data[-c(omiss_rows),]


#[O]
#Calculate the patient wait times


#[8]
dataClean$waitTime <- as.integer(dataClean$Treat_Date - dataClean$Referral_Date)
waitDes <- describe(dataClean$waitTime)


#[H]
#Subset wait time data by month


#[7]
startDate <- min(dataClean$Referral_Date)
endDate <- max(dataClean$Treat_Date, na.rm=TRUE)
numMonths <- length(seq(from=startDate, to=endDate, by='month'))
months <- seq(from=startDate, to=endDate, by='month')
monthData <- list()
waitList <- list()
for (i in 1:(numMonths-1)){
  a <- dataClean[which(dataClean$Treat_Date > months[i+1] &
                         dataClean$Referral_Date < months[i+1]),]
  monthData[[i]] <- a
  waitList[[i]] <- data.frame(a$waitTime,a$Service_Name)
}


#[N]
#Convert wait list into single multilevel dataframe


#[5]
dfWait <- melt(waitList)
dfWait <- droplevels(dfWait)


#[K]
#Patient numbers barchart and descriptives


#[14]
waitListTab <- table(dfWait$L1)
waitListDf <- as.data.frame(waitListTab)
waitListDes <- describe(waitListDf$Freq)
barplot(waitListTab, xlab="Month", ylab="Frequency",
        main="Number of patients on the waiting list",
        cex.names = 0.8)


#[G]
#Patient numbers by team barchart and descriptives


#[10]
teamWaitListTab <- table(dfWait$a.Service_Name,dfWait$L1)
teamWaitListDf <- as.data.frame(teamWaitListTab)
teamWaitListDes <- describeBy(teamWaitListDf$Freq,teamWaitListDf$Var1)
barplot(teamWaitListTab,beside=TRUE, xlab="Month", ylab="Frequency",
        main="Number of patients on the waiting list by team",
        cex.names = 0.8, legend=TRUE,
        args.legend=list(x="topright", legend=c("Team 1", "Team 2")))
abline(h=mean(teamWaitListTab[1,]))
abline(h=mean(teamWaitListTab[2,]), lty=2)


#[I]
#Patient waiting times boxplot and descriptives by month


#[1]
timeWaitTimePlot <- qplot(factor(L1), value, data=dfWait, geom='boxplot')
timeWaitTimePlot + xlab("Month") + ylab("Waiting time (days)") +
  ggtitle("Patient waiting times for service X over time")
waitTimeDes <- describeBy(dfWait$value,group=dfWait$L1)


#[B]
#Patient waiting times boxplot and descriptives by team


#[3]
teamWaitTimePlot <- qplot(a.Service_Name,value, data=dfWait, geom='boxplot')
teamWaitTimePlot + xlab("Month") + ylab("Waiting time (days)") +
  ggtitle("Patient waiting times for service by team")
teamWaitTimeDes <- describeBy(dfWait$value, group=dfWait$a.Service_Name)


#[D]
#Patient waiting times boxplot and descriptives by month and team


#[15]
dfWait$ServTime <- interaction(dfWait$a.Service_Name,dfWait$L1)
breaks <- c("Team1.1","Team1.2","Team1.3","Team1.4","Team1.5",
            "Team1.6","Team1.7","Team1.8","Team1.9","Team1.10",
            "Team1.11","Team1.12","Team1.13","Team1.14","Team1.15",
            "Team1.16","Team1.17","Team1.18","Team1.19","Team1.20",
            "Team1.21","Team1.22","Team1.23")
lab <- as.character(seq(1,23))
teamTimeWaitTimePlot <- qplot(factor(ServTime), value, data=dfWait, fill=a.Service_Name,
                              geom='boxplot')
teamTimeWaitTimePlot + scale_x_discrete(name="Month", breaks=breaks, labels=lab) +
  ylab("Waiting Time")
teamTimeWaitTimeDes <- describeBy(dfWait$value, group=list(dfWait$a.Service_Name,dfWait$L1))







