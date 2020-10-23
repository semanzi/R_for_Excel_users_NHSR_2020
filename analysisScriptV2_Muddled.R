#[A]
#Subset wait time data by month 
#(patients treated and still awaiting treatment during a given month)


#[B]
#Inititate the required libraries


#[C]
#Read in the data


#[D]
#Convert wait list into single multilevel dataframe


#[E]
#Calculate the patient wait times


#[F]
#Check the data for errors
#Missing data


#[G]
#Get list of column names


#[H]
#Remove patients without team entries


#[I]
#Convert dates


#[J]
#Patient waiting times boxplot and descriptives by month


#[K]
#Category typos, ommissions and duplications


#[L]
#Swap incorrectly entered date stamps


#[M]
#Incorrectly entered date stamps


#[N]
#Patient waiting times boxplot and descriptives by team


#[O]
#Patient waiting times boxplot and descriptives by month and team


#[P]
#Patient numbers barchart and descriptives


#[Q]
#Generate wait time descriptives and histogram - Aggregate


#[R]
#Patient numbers by team barchart and descriptives


#[1]
omission_removal <- function(data,cols,targets){
  for (i in 1:length(cols)){
    for (j in 1:length(targets)){
      omiss <- subset(data, data[[cols[i]]] == targets[j])
      omiss_rows <- as.integer(row.names(omiss))
      dataClean <- data[-c(omiss_rows),]
      print("The following rows have been removed")
      print(data[omiss_rows,])
    }
  }
  return(dataClean)
}
dataClean <- omission_removal(data,c(5),c(""))  
#Inputs: dataframe, column(s) to check for targets, target(s)


#[2]
name_list <- function(data){
  nList <- as.list(colnames(data))
  print("The structure of the data is as follows")
  print("Any dates will need to be converted from factor to Date format")
  str(data)
  return(nList)
}
nList <- name_list(data)  #Inputs: dataframe


#[3]
missing_data <- function(data){
  colsN <- colnames(data)
  cols <- seq(1, length(colsN))
  missList <- list()
  for (i in 1:length(cols)){
    missList[i] <- sum(is.na(data[[cols[i]]]))
    if (missList[i] > 0){
      print(paste("In column",cols[i],"there are",missList[i],"missing data points"))
    }
  }
  return(missList)
}
missList <- missing_data(data)  #Inputs: dataframe


#[4]
team_time_patient_wait_time <- function(dfWait){
  dfWait$ServTime <- interaction(dfWait$a.Service_Name,dfWait$L1)
  breaks <- dfWait$ServTime[seq(1,length(levels(dfWait$ServTime)),2)]
  lab <- as.character(seq(1,length(breaks)))
  jpeg("wait_times_monthly_team.jpeg")
  teamTimeWaitTimePlot <- qplot(factor(ServTime), value, data=dfWait,
                                fill=a.Service_Name, geom='boxplot')
  teamTimeWaitTimePlot + scale_x_discrete(name="Month", breaks=breaks, labels=lab) +
    ylab("Waiting Time")
  dev.off()
  teamTimeWaitTimeDes <- describeBy(dfWait$value,
                                    group=list(dfWait$a.Service_Name,dfWait$L1))
  teamTimeWaitTimeDes <- do.call(rbind.data.frame,teamTimeWaitTimeDes)
  write.csv(teamTimeWaitTimeDes,"wait_times_monthly_team.csv")
}
team_time_patient_wait_time(dfWait) #Inputs: dataframe


#[5]
convert_list <- function(waitList){
  dfWait <- melt(waitList)
  dfWait <- droplevels(dfWait)
  print("The column names for the analysis are...")
  print(colnames(dfWait))
  return(dfWait)
}
dfWait <- convert_list(waitList)  #Inputs: dataframe


#[6]
patient_nums <- function(dfWait,addDate){
  waitListTab <- table(dfWait$L1)
  waitListDf <- as.data.frame(waitListTab)
  waitListDes <- describe(waitListDf$Freq)
  if (addDate == 1){
    write.csv(waitListDes,paste(Sys.Date(),"wait_list_numbers_monthly_des.csv",
                                sep="_"))
    jpeg(paste(Sys.Date(),"wait_list_numbers_monthly.csv",sep=""))
  }else{
    write.csv(waitListDes,"wait_list_numbers_monthly_des.csv")
    jpeg("wait_list_numbers_monthly.jpeg")
  }
  barplot(waitListTab, xlab="Month", ylab="Frequency",
          main="Number of patients on the waiting list",
          cex.names = 0.8)
  dev.off()
}
patient_nums(dfWait,1)  #Inputs: dataframe, use 1 to add todays date to filenames


#[7]
library(psych)
library(plyr)
library(reshape2)
library(ggplot2)


#[8]
time_patient_wait_time <- function(dfWait){
  jpeg("wait_times_monthly.jpeg")
  timeWaitTimePlot <- qplot(factor(L1), value, data=dfWait, geom='boxplot')
  timeWaitTimePlot + xlab("Month") + ylab("Waiting time (days)") +
    ggtitle("Patient waiting times for service X over time")
  dev.off()
  waitTimeDes <- describeBy(dfWait$value,group=dfWait$L1)
  waitTimeDes <- do.call(rbind.data.frame,waitTimeDes)
  write.csv(waitTimeDes,"wait_times_monthly_des.csv")
}
time_patient_wait_time(dfWait)  #Inputs: dataframe


#[9]
typo_check <- function(data,cols){
  typoList <- list()
  for (i in 1:length(cols)){
    typoList[[i]] <- unique(data[[cols[i]]])
    print(paste("There are",length(typoList[[i]]),"unique entries in column",cols[i]))
  }
  return(typoList)
}
typoList <- typo_check(data, c(1,2,5))  
#Inputs: dataframe, columns to check for number of unique categories

dup_check <- function(data,nList,idCol){
  idName <- nList[[idCol]]
  id_dup <- count(data, idName)
  id_dup_sub <- subset(id_dup,id_dup$freq > 1)
  dups <- id_dup_sub[[idCol]]
  dupRows <- data[data[[idCol]] %in% dups,]
  print("The rows with duplicated ID's are...")
  print(dupRows)
  return(dupRows)
}
dupRows <- dup_check(data,nList,1)
#Inputs: dataframe, list of column names, column containing unique ID's


#[10]
team_patient_wait_time <- function(dfWait){
  jpeg("wait_times_team.jpeg")
  teamWaitTimePlot <- qplot(a.Service_Name,value, data=dfWait, geom='boxplot')
  teamWaitTimePlot + xlab("Month") + ylab("Waiting time (days)") +
    ggtitle("Patient waiting times for service by team")
  dev.off()
  teamWaitTimeDes <- describeBy(dfWait$value, group=dfWait$a.Service_Name)
  teamWaitTimeDes <- do.call(rbind.data.frame,teamWaitTimeDes)
  write.csv(teamWaitTimeDes,"wait_times_team_des.csv")
}
team_patient_wait_time(dfWait)  #Inputs: dataframe


#[11]
date_check <- function(data){
  dataTime <- data
  dataTime$time_check <- dataTime$Treat_Date - dataTime$Referral_Date
  time_sub <- subset(dataTime, dataTime$time_check < 0)
  print("The following rows likely have incorrectly entered date stamps...")
  print(time_sub)
  return(time_sub)
}
time_sub <- date_check(data)  #Inputs: dataframe


#[12]
top_level_des <- function(dataClean,colNum){
  waitDes <- describe(dataClean[colNum])
  write.csv(waitDes,"top_level_descriptives.csv")
  jpeg("wait_time_hist.jpeg")
  waitHist <- hist(dataClean[[colNum]],main="Frequency of wait times for all patients",
                   xlab="Wait time")
  dev.off()
}
top_level_des(dataClean,6)  #Inputs: dataframe, column to describe and plot


#[13]
data <- read.csv("workshopRawData.csv")


#[14]
correct_dates <- function(data,time_sub,dColOne,dColTwo){
  time_rows <- as.integer(row.names(time_sub))
  for (i in 1:length(time_rows)){
    print("The entries")
    print(data[time_rows[i],])
    a <- data[time_rows[i],dColOne]
    b <- data[time_rows[i],dColTwo]
    data[time_rows[i],dColOne] <- b
    data[time_rows[i],dColTwo] <- a
    print("Have been changed to")
    print(data[time_rows[i],])
  }
  return(data)
}
data <- correct_dates(data,time_sub,3,4)
#Inputs: dataframe,
#subset dataframe of rows with incorrectly entered timestamps,
#column with referral date, column with treatment date


#[15]
convertDates <- function(data, cols){
  for (i in 1:length(cols)){
    data[[cols[i]]] <- as.Date(data[[cols[i]]])
    print(paste("data column",cols[i],"converted to Date format"))
  }
  
  return(data)
}
data <- convertDates(data, c(3,4))  
#Inputs: dataframe, columns to be converted to date format


#[16]
team_patient_nums <- function(dfWait){
  teamWaitListTab <- table(dfWait$a.Service_Name,dfWait$L1)
  teamWaitListDf <- as.data.frame(teamWaitListTab)
  teamWaitListDes <- describeBy(teamWaitListDf$Freq,teamWaitListDf$Var1)
  teamWaitListDes <- do.call(rbind.data.frame,teamWaitListDes)
  jpeg("team_wait_list_numbers_monthly.jpeg")
  barplot(teamWaitListTab,beside=TRUE, xlab="Month", ylab="Frequency",
          main="Number of patients on the waiting list by team",
          cex.names = 0.8, legend=TRUE,
          args.legend=list(x="topright", legend=c("Team 1", "Team 2")))
  abline(h=mean(teamWaitListTab[1,]))
  abline(h=mean(teamWaitListTab[2,]), lty=2)
  dev.off()
  write.csv(teamWaitListDes,"team_wait_list_numbers_monthly_des.csv")
}
team_patient_nums(dfWait) #Inputs: dataframe


#[17]
data_monthly <- function(dataClean,refDate,treatDate){
  startDate <- min(dataClean[,refDate])
  endDate <- max(dataClean[,treatDate], na.rm=TRUE)
  months <- seq(from=startDate, to=endDate, by='month')
  numMonths <- length(months)
  waitList <- list()
  errorCount <- 0
  for (i in 1:(numMonths-1)){
    a <- dataClean[which(dataClean[[treatDate]] > months[i] &
                           dataClean[[refDate]] < months[i+1]),]
    actWait <- rep(0,length(a[[1]]))
    for (j in 1:length(a[[1]])){
      if (a[j,treatDate] < months[i+1]){
        actWait[j] <- a[j,treatDate] - a[j,refDate]
      }else {
        actWait[j] <- months[i+1] - a[j,refDate]
      }
      if (actWait[j] < 0){
        errorCount <- errorCount+1
        print("Negative value in month",i,"row",j)
      }
    }
    a$activeWaitTime <- actWait
    waitList[[i]] <- data.frame(a$activeWaitTime,a$Service_Name)
  }
  if (errorCount > 0){
    print(paste("Warning:",errorCount,"negative values detected check the output"))
  } else{
    print("Calculations completed. No negative values detected")
  }
  return(waitList)
}
waitList <- data_monthly(dataClean,3,4) #Inputs: dataframe, referral date column,
#treatment date column


#[18]
calc_wait <- function(dataClean,dateCols){
  dataClean$waitTime <- as.integer(dataClean[[dateCols[2]]] - dataClean[[dateCols[1]]])
  print("The wait time column has been added to the dataframe dataClean")
  return(dataClean)
}
dataClean <- calc_wait(dataClean,c(3,4))  
#Inputs: dataframe, columns to calculate wait times 