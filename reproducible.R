# Download and unzip data
getwd()
setwd("C:\\Users\\Hlangano\\Downloads\\Reproducible Research\\Project 1")
if(!file.exists("data"))dir.create("data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile = "./data/repdata%2Fdata%2Factivity.zip")
unzip("./data/repdata%2Fdata%2Factivity.zip",exdir = "./data")
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
#Reading csv Data into Data.Table
activityDT <- data.table::fread(input = "data/activity.csv")
activityDT$day <- weekdays(as.Date(activityDT$date))
activityDT$DateTime <- as.POSIXct(activityDT$date,format = "%Y%M%D")
## Data without na's
clean <- activityDT[!is.na(activityDT$steps),]
# Mean Total number of steps taken per day? 

#Total steps per day

totalstepsperday <- aggregate(steps ~ date,data = activityDT,FUN = sum, na.rm = FALSE)
totalstepsperday
# Creating the histogram of total steps per day

hist(totalstepsperday$steps,breaks = 5, xlab = "steps", main = "Total Steps per Day",col = "red")
## Mean of steps
as.integer(mean(totalstepsperday$steps))

##Median of Steps
as.integer(median(totalstepsperday$steps))
## Time series plot of the average number of steps taken 
StepsPerInterval <- tapply(activityDT$steps,activityDT$interval,mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)),StepsPerInterval,xlab = "5-Minute Iterval",ylab = "Average Steps Taken", main = "Average Daily Activity Pattern", type = "l",col = "red")
# Maximum number of steps
maxInterval <- names(sort(StepsPerInterval,decreasing = TRUE)[1])
maxsteps <- sort(StepsPerInterval,decreasing = TRUE)[1]
maxsteps
# Calculate and report the total number of missing values in the dataset
nrow <- sum(is.na(activityDT$steps))
nrow
#Replace NA values with the mean results for five minute intervals
# Create a new dataset to the original dataset but with the missing datafilled in
## Create the average number of steps per weekdays and interval
activityDT2 <- activityDT
nas <- is.na(activityDT2$steps)

StepsPerInterval <- tapply(activityDT$steps,activityDT,mean,na.rm =TRUE)
activityDT2$steps[nas] <- AverageSteps[as.character(activityDT2$interval[nas])]                       
names(activityDT2) 
## Checking for NON NA
sum(is.na(activityDT2))
#Creating the histogram of total steps per day catergorized by dataset
## Histogram with no NA values
StepsPerDay2 <- aggregate(steps~date, data = activityDT2, FUN = sum, na.rm = TRUE)
hist(StepsPerDay2$steps,main = StepsPerDay2,ylab = "interval",col ="red",breaks = 5)
# Create a new catergory based on two levels  indicating whether a given date is weekend or weekday 
## Add the new weekend/weekday field and making a new factor
activityDT2 <-data.table::fread(input ="data/activity.csv")  
activityDT2[, date := as.POSIXct(date,format = "%Y-%m-%d")]  
activityDT2[,"Day of week":= weekdays(x = date)]
activityDT2[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday",x ="Day of week"),"weekday or weekend"] <- "weekday"
activityDT2[grepl(pattern = "Saturday|Sunday",x = "Day of Week"),"weekend or weekday"] <- "weekend"
head(activityDT2,10)
# Making a panel plot containing a time series plot
activityDT2[is.na(steps),"steps"] <- activityDT2[, c(lapply(.SD,median, na.rm =TRUE)), .SDcols =c("steps")]
IntervalDT <- activityDT2[, c(apply(.SD,mean,na.rm = TRUE)), .SDcols = c("steps"), by =(interval,"weekday or weekend")]
#5 Minute Interval
Interval[Steps == max(steps),.(maxInterval == interval)]
StepsPerDay2 <- activityDT2[, c(lapply(.SD,mean,na.rm = TRUE)), .SDcols = c("steps"), by =(interval,'weekday or weekend')]
ggplot(IntervalDT,aes(x = interval, y = steps, color ="weekday or weekend"))+geom_line()+labs(title = "Avg.Daily Steps by Weektype", x = "Interval", y = "No of Steps" )+ facet_wrap(~"weekday or weekend")
