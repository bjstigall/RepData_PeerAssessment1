# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
actdat <- read.csv("activity.csv", na.strings = "NA", header = TRUE)
actdat$date <- as.Date(actdat$date)

## What is mean total number of steps taken per day?

TotalSteps <- aggregate(steps~date, data=actdat, sum, na.rm=TRUE)
hist(TotalSteps$steps, breaks=10)
mean(TotalSteps$steps)
median(TotalSteps$steps)

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
