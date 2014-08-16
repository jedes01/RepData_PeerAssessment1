## Reproducible Research
## Peer Assessment 1
library(plyr)
library(ggplot2)

fileURL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(fileURL, temp)
data <- read.csv(unzip(temp, "activity.csv"), header=T)
rm(fileURL)
rm(temp)

## Setting up useful classes
data$interval <- factor(data$interval)
data$fancyDate <- strptime(data$date, format="%Y-%m-%d")
data$day <- factor(cut(data$fancyDate, breaks="day", labels=F))


## Mean, Median Steps per Day
averageSteps <- aggregate(data$steps, by=list(data$day), function(x) sum(x, na.rm=T))
ggplot(averageSteps, aes(x)) + geom_histogram(binwidth=2000) + 
        labs(title="Histogram of Steps/Interval\n(Binwidth = 2000)", x = "Steps", y = "Count") +
        theme(plot.title = element_text(face="bold"))
meanAverage <- mean(averageSteps$x, na.rm=T)
medianAverage <- median(averageSteps$x, na.rm=T)

## Steps per Interval
intervalMeans <- aggregate(data$steps, list(data$interval), function(x) mean(x, na.rm=T))
names(intervalMeans) <- c("interval","intervalMean")
plot.ts(intervalMeans$intervalMean, xlab="Nth 5-Minute Interval of the Day", ylab="Mean Steps",
        main="Mean Steps per 5-Minute Interval")
peakMean <- round(max(intervalMeans$intervalMean),1)
peakInterval <- which.max(intervalMeans$intervalMean)

## Check for NAs
totalNAs <- sum(is.na(data))
columnNAs <- sapply(1:5, function(x) sum(is.na(data[,x])))


## Imputed Dataset
data <- join(data, intervalMeans)
data$imputedSteps <- data$steps
data$imputedSteps[which(is.na(data$steps))] <- data$intervalMean[which(is.na(data$steps))]

## Create new dataset with imputed values
newData <- data.frame(data$imputedSteps, data$date, as.numeric(as.character(data$interval)))
names(newData) <- c("steps", "date", "interval")

## Setting up useful classes AGAIN, re-joining intervalMeans
newData$fancyDate <- strptime(data$date, format="%Y-%m-%d")
newData$day <- factor(cut(data$fancyDate, breaks="day", labels=F))
newData$interval <- factor(data$interval)
newData <- join(data, intervalMeans)

## Imputed average steps per day
averageImpSteps <- aggregate(newData$imputedSteps, list(newData$day), function(x) sum(x, na.rm=T))
ggplot(averageImpSteps, aes(x)) + geom_histogram(binwidth=2000) +
        labs(title="Histogram of Steps/Interval\n(Binwidth = 2000)", x = "Steps", y = "Count") +
        theme(plot.title = element_text(face="bold"))
meanImpAverage <- round(mean(averageImpSteps$x, na.rm=T),1)
medianImpAverage <- median(averageImpSteps$x, na.rm=T)

## Where are the NAs? (turns out there are eight single-day chunks missing)

dayNAs <- aggregate(newData$steps, list(newData$day), function(x) sum(is.na(x)))
plot(dayNAs$x, xlab="Day", ylab="Total NAs", main="NAs per Day")

## Day of the week
newData$dayOfWeek <- weekdays(newData$fancyDate)
newData$isWeekend <- as.factor(newData$dayOfWeek %in% c("Saturday", "Sunday"))
levels(newData$isWeekend) <- c("weekday","weekend")
aggData <- aggregate(newData$imputedSteps, list(newData$interval, newData$isWeekend), mean)
names(aggData) <- c("interval", "isWeekend", "meanSteps")
p <- ggplot(aggData, aes(x=as.numeric(as.character(interval)), y=meanSteps)) + geom_line() + 
        facet_grid(isWeekend~.)
p <- p + xlab("Interval") + ylab ("Number of Steps") + 
        ggtitle("Average Steps Through the Day\nWeekday vs. Weekend") + 
        theme(plot.title = element_text(face="bold"))
p