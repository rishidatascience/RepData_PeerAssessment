#Data

The data for this assignment was downloaded from the course web site:

[Dataset: Activity monitoring data [52K] ](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

### The variables included in this dataset are:
1. steps Number of steps taking in a 5-minute interval (missing values are coded as NA)
2. date: The date on which the measurement was taken in YYYY-MM-DD format
3. interval: Identifier for the 5-minute interval in which measurement was taken The dataset is stored in a    
4. comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


###Loading and preprocessing the data

#### Download, unzip and load data into data frame  data . 


```r
library(ggplot2)

# Download the file if necessary (29MB)
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filePath <- "./data/repdata%2Fdata%2Factivity.zip"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists(filePath)) {
  download.file(fileUrl, destfile = filePath)
   
  # And unzip it
  unzip(filePath, exdir="./data")
}
```


## Load input data from a zip file from the current R working directory.


```r
# Load the raw activity data
activityData <- read.csv("./data/activity.csv", stringsAsFactors=FALSE)
```



### Calculate the total number of steps taken per day?


```r
# Compute the total number of steps each day (NA values removed)
totalStepsPerDay <- aggregate(activityData$steps, by=list(activityData$date), FUN=sum, na.rm=TRUE)

# Rename the attributes
names(totalStepsPerDay) <- c("date", "StepsDay")

# Compute the histogram of the total number of steps each day
hist(totalStepsPerDay$StepsDay, breaks=seq(from=0, to=25000, by=1000), col="blue", xlab="Total number of steps", ylim=c(0, 20), main="Histogram of the total number of steps taken each day\n(NA removed)")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Calculate and report the mean and median total number of steps taken per day

```r
rmean <- mean(totalStepsPerDay$StepsDay)
rmedian <- median(totalStepsPerDay$StepsDay)
```

  
### What is the average daily activity pattern?

```r
MeanStepsPerInterval <- aggregate(steps ~ interval, activityData, mean)

plot(MeanStepsPerInterval$interval,MeanStepsPerInterval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Mean Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
### Find interval with most average steps. 

```r
maxInterval <- MeanStepsPerInterval[which.max(MeanStepsPerInterval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is  r [maxInterval] 


### Impute missing values. Compare imputed to non-imputed data.


```r
fillDataMissing<- transform(activityData, steps = ifelse(is.na(activityData$steps), MeanStepsPerInterval$steps[match(activityData$interval, MeanStepsPerInterval$interval)], activityData$steps))
```
                
### Recount total steps by day and create Histogram.  


```r
stepsPerDayRecal <- aggregate(steps ~ date, fillDataMissing, sum)
hist(stepsPerDayRecal$steps, main = paste("Total Steps Each Day"), col=3, xlab="Number of Steps")

#Create Histogram to show difference. 
hist(totalStepsPerDay$StepsDay, main = paste("Total Steps Per Day"), col=5, xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c(3, 5), lwd=10)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)


###Calculate new mean and median for imputed data. 

```r
rmeanNew <- mean(stepsPerDayRecal$steps)
rmedianNew <- median(stepsPerDayRecal$steps)
```
  

## Are there differences in activity patterns between weekdays and weekends?

```r
daytype <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
activityData$date <- as.Date(activityData$date)
activityData$day <- sapply(activityData$date, FUN = daytype)
```

## Panel plot with average number of steps taken on weekdays and weekends.

```r
avgSteps <- aggregate(steps ~ interval + day, data = activityData, mean)
ggplot(avgSteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

