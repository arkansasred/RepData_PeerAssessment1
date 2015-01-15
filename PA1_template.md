---
title: "Reproducible Research Project 1"
author: "Josh Oberman"
date: "January 14, 2015"
output: html_document
---

**Read in and preprocess the data**, I'm going to tell R to NOT factor the dates and instead use lubridate to convert them to POSIXct format:



```r
library(lubridate)
activity<-read.csv('activity.csv', stringsAsFactors = FALSE)
activity$date<-ymd(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

**What is mean total number of steps taken per day?**
Let's grab the means, medians, and total for the steps at each date using dplyr:


```r
library(dplyr)
library(ggplot2)
activity.tbl<-tbl_df(activity)
total<-activity.tbl%>%
    group_by(date)%>%
    summarize(totalSteps = sum(steps, na.rm = TRUE))
mn<-mean(total$totalSteps)
mdn<-median(total$totalSteps)
mn
```

```
## [1] 9354.23
```

```r
mdn
```

```
## [1] 10395
```

```r
qplot(total$totalSteps, geom="histogram", main = "Histogram of total steps per day", xlab = "Total Steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

**What is the average daily activity pattern?**
Now let's again use dplyr to grab the average steps across days by 5-minute interval and plot it:


```r
meanByInterval<-activity.tbl%>%
    group_by(interval)%>%
    summarize(meanSteps = mean(steps, na.rm = TRUE))
qplot(meanByInterval$interval, meanByInterval$meanSteps, geom = "line", main = "Mean of Steps by Time Interval", xlab = "Time (minutes)", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
##Get the interval wth the maximum number of average steps
meanByInterval$interval[meanByInterval$meanSteps == max(meanByInterval$meanSteps)]
```

```
## [1] 835
```

**Imputing missing values**
First we'll get the number of missing step counts, then fill in each missing value with the average value for that interval across days. The same code from above can be used to compute the summary statistics.


```r
#number of missing vals
length(activity[,1][is.na(activity[,1])])
```

```
## [1] 2304
```

```r
#make activity2, will be the same as activity to start
activity2<-activity
for (i in 1:length(activity2[,1])){
    #if  value X is NA, look up the mean value for that interval and set X to it
    if (is.na(activity2[i,1])){
        interv<-activity2[i,3]
        meanVal<-meanByInterval$meanSteps[meanByInterval$interval==interv]
        activity2[i,1]<-meanVal
    }
}
#Using the same code from above to calculate summary statistics
activity2.tbl<-tbl_df(activity2)
total2<-activity2.tbl%>%
    group_by(date)%>%
    summarize(totalSteps = sum(steps))
mn2<-mean(total2$totalSteps)
mdn2<-median(total2$totalSteps)
mn2
```

```
## [1] 10766.19
```

```r
mdn2
```

```
## [1] 10766.19
```

```r
qplot(total2$totalSteps, geom="histogram", main = "Histogram of total steps per day", xlab = "Total Steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

**Are there differences in activity patterns between weekdays and weekends?**

For this, we'll define a funciton that takes in day names and tells us if it's a weekend or weekday, then we'll use the same code from above to take the mean steps by interval, and plot the time series graphs using ggplot, faceting by weekend/weekday


```r
isWeekend<-function(day){
    if (day=="Saturday" | day=="Sunday"){
        day <- "Weekend"
    }
    else{
        day <- "Weekday"
    }
}
#apply the weekdays function to get day names, then isWeekend function
activity2$day<-sapply(activity2$date, weekdays)
activity2$day<-sapply(activity2$day, isWeekend)
activity2$day<-factor(activity2$day)

activity2.tbl<-tbl_df(activity2)
meanByInterval2<-activity2.tbl%>%
    group_by(interval, day)%>%
    summarize(meanSteps = mean(steps))
qplot(data = meanByInterval2, x = interval, y = meanSteps, facets = .~day, geom = "line", main = "Mean of Steps by Time Interval", xlab = "Time (minutes)", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The graph for weekday shows a definite peak around 800 minutes, perhaps due to a walk to work or something of the like, the weekend graph also has a peak around that time, bu not significantly higher than the other peaks throughout the day
