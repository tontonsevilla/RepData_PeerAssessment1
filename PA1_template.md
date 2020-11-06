---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

unzip(zipfile = "activity.zip", exdir = getwd(), overwrite = TRUE)

data <- read.delim("activity.csv", header = TRUE, sep = ",", dec = ".", 
                   skipNul = TRUE)

data$interval_revised <- sprintf("%04d", data$interval)

data$interval_time <- strptime(data$interval_revised, format = "%H%M", tz="UTC")
```


## What is mean total number of steps taken per day?
### 1. Make a histogram of the total number of steps taken each day

```r
data_na_omit <- na.omit(data)

data_total_steps_per_day <- with(data_na_omit, aggregate(steps, list(as.Date(date)), sum))

names(data_total_steps_per_day)[names(data_total_steps_per_day) == "Group.1"] <- "Date"
names(data_total_steps_per_day)[names(data_total_steps_per_day) == "x"] <- "TotalSteps"

ggplot(data_total_steps_per_day, aes(x=TotalSteps, fill=..count..)) %>%
+ geom_histogram(binwidth=1000) + labs(y="By Count", x="Total Steps in a  Day", title="Count of Total Steps Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 2. Calculate and report the mean and median of the total number of steps taken per day

```r
data.frame(data_total_steps_per_day %>%
  group_by(Date) %>%
  summarise_at(vars(TotalSteps), list(mean=mean, median=median))) %>%
  gather(key, value, -Date) %>%
  ggplot(aes(Date, value, fill = key)) + geom_bar(stat = "identity", position = "dodge2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x="Dates", y="Steps", title="Mean and Median of Total Steps Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data_pattern <- split(data, data$interval_revised) 

data_average_steps <- sapply(data_pattern, function(x) mean(x$steps, na.rm = TRUE))

data_day_pattern <- data.frame(time = data$interval_time, ave_steps = data_average_steps)

plot_day_pattern <- ggplot(data_day_pattern, aes(x = time, y = ave_steps))

plot_day_pattern + geom_line(size = 1) + labs(x = "Daily Interval", y = "Number of steps", title = "Average Daily Activity Pattern") + scale_x_datetime(date_labels="%H:%M")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data2 <- data
df_data_average_steps <- data.frame(data_average_steps)
for(i in rownames(df_data_average_steps)){
  data2[data2$interval_revised == i & is.na(data2$steps),]$steps <- data_average_steps[i]
}
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
data_total_steps_per_day2 <- with(data2, aggregate(steps, list(as.Date(date)), sum))

names(data_total_steps_per_day2)[names(data_total_steps_per_day2) == "Group.1"] <- "Date"
names(data_total_steps_per_day2)[names(data_total_steps_per_day2) == "x"] <- "TotalSteps"

ggplot(data_total_steps_per_day2, aes(x=TotalSteps, fill=..count..)) %>%
+ geom_histogram(binwidth=1000) + labs(y="By Count", x="Total Steps in a  Day", title="Count of Total Steps Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
##     yday, year
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
data2$day <- ifelse(weekdays(as.POSIXct(data2$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

data_week <- setDT(data2)[ , .(ave_steps = mean(steps)), by = list(interval,day)]

ggplot(data_week, aes(interval, ave_steps)) + facet_wrap(~day, ncol=1)+geom_line(size = 1) + labs(x = "Interval", y = "Number of steps", title = "Average Activity Pattern (Weekdays vs Weekends)")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
