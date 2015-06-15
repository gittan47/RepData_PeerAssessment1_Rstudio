# Reproducible Research_ Assignment 1
Aniella Beser  
Sunday, June 14, 2015  
Dataset: Activity monitoring data [52K]
=======================================
The variables included in this dataset are:

.steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)


.date: The date on which the measurement was taken in YYYY-MM-DD format


.interval: Identifier for the 5-minute interval in which measurement was taken


The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

knitr settings:


```r
require(knitr)
```

```
## Loading required package: knitr
```

```
## Warning: package 'knitr' was built under R version 3.1.3
```

```r
opts_chunk$set(echo = TRUE, cache = TRUE, cache.path = "cache/", fig.path = "figure/")
```


```r
library(data.table)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
```

1. Load the data (i.e. read.csv())
 It is assumed that the file activity.csv is in the current working directory.


```r
actdata<- read.csv(file="activity.csv", header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```




2.Process/transform the data into a format suitable for your analysis

I convert the date field to Date class and interval field to Factor class.


```r
actdata$date <- as.Date(actdata$date, format = "%Y-%m-%d")
actdata$interval <- as.factor(actdata$interval)
```

What is the mean total number of steps taken per day?

For this part of the assignment, I ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day

2.Make a histogram of the total number of steps taken each day

3.Calculate and report the mean and median of the total number of steps taken per day

How does it look now?

```r
str(actdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps_per_day <- aggregate(steps ~ date, actdata, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)

ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of average Steps taken per Day", 
             x = "Number of Steps per Day", y = "Times in a day") + theme_bw() 
```

![](figure/unnamed-chunk-5-1.png) 

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
This is to calculate the mean and median of the number of steps taken per day.

```r
meansteps   <- mean(steps_per_day$steps, na.rm=TRUE)
mediansteps <- median(steps_per_day$steps, na.rm=TRUE)
```

The mean number of steps is 10766.19 and median is 10765

What is the average daily activity pattern?

I calculate the aggregation of steps by intervals of 5-minutes and convert the intervals as integers and save them in a data frame called interval_steps.


```r
interval_steps <- aggregate(actdata$steps, 
                                by = list(interval = actdata$interval),
                                FUN=mean, na.rm=TRUE)

interval_steps$interval <- 
        as.integer(levels(interval_steps$interval)[interval_steps$interval])
colnames(interval_steps) <- c("interval", "steps")
```

The plot is made with the time series of the average number of steps taken (averaged) versus the 5-minute intervals:

```r
        ggplot(interval_steps, aes(x=interval, y=steps)) +   
        geom_line(color="red", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
```

![](figure/unnamed-chunk-8-1.png) 

Find the 5-minute interval containing the maximum number of steps:


```r
        max_interval <- interval_steps[which.max(  
                interval_steps$steps),]
```
The 835th interval has maximum 206 steps.

Imputing missing values:
        
1. Total number of missing values:
        
The total number of missing values in steps is calculated using is.na() to check whether the value is missing or not and then summing.


```r
missing_vals <- sum(is.na(actdata$steps))
```
The total number of missing values is 2304.

2. Strategy for filling all of the missing values into the dataset

To fill with missing values, replace them with the mean value (could use median instead) at the same interval across days.

Create a function na_fill whith the data argument and pervalue argument i the interval_steps data frame.

```r
na_fill <- function(interval_steps, pervalue) {
        na_index <- which(is.na(interval_steps$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = interval_steps[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- interval_steps$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

actdata_fill <- data.frame(  
        steps = na_fill(actdata, interval_steps),  
        date = actdata$date,  
        interval = actdata$interval)
str(actdata_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

Check if there are any missing values remaining

```r
sum(is.na(actdata_fill$steps))
```

```
## [1] 0
```
Zero output shows that there are NO MISSING VALUES.

3. A histogram of the total number of steps taken each day

Plot a histogram of the daily total number of steps taken, with a bin interval of 1000 steps after filling missing values.

```r
fill_steps_per_day <- aggregate(steps ~ date, actdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")
```
Plotting the histogram


```r
ggplot(fill_steps_per_day, aes(x = steps)) + 
        geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day, with imputed data", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```

![](figure/unnamed-chunk-14-1.png) 

Calculate again as above but with imputed data and report the mean and median total number of steps taken per day.


```r
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
```
The mean is 10766.189 and median is 10766.189.

Do these values differ from the estimates from the first part of the assignment?

These values do differ very slightly.

.Before imputing the data
1.Mean : 10766.189
2.Median: 10765


.After imputing the data
1.Mean : 10766.189
2.Median: 10766.189


The values after filling the data mean and median are equal.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

Comparing with the calculations done in the first section of this document, we observe that while the mean value remains unchanged, the median value has shifted and virtual matches to the mean. 

Since our data have shown a near normal distribution (see both histograms), it seems that the impact of imputing missing values has increase the peak, but it doesn't affect negatively the predictions. 

Are there differences in activity patterns between weekdays and weekends?

The comparison is done with the table with filled-in missing values.
1. Augment the table with a column that indicates the day of the week
2. Subset the table into two parts - weekends (Saturday and Sunday) and weekdays (Monday through Friday).
3. Tabulate the average steps per interval for each data set.
4. Plot the two data sets side by side for comparison. 

Because of Swedish install of windows in my PC I have to change to english weekdays, else I get very strange signs that cant be calculated.


```r
Sys.setlocale("LC_TIME", "English")
weekdays_steps <- function(actdata) {
weekdays_steps <- aggregate(actdata$steps, by=list(interval = actdata$interval),
FUN=mean, na.rm=T)
# convert to integers for plotting
weekdays_steps$interval <- 
as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
colnames(weekdays_steps) <- c("interval", "steps")
weekdays_steps
}

data_by_weekdays <- function(actdata) {
actdata$weekday <- 
as.factor(weekdays(actdata$date)) 
weekend_data <- subset(actdata, weekday %in% c("Saturday","Sunday"))
weekday_data <- subset(actdata, !weekday %in% c("Saturday","Sunday"))

weekend_steps <- weekdays_steps(weekend_data)
weekday_steps <- weekdays_steps(weekday_data)

weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

data_by_weekdays <- rbind(weekend_steps, weekday_steps)
data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
data_by_weekdays
}

data_weekdays <- data_by_weekdays(actdata_fill)
```

```
## [1] "English_United States.1252"
```

Below is the panel plot for comparing the average number of steps taken per 5-minute interval across weekdays and weekends:


```r
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
geom_line(color="slategrey") + 
facet_wrap(~ dayofweek, nrow=2, ncol=1) +
labs(x="Interval", y="Number of steps") +
theme_bw()
```

![](figure/unnamed-chunk-17-1.png) 

In the graphs above the activity on the weekday has the greatest peak from all steps intervals. But, weekends activities have more peaks over a hundred than weekday. This could be due to the fact that activities on weekdays mostly follow a work related routine, where we find some more intensity activity in free time where one can do some sports activities. At weekend there is better distribution of effort along all time.
