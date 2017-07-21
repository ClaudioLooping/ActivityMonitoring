# Activity Monitoring
Claudio  
21/07/2017  



# Loading and preprocessing the data

We stop the process if the csv file is not in the working directory.  
Storing data into a data frame, without addressing NA's
Storing data without NA in another data frame


```r
file <- "activity.csv"
  
if (!file.exists(file)) {
        stop("activity.csv file not found")
}

activity <- read.csv(file)
NoNAActivity <- subset(activity, is.na(activity$steps) == F)
```

# What is mean total number of steps taken per day?

## Calculating the total number of steps taken each day

```r
library(plyr)
stepsPerDay <- ddply(NoNAActivity, .(date), summarise, steps=sum(steps))
```

## Creating the histogram

```r
hist(stepsPerDay$steps, breaks = 20, main="Number of Steps a day", xlab="Total number of steps taken each day", ylab = "Number of Days", col="cyan")
```

![](ActivityMonitoring_files/figure-html/histogramsteps-1.png)<!-- -->

## Calculate and report the mean and median total number of steps taken per day
1. Mean

```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

2. Median

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```

# What is the average daily activity pattern?

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Computing the average number of steps taken in each 5-minutes intervals


```r
averageInterval <- ddply(NoNAActivity, .(interval), summarise, steps=mean(steps))
```

Creating the plot


```r
plot(averageInterval$interval, averageInterval$steps,axes = F, type="l", col="blue", xlab="Time", ylab="Average Number of Steps", main="Average Daily Activity Pattern")
axis(1,at=c(0,600,1200,1800,2400), label = c("0:00","6:00","12:00","18:00","24:00"))
axis(2)
```

![](ActivityMonitoring_files/figure-html/Plot-1.png)<!-- -->

## 2. Which 5-minutes interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageInterval[which.max(averageInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

And the winner is ... : interval from 8:35 to 8:40

# Imputing missing values

## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

## 2. Devise a strategy for filling in all of the missing values in the dataset.
Considering the data summary  
Min.   :  0.00  , 1st Qu.:  0.00  , Median :  0.00  , Mean   : 37.38  , 3rd Qu.: 12.00  , Max.   :806.00  , NA's   :2304  , 2012-10-01:  288  , 2012-10-02:  288  , 2012-10-03:  288  , 2012-10-04:  288  , 2012-10-05:  288  , 2012-10-06:  288  , (Other)   :15840  , Min.   :   0.0  , 1st Qu.: 588.8  , Median :1177.5  , Mean   :1177.5  , 3rd Qu.:1766.2  , Max.   :2355.0  , NA,  
I've decided to replace NA's with average value for that 5-min interval

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityNAFilled <- activity

for (i in 1:nrow(activityNAFilled)){
    if (is.na(activityNAFilled$steps[i])){
        activityNAFilled$steps[i] <- averageInterval$steps[which(activityNAFilled$interval[i] == averageInterval$interval)]}
}

activityNAFilled <- arrange(activityNAFilled, interval)
```

## 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.  
###Do these values differ from the estimates from the first part of the assignment?  
###What is the impact of imputing missing data on the estimates of the total daily number of steps?

Calculating the total number of steps taken each day


```r
totalDayNAFilled <- ddply(activityNAFilled, .(date), summarise, steps=sum(steps))
```

Creating the plot


```r
hist(totalDayNAFilled$steps, breaks = 20, main="Number of Steps (no NA's)", xlab="Total number of steps taken each day", ylab = "Number of Days", col="cyan")
```

![](ActivityMonitoring_files/figure-html/plot-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day on the imputed dataset


```r
mean(totalDayNAFilled$steps)
```

```
## [1] 10766.19
```

```r
median(totalDayNAFilled$steps)
```

```
## [1] 10766.19
```

Test whether these values differ from the ones with NA not filled


```r
abs(mean(stepsPerDay$steps)-mean(totalDayNAFilled$steps))
```

```
## [1] 0
```

```r
abs(median(stepsPerDay$steps)- median(totalDayNAFilled$steps))/median(stepsPerDay$steps)
```

```
## [1] 0.0001104207
```

The mean didn't change after the imputing, the median slightly changed about 0.1% of the original value.

Test how total steps taken per day differ


```r
Difference <- sum(activityNAFilled$steps) - sum(NoNAActivity$steps)
Difference
```

```
## [1] 86129.51
```

Conclusion: to fill NA's of the dataset causes the estimation on total steps per day to increase

# Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activityNAFilled$weekdays <- weekdays(as.Date(activityNAFilled$date))
activityNAFilled$weekdays <- ifelse(activityNAFilled$weekdays %in% c("Samedi", "Dimanche"),"weekend", "weekday")
```

## 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Computing the average for each interval


```r
average <- ddply(activityNAFilled, .(interval, weekdays), summarise, steps=mean(steps))
```

Creating the plot


```r
library(lattice)
xyplot(steps ~ interval | weekdays, data = average, layout = c(1, 2), type="l", xlab = "Interval", ylab = "Number of steps")
```

![](ActivityMonitoring_files/figure-html/PlotInt-1.png)<!-- -->
