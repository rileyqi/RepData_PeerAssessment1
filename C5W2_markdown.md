# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
#read file
activity<-read.csv('activity.csv')

#remove missing value
completeac<-activity[complete.cases(activity), ]
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
#calculate the total steps taken per day
aggbydate<-aggregate(completeac$steps, by = list(completeac$date), FUN = sum)
colnames(aggbydate)<-c('date','totalsteps')

#create histogram
hist(aggbydate$totalsteps, main = 'the total number of steps taken each day', xlab = 'step')
```

![](C5W2_markdown_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#calculate mean
mean(aggbydate$totalsteps)
```

```
## [1] 10766.19
```

```r
#calculate median
median(aggbydate$totalsteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#calculet average steps per interval for all days
avg_by_interval<-aggregate(completeac$steps, by=list(completeac$interval), FUN=mean)
colnames(avg_by_interval)<-c('interval','step')

#make a time series plot
plot(avg_by_interval$interval,avg_by_interval$step, type = 'l',col=1, main = 'average number of steps by interval',
     xlab = 'intervals',ylab = 'average number of steps')
```

![](C5W2_markdown_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#identify the interval which has the highest average steps
max_interval<- subset(avg_by_interval,step==max(avg_by_interval$step),select = c(interval))
max_interval
```

```
##     interval
## 104      835
```

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#calculate the total missing values
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
#use the mean of that interval to replace missing value
for (i in 1:nrow(activity)) {
  if(is.na(activity$steps[i])) {
    avg <- avg_by_interval$step[which(avg_by_interval$interval == activity$interval[i])]
    activity$steps[i] <- avg 
  }
}

#aggregate the steps per day
newaggregate<-aggregate(activity$steps, by = list(activity$date), FUN = sum)
colnames(newaggregate)<-c('date','step')

#create a histogram
hist(newaggregate$step, main = 'the total number of steps taken each day',xlab = 'step')
```

![](C5W2_markdown_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#calculate the mean
mean(newaggregate$step)
```

```
## [1] 10766.19
```

```r
#calculate the median
median(newaggregate$step)
```

```
## [1] 10766.19
```

```r
###not changed because we used the average to fill out the missng values
```

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#turn date into weekday types
activity$day <- weekdays(as.Date(activity$date))

#create a function to define the weekday type
weekday <- function(date_day) {
  if  (!(date_day == 'Saturday' || date_day == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}

#create a new column
activity$weekday_type <- as.factor(sapply(activity$day, weekday))

library(ggplot2)

#aggregate the steps by interval and weekday type
steps_per_day_weekday <- aggregate(steps ~ interval+weekday_type, activity, mean)

#create the plot
plot <- ggplot(steps_per_day_weekday, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = weekday_type)) +
  theme_gray() +
  facet_grid(weekday_type ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=expression("Steps")) +
  ggtitle("Steps Per Interval by day type")
plot
```

![](C5W2_markdown_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
### We could see some differences between these two plots. Poeple usually
### start walking later in the weekend than in the weekdays.
```
