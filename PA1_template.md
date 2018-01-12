Loading and preprocessing the data
=========================
Download, unzip and loading of data


```r
filename<- unzip("activity.zip")
file<- read.csv(filename, stringsAsFactors = FALSE)
str(file)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

What is mean total number of steps taken per day?
========================
Calculating the sum of steps taken per day and constructing histogram on the sum of steps taken per day. Mean and median are also displayed.


```r
totalsteps<- tapply(file$steps, file$date, sum)
hist(totalsteps, main = "Total Steps Taken Per Day", xlab = "Total Steps", col = "blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
filemean<- mean(totalsteps[!is.na(totalsteps)])
filemedian<- median(totalsteps[!is.na(totalsteps)])
```

What is the average daily activity pattern?
============================

Constructing time series plot for Average Daily Activity Pattern


```r
averagesteps<- tapply(file$steps, file$interval, mean, na.rm = TRUE)
plot(y = averagesteps, x= names(averagesteps), type = "l", main = "Average Daily Activity Pattern", xlab = "Interval", ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


```r
averagesteps[which.max(averagesteps)]
```

```
##      835 
## 206.1698
```
The maximum number of steps is 206.1698 at the 835 interval.

Imputing missing values
==============================

Calculating total number of missing values


```r
sum(is.na(file))
```

```
## [1] 2304
```

Using mean of the 5-minute interval, creating a new dataset with missing value filled in


```r
act_new <- file

act_new[which(is.na(act_new$steps)),1]<- averagesteps[as.character(act_new[which(is.na(act_new$steps)),3])]

sum(is.na(act_new))
```

```
## [1] 0
```

Constructing histogram with missing values replaced and comparing with the original histogram 


```r
newset<- tapply(act_new$steps, act_new$date, sum)
hist(newset, 10, main = "Total Steps Taken Per day (missing values replaced)", xlab = "Total Steps", ylim = c(0,25))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
hist(totalsteps, 10, main = "Total Steps Taken Per Day", xlab = "Total Steps", col = "blue", ylim = c(0,25))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)

Calculating mean and median of the new histrogram (missing values replaced) and comparing with the original histogram


```r
newmean<- mean(newset)
newmedian<- median(newset)

newmean - filemean
```

```
## [1] 0
```

```r
newmedian - filemedian
```

```
## [1] 1.188679
```
The impact of imputting missing data on the estimates of the total daily number of steps is minimal, with only the median recording a difference of 1 step.

Are there differences in activity patterns between weekdays and weekends?
============

New dataset with two levels: "weekday" and "weekend"

```r
newest<- act_new
weekend<- weekdays(as.Date(newest$date)) %in% c("Saturday", "Sunday")
newest$daytype <- "weekday"
newest$daytype [weekend == TRUE] <- "weekend"
newest$daytype<- as.factor(newest$daytype)
```

Constructing a panel plot


```r
newinterval<- aggregate(steps ~ interval + daytype, newest, mean)

library(lattice)
xyplot(steps~ interval|daytype, newinterval, type = "l", layout = c(1,2), xlab = "Interval", ylab = " Number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
