---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.4.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.4.3
```

``` r
# Read the dataset
activity <- read.csv("activity.csv")

# Check the structure
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

``` r
# Calculate total steps per day
total_steps <- activity %>%
  group_by(date) %>%
  summarize(total = sum(steps, na.rm = TRUE))

# Plot histogram
hist(total_steps$total, breaks=20, main="Total Steps per Day", xlab="Steps", col="blue")
```

![](PA!_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
# Mean and Median
mean_steps <- mean(total_steps$total)
median_steps <- median(total_steps$total)

mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?

``` r
# Average steps per interval
avg_steps_interval <- activity %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))

# Time series plot
plot(avg_steps_interval$interval, avg_steps_interval$mean_steps, type = "l",
     xlab="5-minute Interval", ylab="Average Steps", main="Average Daily Activity Pattern")
```

![](PA!_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
# Interval with maximum average steps
max_interval <- avg_steps_interval[which.max(avg_steps_interval$mean_steps), ]
max_interval
```

```
## # A tibble: 1 × 2
##   interval mean_steps
##      <int>      <dbl>
## 1      835       206.
```


## Imputing missing values


``` r
# Total number of missing values
sum(is.na(activity$steps))
```

```
## [1] 2304
```

``` r
# Fill NA with mean for that interval
activity_imputed <- activity
activity_imputed$steps[is.na(activity_imputed$steps)] <- 
  avg_steps_interval$mean_steps[match(activity_imputed$interval, avg_steps_interval$interval)]
```

```
## Warning in activity_imputed$steps[is.na(activity_imputed$steps)] <-
## avg_steps_interval$mean_steps[match(activity_imputed$interval, : number of
## items to replace is not a multiple of replacement length
```

``` r
# New total steps per day
total_steps_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarize(total = sum(steps))

# Plot histogram after imputation
hist(total_steps_imputed$total, breaks=20, main="Total Steps per Day (Imputed)", xlab="Steps", col="green")
```

![](PA!_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
# New mean and median
mean_steps_imputed <- mean(total_steps_imputed$total)
median_steps_imputed <- median(total_steps_imputed$total)

mean_steps_imputed
```

```
## [1] 10766.19
```

``` r
median_steps_imputed
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# Add a new column: weekday or weekend
activity_imputed$day_type <- ifelse(weekdays(as.Date(activity_imputed$date)) %in% c("Saturday", "Sunday"), 
                                    "weekend", "weekday")

# Average steps by interval and day type
avg_steps_daytype <- activity_imputed %>%
  group_by(interval, day_type) %>%
  summarize(mean_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

``` r
# Plot
library(lattice)
xyplot(mean_steps ~ interval | day_type, data=avg_steps_daytype, type="l",
       layout=c(1,2), xlab="Interval", ylab="Number of steps")
```

![](PA!_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Conclusion
After imputing missing values, mean and median were slightly different.

Weekends and weekdays show different step patterns.
