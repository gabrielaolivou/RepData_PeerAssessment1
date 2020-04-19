---
title: "Reproducible Research: Peer Assessment 1"
author: "by Gabriela Olivo (github: gabrielaolivou)"
output: 
  html_document:
    keep_md: true
    fig_height: 4
    fig_width: 6
    fig.align: 'center'
    toc: yes
---



## Loading and preprocessing the data

```r
### Download & Unzip the data
get.data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(get.data, destfile = paste0(getwd(),'/activity.zip'), method = "auto")
unzip("activity.zip")

### Read the data
AM_data <- read.csv('activity.csv', header = TRUE)

### See the structure of data
str(AM_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
### Change to date format
AM_data$date <- as.Date(AM_data$date)

### Preview ok the data
head(AM_data, 4)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
```

## What is mean total number of steps taken per day?

```r
# For this part of the assignment, you can ignore the missing values in the dataset.

### Load libraries
library(dplyr); library(ggplot2)

# 1. Calculate the total number of steps taken per day
t_steps <- AM_data %>% group_by(date) %>% summarize(total_steps = sum(na.omit(steps)))

# 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
ggplot(t_steps, aes(total_steps)) + geom_histogram(binwidth=500) + 
    labs(title="Total Number of Steps per Day", x="Total Steps per Day", y="Frequency") + 
    theme_light()
```

![](PA1_template_files/figure-html/Total.Steps.pDay-1.png)<!-- -->

```r
# 3. Calculate and report the mean and median of the total number of steps taken per day
steps_mean <- round(mean(t_steps$total_steps),2)
steps_median <- median(t_steps$total_steps)
cbind(steps_mean, steps_median)
```

```
##      steps_mean steps_median
## [1,]    9354.23        10395
```

* *As we can see, the mean of the total number of steps taken per day is 9354.23 and the median of the total number of steps taken per day is 10395.*

## What is the average daily activity pattern?

```r
### Load libraries
library(dplyr); library(ggplot2); library(xtable)

# 1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
int_steps <-
    AM_data %>% group_by(interval) %>% summarize(mean_steps = mean(na.omit(steps)))

ggplot(int_steps, aes(x = interval, y = mean_steps)) + geom_line() +
    ggtitle("Average Steps per 5-minute interval") + 
    theme_light()
```

![](PA1_template_files/figure-html/Activity.Pattern-1.png)<!-- -->

```r
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
which_5.int <- int_steps[which.max(int_steps$mean_steps),]
tbl_5.int <- xtable(which_5.int)
print(tbl_5.int, type="html")
```

<!-- html table generated in R 3.5.1 by xtable 1.8-4 package -->
<!-- Sun Apr 19 10:13:30 2020 -->
<table border=1>
<tr> <th>  </th> <th> interval </th> <th> mean_steps </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 835 </td> <td align="right"> 206.17 </td> </tr>
   </table>

* *As we can see, the 835 interval is the 5-minute interval which contain the maximum number of steps on average.*

## Imputing missing values

```r
library(dplyr); library(ggplot2); library(scales)

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Count.NA <- sum(is.na(AM_data$steps))
Count.NA
```

```
## [1] 2304
```

```r
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

### Calculate the mean per interval (same as Activity Pattern indication)
int_steps <-
    AM_data %>% group_by(interval) %>% summarize(mean_steps = mean(na.omit(steps)))

### Create NEW Variable with the original data
AM_data_without_NA <- AM_data

### Replace the NA with the mean calculated by interval
AM_data_without_NA$steps <-
    replace(AM_data$steps, is.na(AM_data$steps), int_steps$mean_steps)

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

t_steps_wNA <-
    AM_data_without_NA %>% group_by(date) %>% summarize(total_steps = sum(steps))

### Histogram
ggplot(t_steps_wNA, aes(total_steps)) + geom_histogram(binwidth = 500) +
    labs(title = "Total Number of Steps per Day", 
         x = "Total Steps per Day", y = "Frequency") +
    theme_light()
```

![](PA1_template_files/figure-html/Imputing.Missing.Values-1.png)<!-- -->

```r
### Mean & Median with NA fill in.
steps_mean_wNA <- mean(t_steps_wNA$total_steps)
steps_median_wNA <- median(t_steps_wNA$total_steps)
cbind(steps_mean_wNA, steps_median_wNA)
```

```
##      steps_mean_wNA steps_median_wNA
## [1,]       10766.19         10766.19
```

```r
### Comparison of Mean & Mean without/with NA fill in
c <- rbind(cbind(steps_mean, steps_median),cbind(steps_mean_wNA, steps_median_wNA))
rownames(c) <- c("without NA fill in", "with NA fill in")
c
```

```
##                    steps_mean steps_median
## without NA fill in    9354.23     10395.00
## with NA fill in      10766.19     10766.19
```

```r
### Percentage of NA in the data
pct_NA <- scales::percent(Count.NA / nrow(AM_data), accuracy = .01)
```

* *In this section, the first part to answer is that the data have 2304 missing values (NA).*
* *About the second and third part, the strategy used for filling in all of the missing values in the dataset consist in replace with the mean of that 5-minute interval. For this we took the same code of the before section to calculate the mean per interval then, replicate the orginal data in a new variable and replace the missing values in that new variable.*
* *In the fourd part of this section, we have to answer multiple questions, first the mean of the total number of steps taken per day is 1.0766189\times 10^{4} and the median of the total number of steps taken per day is 1.0766189\times 10^{4}, second we can se that the values differ from when we omit to when we fill it in. As well, we can see that the 13.11% of the data have NA, that explain the variation in the mean and median.*

## Are there differences in activity patterns between weekdays and weekends?


```r
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
AM_data_without_NA$levels <-
    ifelse(
        weekdays(AM_data_without_NA$date) == "sábado" |
            weekdays(AM_data_without_NA$date) == "domingo",
        "weekend",
        "weekday"
    )
### Show how many elements have each level
table(AM_data_without_NA$levels)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
# 2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
m_steps_wNA <-
    AM_data_without_NA %>% group_by(interval, levels) %>% summarize(mean_steps = mean(steps))

ggplot(m_steps_wNA, aes(x=interval, y=mean_steps)) + geom_line() + 
    facet_wrap(~levels, ncol = 1, nrow = 2, strip.position="right") + 
    labs(title = "Comparison of Average Number of Steps per 5-minute interval", 
         x = "Intervals", y = "Mean of Steps") + 
    theme_light()
```

![](PA1_template_files/figure-html/Activity.Patterns.by.Levels-1.png)<!-- -->
