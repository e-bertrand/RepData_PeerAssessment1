# Reproducible Research: Peer Assessment 1
Enrique Bertrand  
February, 3, 2016  

This report analyzes the dataset `activity` and documents the answer of some questions about its variables. The dataset `activity` includes the number of steps taken in 5 minute intervals each day from a personal activity monitoring device. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012.

In order to reproduce the analysis, package `ggplot2` must be installed and loaded. If not, execution will be stopped.


```r
resp <- require("ggplot2", quietly = TRUE)

if (!resp) {
  cat("\nPlease, install 'ggplot2' package in your environment before running this script\n")
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg))
}
```


## Loading and preprocessing the data

The raw file `activity.csv` has been obtained, first downloading this zip file <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>, and then unzipping it. It must be available in the same directory where this script is running.

The file is read and loaded in the data.frame `activity`:


```r
if (!file.exists("activity.csv")) {
  cat("\nPlease, 'activity.csv' file  must be in this directory before running the script\n")
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg))
}

activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

head(activity, 5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Before the data can be processed some cleaning steps must be done:

* Column `activity$date` should be formatted as Date.

* Intervals in column `activity$interval` are not really well shaped. Hours and minutes are put together and, as a consequence, there are "jumps" in the time axis. For instance, they jump from `155` to `200` or from `1055` to `1100`. As plots could be slighty distorted (although it is not always noticeable given the size of the plots), minutes should be transformed in fractions of hour.

Let's do this cleaning:


```r
# Converting date column format from character to Date
activity$date <- as.Date(activity$date)

# Extracting minutes and hours from an interval
minutes_interv <- as.integer(with(activity, 
                                  substring(interval, 
                                            nchar(interval) - 1, 
                                            nchar(interval))))
hours_interv <- as.integer(with(activity, 
                                substring(interval, 
                                          1, 
                                          nchar(interval) - 2)))

# hour_interv for hour 0 appears as NA; assingning the right hour
hours_interv[is.na(hours_interv)] <- 0

# Creating a "normalized" interval column with minutes as a fraction of hour  
activity$interval_norm <- as.integer(round((hours_interv + minutes_interv / 60) * 100))

head(activity, 5)
```

```
##   steps       date interval interval_norm
## 1    NA 2012-10-01        0             0
## 2    NA 2012-10-01        5             8
## 3    NA 2012-10-01       10            17
## 4    NA 2012-10-01       15            25
## 5    NA 2012-10-01       20            33
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps        : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date         : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval     : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ interval_norm: int  0 8 17 25 33 42 50 58 67 75 ...
```


## What is mean total number of steps taken per day?

The following histogram shows the distribution of the total number of steps taken per day. Take into account that in this phase of analysis nothing has been done yet with missing values.


```r
steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum)

g <- ggplot(data = steps_per_day, aes(x = steps)) + 
        geom_histogram(binwidth = 2000, color = "darkred", fill = "orange") + 
        labs(title = "Distribution of total steps / day") +
        labs(y = "Frecuency", x = "Number of total steps / day") +
        scale_x_continuous(breaks = seq(0, 22500, by = 2000), expand = c(0, 0))

print(g)
```

![](PA1_template_files/figure-html/step_histogram-1.png)\

Now, let's estimate the mean and the median of the total number of steps taken per day:


```r
mean_steps_per_day <- mean(steps_per_day$steps)

cat("Mean of total steps per day = ", mean_steps_per_day)
```

```
Mean of total steps per day =  10766.19
```

```r
median_steps_per_day <- median(steps_per_day$steps)

cat("Meadian of total steps per day = ", median_steps_per_day)
```

```
Meadian of total steps per day =  10765
```


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
