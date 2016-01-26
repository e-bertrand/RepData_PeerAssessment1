# Reproducible Research: Peer Assessment 1


```r
resp <- require("ggplot2", quietly = TRUE)

if (!resp) {
  cat("\nPlease, install 'readr' package in your environment",
      "before running this script\n")
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), 
                                      collapse=" "));
  stop(simpleError(blankMsg))
}
```


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", stringsAsFactors=FALSE)

# Converting date column format from character to Date
activity$date <- as.Date(activity$date)

# Intervals are no really well formed as, for instance, they jump from  
# 155 to 200 or from 1055 to 1100. As plots could be slighty distorted, 
# we should convert interval minutes to fractions of an hour for having a 
# "normalized" interval

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

# Calculating "normalized" interval with minutes as a fraction of hour  
activity$interval_norm <- round((hours_interv + minutes_interv / 60) * 100)
```


## What is mean total number of steps taken per day?


```r
steps_date <- aggregate(steps ~ date, data = activity, FUN = sum)
mean_steps_date <- mean(steps_date$steps)
median_steps_date <- median(steps_date$steps)
```

The mean vaue for the total number of steps taken per day is 1.0766189\times 10^{4}
steps.

The median is 1.0765\times 10^{4} steps.

The following histogram shows the distribution of the total number of steps taken per day:


```r
g <- ggplot(data = steps_date, aes(x=steps)) + 
        geom_histogram(binwidth = 2000,  
                       color = "darkred", fill = "orange") + 
        labs(title = "Histogram of total steps / day") +
        labs(y = "Frecuency", x = "Number of total steps / day") +
        scale_x_continuous(breaks = seq(0, 22500, by = 2000), expand = c(0, 0))
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
