Loading and preprocessing the data
----------------------------------

    data <- read.csv("activity.csv", header = TRUE)
    main_data <- na.omit(data)

What is mean total number of steps taken per day?
-------------------------------------------------

    steps_day <- aggregate(main_data$steps, by = list(Steps.Date = main_data$date), FUN = "sum")
    hist(steps_day$x, col = "grey", breaks = 20,
         main = "Tot number of steps taken each day", 
         xlab = "Number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean_days<-mean(steps_day[,2])
    mean_days

    ## [1] 10766.19

    median_days<-median(steps_day[,2])
    median_days

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

    av_day <- aggregate(main_data$steps, 
                              by = list(Interval = main_data$interval), 
                              FUN = "mean")
    plot(av_day$Interval, av_day$x, type = "l", 
         main = "Average of the daily activity pattern", 
         ylab = "Avarage number of steps", 
         xlab = "5-min intervals", col="blue")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    interval <- which.max(av_day$x)
    max_int <- av_day[interval,1]
    max_int

    ## [1] 835

Imputing missing values
-----------------------

    nan <- length(which(is.na(data$steps)))
    nan

    ## [1] 2304

    library(magrittr)
    library(dplyr)
    library(ggplot2)

    filled_data <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    mean_data <- data%>% group_by(interval) %>% mutate(steps= filled_data(steps))
    steps_day_filled <- aggregate(mean_data$steps, by = list(Steps.Date = mean_data$date), FUN = "sum")
    hist(steps_day_filled$x, col = "grey", breaks = 20,
         main = "Tot number of steps taken each day (filled data)",
         xlab = "Number of steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean_days_filled<-mean(steps_day_filled[,2])
    mean_days_filled

    ## [1] 10766.19

    median_days_filled<-median(steps_day_filled[,2])
    median_days_filled

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    mean_data$date <- as.Date(mean_data$date)
    mean_data$weekday <- weekdays(mean_data$date)
    mean_data$weekend <- ifelse(mean_data$weekday=="Saturday" | mean_data$weekday=="Sunday", "Weekend", "Weekday" )
    day_data <- aggregate(steps ~ interval + weekend, data=mean_data, FUN = "mean")
    ggplot(day_data, aes(interval, steps)) + geom_line()+
      facet_grid(weekend ~.) + xlab("Interval") + ylab("Steps mean") +
      ggtitle("Weekdays and weekends activity patterns")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
