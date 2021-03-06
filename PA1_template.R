data <- read.csv("activity.csv", header = TRUE)
main_data <- na.omit(data)

steps_day <- aggregate(main_data$steps, by = list(Steps.Date = main_data$date), FUN = "sum")
hist(steps_day$x, col = "grey", breaks = 20,
     main = "Tot number of steps taken each day", 
     xlab = "Number of steps per day")
mean_days<-mean(steps_day[,2])
mean_days
median_days<-median(steps_day[,2])
median_days

av_day <- aggregate(main_data$steps, 
                          by = list(Interval = main_data$interval), 
                          FUN = "mean")
plot(av_day$Interval, av_day$x, type = "l", 
     main = "Average of the daily activity pattern", 
     ylab = "Avarage number of steps", 
     xlab = "5-min intervals", col="blue")
interval <- which.max(av_day$x)
max_int <- av_day[interval,1]
max_int

nan <- length(which(is.na(data$steps)))
nan
library(magrittr)
library(dplyr)

filled_data <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_data <- data%>% group_by(interval) %>% mutate(steps= filled_data(steps))
steps_day_filled <- aggregate(mean_data$steps, by = list(Steps.Date = mean_data$date), FUN = "sum")
hist(steps_day_filled$x, col = "grey", breaks = 20,
     main = "Tot number of steps taken each day (filled data)",
     xlab = "Number of steps per day")
mean_days_filled<-mean(steps_day_filled[,2])
mean_days_filled
median_days_filled<-median(steps_day_filled[,2])
median_days_filled

mean_data$date <- as.Date(mean_data$date)
mean_data$weekday <- weekdays(mean_data$date)
mean_data$weekend <- ifelse(mean_data$weekday=="Saturday" | mean_data$weekday=="Sunday", "Weekend", "Weekday" )

day_data <- aggregate(steps ~ interval + weekend, data=mean_data, FUN = "mean")
ggplot(day_data, aes(interval, steps)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Steps mean") +
  ggtitle("Weekdays and weekends activity patterns")

  