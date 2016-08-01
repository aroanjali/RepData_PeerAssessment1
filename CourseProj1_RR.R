getwd()
setwd("C:/Users/aarora/Personal/Anjali/Personal/Coursera - Data Science/Course 9 - Reproducible Research")
data1 <- read.csv("activity.csv")
head(data1)
data_sum <- aggregate(data1$steps, by=list(Category=data1$date), FUN=sum)
head(data_sum)
## Histogram of number of steps taken each day
hist(data_sum$x, main="Total steps per day", xlab="Steps", ylab="Frequency")

## Mean and median number of steps taken each day
# data_mean <- aggregate(data1$steps, by=list(Category=data1$date), FUN=mean, na.rm=TRUE)
# head(data_mean)
data_mean1 <- mean(data1$steps, na.rm=TRUE)
print(paste("The mean steps per day is: ", data_mean1))

# data_median <- aggregate(data1$steps, by=list(Category=data1$date), FUN=median)
# head(data_median)
data_median1 <- median(data1$steps, na.rm=TRUE)
print(paste("The median steps per day is: ", data_median1))

## Time series plot of the average number of steps taken by day
##plot(data_mean$Category, data_mean$x, type="l")

## Time series plot of the average number of steps taken by interval
data_mean_interval <- aggregate(data1$steps, by=list(Category=data1$interval), FUN=mean, na.rm=TRUE)
head(data_mean_interval)
##lines(x=data_mean_interval$Category, y=data_mean_interval$x, type="l")
plot(data_mean_interval$Category, data_mean_interval$x, type="l", main="Average steps per five minute interval", xlab="Interval No", ylab="steps")

## 5 minute interval with max values
maxsteps <- max(data_mean_interval$x)
print(paste("The maximum number of steps in a five minute interval was: ", maxsteps))
maxinterval <- data_mean_interval[data_mean_interval$x==max(data_mean_interval$x),]
print(paste("The interval with maximum number of steps was: ", maxinterval$Category))

## Number of rows with missing values 
sum(!complete.cases(data1)) 

## Strategy for imputing missing values - Use the median
data2 <- data1
data2$steps[is.na(data2$steps)] <- median(data2$steps,na.rm=TRUE)
sum(!complete.cases(data2)) 
data_sum_impute <- aggregate(data1$steps, by=list(Category=data1$date), FUN=sum)
hist(data_sum_impute$x)

## Calculating new mean and median
data_mean2 <- mean(data2$steps)
print(paste("The mean is: ", data_mean2))
data_median2 <- median(data2$steps)
print(paste("The median is: ", data_median2))

## Differences in activity patterns between weekdays and weekends
data2$date <- as.Date(data2$date)
data2$dayname <- weekdays(data2$date)
data2$weekend <- as.factor(ifelse(data2$dayname == "Saturday" |
                                    data2$dayname == "Sunday", "weekend", "weekday"))
library(lattice)
plotdata <- aggregate(steps ~ interval + weekend, data2, mean)
xyplot(steps ~ interval | factor(weekend), data=plotdata, aspect=1/3, type="l")





