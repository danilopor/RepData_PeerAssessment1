library(dplyr)
library(ggplot2)

# Loading and preprocessing the data

activity <- read.csv("~/Desktop/activity.csv")

dim(activity)
summary(activity)
head(activity)
tail(activity)
str(activity)

activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$interval <- factor(activity$interval)

# What is mean total number of steps taken per day?

StepsPerDay <- aggregate(activity$steps, list(activity$date), sum)

TotalSteps <- activity %>% group_by(date) %>%
    summarize(total.steps = sum(steps, na.rm = TRUE), 
              mean.steps = mean(steps, na.rm = TRUE))
   
Plot_TotalSteps <- ggplot(TotalSteps, aes(x=date, y=total.steps))+
    geom_bar(stat = "identity")

StepsMean <- mean(StepsPerDay$x, na.rm = TRUE)
print(StepsMean)

StepsMedian<- median(StepsPerDay$x, na.rm = TRUE)
print(StepsMedian)

# What is the average daily activity pattern?

steps_per_interval <- aggregate(activity$steps, by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
colnames(steps_per_interval) <- c("interval", "average_steps")

plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_steps <- max(steps_per_interval$average_steps)
print(max_steps)

intervale_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
print(intervale_max_steps)

# Imputing missing values

# Calculate and report the total number of missing values in the dataset 

Total_StepNA <- sum(is.na(as.character(activity$steps)))
print(Total_StepNA)

Total_DateNA <- sum(is.na(as.character(activity$date)))
print(Total_DateNA)

Total_IntervalNA <- sum(is.na(as.character(activity$interval)))
print(Total_IntervalNA)

TotalNA <- Total_StepNA + Total_IntervalNA + Total_DateNA
print(TotalNA)

# Create a new dataset that is equal to the original dataset 

# Devise a strategy for filling in all of the missing values in the dataset
activity_NA <- apply(activity, 1, function(x){any(is.na(x))})

sum(activity_NA)
activity_NA <- activity[!activity_NA,]

# Make a histogram of the total number of steps taken each day
ggplot(activity_NA, aes(x=date, y=steps))+geom_bar(stat = "identity")

# Calculate and report the mean and median
perDay <- aggregate(activity_NA$steps, list(activity_NA$date), sum)
mean_perday <- mean(perDay$x, na.rm = TRUE)
print(mean_perday)

median_perday <- median(perDay$x, na.rm = TRUE)
print(median_perday)

#Are there differences in activity patterns between weekdays and weekends?

newData <- activity_NA

newData$day <- as.factor(weekdays(newData$date))
newData$is_weekday <- ifelse(!(newData$day %in% c("Sábado","Domingo")), TRUE, FALSE) 

weekdays_data <- newData[newData$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)

weekends_data <- newData[!newData$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")

steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)
week_data$day <- as.factor(week_data$day)

library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")



