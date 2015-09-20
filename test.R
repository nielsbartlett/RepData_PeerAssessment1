## test code
library(plyr)

data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
cleandata <- data
y <- sprintf("%s %04d", data$date, data$interval)
cleandata$date <- strptime(y, format="%Y-%m-%d %H%M")

dailysum <- aggregate(x = data["steps"],
                     FUN = sum,
                     by = list(Group.date = data$date), na.rm=TRUE)

hist(x=dailysum$steps, breaks=10, main="Histogram of Daily Steps", xlab="Steps", ylab="Frequency", xlim=c(0, 25000), ylim=c(0, 20))
abline(v=mean(dailysum[["steps"]]), col="blue")
abline(v=median(dailysum[["steps"]]), col="red")

mean(dailysum[["steps"]])
median(dailysum[["steps"]])

dailyav <- aggregate(x = data["steps"],
                      FUN = mean,
                      by = list(Group.interval = data$interval), na.rm=TRUE)

maxrow <- dailyav[ dailyav$steps == max(dailyav$steps), ]
plot(dailyav, type="l", xlab="Interval", ylab="Steps", main="Average steps by time of day")
abline(v=maxrow$Group.interval, col="red")
abline(h=maxrow$steps, col="red")
##maxrow$Group.interval
##maxrow$steps

##cleandata %>%
##    group_by(date) %>%
##    summarise(TotalSteps = sum(steps))

head(cleandata,50)
head(y,50)
head(temp,50)



cleandata$date <- strptime(sprintf("%s %04d", data$date, data$interval), format="%Y-%m-%d %H%M")

isweekend <- function(date) {
    if (weekdays(date) == "Saturday" | weekdays(date) == "Sunday")
        return("weekend")
    else
        return("weekday")
}

visweekend <- Vectorize(isweekend, "date")


newdailyav <- aggregate(steps ~ interval + DayType, test, mean)
head(newdailyav)
xyplot(steps ~ interval | DayType, newdailyav, type = "l", layout = c(1,2),
       main = "Average number of steps taken by interval for weekdays and Weekends",
       xlab = "Interval (time of day)", ylab = "Steps" )






