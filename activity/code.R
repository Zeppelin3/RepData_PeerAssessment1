

setwd("C:/Users/chapp/Documents/Coursera/Reproducable research/RepData_PeerAssessment1/activity")


activity<-read.csv("activity.csv")

aggact<-aggregate(steps ~ date, activity, sum, na.action = na.pass)

hist(aggact$steps, breaks=53, xlab = "Steps per day", main="Histogram of number of steps per day")

mean(aggact$steps, na.rm=TRUE)

median(aggact$steps, na.rm=TRUE)

aggact1<-aggregate(steps ~ interval, activity, mean)


plot(aggact1$interval, aggact1$steps, type="l", xlab="Intervals", ylab="Steps taken", 
     main = "Time series plot of steps per interval")

aggact1[which.max(aggact1$steps),]


activity1<-activity
sum(is.na(activity1))

na_act <- which(is.na(activity1$steps))

mean_vec <- rep(mean(activity1$steps, na.rm=TRUE), times=length(na_act))

activity1[na_act, "steps"] <- mean_vec

aggact2<-aggregate(steps ~ date, activity1, sum)

hist(aggact2$steps, breaks=53, xlab = "Steps per day", main="Histogram of number of steps per day")

mean(aggact2$steps)

median(aggact2$steps)


activity1$day <- ifelse(weekdays(as.Date(activity1$date)) == "Saturday" | 
            weekdays(as.Date(activity1$date)) == "Sunday", "weekend", "weekday")


