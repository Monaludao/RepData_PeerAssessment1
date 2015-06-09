fun1 <- function(){  
    Sys.setlocale("LC_ALL","English")
    library(reshape2)
    library(lattice)
    
    unzip("activity.zip")
    act_data<-read.csv("activity.csv")
    act_data$date<-as.Date(act_data$date,format="%Y-%m-%d")
    
    melt.data<-melt(act_data,id=c("date","interval"),
                    measure.vars=c("steps"))
    
    date.step<-dcast(melt.data,date ~ "steps", sum)
    
    png(file="hist_date_steps.png")
    with(date.step,hist(steps,
                        main="The Total Number of Steps Taken Each Day"))
    dev.off()
    
    mean.date.step<-mean(date.step$steps,na.rm=TRUE)
    med.date.step<-median(date.step$steps,na.rm=TRUE)
    
    print(paste("The mean of the total number of steps taken per day is ",
                mean.date.step, ".", sep=""))
    print(paste("The median of the total number of steps taken per day is ",
                med.date.step, ".", sep=""))
    
    
    interval.step<-dcast(melt.data, interval ~ "steps", mean,
                         na.rm=TRUE)
    max.interval<-interval.step[interval.step$steps == 
                                    max(interval.step[,2]),1]
    
    png("plot_interval_steps.png")
    with(interval.step,plot(interval,steps,type="l", 
                            main="The Average Number of Steps Taken per 5-minute Interval"))
    dev.off()
    
    print(paste("The interval of", max.interval, 
                "contains the max number of steps."))
    
    print(paste("The total number of missing values in the dataset is:",
                nrow(subset(act_data,is.na(act_data$steps)))))

    fill_data <- merge(act_data, interval.step, by="interval", 
                     suffixes=c("", "_intervalmean"))
    fill_data[is.na(fill_data$steps), "steps"] = 
        fill_data[is.na(fill_data$steps), "steps_intervalmean"]
    fill_data$steps_intervalmean <- NULL
    ##fill_data <- fill_data[,c(2,3,1)]
    ##fill_data <- fill_data[order(fill_data$date,fill_data$interval),]
    ##row.names(fill_data) <- NULL
    
    week <- factor(weekdays(fill_data$date), 
                   c("Monday", "Tuesday", "Wednesday", "Thursday", 
                     "Friday", "Saturday", "Sunday"))
    levels(week)[1:5]<-"weekday"
    levels(week)[2:3]<-"weekend"
    fill_data <- cbind(fill_data,week)
    
    melt.fill.data<-melt(fill_data,id=c("date","interval","week"),
                         measure.vars=c("steps"))
    date.step.f<-dcast(melt.fill.data,date ~ "steps", sum)
    
    png(file="hist_date_steps_filled.png")
    with(date.step.f,hist(steps,
                        main="The Total Number of Steps Taken Each Day"))
    dev.off()
    
    mean.date.step.f<-mean(date.step.f$steps,na.rm=TRUE)
    med.date.step.f<-median(date.step.f$steps,na.rm=TRUE)
    
    print(paste("The mean of the total number of steps taken per day is ",
                mean.date.step.f, ".", sep=""))
    print(paste("The median of the total number of steps taken per day is ",
                med.date.step.f, ".", sep=""))
    
    week.interval.f<-dcast(melt.fill.data, week + interval ~ "steps", mean)
    
    png(file="plot_interval_steps_by_weekday_end.png")
    with(week.interval.f,xyplot(steps ~ interval | week, type="l", 
                                layout=c(1,2), xlab="Inteval", 
                                ylab="Number of Steps"))
    dev.off()
}