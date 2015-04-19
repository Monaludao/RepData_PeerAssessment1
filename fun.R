fun1 <- function(){  
    library(reshape2)
    
    unzip("activity.zip")
    act_data<-read.csv("activity.csv")
    
    act_data$date<-as.Date(act_data$date,format="%Y-%m-%d")
    melt.data<-melt(act_data,id=c("date","interval"),
                    measure.vars=c("steps"))
    
    date.step<-dcast(melt.data,date ~ "steps", sum)
    
    hist(date.step$steps)
    
    mean.date.step<-mean(date.step$steps,na.rm=TRUE)
    med.date.step<-median(date.step$steps,na.rm=TRUE)
    
    print(paste("The mean of the total number of steps taken per day is",
                mean.date.step))
    print(paste("The median of the total number of steps taken per day is",
                med.date.step))
    
    
    interval.step<-dcast(melt.data, interval ~ "steps", mean, na.rm=TRUE)
    
    plot(interval.step$interval,interval.step$steps,type="l")
}