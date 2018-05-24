Reproducible_Research<-function(){
  data<-read.csv("activity.csv")
  hist(data$steps,xlab = "Steps")
  totalStepsPerDay<-data %>% group_by(date) %>% summarise(mean.steps=mean(steps,na.rm=TRUE),sum.steps=sum(steps,na.rm = TRUE),median.steps=median(steps,na.rm = TRUE))
  print(totalStepsPerDay)
  hist(totalStepsPerDay$sum.steps,xlab= "total steps")
  totalStepsPerInterval<-data %>% group_by(interval) %>% summarise(mean.steps=mean(steps,na.rm = TRUE))
  g<-ggplot(totalStepsPerInterval,aes(x=totalStepsPerInterval$interval,y=totalStepsPerInterval$mean.steps))+geom_line()
  g+labs(x="Interval",y="Mean Steps")
  print(g)
  maxStepsInInterval<-max(totalStepsPerInterval$mean.steps)
  print(maxStepsInInterval)
  totalNAs<-sum(is.na(data$steps))
  print(totalNAs)
  j<-1
  newsteps<-c()
  for(i in data$steps){
    if(is.na(i)){
      index<-match(data$interval[[j]],totalStepsPerInterval$interval)
      newsteps<-c(newsteps,totalStepsPerInterval$mean.steps[[index]])
    }
    else{
      newsteps<-c(newsteps,data$steps[[j]])
    }
    j<-j+1
  }
  
  newdf<-data.frame(steps=newsteps,date=data$date,interval=data$interval)
  totalStepsPerDay<-newdf %>% group_by(date) %>% summarise(mean.steps=mean(steps,na.rm=TRUE),sum.steps=sum(steps,na.rm = TRUE),median.steps=median(steps,na.rm = TRUE))
  print(totalStepsPerDay)
  hist(totalStepsPerDay$sum.steps,xlab="Total Steps")
  day<-as.Date(newdf$date)
  day<-weekdays(day,abbreviate = FALSE)
  day<-if_else(day %in% c("Monday","Tuesday","Wednesday","Thusday","Friday"),"Weekday","Weekend")
  newdf<-mutate(newdf,day=day)
  
  groupedDf<-newdf %>% group_by(interval,day) %>% summarise(mean.steps=mean(steps))
  g<-ggplot(groupedDf,aes(x=interval,y=mean.steps))+geom_line(aes(col=day))+facet_wrap(~day,ncol = 1)
  print(g)
  
}