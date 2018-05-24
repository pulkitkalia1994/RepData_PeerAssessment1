#Reproducible Research

## Code for reading in the dataset and/or processing the data

```r
data<-read.csv("activity.csv")
```

## Histogram of the total number of steps taken each day

```r
totalStepsPerDay<-data %>% group_by(date) %>% summarise(mean.steps=mean(steps,na.rm=TRUE),sum.steps=sum(steps,na.rm = TRUE),median.steps=median(steps,na.rm = TRUE))
hist(totalStepsPerDay$sum.steps,xlab= "total steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
## Mean and median number of steps taken each day

```r
  print(totalStepsPerDay)
```

```
## # A tibble: 61 x 4
##    date       mean.steps sum.steps median.steps
##    <fct>           <dbl>     <int>        <dbl>
##  1 2012-10-01    NaN             0           NA
##  2 2012-10-02      0.438       126            0
##  3 2012-10-03     39.4       11352            0
##  4 2012-10-04     42.1       12116            0
##  5 2012-10-05     46.2       13294            0
##  6 2012-10-06     53.5       15420            0
##  7 2012-10-07     38.2       11015            0
##  8 2012-10-08    NaN             0           NA
##  9 2012-10-09     44.5       12811            0
## 10 2012-10-10     34.4        9900            0
## # ... with 51 more rows
```

## Time series plot of the average number of steps taken

```r
totalStepsPerInterval<-data %>% group_by(interval) %>% summarise(mean.steps=mean(steps,na.rm = TRUE))
  g<-ggplot(totalStepsPerInterval,aes(x=totalStepsPerInterval$interval,y=totalStepsPerInterval$mean.steps))+geom_line()
  g+labs(x="Interval",y="Mean Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## The 5-minute interval that, on average, contains the maximum number of steps

```r
maxStepsInInterval<-max(totalStepsPerInterval$mean.steps)
  print(maxStepsInInterval)
```

```
## [1] 206.1698
```

## Code to describe and show a strategy for imputing missing data

```r
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
```

## Histogram of the total number of steps taken each day after missing values are imputed

```r
newdf<-data.frame(steps=newsteps,date=data$date,interval=data$interval)
  totalStepsPerDay<-newdf %>% group_by(date) %>% summarise(mean.steps=mean(steps,na.rm=TRUE),sum.steps=sum(steps,na.rm = TRUE),median.steps=median(steps,na.rm = TRUE))
  print(totalStepsPerDay)
```

```
## # A tibble: 61 x 4
##    date       mean.steps sum.steps median.steps
##    <fct>           <dbl>     <dbl>        <dbl>
##  1 2012-10-01     37.4      10766.         34.1
##  2 2012-10-02      0.438      126           0  
##  3 2012-10-03     39.4      11352           0  
##  4 2012-10-04     42.1      12116           0  
##  5 2012-10-05     46.2      13294           0  
##  6 2012-10-06     53.5      15420           0  
##  7 2012-10-07     38.2      11015           0  
##  8 2012-10-08     37.4      10766.         34.1
##  9 2012-10-09     44.5      12811           0  
## 10 2012-10-10     34.4       9900           0  
## # ... with 51 more rows
```

```r
  hist(totalStepsPerDay$sum.steps,xlab="Total Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
day<-as.Date(newdf$date)
  day<-weekdays(day,abbreviate = FALSE)
  day<-if_else(day %in% c("Monday","Tuesday","Wednesday","Thusday","Friday"),"Weekday","Weekend")
  newdf<-mutate(newdf,day=day)
  
  groupedDf<-newdf %>% group_by(interval,day) %>% summarise(mean.steps=mean(steps))
  g<-ggplot(groupedDf,aes(x=interval,y=mean.steps))+geom_line(aes(col=day))+facet_wrap(~day,ncol = 1)
  print(g)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

```r
 data<-read.csv("activity.csv")
  hist(data$steps,xlab = "Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
  totalStepsPerDay<-data %>% group_by(date) %>% summarise(mean.steps=mean(steps,na.rm=TRUE),sum.steps=sum(steps,na.rm = TRUE),median.steps=median(steps,na.rm = TRUE))
  print(totalStepsPerDay)
```

# A tibble: 61 x 4
   date       mean.steps sum.steps median.steps
   <fct>           <dbl>     <int>        <dbl>
 1 2012-10-01    NaN             0           NA
 2 2012-10-02      0.438       126            0
 3 2012-10-03     39.4       11352            0
 4 2012-10-04     42.1       12116            0
 5 2012-10-05     46.2       13294            0
 6 2012-10-06     53.5       15420            0
 7 2012-10-07     38.2       11015            0
 8 2012-10-08    NaN             0           NA
 9 2012-10-09     44.5       12811            0
10 2012-10-10     34.4        9900            0
# ... with 51 more rows

```r
  hist(totalStepsPerDay$sum.steps,xlab= "total steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-2.png)

```r
  totalStepsPerInterval<-data %>% group_by(interval) %>% summarise(mean.steps=mean(steps,na.rm = TRUE))
  g<-ggplot(totalStepsPerInterval,aes(x=totalStepsPerInterval$interval,y=totalStepsPerInterval$mean.steps))+geom_line()
  g+labs(x="Interval",y="Mean Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-3.png)

```r
  maxStepsInInterval<-max(totalStepsPerInterval$mean.steps)
  print(maxStepsInInterval)
```

[1] 206.1698

```r
  totalNAs<-sum(is.na(data$steps))
  print(totalNAs)
```

[1] 2304

```r
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
```

# A tibble: 61 x 4
   date       mean.steps sum.steps median.steps
   <fct>           <dbl>     <dbl>        <dbl>
 1 2012-10-01     37.4      10766.         34.1
 2 2012-10-02      0.438      126           0  
 3 2012-10-03     39.4      11352           0  
 4 2012-10-04     42.1      12116           0  
 5 2012-10-05     46.2      13294           0  
 6 2012-10-06     53.5      15420           0  
 7 2012-10-07     38.2      11015           0  
 8 2012-10-08     37.4      10766.         34.1
 9 2012-10-09     44.5      12811           0  
10 2012-10-10     34.4       9900           0  
# ... with 51 more rows

```r
  hist(totalStepsPerDay$sum.steps,xlab="Total Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-4.png)

```r
  day<-as.Date(newdf$date)
  day<-weekdays(day,abbreviate = FALSE)
  day<-if_else(day %in% c("Monday","Tuesday","Wednesday","Thusday","Friday"),"Weekday","Weekend")
  newdf<-mutate(newdf,day=day)
  
  groupedDf<-newdf %>% group_by(interval,day) %>% summarise(mean.steps=mean(steps))
  g<-ggplot(groupedDf,aes(x=interval,y=mean.steps))+geom_line(aes(col=day))+facet_wrap(~day,ncol = 1)
  print(g)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-5.png)
