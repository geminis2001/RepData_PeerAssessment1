# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
the NA values are removed and aggregate the steps by day

```r
   activity<-read.csv("activity.csv")
   c_act<-subset(activity,!is.na(activity$steps))
   total<-aggregate(steps~date,activity,sum)    
```

## What is mean total number of steps taken per day?
The histogram of steps by day

```r
barplot(total$steps,names.arg=as.matrix(total$date),xlab="date",ylab="steps",main="Steps by day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
Mean and median

```r
data.frame(mean=round(mean(total$steps),digits=2),median=median(total$steps))
```

```
##    mean median
## 1 10766  10765
```

## What is the average daily activity pattern?

Time Series of 5-minutes interval

```r
media<-aggregate(steps~interval,c_act,mean)
with(media,plot(interval,steps,type="l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

5-interval with the maximun value 

```r
activity[which.max( activity$steps),]
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```
## Imputing missing values
Total de NA values in the data

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```
Change the NA with 0

```r
impute<-function (x){
  elrow<-activity[x,1]
  if(is.na(elrow))
    ret<-0
  else
    ret<-elrow
  ret
}
for (i in 1:nrow(activity))
{
   activity[i,1]<-impute(i)
}
total<-aggregate(steps~date,activity,sum)
```
The histogram of imputed data

```r
barplot(total$steps,names.arg=as.matrix(total$date),xlab="date",ylab="steps",main="Steps by day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
Mean and median of imputed data

```r
data.frame(mean=round(mean(total$steps),digits=2),median=median(total$steps))
```

```
##   mean median
## 1 9354  10395
```
The mean and median are different because NA is changed by 0, then the number of rows increases and the media and mean go down.


## Are there differences in activity patterns between weekdays and weekends?
Make the new factor

```r
options(verbose=FALSE)
#total<-aggregate(steps~date,activity,mean)
wd<-data.frame(activity,weekd="",stringsAsFactors=FALSE)
wd.wnd <- function(date) {
    day <- weekdays(date)
    if (day %in% c("lunes", "martes", "miércoles", "jueves", "viernes")) 
        return("weekday") else if (day %in% c("sábado", "domingo")) 
        return("weekend") else stop("invalid")
}
wd$date<-as.Date(wd$date)
wd$weekd<-sapply(wd$date, FUN = wd.wnd)
```
Panel showing weekend activity versus weekday

```r
medias <- aggregate(steps ~ interval + weekd, mean,data=wd)
ggplot(medias, aes(interval, steps)) + geom_line() + facet_grid(weekd ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
.-
