---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, we download and unzip the file retrieved from [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The data will be downloaded in a subdirectory of the working directory called **data**  
If it does not exists it will be created  
Plot will be saved to the **figure** directory.  
If it does not exists it will be created as a subdirectory of the working directory  

The following code is used for downloading, unzipping and creating the required directories:  


```r
# Create directory which the data will be downloaded to if it doesn't already exists
if(!dir.exists("data")) {
    dir.create ("data")
}

# Create directory which the plots will be saved to if it doesn't already exists
if(!dir.exists("figure")) {
    dir.create ("figure")
}

# Download and extract files if the file has not been downloaded already
if(!file.exists("./data/dataset.zip")){
    
    # Download file
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl,"./data/dataset.zip",method = "curl")
    
    #extract file
    unzip("./data/dataset.zip",exdir = "data") 
} 
```

Then we read in the data

```r
data <- read.csv("./data/activity.csv",header=TRUE,sep = ",",stringsAsFactors = FALSE)
```



## What is mean total number of steps taken per day?



```r
require(dplyr)
```


```r
stepsperday <- data %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(totalsteps = sum(steps))
```

In a histogram this will look like this:  

```r
png("./figure/histogram_not imputated.png")
hist(stepsperday$totalsteps,main="steps per day",xlab = "Number of steps")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

The mean of the total number of steps taken per day is  


```r
mean(stepsperday$totalsteps)
```

```
## [1] 10766.19
```

The median of the total number of steps taken per day is  


```r
median(stepsperday$totalsteps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
stepsperinterval <- data %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(averagesteps = mean(steps))

png("./figure/daily_activity_pattern.png")
plot(x=stepsperinterval$interval,y=stepsperinterval$averagesteps,type="l",xlab="interval",ylab="average nr of steps")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

The maximum amount of steps in a 5-minute interval is:


```r
stepsperinterval[stepsperinterval$averagesteps==max(stepsperinterval$averagesteps),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```




## Imputing missing values

The total number of missing values is calculated as follows:


```r
sum <- sum(is.na(data$steps))
```

The total number of missing values is 2304

We imputate the missing values by the average number of steps per interval across all days as follows:


```r
for (i in unique(data[is.na(data$steps),]$interval)) {
    value <- min(subset(stepsperinterval, interval == i, select = c("averagesteps")))
    data[is.na(data$steps) & data$interval == i,]$steps <- value
}
```

Now we have created a dataset called data with imputed values we reproducte the historgram and recalculate the mean and median values by running similar code as before.


```r
stepsperday_new <- data %>%
    group_by(date) %>%
    summarize(totalsteps = sum(steps))

png("./figure/histogram_imputated.png")
hist(stepsperday_new$totalsteps,main="steps per day",xlab = "Number of steps")
dev.off()
```

```
## quartz_off_screen 
##                 2
```




```r
meannew <- as.integer(mean(stepsperday_new$totalsteps))
```

The mean of the total number of steps taken per day now is  10766




```r
mediannew <-as.integer(median(stepsperday_new$totalsteps))
```

The median of the total number of steps taken per day now is 10766  

The difference between the mean and median values after imputation has become negligibly small.  


```r
diff <- as.integer(sum(stepsperday_new$totalsteps) - sum(stepsperday$totalsteps))
```

By imputating values the total number of steps increased by 86129


## Are there differences in activity patterns between weekdays and weekends?


```r
require(ggplot2)
require(lubridate)
```

To see if there is any difference in activity patterns between weekdays and weekends we first create a variabele that indicates whether the day is a weekday or a day of the weekend. For this I choose to use the lubridate pacakge.  


```r
data <- data%>%
    mutate(day= wday(as.Date(date,"%Y-%m-%d")))

data$daytype <- ifelse(data$day %in% c(7, 1), "weekend", "weekday")
data$daytype <- as.factor(data$daytype)

#Make a dataset wioth mean steps per interval
data <- data%>%
    group_by(interval,daytype) %>%
    summarize(averagesteps = mean(steps))
```


```r
png("./figure/activity_weekend_weekdays.png", width = 800, height = 600)
ggplot(data,(aes(x=data$interval,y=data$averagesteps)))+
    geom_line(aes(colour=data$daytype))+
    facet_grid(data$daytype~.)+
    labs(x= "Interval", y= "Number of steps",colour="Type of day") 
dev.off()
```

```
## quartz_off_screen 
##                 2
```
