# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The analysis searches first for the unzipped file **activity.csv**, if absent 
then the zipped archive **activity.zip** (assuming them to be in the same directory as the .Rmd script generating this output).  
Failing this the .zip file is called from a given url ***(http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)***. This is the choice of last resort given the potential for URLs to change.  
The file is extracted as required and the data read into the dataframe **rawData**.  
The option cache = TRUE is used to ensure this process is performed only when necessary.


```r
   if(file.exists("activity.csv")){
       rawData<-read.csv("activity.csv")
   } else if(file.exists("activity.zip")){
        unzip("activity.zip")
        rawData<-read.csv("activity.csv")         
   } else{
         temp<-tempfile()
         download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
         unzip(temp)
         rawData<-read.csv("activity.csv")          
  } 
```
The time codes in this data set have no standard tokens to allow parsing of the hrs:mins data. The function **parseTimeData()** performs this extraction, and creates a Posix conformant object with a constant placeholder date and the appropriate hr:min values. The utility of this step will be shown later. The function assumes that the package **lubridate** is available and loaded


```r
#  pass the numeric time code to the function
parseTimeData<-function(timeStr){
# use integer division to extract the hour value
    hr<-timeStr%/%100
# use modulus to extract the minute value       
    mn<-timeStr%%100
# create a constant placeholder date
    interval<-dmy("1/1/2000")
# append the hour and minute values to the date object
    hour(interval)<-hr
    minute(interval)<-mn
# return
    return(interval)
}

# accesss the lubridate package
library(lubridate)
# set the significant figures
options(digits=7)
# convert the date values to posix conformant objects
asDates<-ymd(rawData$date)
# replace the date column with this vector
rawData$date<-asDates
# parse the time codes with parseTimeData() to date objects
times<-parseTimeData(rawData$interval)
# replace the interval column with this vector
rawData$interval<-times
# re-order the columns in the dataframe
rawData<-rawData[,c("date","interval","steps")]
```

## What is mean total number of steps taken per day?

This analysis can be performed by aggregating the data by the date values  
and defining the function as 'sum'.  
The 'granularity' of the histogram is optimised by using 'breaks=15' as an option  
The statistics associated with the distribution of total steps per day are determined  
with the summary() function, mean and median values extracted as required


```r
# aggregate the daily totals of steps
stepByDate<-aggregate(steps~date,data=rawData,FUN=sum)

# plot as a histogram
hist(stepByDate$steps,
     breaks=15,
     main="Steps per day", 
     xlab="Total daily steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# determine summary statistics for the daily step totals
stepStats<-summary(stepByDate$steps)
print(stepStats['Mean']) 
```

```
##  Mean 
## 10770
```

```r
print(stepStats['Median'])
```

```
## Median 
##  10760
```
The mean steps recorded per day is **10770**   
The median value is **10760**  


## What is the average daily activity pattern?
The interval values are expressed as posixCt compliant objects with the same placeholder date for each.  
Therefore aggregating on the interval values will extract and aggregate data for each timepoint *within* each day  


```r
thruDay<-aggregate(steps~interval,
                   data=rawData,
                   FUN=mean)
plot(thruDay,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
     
The next task is to determine which interval has the highest mean steps associated  
with it

```r
# determine which interval value has the highest mean steps/interval associated with it
maxit<-thruDay[which.max(thruDay$steps),]
print(maxit)
```

```
##                interval    steps
## 104 2000-01-01 08:35:00 206.1698
```

```r
# extract the highest activity interval value; construct a string giving the interval value
hiActiv<-maxit$interval
maxtime<-paste(hour(hiActiv),
               minute(hiActiv),
               sep=":")
```
The 5minute interval in which the maximum average number of steps was recorded is **8:35** with an average value of 206.2 .


## Imputing missing values
Generate a copy of the rawData dataframe in order to perform the imputation required  
Check copy is faithful to the original  

```r
assign("copy",rawData)
if(identical(rawData,copy))
    {res1<-"Copy sucessfully generated"} else {
     res1<-"Failed to copy rawData"}
```
Copy sucessfully generated   
Total NA values in the data set:  

```r
# logical vector of NA values 
   NA_yes<-is.na(copy$steps)
   numNAs<-sum(NA_yes)
```
The total number of NAs in the data set is 2304  
  
Perform data imputation by replacing any NA value with the mean steps value calculated for the corresponding interval value.

```r
# iterate through the logical vector for NA values
for(i in 1:length(NA_yes)){

# if the value is NA, determine the mean steps determined for that interval
# and replace NA with that value  
    if(NA_yes[i]){
        index<-which(thruDay[,1]==copy[i,2])
        nuStep<-thruDay[index,2]
        copy[i,3]<-nuStep
    }
}    

# aggregate the daily totals of steps with the imputed data included
stepByDate2<-aggregate(steps~date,
                       data=copy,
                       FUN=sum)

# panel plot comparing the distributions of the two data sets
    par(mfrow = c(2, 1))
    par(oma=c(0,0,0,0))
    par(mar=c(4,4,3,1))
hist(stepByDate$steps,
     breaks=15,
     main="Steps per day",
     ylim=c(0,25),
     xlab="Total Daily Steps")

hist(stepByDate2$steps,
     breaks=15,
     main="Steps per day: post imputation of NAs",
     ylim=c(0,25),
     xlab="Total Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
# generate summary statistics for the newly imputed dataset
stepStats2<-summary(stepByDate2$steps)
print(stepStats)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

```r
print(stepStats2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
 Min.|1st Qu.|Median|Mean|3rd Qu.|Max.   
    41| 8841|10760|10770|13290|21190   
 *Dataset following imputation of NAs*  
    41| 9819|10770|10770|12810|21190  
  
The use of this imputation method causes the **central intervals** in the distribution to markedly increase their recorded frequency with only minimal changes to the **right and left tail** of the distribution.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and 
"weekend" indicating whether a given date is a weekday or weekend day. Convert this to a factor vector and append to the rawData dataframe 

```r
# Weekened dates have wday() values of 1(Sunday) or 7 (Saturday)
weekends<-(wday(rawData$date)==1) | ((wday(rawData$date)==7) )
rawData$dayType<-as.factor(weekends)
levels(rawData$dayType)<-c("weekday","weekend")
```
Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and  
the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  


```r
print(summary(rawData$dayType))
```

```
## weekday weekend 
##   12960    4608
```

```r
# aggragate on both the interval(activity through the day) and dayType varaibles
thruDay2<-aggregate(steps~interval+dayType,
                    data=rawData,
                    FUN=mean)

# split the data between weekday and weekend dat types
weekends<-subset(thruDay2,dayType=="weekend")
weekdays<-subset(thruDay2,dayType=="weekday")

#parameters for 2 panel plot
    par(mfrow = c(2, 1))
    par(oma=c(1,1,1,1))
    par(mar=c(4,4,3,2))

#plot the weekend data
plot(weekends$interval,
     weekends$steps,
     type="l",
     xlab="Time of day",
     ylab="Mean steps/interval", 
     main="Weekend Data",
     ylim=c(0,250))

#plot the weekday data
plot(weekdays$interval,
     weekdays$steps,
     type="l",
     xlab="Time of day",
     ylab="Mean steps/interval", 
     main="Weekday Data",
     ylim=c(0,250))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Comparison of the two plots reveals differences in the pattern of activity seen through week days and weekend days. These are interpretable in terms of the expected constraints on activity found in working in a sedentary environment, with regular working hours.  
  
    
1. the period between 00:00 and 06:00 has no appreciable activity, consistent with sleep.
2. on weekdays a marked peak of activity (maximum for the day) is observed between 07:00 and 09:00, this is consistent with the subject undertaking regular exercise prior to attending work.
1. Activity levels on weekdays are lower in the period 09:00 to 17:00, consistent with sedentary work, although a minor peak around 12:00-13:00 may indicate the subject undertaking exercise at lunchtime.
7. The weekend activity profiles show a later start to activity, lower peaks in the activity profiles, which are spread throughout the day. This may indicate the subject exercises at more varied times as weekend activities are often volountary, and not time constrained as weekdays may be.  

