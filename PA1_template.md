---
title: 'Reproducible Research: Peer Assessment 1'
author: "Eric BAUDON"
date: "Sunday, September 13, 2015"
output: html_document
---

In this is an R Markdown document we are going to perform simple data exploratory with a collection of data coming from an experimental measurment performed in Ocotber and November 2012. In this lattest, we measured the number of steps performed every 5min interval during 61 days. Results are collected in csv file called "activity".

#Loading and preprocessing the data
First we need to load and clean data for working. I loaded the "activity.csv" file from my desktop and i studied the dataframe collection with following code:

```{r,echo = TRUE }
data<-read.csv("C:/Users/Eric BAUDON/Desktop/activity.csv")
str(data)
```

As we can see, it is composed of 3 varaibles:
- step: An integer counting the numbers of steps performed
- date: A factor representing the date the observation was performed
- interval: An integer representing the subdvision of day in 5 min steps.

For latter usage, we need to convert the date format as Date and not as Factor. We did so with the following code:

```{r, echo=TRUE}
data[,2]<-as.Date(data[,2],format="%Y-%m-%d")
```


#What is mean total number of steps taken per day?
We are interesting in several questions here regarding the mean and median total number of steps per days. 

Firts we calculated the Total number of steps per days with the following code:
```{r, echo=TRUE}
library(dplyr)
data.tot<-group_by(data,date)
somme<-summarize(data.tot,sm=sum(steps))
```

Then we printed this repartition 
```{r, echo=TRUE}
hist(somme$sm, main="Number of steps per day",xlab="Total steps",ylab="")
```
![alt tag](https://github.com/ericbaudon/RepData_PeerAssessment1/blob/master/Plots/Plot1.png)

Finally we calculated the mean and median to have a global statistic on our experiment (we need to remove the NA to perform these statisitics):
```{r, echo=TRUE}
mean.tot<-mean(somme$sm,na.rm = TRUE)
median.tot<-median(somme$sm, na.rm = TRUE)
```


#What is the average daily activity pattern?
In this section we would like to analyse and compare the daily activity of the subject based upon day time. For this we can used our intervals subdivisions and we are going to find mean over each intervals.
For this, we re-arrnage our datas with the following R code:

```{r, echo=TRUE}
data.daily<-group_by(data,interval)
steps<-summarize(data.daily,mn=mean(steps,na.rm = TRUE))
```

We can then print our daily activies:
```{r, echo=TRUE}
plot(steps$interval,steps$mn,type = "l")
```

We can also calculate at what time, in average, we perform the maximum of steps during the day:
```{r, echo=TRUE}
test<-max(steps$mn)
filter(steps, mn==test)$interval
```


#Imputing missing values
As we observed previously, there are some missing values stored as NA values. We can calculate the number of missing with the following code:
```{r, echo=TRUE}
sum(is.na(data$steps))
```

In order to see if these missing values have any impact, we are going to fill them with daily interval averages calculated previously. So, for each NA value, we are going to replace it with by the mean value calculated by intervals with the following code:
```{r, echo=TRUE}
data.clean<-data
for (i in 1:length(data$steps))
{
    if(is.na(data.clean[i,1]))
    {
        inter = data.clean[i,3]
        stepnew = filter(steps,interval == inter)$mn
        data.clean[i,1]=stepnew
    }
}
```

And we can check there are no missing values with again:
```{r, echo=TRUE}
sum(is.na(data$steps))
```

Finally, lets check if the missing NA creat any bias in our study by reperforming the Total Number steps by days calculations:
```{r, echo=TRUE}
data.clean.tot<-group_by(data.clean,date)
somme.clean<-summarize(data.clean.tot,sm=sum(steps))

mean.clean<-mean(somme.clean$sm)
median.clean<-median(somme.clean$sm)
```

And we can plot the new histogram:
```{r, echo=TRUE}
hist(somme.clean$sm, main="Number of steps per day",xlab="Total steps",ylab="")
```

Difficult to see any differences. in order to highlight this point, we performed the following coding/plotting to show the diffrences between the two collections of data:
```{r, echo=TRUE}
data1<-mutate(somme,type="init")
data2<-mutate(somme.clean,type="clean")
data.print<-rbind.data.frame(data1,data2)
library(ggplot2)
ggplot(data.print, aes(sm, fill = type)) + geom_density(alpha = 0.2) + labs(x="steps sum per days")+labs(title="Comparison of clean vs. initial")
```

As we can see, our manipulation only re-centered our collection to give it a more gaussien shap. So the missing NA were not critical and didn't bias our initial studies.


#Are there differences in activity patterns between weekdays and weekends?
Finally, we want to compare weeks activity with weekend activity and see if we can draw any conclusions from this collection. So we added a new variable called Type wich represent if a day is in "weekday" or "weekend".
```{r, echo=TRUE}
data.week<-data.clean
data.week<-mutate(data.week,daytype="weekday")
for (i in 1:length(data.week$steps))
{
    wday=weekdays(data.week[i,2])
    if(wday=="samedi"){data.week[i,4]<-"weekend"}
    if(wday=="dimanche"){data.week[i,4]<-"weekend"}
    }
```

Then we performed the daily activity study for intervals activity but we subdivided our calculation on both separated weekdays and weekends days. The final result is a plot showing the average activity pattern for our subject during weekday and weekends.
```{r, echo=TRUE}
data.week<-group_by(data.week,daytype,interval)
final<-summarize(data.week,mn=mean(steps))
library("lattice")
xyplot(mn~interval|daytype, data = final,type="l",layout = c(1, 2))
```



