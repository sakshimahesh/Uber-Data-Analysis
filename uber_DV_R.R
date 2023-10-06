###Uber Data visualization (worked in April 2023)
library(tidyverse)#DS package all in one
library(DT)#java datatables
library(ggplot2)
library(ggthemes)
library(lubridate) #date time
library(scales)#scale vector, dataframes
#####reading files#####
april<-read.csv("C:/selfstudy_R_CS/Projects_ML/uber_DA/uber-raw-data-apr14.csv")
dim(april)
may<-read.csv("C:/selfstudy_R_CS/Projects_ML/uber_DA/uber-raw-data-may14.csv")
june<-read.csv("C:/selfstudy_R_CS/Projects_ML/uber_DA/uber-raw-data-jun14.csv")
july<-read.csv("C:/selfstudy_R_CS/Projects_ML/uber_DA/uber-raw-data-jul14.csv")
aug<-read.csv("C:/selfstudy_R_CS/Projects_ML/uber_DA/uber-raw-data-aug14.csv")
sept<-read.csv("C:/selfstudy_R_CS/Projects_ML/uber_DA/uber-raw-data-sep14.csv")
#####bind files in one##########
#uberdata in one variable
a<-rbind(april,may, july, june, aug, sept)
dim(a)
head(a)
#######change time format####
a$Date.Time <- as.POSIXct(a$Date.Time, format="%m/%d/%Y %H:%M:%S")
head(a)
###creating a column "time" and formating it#######

a$Date<-format(as.POSIXct(a$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%Y-%m-%d")
a$Time <- format(as.POSIXct(a$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
a$Date <- ymd_hms(a$Date.Time)
head(a)

#creating separate columns#
a$day<-factor(day(a$Date.Time))
a$month<-factor(month(a$Date.Time, label = TRUE))
a$year <- factor(year(a$Date.Time))
a$dayofweek <- factor(wday(a$Date.Time, label=TRUE))

a$second = factor(second(hms(a$Time)))
a$minute = factor(minute(hms(a$Time)))
a$hour = factor(hour(hms(a$Time)))
head(a)

##1.visualizing number of trips by hour#
##b=data####
b<-a %>% group_by(hour) %>% dplyr::summarize(Total = n())
datatable(b)

#### creating  vector of colors for the plots###
colr=c("#cc1011", "#665555", "#05a399")
colr

##### creating continuous bar plot grouped by trips taken by hours#########
##1.1
ggplot(data = b, aes(x=hour, y=Total)) + geom_bar(stat = "identity", fill="blue", color="red") + 
  ggtitle("n of trips by hours", subtitle = "totday") + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma)

###data visualization for trips taken for the month and hour#######
##1.2
mh<-a %>% group_by(month, hour) %>% dplyr::summarize(Total = n())
ggplot(mh, aes(hour, Total, fill=month)) + geom_bar(stat="identity") +
  ggtitle("trips by month and hours")+
  scale_y_continuous(labels = comma)

####grouping original data i.e a by the column day######
##2
day_data <- a %>% group_by(day) %>% dplyr::summarize(Trips = n())
day_data

##2.1 Data visualization by trips taken on number of days#######
###shows Day 31st has the lowest trips##
ggplot(day_data, aes(day, Trips)) + geom_bar(stat = "identity") + 
  ggtitle("trips according to days") +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)


###Data by day of the week and month ###
##3
c<- a %>% group_by(dayofweek, month) %>% dplyr::summarize(trips=n())
c
ggplot(c, aes(dayofweek, trips, fill = month)) + geom_bar(stat="identity", aes(fill = month ), position = "dodge") +
  scale_y_continuous(labels = "comma") + ggtitle("dayofweek vs month") + scale_fill_manual(values = colors)
 
ggplot(c, aes(dayofweek, trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trias by Day and Month") + 
  scale_y_continuous(labels = comma)  
 # scale_fill_manual(values = colors)
  