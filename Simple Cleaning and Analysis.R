READING, CLEANING AND ANALYSIS OF EXCHANGE RATES FROM 2013-2017
setwd("C:/Users/pc/Desktop/data")     
library("readxl")
rates<-read.csv("forex.csv", header=TRUE)
head(rates)
date<-as.Date(rates[,1],format="%d/%m/%Y")
head(rates)
rates<-cbind(date,rates[,-1])
head(rates)
rates<-rates[order(rates$date),]
head(rates)
tail(rates)
class(rates)
library(xts)
rates<-xts(rates[,2:5],order.by = rates[,1])
head(rates)
class(rates)
names(rates)<-paste(c("currency","mean","buy","sell"))
head(rates)
y.range<-range(rates[,2:4])
y.range
par(mfrow=c(1,1))
plot(x=index(rates), xlab="Years", y=rates$mean, ylab="Exchange Rate", type="l",lwd=2, main="Time Plot of KES/USD from 2013 to 2017")
dim(rates)
summary(rates)

#EXPLORATORY DATA ANALYSIS
#STEP 1: TIME PLOT
plot(x=index(rates), xlab="Date", y=rates$mean, ylab="Exchange Rates", type="l", main="Time Series KSH/USD Rates")
#STEP 2: SUMMARY STATISTICS
summary(rates$mean)
library(e1071)
kurtosis(as.numeric(rates[,2])) 
skewness(as.numeric(rates[,2]))
sd(as.numeric(rates[,2]))
mean(as.numeric(rates[,2]))