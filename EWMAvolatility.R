#EWMA FUNCTION
rates2016<-read.csv("2016r.csv", header=TRUE)
date<-as.Date(rates2016[,1],format="%d/%m/%Y")
rates2016<-cbind(date,rates2016[,-1])
head(rates2016)
rates2016<-rates2016[order(rates2016$date),]
head(rates2016)
class(rates2016)
library(xts)
rates2016<-xts(rates2016[,2:3],order.by = rates2016[,1])
head(rates2016)
class(rates2016)

library(quantmod)
EWMA<-function(x, lambda)
{
#calculate the log returns
returns<-Delt(x,type="log")
#squared log returns
return_sq <-returns^2 
#convert from an xts object to a matrix
y<-as.matrix(x)
#an empty numeric vector for weights
n<-(1:nrow(y)-1)
#empty numeric vector to a matrix
z<-as.matrix(n)
#calculate the weights
weights<-(1-lambda)*lambda^z
#sorting the weights from the least
weights<-sort(weights, decreasing=FALSE)

product<-weights*return_sq
#remove all NAs from the data
product<-na.omit(product)

variance<-colSums(product)

volatility<-sqrt(variance)
final<-cbind(variance,volatility)
}
x<-vect
#use a lambda of 0.94 according to the risk metrics database
vol<-EWMA(x,0.94)
vol
annualizedvol<-0.0004299244 * sqrt(252)
annualizedvol

