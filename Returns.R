###Applied Econometrics R script, Catholic University of Milan, BSc Economics####
#Author: Alberto Ciampini

require(quantmod) #main package used
require(dygraphs) #interactive graphs
require(PerformanceAnalytics) #marginal: table.Stats
require(tseries) #Jarque-Bera test for normality
require(forecast) #forecasts for ARMA and MA models

#download stock prices  
getSymbols(c("GOOG","AAPL"),from="2013-01-01",to="2014-10-20")
 
#fetch adjusted stock prices 
GOOG.Adjusted<-GOOG[,6]
AAPL.Adjusted<-AAPL[,6]
 
#two separate time series plots of stock prices
chartSeries(GOOG.Adjusted,theme=chartTheme("white", up.col='green'))
chartSeries(AAPL.Adjusted,theme=chartTheme("white", up.col='blue'))
 
#single interactive time series plot of stock prices
dygraph(merge(GOOG.Adjusted,AAPL.Adjusted),ylab="Adjusted price",main="Google and Apple Adjusted Stock Prices") 

#fetch log daily returns
rG<-dailyReturn(GOOG.Adjusted,type="log")
rA<-dailyReturn(AAPL.Adjusted,type="log")
head(merge(rG,rA))

#two separate time series plots of log daily returns
chartSeries(rG,theme=chartTheme("white", up.col='green'))
abline(b=0,a=mean(rG),col="red")
chartSeries(rA,theme=chartTheme("white", up.col='blue'))
abline(b=0,a=mean(rA),col="red")
 
#fetch log daily returns
returns<-cbind(rG,rA)
colnames(returns)<-c("rG","rA")
 
#fetch cumulative daily returns 
crG<-cumprod(1+rG) 
str(crG)
crA<-cumprod(1+rA) 
str(crA) 
basket<-cbind(crG,crA)
colnames(basket)<-c("crG","crA")
zoo.basket<-as.zoo(basket)
 
#single time series plot of cumulative daily returns
dygraph(basket,ylab="Cumulative daily return",main="Google and Apple Cumulative daily returns")

#descriptive statistics of log daily returns
table.Stats(returns)

#test for normality using Skeweness and Kurtosis parameters (Jarqe-Brera)
jarque.bera.test(rG)
jarque.bera.test(rA)

#two histograms of frequencies with fitted gaussian shape lines
hist(rG,nclass=100,xlab="Daily returns",ylab="Frequency",main="Histogram of frequencies of Google log daily returns",col="green")
points(seq(min(rG),max(rG),length.out=500),dnorm(seq(min(rG),max(rG),length.out=500),mean(rG),sd(rG)),type="l",col="red")
hist(rA,nclass=100,xlab="Daily returns",ylab="Frequency",main="Histogram of frequencies of Apple log daily returns",col="blue")
points(seq(min(rA),max(rA),length.out=500),dnorm(seq(min(rA),max(rA),length.out=500),mean(rA),sd(rA)),type="l",col="red")

#ACF and PACF of log daily returns (autocorrelation) 
plot(acf(rG),main="Autocorrelation of Google log daily returns")
plot(acf(rA),main="Autocorrelation of Apple log daily returns")
plot(pacf(rG),main="Partial autocorrelation of Google log daily returns")
plot(pacf(rA),main="Partial autocorrelation of Apple log daily returns")

#ARMA models for Google
summary(arima(rG,order=c(5,0,5)))
summary(arima(rG,order=c(5,0,0)))
summary(arima(rG,order=c(0,0,5)))

#ARMA models for Apple
summary(arima(rA,order=c(5,0,5)))
summary(arima(rA,order=c(5,0,0)))
summary(arima(rA,order=c(0,0,5)))

#MA model prediction for Google
forecast(arima(rG,order=c(0,0,5)))
head(forecast(arima(rG,order=c(0,0,5))))
plot(forecast(arima(rG,order=c(0,0,5))),main="Forecasts from MA(5) of Google log daily returns")

#ARMA model prediction for Apple
forecast(arima(rA,order=c(5,0,5)))
head(forecast(arima(rA,order=c(5,0,5))))
plot(forecast(arima(rA,order=c(5,0,5))),main="Forecasts from ARMA(5,5) of Apple log daily returns")