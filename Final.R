library(forecast)
library(fpp)
library(TTR)
new=Data_Fall_2018_Crimes
new
plot(new$Data,type="o")
#Question 1
timeseries= ts(new$Data,frequency = 12,start=c(2008,1))
plot(timeseries)
title("Timeseries Plot for Crimes")
#Question 2: Please summaries your observations of the times series plot

#Question 3: Central Tendency
#What are the min, max, mean, median, 1st and 3rd Quartile values of the times series? 
summary(timeseries)
boxplot(timeseries, main= "BoxPlot for theft, Robberies and Larcency",ylab="# of crimes")

#Question 4:
decomp=decompose(timeseries)
plot(decomp)
decomp$type
decomp$seasonal

adjust=seasadj(decomp)
plot(adjust, main="Seasonally Adjusted",ylab="# of theft")
plot(timeseries, main="Timeseries vs Seasonal Adjusted",ylab="# of theft")
lines(adjust,col="Red",type="o")

#Naive method
naive= naive(timeseries,h=18)
plot(forecast(naive),ylab = "# of Crime",xlab = "Time")

naive_resid=naive$residuals
plot(naive_resid)

hist((naive_resid))
fitted=naive$fitted

#Fitted vs Residual
naive$residuals
residual=naive$residuals[1-12]
fitted=naive$fitted[1-12]
plot(fitted~residual,main="Fitted vs Residual",col=c("Red","Blue"))
legend(-380,800,legend= c("Fitted","Residuals"),col=c("Red","Blue"),lwd = 1:1,cex=1)

#Actuals vs Residuals
actual=naive$x[1-12]
residual=naive$residuals[1-12]
plot(actual~residual,main="Actual vs Residual",col=c("Red","black"))
legend(-380,1750,legend= c("Actual","Residuals"),col=c("Red","black"),lwd = 1:1,cex=1)

#ACF of residuals
ACF_plot=Acf(residual)
accuracy(naive)

forecast(naive)

#SIMPLE MOVING AVERAGE
plot(timeseries)

#order 3
plot(timeseries)
simple_3= SMA(timeseries, n=3)
lines(simple_3,col="red",title("Simple Moving Average (order:3)"),type="l")
legend(2014.7,1700,legend=c("timeseries","order_3"),col = c("black","red"),lwd = 1:1,cex=1)
#order 6
plot(timeseries)
simple_3= SMA(timeseries, n=3)
lines(simple_3,col="red")
simple_6=SMA(timeseries,n=6)
lines(simple_6,col="blue",title("Simple Moving Average (order:3 and 6)"),type="l")
legend(2014.7,1700,legend=c("timeseries","order_3","order_6"),col = c("black","red","blue"),lwd = 1:1,cex=1)
#order 9
plot(timeseries)
simple_3= SMA(timeseries, n=3)
lines(simple_3,col="red")
simple_6=SMA(timeseries,n=6)
lines(simple_6,col="blue")
simple_9=SMA(timeseries,n=9)
lines(simple_9,col="green",title("Simple Moving Average (order:3,6 and 9)"),type="l")
legend(2014.7,1700,legend=c("timeseries","order_3","order_6","order_9"),col = c("black","red","blue","green"),lwd = 1:1,cex=1)

#Forecast for next 12 months using SMA(order 3)
forecast_12=forecast(simple_3,h=12)
forecast_12
plot(forecast_12,ylab = "# of Crimes",xlab = "Years")

#Simple Exponential Smoothing:
smooth=ses(timeseries,h=12)
summary(smooth)
smooth_resid=smooth$residuals
plot(smooth_resid, main = "Residual plot")

hist(smooth_resid,breaks = 15)

#Fitted vs Residual
smooth_fit=smooth$fitted[1-12]
smooth_resid=smooth$residuals[1-12]
plot(smooth_fit~smooth_resid,col=c("red","black"),main="Fitted vs Residual")
legend(-350,825,legend=c("Fitted","Residual"),col=c("red","black"),lwd = 1:1,cex=1)

#Actuals vs Residuals
smooth_act=smooth$x[1-12]
plot(smooth_fit~smooth_act,col=c("red","blue"), main="Fitted vs Actual")
legend(600,1650,legend=c("Fitted","Actual"),col=c("red","blue"),lwd = 1:1,cex=1)

#ACF of residuals
Acf(smooth_resid)

#Accuracy
accuracy(smooth)

#Forecast
plot(forecast(smooth,h=12))
holt_resid
holt_fit

#Holt Winters
holt=HoltWinters(timeseries)
holt_forecast= forecast(holt,h=12)
holt_forecast

sigma=sd(complete.cases(holt_forecast$residuals))
sigma

holt_residual=holt_forecast$residuals
#residuals
plot(holt_residual,main="Residual Plot")

hist(holt_residual,breaks=10)

#Fitted vs Residual 
holt_resid=holt_forecast$residuals[1-12]
holt_fit=holt_forecast$fitted[1-12]
plot(holt_resid~holt_fit,main="Residual vs Fitted",col=c("red","blue"))
legend(550,350,legend = c("Residual","Fitted"), col=c("blue","red"),lwd = 1:1,cex = 1)

#Actual vs Residuals
holt_resid=holt_forecast$residuals[1-12]
holt_actual=holt_forecast$x[1-12]
plot(holt_resid~holt_actual,main="Residual vs Actual",col=c("blue","red"))
legend(800,350,legend = c("Residual","Actual"), col=c("blue","red"),lwd = 1:1,cex = 1)

#ACF Residual
Acf(holt_forecast$residuals)

#Accuracy
accuracy(holt_forecast)

#Forecasting
holt_forecast= forecast(holt,h=24)
holt_forecast

plot(forecast(holt_forecast))

# ARIMA or Box-Jenkins
#To test if time series is stationary
# To test if the timeseries is stationary we can use KPSS test. 
kpss.test(timeseries)
# Our timeseries is not stationary, we can verify that by p-value if p-value is less than 0.05 then the data is not stationary.
#By Kpss.test we came to know that our data is not atationary, to make our timeseries stationary we should keep differencing.

#We need 1 diff to make the time series stationary
ndiffs(timeseries)
#Yes we need seasonal component as well
nsdiffs(timeseries)

#Here we are removing the difference and seasonality
second_diff=diff((timeseries), 12)
ndiffs(second_diff)
nsdiffs(second_diff)
#ACF and PACF plot
tsdisplay(second_diff)
Acf(second_diff)
Pacf(second_diff)
#Second diff(exapmle)
diff_seas_1=diff(diff((timeseries), 12))
ndiffs(diff_seas_1)
nsdiffs(diff_seas_1)
Acf(diff_seas_1)
Pacf(diff_seas_1)
tsdisplay(diff_seas_1)

#Best Arima model
arima=auto.arima(second_diff)
summary(arima)
arima$aic
arima$bic
arima$sigma2

arima_auto= auto.arima(second_diff, stepwise = FALSE, trace = TRUE, approximation = FALSE)
accuracy(arima_auto)
arima_auto$aic
arima_auto$bic
arima_auto$sigma2

#Residual plot
arima_ts = Arima(timeseries, order=c(0,0,4), seasonal=c(0,0,1))
plot(arima_ts$residuals)

#Hist of residual
hist(arima_ts$residuals, breaks = 20)

#Fitted vs Residual
arima_fit = arima_ts$fitted[1-12]
arima_resid = arima_ts$residuals[1-12]
plot(arima_fit~arima_resid,col=c("red","black"),main="Fitted vs Residual")
legend(-350,1675,legend=c("Fitted","Residual"),col=c("red","black"),lwd = 1:1,cex=1)

#Actuals vs Residuals
arima_act=arima_ts$x[1-12]
plot(arima_resid~arima_act,col=c("red","blue"), main="Residual vs Actual")
legend(600,350,legend=c("Residual","Actual"),col=c("red","blue"),lwd = 1:1,cex=1)

#Acf
Acf(arima_resid)

accuracy(arima_ts)

#Forecast
arima_forecast=forecast(arima_ts,h=12)
plot(arima_forecast)
arima_forecast

arima_forecast=forecast(arima_ts,h=24)
plot(arima_forecast)
arima_forecast
