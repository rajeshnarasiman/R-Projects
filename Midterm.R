library(readxl)
apple_sales=apple_data
head(apple_sales)
plot(apple_sales)
attach(apple_sales)
library(forecast)
library(fpp)
library(fpp2)
library(TTR)

#mac_ts = ts(apple_sales,start=c(1998,4),frequency = 4)
#plot(mac_ts)
plot(apple_data$Mac)
title(main="MAC Sales")
#Plot and Inference
timeseries = ts(Mac,frequency = 4,start = c(1998,4))
plot(timeseries,type = "o", main = "TimeSeries plot for MAC sales")
summary(Mac)
#Central Tendency
summary(timeseries)
#Boxplot
boxplot(timeseries,main = "BoxPlot",col = "light blue", xlab = "MAC")

#Decomposition
x = decompose(timeseries, filter = NULL)
x$seasonal

# Plotting Adjusted value
adjust = seasadj(x)
plot(adjust)
lines(adjust,col="blue")
#Plotting time series
plot(timeseries)
lines(adjust,col="blue")
title(main = "Actual vs Adjusted")

##NAIVE Method
# Forecasting using Naive method
naive_method= naive(timeseries)
plot(naive_method, xlab = "Time", ylab = "Frequency",type = "l")

#Residual plot
Resi= plot(naive_method$residuals)
title(main = "Residual")
#Histogram plot
histogram= hist(naive_method$residuals,xlab = "Residuals",breaks = 20)
?accuracy

#Plotting Fitted vs Residuals
fit= (naive_method$fitted[2-5])
residual=(naive_method$residuals[2-5])
plot((fit~residual),col= c("blue","Red"), main = "Fitted vs Residuals")
legend(-1.35,2,legend= c("Fitted","Residuals"),col=c("blue","red"),lwd = 1:1,cex=1)

#Plotting Actual vs Residuals
actual= naive_method$x[2-5]
plot(actual~residual, col=c("blue","Red"),main = "Actuals vs Residuals")
legend(-1.35,2,legend= c("Actuals","Residuals"),col=c("blue","red"),lwd = 1:1,cex=1)

#Plottig ACF for residuals
acf= Acf(residual)

#Five measures of accuracy 
accuracy(naive_method)

#Timeseries value for next year
forecast(naive_method,h=7)

plot(forecast(naive_method,h=7))

## SIMPLE MOVING AVERAGES
#Moving Average of order 3
plot(timeseries)
moving_3=ma(timeseries,order = 3)
lines(moving_3,col= "Red")
moving_6=ma(timeseries,order =6)
lines(moving_6, col ="Blue")
moving_9 =ma(timeseries,order = 9)
lines(moving_9, col = "Green")
title(main = "Simple Moving Average for orders(3,6 and 9)")
legend(1999,5.7,legend= c("timeseries","order 3","order 6","order 9"),col=c("black","red","blue","green")
       ,lwd = 1:1,cex=1)


## SIMPLE MOVING AVERAGES
plot(timeseries)
#Moving Average of order 3
plot(timeseries)
moving_3=SMA(timeseries,order = 3)
lines(moving_3,col= "Red")
#title(main = "Simple Moving Average for order 3")
# Moving Average for order 6
#plot(timeseries)
moving_6=SMA(timeseries,order =6)
lines(moving_6, col ="Blue")
#title(main = "Simple Moving Average for order 6")
# Moving Average for order 12
#plot(timeseries)
moving_9 =SMA(timeseries,order = 9)
lines(moving_9, col = "Green")
title(main = "Simple Moving Average for order 9")

#Forecasting using simple moving average for Order 9
?SMA
sma= ma(timeseries,order =3)
sma_one_year=(forecast(sma,7))
sma_one_year
plot(sma_one_year)
title(xlab = "Year",ylab = "Frequency")

sma= ma(timeseries,order=9)
sma_one_year=(forecast(sma,7))
sma_one_year
plot(sma_one_year)
title(xlab = "Year",ylab = "Frequency")


# SIMPLE SMOOTHING

simple= ses(timeseries,h=7)
summary(simple)
plot(simple)
#Residual Analysis
residual_smooth= simple$residuals[2-5]
plot(residual_smooth)
title(main = "Residual for simple smoothing")

#Histogram for Smoothing Residuals
hist(residual_smooth, breaks = 20)

# Plot for Fitted vs Residuals
Fitted_smooth= (simple$fitted[2-5])
plot(Fitted_smooth, col ="Red")
residual_smooth= simple$residuals[2-5]
plot(residual_smooth, col="black")
plot((Fitted_smooth~residual_smooth), col=c("Red","Black"), main = "Fitted smooth vs Residual smooth")
legend(-1.3,1.5,legend= c("Residual","Fitted"),col=c("black","red"),lwd = 1:1,cex=1)
# Plot for actual vs Residuals
Actual_smooth= simple$x[2-5]
plot(Actual_smooth)
plot((Actual_smooth~residual_smooth), col=c("Red","Black"), main = "Actual smooth vs Residual smooth")
legend(-1.3,5.5,legend= c("Residual","Actual"),col=c("black","red"),lwd = 1:1,cex=1)

#ACF plot for Residuals
Acf(residual_smooth)

#Accuracy
accuracy(simple)

#Timeseries for next year
summary(forecast(simple, h=7))
plot(forecast(simple, h=7), xlab = "Year", ylab = "Frequency")


##HOLT WINTERS
holt_winter =HoltWinters(timeseries)
holt_forecast=forecast(holt_winter)
(holt_winter)

sd(complete.cases(holt_forecast$residuals))
holt_forecast
#Residual
plot(holt_forecast$residual,main="Holt Winter Residual plot")
#Histogram for Residual
hist(holt_forecast$residual,breaks = 55)

#fitted values vs. residuals
holt_fitted= holt_forecast$fitted[2-5]
holt_residual= holt_forecast$residuals[2-5]
plot(holt_fitted~holt_residual, main= "Plot for fitted values vs. residuals",col=c("red","black"))
legend(-0.9,1.9,legend= c("Residual","Fitted"),col=c("black","red"),lwd = 1:1,cex=1)

#actual values vs. residuals
holt_actual= holt_forecast$x[2-5]
plot(holt_actual~holt_residual, main= "Plot for actual values vs. residuals",col=c("red","black"))
legend(-0.9,1.9,legend= c("Residual","Actual"),col=c("black","red"),lwd = 1:1,cex=1)

#Acf for Residual
Acf(holt_residual)

#Accracy
accuracy(holt_forecast)

#Timeseries value for next year
timeseries_next_year=forecast(holt_winter,h=7)
plot(timeseries_next_year)
summary(timeseries_next_year)
