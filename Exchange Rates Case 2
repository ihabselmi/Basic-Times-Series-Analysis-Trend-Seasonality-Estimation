# Question 2: Currency Conversion Analysis (40 Points)

library(TSA)

library(mgcv)

#Choose the 'USD-EUR Exchange.csv' dataset, wherever it is located on your computer

fname <- "~/Desktop/Georgia Tech Classes/ISYE 6402 - Time Series Analysis/USD-EUR Exchange.csv"

#Load data

data_usdeur <- read.csv(fname)
View(data_usdeur)

data_usdeur <- data_usdeur[,2]

#Convert to TS data in proper frame

rate <- ts(data_usdeur,start=c(2000,1),freq=52)

#Generate differenced data

rate.diff <- diff(rate)

# Question 2a: Exploratory Data Analysis:

# Plot the Time Series and ACF plots. Comment on the main features, and identify what 
# (if any) assumptions of stationarity are violated.
ts.plot(rate,ylab="Exchange Rate")
acf(rate,main="Exchange Rate", lag.max=52)

# Using the differenced rate data ('rate.dif'), plot both the Time Series and ACF plots. 
# Comment on the main features, and identify what (if any) assumptions of stationarity are 
# violated. Additionally comment if you believe the differenced data is more appropriate for 
# use in analysis. Support your position with your graphical analysis.

ts.plot(rate.diff,ylab="Exchange Rate")
acf(rate.diff,main="Exchange Rate", lag.max=52)

# Based on the acf and Time Series plot of the differenced rate data look more appropriate for 
# the analysis. The ACF get close to 0 from the lag 1 which support the idea of stationarity. 
# Based on the acf and Time Series plot of the exchange rates, the data is not stationary because the
# mean increase with time and the variance is infinite time variant.


# Question 2b: Trend-Seasonality Estimation:

## Fit a parametric quadraric polynomial
rate.pts = c(1:length(rate))
rate.pts = c(rate.pts - min(rate.pts))/max(rate.pts)
x1 = rate.pts
x2 = rate.pts^2
lm.fit_rate = lm(rate~x1+x2)
summary(lm.fit_rate)
rate.fit.lm = ts(fitted(lm.fit_rate),start=2000,frequency=52)
ts.plot(rate,ylab="Exchange rate")
lines(rate.fit.lm,lwd=2,col="red")
legend(x=2000,y=1.6,legend=c("Original Time Series","Quadraric Polynomial Trend"),lty = 1, col=c("black","red"))



## Fit a trend using non-parametric regression

## Splines Trend Estimation
library(mgcv)
gam.fit_rate = gam(rate~s(rate.pts))
summary(gam.fit_rate)
rate.fit.gam = ts(fitted(gam.fit_rate),start=2000,frequency=52)
ts.plot(rate,ylab="Exchange rate")
lines(rate.fit.gam,lwd=2,col="red")
legend(x=2000,y=1.6,legend=c("Original Time Series","Splines Trend"),lty = 1, col=c("black","red"))


## Is there a trend? 
ts.plot(rate,ylab="Exchange Rate")
lines(rate.fit.gam,lwd=2,col="red")
lines(rate.fit.lm,lwd=2,col="green")
legend(x=2000,y=1.65,legend=c("Original Time Series","Splines Trend", "Quadraric Polynomial Trend"),lty = 1, col=c("black","red", "green"))


# Residuals plot

resid.1_rate = rate-rate.fit.lm
ts.plot(resid.1_rate,ylab="Residuals")
acf(resid.1_rate,main="", lag.max=52)


resid.3_rate = rate-rate.fit.gam
ts.plot(resid.3_rate,ylab="Residuals")
acf(resid.3_rate,main="", lag.max=52)

# The ACF of non parametric model decrease much faster towards 0 than parametric one. 


# Seasonality Estimation

library(TSA)

## Estimate seasonality using ANOVA approach
week = season(rate)

## Drop January (model with intercept)
model1_rate = lm(rate~week)
summary(model1_rate)
## All seasonal mean effects (model without intercept)
model2_rate = lm(rate~week-1)
summary(model2_rate)
rate.fit.gam_seasonal = ts(fitted(model2_rate),start=2000,frequency=52)
ts.plot(rate,ylab="Exchange rate")
lines(rate.fit.gam_seasonal,lwd=2,col="green")

resid.1_rate_seasonal = rate-rate.fit.gam_seasonal
ts.plot(resid.1_rate_seasonal,ylab="Residuals")
acf(resid.1_rate_seasonal,main="", lag.max=52)

# Based on the residuals of the trend estimation, the Times Series is still not stationary as 
# the ACF is higher than 0 during at least 6 month lag. Regarding the seasonality, it is obvious that
# data display no seasonality. The seasonality fitting is not really needed here.

# Question 2c: Trend-Seasonality Estimation with Differenced Data

## Fit a parametric quadraric polynomial
rate.diff.pts = c(1:length(rate.diff))
rate.diff.pts = c(rate.diff.pts - min(rate.diff.pts))/max(rate.diff.pts)
x1 = rate.diff.pts
x2 = rate.diff.pts^2
lm.fit_rate.diff = lm(rate.diff~x1+x2)
summary(lm.fit_rate.diff)
rate.diff.fit.lm = ts(fitted(lm.fit_rate.diff),start=2000,frequency=52)
ts.plot(rate.diff,ylab="Differenced Exchange rate")
lines(rate.diff.fit.lm,lwd=2,col="red")
legend(x=2000,y=0.08,legend=c("Residuals","Quadraric Polynomial Trend"),lty = 1, col=c("black","red"))

## Splines Trend Estimation
library(mgcv)
gam.fit_rate.diff = gam(rate.diff~s(rate.diff.pts))
summary(gam.fit_rate.diff)
rate.diff.fit.gam = ts(fitted(gam.fit_rate.diff),start=2000,frequency=52)
ts.plot(rate.diff,ylab="Differenced Exchange rate")
lines(rate.diff.fit.gam,lwd=2,col="green")
legend(x=2000,y=0.08,legend=c("Residuals","Splines Trend"),lty = 1, col=c("black","red"))

## Is there a trend? 
ts.plot(rate.diff,ylab="Exchange Rate")
lines(rate.diff.fit.gam,lwd=2,col="red")
lines(rate.diff.fit.lm,lwd=2,col="green")

ts.plot(rate.diff.fit.gam,ylab="Residuals")
lines(rate.diff.fit.lm,lwd=2,col="red")
legend(x=2000,y=0.0012,legend=c("Quadraric Polynomial Trend","Splines Trend"),lty = 1, col=c("black","red"))

resid.1_rate.diff = rate.diff-rate.diff.fit.lm
acf(resid.1_rate.diff,main="", lag.max=52)

resid.3_rate.diff = rate.diff-rate.diff.fit.gam
acf(resid.3_rate.diff,main="", lag.max=52)

# Seasonality Estimation

library(TSA)

## Estimate seasonality using ANOVA approach
week.diff = season(rate.diff)

## Drop January (model with intercept)
model1_rate.diff = lm(rate.diff~week.diff)
summary(model1_rate.diff)
## All seasonal mean effects (model without intercept)
model2_rate.diff = lm(rate.diff~week.diff-1)
summary(model2_rate.diff)
rate.diff.fit.gam_seasonal = ts(fitted(model2_rate.diff),start=2000,frequency=52)
ts.plot(rate.diff,ylab="Exchange rate")
lines(rate.diff.fit.gam_seasonal,lwd=2,col="green")

resid.1_rate.diff_seasonal = rate.diff-rate.diff.fit.gam_seasonal
ts.plot(resid.1_rate.diff_seasonal,ylab="Residuals")
acf(resid.1_rate.diff_seasonal,main="", lag.max=52)

# Based on the residuals of the trend estimation, the Times Series is stationary as 
# the ACF is close to 0 from lag 1. Regarding the seasonality, it is obvious that
# data does not display seasonality. The seasonality fitting is not really needed here.

















