library(TSA)

library(mgcv)

#Choose the 'LA Temp Monthly.csv' dataset, wherever it is located on your computer

#Additionally, just skip this step and replace ???fname??? with the files direct location

fname <- "~/Desktop/Georgia Tech Classes/ISYE 6402 - Time Series Analysis/LA Temp Monthly.csv"

#Load Data

data_lat_temp <- read.csv(fname)
View(data_lat_temp)


data_lat_temp <- data_lat_temp[,2]

#Convert to TS data in proper frame

temp_la <- ts(data_lat_temp,start=c(1950,1),freq=12)


# Question 1a: Exploratory Data Analysis:
# Plot the Time Series and ACF plots. Comment on the main features, and identify what (if any) 
# assumptions of stationarity are violated. 
# Hint: Before plotting, can you infer anything from the nature of the data?

ts.plot(temp_la,ylab="Temperature")
acf(temp_la,lag.max=12*4,main="Temperature")

# Based on the Time Series and ACF plots, we observe seasonality as expected.
# Also, the temperature series shown in the Time Series plots contains some evidence of a trend over time.
# The fact that we observe seasonality in the presence of probable trend, 
# we conclude that Time Series data is not stationary.
# If there is a Trend, it means the mean is not constant and it increases over time. 
# Presence of seasonality, violates the condition of finite variance. In this situation,
# the variance is a function of time.
# The 3 condition of mean value is constant, covariance function is time-invariant and 
# variance is constant over time are violated.

# On its own, which type of model do you think will fit the data best: trend or seasonality 
# fitting?

# Based on the Time Series and ACF plots, the data display Seasonality and Trend. We believe both model
# are needed to fit the data best. However, if we need to pick one model, we think that seasonality
# will fit the data best.

# Question 1b: Trend Estimation:

## Is there a trend in the average temperature?
time_la.pts = c(1:length(temp_la))
time_la.pts = c(time_la.pts - min(time_la.pts))/max(time_la.pts)

## Fit a moving average 
mav_la.fit = ksmooth(time_la.pts, temp_la, kernel = "box")
temp.fit.mav_la = ts(mav_la.fit$y,start=1950,frequency=12)
ts.plot(temp_la,ylab="Temperature", main = "Moving Average Trend Estimation")
lines(temp.fit.mav_la,lwd=4,col="red")
abline(temp.fit.mav_la[1],0,lwd=2,col="blue")
legend(x=1950,y=76,legend=c("Original Time Series","Trend Estimation"),lty = 1, col=c("black","red"))


## Fit a parametric quadraric polynomial
x1 = time_la.pts
x2 = time_la.pts^2
lm.fit_la = lm(temp_la~x1+x2)
temp_la.fit.lm = ts(fitted(lm.fit_la),start=1950,frequency=12)
ts.plot(temp_la,ylab="Temperature", main = "Quadraric Polynomial Trend Estimation")
lines(temp_la.fit.lm,lwd=2,col="red")
abline(temp.fit.mav_la[1],0,lwd=2,col="blue")
legend(x=1950,y=76,legend=c("Original Time Series","Trend Estimation"),lty = 1, col=c("black","red"))

## Fit a trend using non-parametric regression
## Local Polynomial Trend Estimation
loc.fit_la = loess(temp_la~time_la.pts)
temp_la.fit.loc = ts(fitted(loc.fit_la),start=1950,frequency=12)
## Splines Trend Estimation
library(mgcv)
gam.fit_la = gam(temp_la~s(time_la.pts))
temp_la.fit.gam = ts(fitted(gam.fit_la),start=1950,frequency=12)

## Is there a trend? 
ts.plot(temp_la,ylab="Temperature", main = "Non-Parametric Trend Estimation")
lines(temp_la.fit.loc,lwd=2,col="green")
lines(temp_la.fit.gam,lwd=2,col="red")
abline(temp_la.fit.loc[1],0,lwd=2,col="blue")
legend(x=1950,y=75,legend=c("Original Time Series", "Local Polynomial Trend Estimation","Splines Trend Estimation"),lty = 1, col=c("black","green","red"))


## Compare all estimated trends
all.val = c(temp.fit.mav_la,temp_la.fit.lm,temp_la.fit.gam,temp_la.fit.loc)
ylim= c(min(all.val),max(all.val))

#ts.plot(temp_la.fit.lm,lwd=2,col="green",ylim=ylim,ylab="Temperature")
ts.plot(temp_la,ylab="Temperature")
lines(temp_la.fit.lm,lwd=2,col="green")
lines(temp.fit.mav_la,lwd=2,col="purple")
lines(temp_la.fit.gam,lwd=2,col="red")
lines(temp_la.fit.loc,lwd=2,col="brown")
legend(x=1950,y=75,legend=c("Moving Average ","Quadraric Polynomial","Splines","Local Polynomial"),lty = 1, col=c("purple","green","red","brown"))

## Residual Process: Trend Removal
# Residuals from moving average fitting:

resid.1_la = temp_la-temp.fit.mav_la
ts.plot(resid.1_la,ylab="Residuals Moving Average Model")
acf(resid.1_la,lag.max=12*4,main="Moving Average Model")

# Residuals from parametric quadraric polynomial:
resid.2_la = temp_la-temp_la.fit.lm
ts.plot((resid.2_la),ylab="Residuals Quadraric Polynomial Model")
acf(resid.2_la,lag.max=12*4,main="Quadraric Polynomial Model")

# Residuals Splines:
resid.3_la = temp_la-temp_la.fit.gam
ts.plot(resid.3_la,ylab="Residuals Splines Model")
acf(resid.3_la,lag.max=12*4,main="Splines Model")

# Residuals Local Polynomial:
resid.4_la = temp_la-temp_la.fit.loc
ts.plot(resid.4_la,ylab="Residuals Local Polynomial Model")
acf(resid.4_la,lag.max=12*4,main="Local Polynomial Model")

# Comment on the four models fit and on the appropriateness of the stationarity 
# assumption of the residuals.

# Based on the plots, we still have seasonality, thus the Time Series is non stationary.
all.val = c(temp.fit.mav_la,temp_la.fit.lm,temp_la.fit.gam,temp_la.fit.loc)
ylim= c(min(all.val),max(all.val))
ts.plot(temp_la.fit.lm,lwd=2,col="green",ylim=ylim,ylab="Temperature")
lines(temp.fit.mav_la,lwd=2,col="purple")
lines(temp_la.fit.gam,lwd=2,col="red")
lines(temp_la.fit.loc,lwd=2,col="brown")
legend(x=1950,y=65,legend=c("Moving Average ","Quadraric Polynomial","Splines","Local Polynomial"),lty = 1, col=c("purple","green","red","brown"))

# From this plot we see that the moving average trend is quite weak, slightly capturing some 
# of the trend seasonality thus not a good estimate. 
# The estimated trend using the parametric quadratic polynomial model is comparable to that 
# fitted using the splines regression. 
# Except that the fitting model is slightly quadratic, 
# but generally both capture a similarly increasing trend over time.

# The fit using local polynomial regression shows more complexity in the trend with 
# some smooth ups and downs. 
# Although generally it also shows an increase in the temperature over time. 
# Generally from all four approaches we see an increase in temperature with about 6 degrees 
# in the last 70 years. However the trend model is not good enough to fit the data. It does not capture the seasonality.

# Question 1c: Seasonality Estimation

library(TSA)

## Estimate seasonality using ANOVA approach
month_la = season(temp_la)

## Drop January (model with intercept)
model1_la = lm(temp_la~month_la)
summary(model1_la)
#temp_la.fit.model1 = ts(fitted(model1_la),start=1950,frequency=12)
## All seasonal mean effects (model without intercept)
model2_la = lm(temp_la~month_la-1)
#temp_la.fit.model2_la = ts(fitted(model2_la),start=1950,frequency=12)
summary(model2_la)
anova(model1_la,model2_la)
temp_la.fit.model2_la = ts(fitted(model2_la),start=1950,frequency=12)


## Estimate seasonality using cos-sin model
har_la=harmonic(temp_la,1)
model3_la=lm(temp_la~har_la)
temp_la.fit.model3_la = ts(fitted(model3_la),start=1950,frequency=12)
summary(model3_la)
har2_la=harmonic(temp_la,2)
model4_la=lm(temp_la~har2_la)
temp_la.fit.model4_la= ts(fitted(model4_la),start=1950,frequency=12)
summary(model4_la)

## Compare Seasonality Estimates
## Seasonal Means Model - ANOVA approach
st1_la = coef(model2_la)
## Cos-Sin Model
st2_la = fitted(model4_la)[1:12]
plot(1:12,st1_la,lwd=2,type="l",xlab="Month",ylab="Seasonality", col = "green")
lines(1:12,st2_la,lwd=2, col="red")
legend(x=1,y=68,legend=c("ANOVA approach","Cos-Sin Model"),lty = 1, col=c("green","red"))

# Here we compared the fit seasonality for the two models to see whether there is a significant difference 
# in the fit. As seen in this plot, both are very similar in fitting the seasonality. 
# Because the fit is almost the same, we would prefer the model with fewer regression coefficients 
# particularly the sine-cosine model.

## Compare all seasonal model
# We select the ANOVA approach without intercept and the Cos-Sin Model with higher 
# frequency harmonics becasue they have both the highest R-squared compared to their alternatives.
all.val = c(temp_la.fit.model2_la,temp_la.fit.model4_la)

ts.plot(temp_la,ylab="Temperature")
lines(temp_la.fit.model2_la,lwd=2,col="green")
legend(x=1950,y=77,legend=c("Original Time Series","ANOVA Approach Fitted Values"),lty = 1, col=c("black","green"))

ts.plot(temp_la,ylab="Temperature")
lines(temp_la.fit.model4_la,lwd=2,col="purple")
legend(x=1950,y=77,legend=c("Original Time Series","Cos-Sin Model Fitted Values"),lty = 1, col=c("black","purple"))

## Residual Process: Seasonal Removal

resid.1_la_seasonal = temp_la-temp_la.fit.model2_la

ts.plot(resid.1_la_seasonal,ylab="Residuals ANOVA Approach")
acf(resid.1_la_seasonal,lag.max=12*4,main="ANOVA Approach")

# Residuals from parametric quadraric polynomial:
resid.2_la_seasonal = temp_la-temp_la.fit.model4_la

ts.plot((resid.2_la_seasonal),ylab="Residuals Cos-Sin Model")
acf(resid.2_la_seasonal,lag.max=12*4,main="Cos-Sin Model")
ts.plot(resid.1_la_seasonal,ylab="Residual Process",col="red")
lines(resid.2_la_seasonal,col="blue")
legend(x=1950,y=9,legend=c("Residuals ANOVA Approach","Residuals Cos-Sin Model"),lty = 1, col=c("red","blue"))



# Given the plot of the fitted values and ACF of the residuals, we observe that the data is not stationary yet,
# because of the trend effect. To make it stationary, we need to fit a trend and seasonality estimation.

# Seasonality & Trend: Compare Model
# Given the question 1b we found that there is a slightly increasing trend in temperature over time. 
# Thus, we would like to estimate both seasonality and trend jointly. 

# We'll begin with quadratic trend and seasonality modeled using the cosine-sine model. 

x1 = time_la.pts
x2 = time_la.pts^2
lm.fit = lm(temp_la~x1+x2+har2_la)
dif.fit.lm = ts((temp_la-fitted(lm.fit)),start=1950,frequency=12)
ts.plot(dif.fit.lm,ylab="Residual Process")
acf(dif.fit.lm,lag.max=12*4,main="ACF of residuals")

#The plot the residuals and ACF introduced after remove in removing the trend and seasonality show that 
# data is stationary as the ACF is close to 0 after lag 5. Specifically, subtracting from the time series, the fitted values of the linear model.
 

# Below we fit a non-parametric trend Splines along with the seasonality given by the cosine-sine model. 
gam.fit = gam(temp_la~s(time_la.pts)+har2_la)
dif.fit.gam = ts((temp_la-fitted(gam.fit)),start=1950,frequency=12)
acf(dif.fit.gam,lag.max=12*4,main="")
# Here the ACF decrease much faster than the quadratic model, we conclude that non parametric model and Splines model
# capture better the trend and seasonality effect.

# Next, we compare the residual processes for both methods. 
# Here is the plot comparing the residual processes, where brown is the process for the parametric model 
# and in blue is residual process from the non-parametric model. 
# The differences are small between the two approaches.

ts.plot(dif.fit.lm,ylab="Residual Process",col="red")
lines(dif.fit.gam,col="blue")
legend(x=1950,y=8,legend=c("Residuals quadratic + splines","Residuals non parametric + splines"),lty = 1, col=c("red","blue"))







