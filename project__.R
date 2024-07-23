#In this project we firstly analyze 20 years of data about the consumption of 
#gas in UK and then based on that analysis, we try to forecast the next 10 year
#consumption and then validate it.










#this project is based on ARIMA forecasting
#ARIMA stands for AutoRegressive Integrated Moving Average, which is a 
#widely used statistical method for time series forecasting. 
#It combines three components:

#1.Autoregressive (AR): This part of the model explains the value at time 
#𝑡,t using a linear combination of previous values. It is specified by the parameter 
#𝑝,p, which denotes the number of lagged observations included in the model.

#2Integrated (I): This component involves differencing the time series data to 
#make it stationary, which means removing trends and seasonality. 
#It is specified by the parameter 𝑑d, which denotes the number 
#of differencing steps required.


#3Moving Average (MA): This part of the model uses past forecast errors in a 
#regression-like model. It is specified by the parameter 
#𝑞q, which denotes the number of lagged forecast errors included in the model.


#Following are the assumptions arima model makes before being applicable:
  #1.1. Stationarity: A time series is stationary if its statistical properties,
  #such as mean, variance, and autocorrelation, are constant over time.
  #Assumption: The ARIMA model assumes that the time series is stationary. 
  #This means that the data should not show trends or seasonal patterns.

  #2.No Autocorrelation in Residuals: Residuals are the differences between 
  #observed and predicted values.
  #Assumption: The residuals from the model should be uncorrelated 
  #(i.e., no patterns in the residuals). This implies that the model
  #has captured all the underlying structure in the data.

  #3.Sufficient Sample Size: The number of observations in the time series data.
  #Assumption: ARIMA models require a sufficient number of observations to produce 
  #reliable estimates.

#following are the steps included in our forecasting

#1.GATHERING THE DATA
#R has this dataset pre installed so we just have to load it into the compiler



UKgas  #this is the dataset
View(UKgas)



#2. DATA VISUALIZATION 





summary(UKgas)
str(UKgas)
plot(UKgas, main='Time Series Plot', ylab='Value', xlab='Time',col="red")
#this step shows us the data graphically
abline(reg=lm(UKgas~time(UKgas)))
plot(UKgas,main='Time Series Plot',ylab='value',xlab='Time',type='h',col='green',border='orange')
#this is a histogram showcasing the same data in the form of a histogram
boxplot(UKgas ~ cycle(UKgas), main='quarterly boxplot', xlab='quarter', ylab='Value')
#a boxplot showing that consumption is high in first quarter of the year i.e. during winter
decomposed <- decompose(UKgas)
plot(decomposed)


#3. SMOOTHING OUT THE DATA 
     #Now smoothing out is necessary as it helps in
        #1. Noise reduction: Noise is random variations without any underlying patterns.
        #2. Trend identification: The trend becomes more apparent by smoothening.
        #3. Forecasting         : Forecasting becomes easier.
        #4. Data preparation    : Smoothed data is a better input for ML algorithms.




library(zoo)
smoothed <- rollmean(UKgas, k=4)
plot(UKgas)
lines(smoothed, col='red')
      #rollmean refers to the rolling mean (or moving average), 
      #a commonly used technique in time series analysis for smoothing data. 
      #The rolling mean is calculated by averaging a fixed number of consecutive data points
      #in the series, which helps to reduce noise and highlight underlying trends.
      #here we're using 12 data points at once


#4. STATIONARITY AND DIFFERENCING
    # For applying arima modelling it is necessary that the data is stationary i.e. 
    # the mean and variances are homogenized



  #4.1 Homogenizing the variance
plot(UKgas)
abline(reg=lm(UKgas~time(UKgas)))
#here we can see that the variance (the distance between the spark line and regression line)
#is constantly changing so we take the log values so as to minimize the variance
y<-log(UKgas)
plot(log(UKgas))
abline(reg = lm(y~time(y)))
  
  #4.2 Homogenizing the mean  
p<-diff(log(UKgas))
plot(diff(log(UKgas)))
adf.test(p)
#this is a stationarity test and it shows that p value is smaller than printed value indicating that we reject 
#the null hypothesis of the data being non stationary.
#hence we can say that the data is stationary.


#Here we will use ARIMA manually and then compare our findings with auto ARIMA ones.

library(tseries)
acf(UKgas)
pacf(UKgas)
acf(diff(log(UKgas)))#acf-auto correlation factor
#this determines the value of q
#q=0
pacf(diff(log(UKgas)))#pacf-partial auto correlation factor
#this determines the value of p
#p=0,d=1,d is the number of times we have to differentiate so as to homogenize the mean
# arima(0,1,0) is the best fit
#c(pdq)=c(0,1,0)


#5. Now fitting the arima model 



fit=arima(log(UKgas),c(0,1,0),seasonal = list(order=c(0,1,0),period=4))
fit
pred=predict(fit,n.ahead = 10*4)
pred
pred1=round(2.718^pred$pred,0) #since the predicted data is actually the logarithmic value of the actual data 
#hence this extra step
pred1
#here we've got the predicted values for the next 10 years
ts.plot(UKgas,pred1,log="y",lty=c(1,3)) #using tsplot function to plot both the available data and future data
data1=round(head(pred1,10),0)
data2=round(tail(UKgas,12),0)
plot(data1,col="red",type="l")
plot(data2,col="blue",type="l")
#data 1 is the first 10 values of predicted data and data 2 is the last 12 values of the available data
#plotting them shows us the consistency of the prediction of the data




