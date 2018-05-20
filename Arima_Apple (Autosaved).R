# R PROGRAM TO BUILD ARIMA MODEL FOR FORECAST STOCK RETURN
# CREATED BY : KUMARI ANJALI, PG - Quantitative Finance  2017 -18, NISM

# Loading the relevant R package for time series analysis
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

# Pulling data from Yahoo finance 
AAPL = getSymbols('AAPL', from='2017-04-01', to='2018-01-01',auto.assign = FALSE)
AAPL = na.omit(AAPL)

# Selecting the relevant close price from the series pulled by Yahoo in above code.
stock_prices = AAPL[,4]

# Compute the logarithmic returns of the stock 
# as we want the ARIMA model to forecast the log returns and not the stock price. 
# Compute the log returns for the stock

stock = diff(log(stock_prices),lag=1)
stock = stock[!is.na(stock)]

# Plot the log return series using the plot function. "LOG RETURN PLOT".

plot(stock,type='l', main='log returns plot')


# We now call the ADF test on the returns series data to check for stationarity. 
# The p-value of 0.01 from the ADF test tells us that the series is stationary. 
# If the series were to be non-stationary, we would have first differenced the returns series 
# to make it stationary.

# Conduct ADF test on log returns series

print(adf.test(stock))

# In the next step, we fixed a breakpoint which will be used to split the returns dataset 
# in two parts further down the code.
# Split the dataset in two parts - training and testing

breakpoint = floor(nrow(stock)*(2.9/3))

# We truncate the original returns series till the breakpoint, 
# Call the ACF and PACF functions on this truncated series.

# Apply the ACF and PACF functions
par(mfrow = c(1,1))
acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)

#  Observe above plots to arrive at the Autoregressive (AR) order and Moving Average (MA) order.

#  For AR models, the ACF will dampen exponentially 
#  Hence the PACF plot will be used to identify the order (p) of the AR model. 

#  For MA models, the PACF will dampen exponentially 
#  Hence the ACF plot will be used to identify the order (q) of the MA model. 

#  From these plots, let us select AR order = 2 and MA order = 2. 
#  Thus, our ARIMA parameters will be (2,0,2).

#  Our objective is to forecast the entire returns series from breakpoint onwards. 

#  We will make use of the For Loop statement in R 
#  within this loop we will forecast returns for each data point from the test dataset.

#  In the code given below, we first initialize a series which will store the actual returns 
#  and another series to store the forecasted returns. 
#  In the For Loop, we first form the training dataset and the test dataset based on the dynamic breakpoint.

#  We call the arima function on the training dataset for which the order specified is (2, 0, 2). #  We use this fitted model to forecast the next data point by using the forecast function. 
#  The function is set at 99% confidence level. 
#  One can use the confidence level argument to enhance the model. 
#  We will be using the forecasted point estimate from the model. 
#  The ???h??? argument in the forecast function indicates the number of values that we want to forecast, in this case, the next day returns.

#  We can use the summary function to confirm the results of the ARIMA model are within acceptable limits. 
#  In the last part, we append every forecasted return and the actual return to the forecasted returns series and the actual returns series respectively.

# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2014-11-25","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())

for (b in breakpoint:(nrow(stock)-1)) {
  
  stock_train = stock[1:b, ]
  stock_test = stock[(b+1):nrow(stock), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit = arima(stock_train, order = c(2, 0, 2),include.mean=FALSE)
  summary(fit)
  
  # plotting a acf plot of the residuals
  acf(fit$residuals,main="Residuals plot")
  
  # Forecasting the log returns
  arima.forecast = forecast(fit, h = 1,level=99)
  summary(arima.forecast)
  
  # plotting the forecast
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  # Creating a series of actual returns for the forecasted period
  Actual_return = stock[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  print(stock_prices[(b+1),])
  print(stock_prices[(b+2),])
  
}



# Checking the accuracy of the ARIMA model by comparing the forecasted returns versus the actual returns. 
#  The last part of the code computes this accuracy information.

# Adjust the length of the Actual return series
Actual_series = Actual_series[-1]

# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series, index(Actual_series))

# Create a plot of the two return series - Actual versus Forecasted
plot(Actual_series, type='l', main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series, lwd=1.5, col='red')
legend('bottomright', c("Actual", "Forecasted"), lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

# Create a table for the accuracy of the forecast
comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)


### THE END OF PROGRAM  ####
