### In this practice, we will look at time series and ARIMA.

### The data we have comes from Kaggle, and is daily climate data of Delhi.

### Load in data
df <- read.csv('DailyDelhiClimate.csv', header = TRUE)
View(df)

### Check for NA values
sum(is.na(df)) # Ther are no NA values.

### Look at structure
str(df)

### We can see that the date variable is not formatted as a date.
### Fix this.

df$date <- as.Date(df$date)
head(df)
str(df)


### We are interested in the mean temperature data.
temp <- df[, c('date', 'meantemp')]
head(temp)


### Data Summary
summary(temp)

### We see that the data starts on 01/01/2013 and ends 01/01/2017

### Plot the data
plot(temp$date, temp$meantemp,
     main = 'Mean Temperature in Delhi',
     xlab = 'Date', ylab = 'Mean Temperature (C)',
     type = 'l')


### From this graph, we can see that there is seasonality to the temperatures

### Let's make some year and month variables

year <- format(temp$date, '%Y')
month <- format(temp$date, '%m')

### Add these to data
temp <- cbind(temp, year, month)

head(temp)
str(temp)

### Let's look at box plots for each month
boxplot(temp$meantemp ~ temp$month,
        main = 'Temperatures by Month',
        xlab = 'Month', ylab = 'Mean Temperature (C)')


### Let's add some moving averages to the original data.
library(zoo)

weekRA <- rollmean(temp$meantemp, k = 7, fill = NA, align = 'right') ### 7-day (week) average
monthRA <- rollmeanr(temp$meantemp, k = 30, fill = NA) ### 30-day (month) average
yearRA <- rollmeanr(temp$meantemp, k = 365, fill = NA) ### 365-day (year) average


### Add to data
temp <- cbind(temp, weekRA, monthRA, yearRA)
View(temp)



### Visualize rolling averages
plot(temp$date, temp$meantemp,
     main = 'Mean Temperature in Delhi',
     xlab = 'Date', ylab = 'Mean Temperature (C)',
     type = 'l', lwd = 3)
points(temp$date, temp$weekRA, type = 'l', col = 'red',
       lwd = 2)
points(temp$date, temp$monthRA, type = 'l', col = 'blue',
       lwd = 2)
points(temp$date, temp$yearRA, type = 'l', col = 'green',
       lwd = 2)
legend('bottomright', legend = c('Original Data', '7-day Rolling Average',
                                '30-day Rolling Average', '365-day Rolling Average'),
       col = c('black', 'red', 'blue', 'green'),
       lwd = c(3,2,2,2),
       horiz = TRUE,
       cex = 0.68)


### From the plot, we can see that there is seasonality.
### We can see from the year rolling average, the data looks nearly stationary.

### Lets run a KPSS test to be sure
library(tseries)
kpss.test(temp$meantemp)

### The null hypothesis is that the data is stationary. 
### At a 95% CL, the mean is not stationary.

### Also check the Phillip-Perron Test
library(aTSA)
pp.test(temp$meantemp)

### The alternative hypothesis is that the data is stationary.
### At a 95% CL level, the data is stationay.

### There is one more thing we could check. An ACF plot
acf(temp$meantemp)
pacf(temp$meantemp)

### Based on these plots. The data is not stationary.

### To get the data to be stationary, we can try differencing it

tempDiff <- diff(temp$meantemp) # first order difference

plot(tempDiff, ylab = 'Temperature Diff', main = 'First Order Difference',
     type = 'l')

### That looks much better. Let's run the tests again.
tseries::kpss.test(tempDiff)

## Our p-value is greater than 0.05. The data is stationary

pp.test(tempDiff)

### Our p-value is less than 0.05. The data is stationary.

acf(tempDiff)
pacf(tempDiff)

### This looks much better.


### Now let's investigate for any AR processes.
acf(tempDiff)

### The first lag looks to indicate an AR(1) process.



################################### MODELING ################################

### Split the data, leaving the last 30 day as testing
Train <- temp[1:(nrow(temp) - 30), c('date', 'meantemp')]
Test <- temp[(nrow(temp) - 29):nrow(temp), c('date', 'meantemp')]


### From our investigation, let's try fitting an ARIMA(1,1,0) model
model1 <- arima(Train$meantemp, order = c(1,1,0))
library(lmtest)
coeftest(model1)

confint(model1)

### Based on the model results, the AR(1) process is significant.


### We need to make sure that the residuals look like white noise.
acf(model1$residuals)

### From the acf plot of the residuals, the residuals are white noise.

### Perform a Ljung-Box test to be sure

library(FitAR)
boxresults <- LjungBoxTest(model1$residuals, k = 1)
plot(boxresults[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")

### All the p-values are well below 0.05. This means that there is not white noise in teh residuals.

### This means that model assumptions are violated






### Let's try using the built in auto selection model
library(forecast)
model2 <- auto.arima(Train$meantemp, trace = TRUE)

### The best fit model was a ARIMA(2,1,2)
coeftest(model2)

### All variables are significant.
acf(model2$residuals)
boxresults <- LjungBoxTest(model2$residuals, k = 4)
boxresults
plot(boxresults[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")

### Many of the p-values are above 0.05, so we can be sure that we are not violating model assumptions.

### Look at the distbution of the residuals
qqnorm(model2$residuals)
qqline(model2$residuals)

### Based on the plot, the residuals are approx. normal
shapiro.test(model2$residuals)

### However, this test says the residuals are not normally distributed.



######################################## FORECASTING ########################################

### Now forecast out
preds <- forecast(model2, h = 30, level = 0.95)
plot(preds)


### Plot
plot(temp$date, temp$meantemp,
     main = 'ARIMA(2,1,2) on Data',
     xlab = 'Date',
     ylab = 'Mean Temperature (C)',
     type = 'l',
     lwd = 3)
points(Train$date, model2$fitted,
       type = 'l',
       col = 'red',
       lwd = 1)
points(Test$date, preds$mean,
       type = 'l',
       col = 'green',
       lwd = 1)
abline(v = Test$date[1], col = 'blue', lwd = 3)
legend('bottomright', legend = c('Original Data', 'ARIMA Fit', 'Forecast'),
       col = c('black', 'red', 'green'),
       lwd = c(3,1,1),
       horiz = TRUE,
       cex = 0.68)




plot(temp$date, temp$meantemp,
     main = 'ARIMA(2,1,2) on Data',
     xlab = 'Date',
     ylab = 'Mean Temperature (C)',
     type = 'l',
     lwd = 3,
     xlim = c(temp$date[nrow(temp) - 40], temp$date[nrow(temp)]))
points(Train$date, model2$fitted,
       type = 'l',
       col = 'red',
       lwd = 1)
points(Test$date, preds$mean,
       type = 'l',
       col = 'green',
       lwd = 1)
abline(v = Test$date[1], col = 'blue', lwd = 3)
legend('bottomright', legend = c('Original Data', 'ARIMA Fit', 'Forecast'),
       col = c('black', 'red', 'green'),
       lwd = c(3,1,1),
       horiz = TRUE,
       cex = 0.68)



### Calculate MSE

trainMSE = mean((model2$fitted - Train$meantemp)^2)
trainMSE

testMSE = mean((preds$mean - Test$meantemp)^2)
testMSE









