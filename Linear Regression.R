### In this practice of linear regression, we will examine and model house prices in the city of Windsor, Canada.
###Data is in the AER Package

library(AER)

### Bring in data

data("HousePrices")
View(HousePrices)

### We have 546 observations of the following information:

# price
#   Sale price of a house.
# 
# lotsize
#   Lot size of a property in square feet.
# 
# bedrooms
#   Number of bedrooms.
# 
# bathrooms
#   Number of full bathrooms.
# 
# stories
#   Number of stories excluding basement.
# 
# driveway
#   Factor. Does the house have a driveway?
#   
# recreation
#   Factor. Does the house have a recreational room?
#   
# fullbase
#   Factor. Does the house have a full finished basement?
#   
# gasheat
#   Factor. Does the house use gas for hot water heating?
#   
# aircon
#   Factor. Is there central air conditioning?
#   
# garage
#   Number of garage places.
# 
# prefer
#   Factor. Is the house located in the preferred neighborhood of the city?



### Let's first check for any missing values
HousePrices[!complete.cases(HousePrices),]

### There are no missing values



####################################### DATA EXPLORATION #################################

### Let's summarize the data
summary(HousePrices)

### House price vs lot size
plot(HousePrices$lotsize, HousePrices$price, 
     xlab = 'Lot Size', ylab = 'Price',
     main = 'House Price vs. Lot Size')

### From this plot, we can see that there are many potential outliers that would effect
### the linear regression model. We can use Box-Cox to potential address this.

hist(HousePrices$price)

hist(HousePrices$lotsize)



par(mfrow = c(2,5))
for(i in 3:length(colnames(HousePrices))){
  boxplot(HousePrices$price ~ HousePrices[,i],
          main = colnames(HousePrices[i]),
          xlab = colnames(HousePrices[i]), ylab = 'Price')
}

par(mfrow = c(1,1))
boxplot(HousePrices$price ~ HousePrices$garage)

### From these plots, we can see there are major differences in price between variabe groups.


nrow(HousePrices[HousePrices$bathrooms == 4, ])

### We can see that there is only 1 observation where the number of bathrooms is 4.
### This will be removed for the analysis

data <- HousePrices[-(HousePrices$bathrooms == 4), ]



####################################### MODELING ##############################################

### Let's first make an 70/20/10 split of the data.

set.seed(210)

TrainTestSplit <- sample(c(1:nrow(data)), size = nrow(data)*0.9)

TrainVal <- data[TrainTestSplit, ]

Test <- data[-TrainTestSplit, ]

TrainValSplit <- sample(c(1:nrow(TrainVal)), size = nrow(TrainVal)*0.7)

Training <- TrainVal[TrainValSplit, ]

Validation <- TrainVal[-TrainValSplit, ]

nrow(Training) + nrow(Validation) + nrow(Test)




### Let's make a regression model with all the variables
model1 <- lm(price ~ ., data = Training)

summary(model1)

par(mfrow = c(2,2))
plot(model1)

### We have an adjusted R2 of 0.6563. Recreation is the only variable that is not significant.

### Check for multi-coliniarity based on VIF using car package

library(car)
vif(model1)

### There is no multi-colinearity

### Let's try another model, this time with out recreation

model2 <- lm(price ~ ., data = Training[-7])
summary(model2)
plot(model2)

### Not much has changed

### Let's try adding a Box-Cox transformation to the continuous variables

X <- cbind(Training$price, Training$lotsize)
summary(powerTransform(X))

### Based on this, a log transformation of lotsize is recommended.


TrainingTrans <- Training
TrainingTrans$lotsize <- log(TrainingTrans$lotsize)

model3 <- lm(price ~ ., data = TrainingTrans)
summary(model3)

par(mfrow = c(2,2))
plot(model3)


### Remove recreation
model4 <- lm(price ~ ., data = TrainingTrans[-7])
summary(model4)

par(mfrow = c(2,2))
plot(model4)


### Remove driveway
model5 <- lm(price ~ ., data = TrainingTrans[c(-7,-6)])
summary(model5)

par(mfrow = c(2,2))
plot(model5)


### Remove bedrooms
model6 <- lm(price ~ ., data = TrainingTrans[c(-7,-6,-3)])
summary(model6)

par(mfrow = c(2,2))
plot(model6)


################################# VALIDATION ###################################

### Model1: Full data
### Model2: Remove Recreation
### Model3: Log(LotSize)
### Model4: Remove Recreation
### Model5: Remove Driveway
### Model6: Remove bedrooms

### Test models on validation set.
ValidationTrans <- Validation
Validation$lotsize <- log(Validation$lotsize)

val1 <- predict(model1, newdata = Validation, interval = 'prediction')
val2 <- predict(model2, newdata = Validation[-7], interval = 'prediction')
val3 <- predict(model3, newdata = ValidationTrans, interval = 'prediction')
val4 <- predict(model4, newdata = ValidationTrans[-7], interval = 'prediction')
val5 <- predict(model5, newdata = ValidationTrans[c(-7,-6)], interval = 'prediction')
val6 <- predict(model6, newdata = ValidationTrans[c(-7,-6,-3)], interval = 'prediction')

### Validation MSEs

cat('Validation MSE for Model1:', mean((Validation$price - val1)^2))
cat('Validation MSE for Model2:', mean((Validation$price - val2)^2))
cat('Validation MSE for Model3:', mean((ValidationTrans$price - val3)^2))
cat('Validation MSE for Model4:', mean((ValidationTrans$price - val4)^2))
cat('Validation MSE for Model5:', mean((ValidationTrans$price - val5)^2))
cat('Validation MSE for Model6:', mean((ValidationTrans$price - val6)^2))




########################## TESTING ###############################



### Test models.
TestTrans <- Test
TestTrans$lotsize <- log(Test$lotsize)

test1 <- predict(model1, newdata = Test, interval = 'prediction')
test2 <- predict(model2, newdata = Test[-7], interval = 'prediction')
test3 <- predict(model3, newdata = TestTrans, interval = 'prediction')
test4 <- predict(model4, newdata = TestTrans[-7], interval = 'prediction')
test5 <- predict(model5, newdata = TestTrans[c(-7,-6)], interval = 'prediction')
test6 <- predict(model6, newdata = TestTrans[c(-7,-6,-3)], interval = 'prediction')

### Test MSEs

cat('Test MSE for Model1:', mean((Test$price - test1)^2))
cat('Test MSE for Model2:', mean((Test$price - test2)^2))
cat('Test MSE for Model3:', mean((TestTrans$price - test3)^2))
cat('Test MSE for Model4:', mean((TestTrans$price - test4)^2))
cat('Test MSE for Model5:', mean((TestTrans$price - test5)^2))
cat('Test MSE for Model6:', mean((TestTrans$price - test6)^2))










########################################### CENTERING & SCALING DATA #######################################

data2 <- HousePrices[-(HousePrices$bathrooms == 4), ]

price <- data2$price

numVar <- data2[, unlist(lapply(data2, is.numeric))][-1]

catVar <- data2[, unlist(lapply(data2, is.factor))]

scaled <- scale(numVar) ### Center and scale

newData <- cbind(price, scaled, catVar)


par(mfrow = c(1,1))
plot(newData$lotsize, newData$price)



set.seed(210)

TrainTestSplit2 <- sample(c(1:nrow(newData)), size = nrow(newData)*0.9)

TrainVal2 <- newData[TrainTestSplit2, ]

Test2 <- newData[-TrainTestSplit2, ]

TrainValSplit2 <- sample(c(1:nrow(TrainVal2)), size = nrow(TrainVal2)*0.7)

Training2 <- TrainVal2[TrainValSplit2, ]

Validation2 <- TrainVal[-TrainValSplit2, ]

nrow(Training2) + nrow(Validation2) + nrow(Test2)





model1.1 <- lm(price ~ ., data = Training2)
summary(model1.1)

par(mfrow = c(2,2))
plot(model1.1)







?HousePrices












