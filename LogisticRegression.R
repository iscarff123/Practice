### In this logistic regression practice, we will be looking at the titanic dataset.

df <- read.csv('TitanicSurvival.csv')
View(df)


### Let's look for any missing values

df[!complete.cases(df), ]

### We can see we have missing values for age

nrow(df[!complete.cases(df), ]) / nrow(df)

### This is 20% of the data. For this exercise, let's just remove them
### Note, we could try to find the actual ages, or try imputation.

df <- df[complete.cases(df), ]

df

### Let's remove the name variable
df <- df[-1]

df


######################################### DATA EXPLORATION #########################################

### Get data summary

summary(df)

### We will need to convert the character variables into factors.
df$survived <- as.factor(df$survived)
df$sex <- as.factor(df$sex)
df$passengerClass <- as.factor(df$passengerClass)

summary(df)

plot(df)

### Let's look at age based on the different factors

par(mfrow = c(1,3))

boxplot(df$age ~ df$survived,
        main = 'Age & Survival',
        xlab = 'Survived?', ylab = 'Age')

boxplot(df$age ~ df$sex,
        main = 'Age & Sex',
        xlab = 'Sex', ylab = 'Age')

boxplot(df$age ~ df$passengerClass,
        main = 'Age & Class',
        xlab = 'Class', ylab = 'Age')

### From the boxplots, we can see that survival and sex have similar distributions across age,
### and similar means.

### However, we can see that there is difference in age passenger class.

### Let's throw in survival color points into the sex and class.

library(ggplot2)
library(tidyverse)
library(gridExtra)


p1 <- df %>% ggplot(aes(x = sex, y = age)) + 
  geom_boxplot() + 
  geom_jitter(aes(x = sex, y =age, color = survived))

p2 <- df %>% ggplot(aes(x = passengerClass, y = age)) + 
  geom_boxplot() + 
  geom_jitter(aes(x = passengerClass, y =age, color = survived))

grid.arrange(p1, p2, ncol = 2)

### From these plots, we can see that more men did not survive.
### More people in 3rd class did not survive than other classes.
### A large concentration of deaths occur above the age of 18






####################################### MODELING ################################

### Because this is more of an examination, we will not use a validation set.

TrainTestSplit <- sample(c(1:nrow(df)), size = nrow(df)*0.8)

Training <- df[TrainTestSplit, ]
Test <- df[-TrainTestSplit, ]


### Fit logistic regression model
model1 <- glm(survived ~ ., data = Training, family = 'binomial')

summary(model1)


### Prediction
preds <- predict(model1, newdata = Test, interval = 'prediction', type = 'response')

preds <- ifelse(preds >= 0.5, 'yes', 'no')

conf <- table(preds, Test$survived)

Accuracy <- (conf[1,1] + conf[2,2]) / sum(conf)
Accuracy

### We have a model accuracy of 0.82

### Let's look at ROC curve

library(pROC)
ROC_obj <- roc(ifelse(Test$survived == 'yes', 1, 0), ifelse(preds == 'yes', 1,0),
               smoothed = TRUE,
               ci = TRUE, ci.alpha = 0.05, strtified = FALSE,
               plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
               print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(ROC_obj)
plot(sens.ci, type="shape", col="lightblue")

plot(sens.ci, type="bars")























