#Roger Hayden III
#Kaggle - Heart Failure Data set
#12/15/21

install.packages("tidyverse")
library(tidyverse)

mydata <- read.csv(file.choose(), header=TRUE)

--------------------------------------------------------------------------------
 
summary(mydata$HeartDisease)
summary(mydata$Cholesterol)
   
--------------------------------------------------------------------------------

ggplot(data=mydata, aes(x=Age, y=RestingBP))+geom_point()
#Plotting Age vs.BP

ggplot(data=mydata, aes(x=Age, y=RestingBP))+geom_point() + geom_smooth()
#Plotting Age vs. BP with a "smooth" line added 

ggplot(data=mydata, aes(x=Age, y=RestingBP))+geom_point() + geom_quantile()
#Plotting Age vs. BP with the geom_quantile added - showing 3 regression lines

hist(x=mydata$RestingBP)
#histogram of RestingBP

--------------------------------------------------------------------------------

plot(mydata$Cholesterol,mydata$HeartDisease)
plot(mydata$HeartDisease,mydata$Cholesterol)

#Attempting to find some correlation between Cholesterol and Heart Disease

model <- lm(mydata$HeartDisease~mydata$Cholesterol)
summary(model)

pred.int <- predict(model, interval = "prediction")
mydata <- cbind(mydata, pred.int)

p <- ggplot(mydata, aes(HeartDisease, Cholesterol)) +
  geom_point() +
  stat_smooth(method = lm)

#Here adding the upper and lower bounds does not work correctly the way the data
#is set up 
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

--------------------------------------------------------------------------------

multiple <- lm(mydata$Age~mydata$RestingBP + mydata$Cholesterol)
summary(multiple)
#Y value is Age and X value is RestingBP and Cholesterol
layout(matrix(c(1,2,3,4),2,2))
#Provides 4 graphs  
#Residuals vs Fitted, Scale-Location, Normal Q-Q and Residuals vs. Leverage
plot(multiple)

# Calculate Relative Importance for Each Predictor
install.packages("relaimpo")
library(relaimpo)

#Trying a bootstrap confidence interval
calc.relimp(multiple,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(multiple, b = 1000, type = c("lmg",
                                            "last", "first", "pratt"), 
                    rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result

--------------------------------------------------------------------------------

#Attempting to do a prediction model
model <- lm(mydata$RestingBP~mydata$Cholesterol)
summary(model)
plot(mydata$RestingBP,mydata$Cholesterol)
 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(mydata, pred.int)

p <- ggplot(mydata, aes(RestingBP, Cholesterol)) +
  geom_point() +
  stat_smooth(method = lm)

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
#Prediction model does not appear to really show anything good. Majority of the
#data is outside both the upper and lower bounds