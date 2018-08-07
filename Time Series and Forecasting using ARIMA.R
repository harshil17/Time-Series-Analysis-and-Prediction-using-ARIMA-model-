#library Installation
library(gtrendsR)
library(forecast)
library(tseries)
library(ggplot2)

#Extract data via gtrendR libray 
suicidedata <- gtrends("suicide - squad", geo = "US", time = "2017-01-15 2017-04-18" )
suicidedata <- suicidedata$interest_over_time
suicidedata

#Split data into train and test to evaluate time series 
s_train <- suicidedata[1:75, 1:2]
s_train
s_test <- suicidedata[76:94, 1:2]
s_test
test_s <- s_test

#Time series of train and test data 
train_ts <- ts(suicidedata$hits[1:75], frequency = 12, start = c(12,1))
train_ts
test_ts <- ts(suicidedata$hits[76:94], frequency = 12, start = c(12,1))
test_ts

# Visualize train and test data by using ggplot
ggplot(s_train, aes(x=date, y=hits), col = "red")+
  geom_line()
ggplot(s_test, aes(x=date, y=hits), col = "blue")+
  geom_line()

#Decomposition of train data by using stl and acf and pacf
components <- stl(train_ts, s.window = 12)
components
plot(components, col = "blue")
acf(train_ts, lag.max = 50)
pacf(train_ts, lag.max = 50)

#Creating Arima model for train data 
model_arima <- arima(train_ts, order = c(1,0,2))
model_arima

# Made a Prediction using Arima Model 
prediction <- predict(model_arima, n.ahead = 19)
prediction
for (i in prediction[1]){
  s_test[2]<-i
}
s_test

# Visualize predicted value 
ggplot(s_train, aes(x=date, y=hits))+
  geom_line()+
  geom_line(data = s_test, aes(x=date, y=hits), col ="green")+
  geom_line(data= test_s, aes(x=date, y=hits), col = "red")

# Measuring the average difference between forcaseted value and real value  
for (i in prediction[1]){
  prediction<-as.integer(i)
}
prediction
avg_test <-mean(test_ts)
avg_test
avg_pred <- mean(prediction)
avg_pred
avg_diff<-((mean_test-mean_pred)/mean_pred)*100
avg_diff
Apr_18<-((test_ts[19]-pred[19])/mean_pred)*100
Apr_18
auto.arima(train_ts, trace = 1)

