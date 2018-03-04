library(sqldf)

setwd("C:/Users/new/Desktop/Data Bike Sharing/Input Data")

# We have read traindata and testdata mydata 
traindata=read.csv("train_bike.csv")
testdata=read.csv("test_bike.csv")
str(traindata)

#Initialised additional variables in testdata mydata
testdata$registered=0
testdata$casual=0
testdata$count=0

#Combined testdata and traindata mydata into mydata
mydata=rbind(traindata,testdata)
summary(mydata)

#foctorized season, weather, holiday  and workingday variables of mydata
mydata$season=as.factor(mydata$season)
mydata$weather=as.factor(mydata$weather)
mydata$holiday=as.factor(mydata$holiday)
mydata$workingday=as.factor(mydata$workingday)

#Made new variable hour in mydata
mydata$hour=substr(mydata$datetime,12,13)
mydata$hour=as.factor(mydata$hour)

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Visualised to check use of casual and registed users
boxplot(traindata$count~traindata$hour,xlab="hour", ylab="count of users")
boxplot(traindata$casual~traindata$hour,xlab="hour", ylab="casual users")
boxplot(traindata$registered~traindata$hour,xlab="hour", ylab="registered users")

#taken date as new variable and encorporated into mydata
date=substr(mydata$datetime,1,10)
days<-weekdays(as.Date(date))
mydata$day=days

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Visualised to check use of casual and registed users with different variables

#day
boxplot(traindata$registered~traindata$day,xlab="day", ylab="registered users")
boxplot(traindata$casual~traindata$day,xlab="day", ylab="casual users")

#weather
boxplot(traindata$registered~traindata$weather,xlab="weather", ylab="registered users")
boxplot(traindata$casual~traindata$weather,xlab="weather", ylab="casual users")

#temp
boxplot(traindata$registered~traindata$temp,xlab="temp", ylab="registered users")
boxplot(traindata$casual~traindata$temp,xlab="temp", ylab="casual users")

#created new variable year
mydata$year=substr(mydata$datetime,1,4)
mydata$year=as.factor(mydata$year)

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Visualised to check use of casual and registed users
#year
boxplot(traindata$registered~traindata$year,xlab="year", ylab="registered users")
boxplot(traindata$casual~traindata$year,xlab="year", ylab="casual users")

#windspeed
boxplot(traindata$registered~traindata$windspeed,xlab="year", ylab="registered users")
boxplot(traindata$casual~traindata$windspeed,xlab="year", ylab="casual users")

#humidity
boxplot(traindata$registered~traindata$humidity,xlab="humidity", ylab="registered users")
boxplot(traindata$casual~traindata$humidity,xlab="humidity", ylab="casual users")
