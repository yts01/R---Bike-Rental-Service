rm(list = ls())
setwd("C:/Users/ravhon/Desktop/big data")
list.files()
mydata<-read.csv("bike_train (1).csv",header = T)
bike_test<-read.csv("bike_test.csv",header = T)
library(ggplot2)

###  A  ###

#delete observation without value
mydata<-na.omit(mydata)

mydata$is_na<-is.na(mydata$datetime)
mydata<-mydata[!mydata$is_na,]
mydata$is_na<-NULL
#set the date 

mydata$datetime<-as.POSIXct(mydata$datetime,format= "%Y-%m-%d%H:%M:%S")

#set the categorial variable as factor and give them labels
#count
summary(mydata$count)
quantile(mydata$count,probs=seq(0,0.1,0.01))
ggplot(mydata)+aes(count)+
  geom_histogram(color="black",fill="darkred",binwidth = 10)+
  labs(title = "count Distribution")


ggplot(mydata)+aes(count)+
  geom_histogram(color="black",fill="darkred",binwidth = 10)+
  labs(title = "count Distribution")


mydata$season<-as.factor(mydata$season)
mydata$weather<-factor(mydata$season, labels=c("winter","spring","summer","fall"))
mydata$holiday<-as.factor(mydata$holiday)
mydata$workingday<-as.factor(mydata$workingday)
mydata$weather<-as.factor(mydata$weather)
mydata$weather<-factor(mydata$weather, labels=c("good","normal","bed","very_bed"))
mydata$season <- factor(mydata$season,
                        levels = c(1,2,3,4),
                        labels = c("winter", "spring", "summer","fall"))

####DESCRIPTION STATISTICS of numeric variable


#temp
summary(mydata$temp)
quantile(mydata$temp,probs=seq(0,1,0.1))# we can see big jump between the 90% quatile to 100% quantile and the same between 10%to 0%
ggplot(mydata)+aes(temp)+
  geom_histogram(color="black",fill="darkred",binwidth = 1)+
  labs(title = "Temp Distribution")+
  facet_wrap(~season)


#atemp
summary(mydata$atemp)
quantile(mydata$atemp,probs=seq(0,1,0.1))# we can see big jump between the 90% quatile to 100% quantile and the same between 10%to 0%
ggplot(mydata)+aes(atemp)+
  geom_histogram(color="black",fill="darkred",binwidth = 1)+
  labs(title = "atemp Distribution")+
  facet_wrap(~season)


#humidity
summary(mydata$humidity)
quantile(mydata$humidity,probs=seq(0,1,0.1))# we can see big jump between the 90% quatile to 100% quantile and the same between 10%to 0%
ggplot(mydata)+aes(humidity)+
  geom_histogram(color="black",fill="darkred",binwidth = 1)+
  labs(title = "humidity Distribution")+
  facet_wrap(~season)


#windspeed
summary(mydata$windspeed)
quantile(mydata$humidity,probs=seq(0,1,0.1))
ggplot(mydata)+aes(windspeed,count)+
  geom_point(color="black",fill="darkred")+
  labs(title = "windspeed Distribution")
#we can see alot of observation with value 0 maybe because its a defulte

###Visualize seasonality of hours in the day and months

library(lubridate)

mydata$hour<-hour(mydata$datetime)
mydata$month<-month(mydata$datetime,label = T)
mydata$wdays<-wday(mydata$datetime,label = T)
mydata<-na.omit(mydata)

# plot of hour and count
ggplot(mydata)+aes(x=hour,y=count)+
  geom_point(color="darkred",size=1.5,na.rm = F)+
  scale_x_continuous(0:23,0:23)+
  labs(title = "seasonalty in months")+
    facet_wrap(mydata$month)


ggplot(mydata)+aes(x=wdays,y=count)+
  geom_point(color="darkred")+
  labs(title = "seasonalty by days")+
    facet_wrap(~month)
#weekend days are less demend

ggplot(mydata)+aes(x=month,y=count)+
  geom_point(color="darkblue")+
  labs(title = "seasonalty by month")

########################################   B   #####################################3###
#Preliminary Regressions


###   1   ###

model1<-lm(count~temp,data = mydata)
summary(model1)


###   2   ###
ggplot(mydata)+aes(x=temp,y=count)+
  geom_point(color="red")+
  geom_smooth(method = "lm")+
  labs(title = "Preliminary Regressions")


###   3   ###
set.seed(1234)   #making the sampeling be the same every time we run it

train <- sample(1:nrow(mydata),size =0.7*nrow(mydata),replace = F )   # sampales 70% of the numbers from 1 till the number of rows in hosuing
bike_train <- mydata[train,]   # creats a new dataset only with the rows sampaled
bike_validation <- mydata[-train,]   # creats a new dataset only with the rows that were not sampaled

#### winzorizing on count variable  #####
quant99<-quantile(bike_train$count,probs = 0.99)
bike_train_clean<-bike_train
bike_train_clean<-bike_train_clean[bike_train$count<quant99,]  # delete all the observation above the 99%
bike_train<-bike_train_clean

###winzorizing on windspeed variable###
quant1_wind<-quantile(bike_train$windspeed,probs = 0.01)
bike_train_clean<-bike_train_clean[bike_train$windspeed>quant1_wind,]
bike_train<-bike_train_clean

###   4   ###
bike_train$hour<-as.numeric(bike_train$hour)   #set hour as factor variable
model2<-lm(count~temp+hour,data = bike_train)   #run regression with count as dependent value and temp, hour as independent
summary(model2)


###   5   ###

bike_train$hour<-as.factor(bike_train$hour)    #set hour as factor variable
model2<-lm(count~temp+hour,data = bike_train)    #run regression with count as dependent value and temp, hour as independent
summary(model2)



###   6   ###
bike_train$daytime<-NA
bike_train$daytime <- ifelse(as.numeric(bike_train$hour) >= 7 & as.numeric(bike_train$hour) <= 9,"morning",
                           ifelse(as.numeric(bike_train$hour) >= 10 & as.numeric(bike_train$hour) <= 12,"branch",
                            ifelse(as.numeric(bike_train$hour) >= 13 & as.numeric(bike_train$hour) <= 17,"noon",
                             ifelse(as.numeric(bike_train$hour) >= 18 & as.numeric(bike_train$hour) <= 19,"afternoon",
                                    ifelse(as.numeric(bike_train$hour) >= 20 & as.numeric(bike_train$hour) <= 24,"evening",
                                           ifelse(as.numeric(bike_train$hour) >= 1 & as.numeric(bike_train$hour) <= 6,"latenight",bike_train$datetime))))))
bike_validation$hour<-as.factor(bike_validation$hour)#set hour as factor variable

bike_validation$daytime<-NA
bike_validation$daytime <- ifelse(as.numeric(bike_validation$hour) >= 7 & as.numeric(bike_validation$hour) <= 9,"morning",
                              ifelse(as.numeric(bike_validation$hour) >= 10 & as.numeric(bike_validation$hour) <= 13,"branch",
                                ifelse(as.numeric(bike_validation$hour) >= 13 & as.numeric(bike_validation$hour) <= 17,"noon",      
                                 ifelse(as.numeric(bike_validation$hour) >= 18 & as.numeric(bike_validation$hour) <= 19,"afternoon",
                                    ifelse(as.numeric(bike_validation$hour) >= 20 & as.numeric(bike_validation$hour) <= 24,"evening",
                                           ifelse(as.numeric(bike_validation$hour) >= 1 & as.numeric(bike_validation$hour) <= 6,"latenight",bike_train$datetime))))))


###   7   ###
model3<-lm(count~temp+daytime,data = bike_train)
summary(model3)


###   8   ###
model4<-lm(count~temp+daytime+temp:daytime,data = bike_train)
summary(model4)


###   10   ###
bike_validation<-na.omit(bike_validation)   # delete observation with NA 

bike_validation$pred1<-predict(model1,newdata = bike_validation)   #insert the prediction to the dataset
pred_model1<-sum((bike_validation$pred1-bike_validation$count)^2)

model2<-lm(count~temp+hour,data = bike_train)   #run regression with count as dependent value and temp, hour as independent
summary(model2)

bike_validation$pred2<-predict(model2,newdata = bike_validation)
pred_model2<-sum((bike_validation$pred2-bike_validation$count)^2)

bike_validation$pred3<-predict(model3,newdata = bike_validation)
pred_model3<-sum((bike_validation$pred3-bike_validation$count)^2)

bike_validation$pred4<-predict(model4,newdata = bike_validation)   #insert the prediction to the dataset
pred_model4<-sum((bike_validation$pred4-bike_validation$count)^2)

predict(model1,newdata = data.frame(temp=1))#prediction of certian number



############################################## part C   #######################################
########## final model #########
bike_train$weather<-as.numeric(bike_train$weather)
bike_validation$weather<-as.numeric(bike_validation$weather)

model5 <- lm(weather~+atemp+humidity+windspeed,data = bike_train)
summary(model5)


bike_train$atemp_scale<-model5$coefficients[2]*bike_train$atemp
bike_train$humidity_scale<-model5$coefficients[3]*bike_train$humidity
bike_train$windspeed_scale<-model5$coefficients[4]*bike_train$windspeed

bike_train$agg_climet<-bike_train$atemp_scale+bike_train$humidity_scale+bike_train$windspeed_scale

bike_validation$atemp_scale<-model5$coefficients[2]*bike_validation$atemp
bike_validation$humidity_scale<-model5$coefficients[3]*bike_validation$humidity
bike_validation$windspeed_scale<-model5$coefficients[4]*bike_validation$windspeed

bike_validation$agg_climet<-bike_validation$atemp_scale+bike_validation$humidity_scale+bike_validation$windspeed_scale+

model6<-lm(count~agg_climet+hour+agg_climet:hour,data = bike_train)
summary(model6)

bike_validation$pred6<-predict(model6,newdata = bike_validation)   #insert the prediction to the dataset
bike_validation$pred6[bike_validation$pred6<0]<-0
pred_model6<-sum((bike_validation$pred6-bike_validation$count)^2)

ggplot(bike_validation)+aes(pred6)+
  geom_histogram(color="black",fill="darkred",binwidth = 10)+
  labs(title = "count Distribution")

#############################      predection on bike test        ###################

bike_test$hour<-hour(bike_test$datetime)
bike_test$hour<-as.factor(bike_test$hour)
bike_test$atemp_scale<-model5$coefficients[2]*bike_test$atemp
bike_test$humidity_scale<-model5$coefficients[3]*bike_test$humidity
bike_test$windspeed_scale<-model5$coefficients[4]*bike_test$windspeed

bike_test$agg_climet<-bike_test$atemp_scale+bike_test$humidity_scale+bike_test$windspeed_scale

bike_test$pred<-predict(model6,newdata = bike_test)    #insert the prediction to the bike test
bike_test$pred[bike_test$pred<0]<-0       #all the negative value get 0
write.csv(bike_test, file = "C:/Users/ravhon/Desktop/big data/bike_test.csv")

predict(model6,data.frame(agg_climet=2,hour="2"))      #prediction of certian number
predict(model6,data.frame(agg_climet=2,hour="8"))      #prediction of certian number

