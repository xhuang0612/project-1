library(nnet)
library(MASS)
day_data<-read.csv(file="day.csv")
day_data <- day_data[,c(-1,-2)]
class(day_data)
dim(day_data)
names(day_data)
summary(day_data)
day_data$season<- factor(day_data$season, levels=sort(unique(day_data$season)))
day_data$mnth<- factor(day_data$mnth, levels=sort(unique(day_data$mnth)))
day_data$weekday<- factor(day_data$weekday, levels=sort(unique(day_data$weekday)))



### model 1
#workingday is redundant, as it is included in weekday. 
model.fit1<-lm(cnt ~ mnth+season + yr + holiday
               + weekday + weathersit + atemp + temp + hum + windspeed + registered,data=day_data)
summary(model.fit1)
plot(model.fit1)
#run backwards subset selection
m1.bw<-stepAIC(model.fit1,direction="backward")
summary(m1.bw)
#plot(m1.bw)
#run backwards subset selection
m1.fw<-stepAIC(model.fit1,direction="forward")
summary(m1.fw)
#plot(m1.fw)
#run backwards subset selection
m1.both<-stepAIC(model.fit1,direction="both")
summary(m1.both)
#plot(m1.both)

### split data into training, & testing by 7:3
set.seed(1)
training <- sample(nrow(day_data),nrow(day_data)*0.7)
test.data <- day_data[-training,]

### fit linear model
ytest.predict <- predict(model.fit1, test.data)
mean((test.data$cnt-ytest.predict)^2)

ytest.predict <- predict(m1.bw, test.data)
mean((test.data$cnt-ytest.predict)^2)

ytest.predict <- predict(m1.fw, test.data)
mean((test.data$cnt-ytest.predict)^2)

ytest.predict <- predict(m1.both, test.data)
mean((test.data$cnt-ytest.predict)^2)

