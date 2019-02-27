rm(list = ls())

setwd('E:/Project/Bike sharing R')

getwd()

#Installing Packages

install.packages(c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", 
                 "Information","MASS", "rpart", "gbm", "ROSE",
                 "dplyr","reshape","data.table",'sampling', 'DataCombine', 'inTrees',"GGally"))
install.packages('ggcorrplot')
# loading libraries
# Install  Require libraries
library("dplyr")
library("ggplot2")
library("data.table")
library('scales')
library('psych')
library('corrgram')
library('ggcorrplot')
library('rpart')
library('randomForest')

#loading dataset

br_day = read.csv('E:/Project/Bike sharing R/bike_rental_day.csv',header = T)

#Checking if any NA are there in data set

sum(is.na(br_day)) #0 NA are available

head(br_day)

summary(br_day)


# #Here we are removing few variables. instant has no information as it is like recor index dteday has no 
# meaning to us here as we are focusing on seasonal setting and not dates of month or of year. yr variable 
#also has no importance here. As we are interested in finding total count i.e. variable  cnt which 
#is our target variable and it is sum of casual and registered so we will remove casual and registerd 
#variable also.

br_day_subset = subset.data.frame(br_day,select = -c(1,2,4,14,15))

head(br_day_subset)

#As we can see here season,mnth, holiday, weekday, workingday and weathersit are categorical and are in 
# in numeric form but we will not convert them to factor now as we need numeric data types here for study
#and later we can place original values to interpret the data.


############# Outlier Analysis #########################

#There are chances of presence of outlier in variable hum and windspeed so we will use boxplot to check 
# presence of an outlier in variables hum and windspeed.

ggplot(data = br_day_subset, aes(x = , y = hum)) + 
  geom_boxplot() + geom_boxplot(outlier.colour = 'red', outlier.size = 3 )+
  ggtitle('Outlier Analysis') +   theme(text = element_text(size = 20))


ggplot(data = br_day_subset, aes(x = "", y = windspeed)) + 
  geom_boxplot() + geom_boxplot(outlier.colour = 'red', outlier.size = 3 )+
  ggtitle('Outlier Analysis') +   theme(text = element_text(size = 20))

## from abve boxplot we can see there are outlier present.

#Removing outliers using boxplot method
df = br_day_subset

#loop to remove outlier from  variables
cnames = c('hum','windspeed')

for(i in cnames){
   print(i)
   val = br_day_subset[,i][br_day_subset[,i] %in% boxplot.stats(br_day_subset[,i])$out]
   #print(length(val))
   br_day_subset = br_day_subset[which(!br_day_subset[,i] %in% val),]
}

####### Box plot after deleting outliers #######

ggplot(data = br_day_subset, aes(x = , y = hum)) + 
  geom_boxplot() + geom_boxplot(outlier.colour = 'red', outlier.size = 3 )+
  ggtitle('Outlier Analysis') +   theme(text = element_text(size = 20))


ggplot(data = br_day_subset, aes(x = "", y = windspeed)) + 
  geom_boxplot() + geom_boxplot(outlier.colour = 'red', outlier.size = 3 )+
  ggtitle('Outlier Analysis') +   theme(text = element_text(size = 20))


##### Loading the data again
br_day_subset = df



# histograms to check the the distrubution of  variables

hist(br_day$season, xlab = 'season', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$mnth, xlab = 'mnth', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$holiday, xlab = 'holiday', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$weekday, xlab = 'weekday', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$workingday, xlab = 'workingday', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$weathersit, xlab = 'weathersit', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$temp, xlab = 'temp', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$atemp, xlab = 'atemp', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$hum, xlab = 'hum', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$windspeed, xlab = 'windspeed', ylab = 'cnt', col = 'violet', main = 'Distribution')


hist(br_day$cnt, xlab = 'cnt', ylab = 'cnt', col = 'violet', main = 'Distribution')



########## Feature Selection ##########
## Correlation Plot 

colnames(br_day_subset)
head(br_day_subset)
ggcorrplot(cor(br_day_subset),method = 'square', lab = T, title =  'Correlation plot',ggtheme = theme_dark())

#From above correlation plot we can see that season and mnth have high correlation, variabel
#temp and atemp and variable weathersit ad hum have high correlation. so we will drop
# mnth,temp and hum variable.



##Scatter regression plot to see positive or negative relation with target variable 'cnt'


ggplot(br_day_subset, aes(x= season,y=cnt)) +geom_point()+ geom_smooth(method = 'lm')+
ggtitle('season')


ggplot(br_day_subset, aes(x= mnth,y=cnt)) + geom_point()+ geom_smooth(method = 'lm')+ ggtitle('mnth')


ggplot(br_day_subset, aes(x= holiday,y=cnt)) + geom_point()+ geom_smooth(method = 'lm')+ ggtitle('holiday')


ggplot(br_day_subset, aes(x= weekday,y=cnt)) +geom_point()+ geom_smooth(method = 'lm')+ ggtitle('weekday')


ggplot(br_day_subset, aes(x= workingday,y=cnt)) +geom_point()+geom_smooth(method = 'lm')+ ggtitle('workingday')


ggplot(br_day_subset, aes(x= weathersit,y=cnt)) + geom_point()+geom_smooth(method = 'lm')+
  ggtitle('weathersit')


ggplot(br_day_subset, aes(x= temp,y=cnt)) +geom_point()+geom_smooth(method = 'lm')+ ggtitle('temp')


ggplot(br_day_subset, aes(x= atemp,y=cnt)) +geom_point()+geom_smooth(method = 'lm')+ ggtitle('atemp')


ggplot(br_day_subset, aes(x= hum,y=cnt)) +geom_point()+geom_smooth(method = 'lm')+ ggtitle('hum')


ggplot(br_day_subset, aes(x= windspeed,y=cnt)) +geom_point()+geom_smooth(method = 'lm')+ ggtitle('windspeed')

head(br_day_subset)

##model development
#Dropping variable mnth, temp and hum 
br_day_subset = subset.data.frame(br_day_subset,select = -c(2,7,9))

head(br_day_subset)

df = br_day_subset

#Divide data into train and test using stratified sampling method
set.seed(1234)
library(caret)
train.index = createDataPartition(br_day_subset$season, p = .80, list = FALSE)
train = br_day_subset[ train.index,]
test  = br_day_subset[-train.index,]

#### Defining MAPE function

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}


##############  develop Decision tree model ######################


# ##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-8])


############# Evaluate  Decision tree ###################

MAPE(test[,8], predictions_DT)*100

#Error rate is 27.88% which means our model is 72.12% correct


########################## Random Forest ##########################
br_day_subset = df

br_rental_rf = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)

br_rental_rf

################# Evaluate Random Forest ##############

#Predict for new test cases
predictions_DT = predict(br_rental_rf, test[,-8])


MAPE(test[,8], predictions_DT)*100

#Error rate is 28.33% which means our model is 71.67% correct

#################### Develop  Linear Regression Model ##########################
df = br_day_subset

#check multicollearity
install.packages('usdm')
library(usdm)
vif(train[,-8])
vifcor(br_day_subset[,-8], th = 0.9)

# developing Linear Regression  model


#executing regression model
lm_model = lm(cnt ~., data = train)


#Summary of the model
summary(lm_model)


# observe the  residuals and   coefficients  of the linear regression model

# Predict  the Test data 

#Predict
predictions_LR = predict(lm_model, test[,-8])

# Evaluate Linear Regression Model

MAPE(test[,8], predictions_LR)*100

#Error rate is 30.49% which means our model is 69.51% correct


##Decision tree model is best fitted model for bike count prediction.