# Talal Khodr 
# Project for Data Science code script
# WD is already set
#install.packages("openData")
#install.packages("tidyverse")
#install.packages("questionr")
#install.packages("ggmap")
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(questionr)
library(rpart.plot)
library(randomForest)
library(caret)
library(rpart)
library(rattle)

car_crash <- read.csv("/Users/tkhodr/Desktop/Data project/database.csv")

sum(is.na(car_crash))

head(car_crash)


# A really important tool to understand data. str()
str(car_crash)

# Frequency for na's
freq.na(car_crash)

#create_report(car_crash)

# Attempting to count the empty cells.
count((car_crash == " "))

# Creating a new data without NA's
no_na_car <- na.omit(car_crash)


# Using Lubridate to assort the dates for a better undersanding
# Creates a new columns and then arranges it

no_na_car$DATE_TIME <- paste(no_na_car$DATE,no_na_car$TIME)

no_na_car$DATE <- mdy(no_na_car$DATE)

no_na_car$DATE_TIME <-mdy_hm(no_na_car$DATE_TIME)

#Adding the day column
no_na_car$day <- wday(no_na_car$DATE_TIME,label = T)
#Adding the month column
no_na_car$month <- month(no_na_car$DATE_TIME,label = T)
#Adding the hour column
no_na_car$hour <- hour(no_na_car$DATE_TIME)

str(no_na_car)

ggplot(data = car_crash, aes(x = CYCLISTS.KILLED, y = BOROUGH, fill= BOROUGH))

# Creating a eport for eda
# create_report(no_na_car)
colnames(no_na_car)

# car_modeling <- no_na_car[c( "DATE_TIME", "PERSONS.KILLED","PEDESTRIANS.INJURED" ,"PEDESTRIANS.KILLED" ,
# "CYCLISTS.INJURED" ,   "CYCLISTS.KILLED"   ,  "MOTORISTS.INJURED" ,  "MOTORISTS.KILLED" ,   "VEHICLE.1.TYPE" ,    
# "VEHICLE.2.TYPE"   ,   "VEHICLE.3.TYPE"   ,   "VEHICLE.4.TYPE"  ,    "VEHICLE.5.TYPE"    ,  "VEHICLE.1.FACTOR" ,  
# "VEHICLE.2.FACTOR"  ,  "VEHICLE.3.FACTOR"  ,  "VEHICLE.4.FACTOR"  ,  "VEHICLE.5.FACTOR"   )]


car_final <- car_crash[c("CYCLISTS.INJURED","MOTORISTS.INJURED","MOTORISTS.KILLED","CYCLISTS.KILLED","PEDESTRIANS.INJURED","PEDESTRIANS.KILLED","PERSONS.KILLED","VEHICLE.1.TYPE")]


rf_car_FINAL <- randomForest(PERSONS.KILLED~., data = car_final, ntree= 100, mtry =2, importance=TRUE)



train.tree <- rpart(PERSONS.KILLED~., data = car_final,method = "class")


fancyRpartPlot(model = train.tree, main = "Tree for Legendary")


#car_factor <- as.factor(no_na_car)

set.seed(118)
car_sampling_vector<- createDataPartition(car_crash$VEHICLE.1.FACTOR, p = 0.70, list = FALSE)

car_train <- car_crash[car_sampling_vector,]
car_test <-  car_crash[-car_sampling_vector,]


linear_clean <- lm(VEHICLE.1.FACTOR~., data=car_train)
summary(linear_clean)
plot(linear_clean)


rf_car_L <- randomForest(PERSONS.KILLED~., data = car_train, ntree= 500, mtry =2, importance=TRUE)


train.tree <- rpart(PERSONS.KILLED~., data = car_final,method = "class")


fancyRpartPlot(model = train.tree, main = "Tree")


# FACTORIZING ALL
#is.null(car_crash)

library(corrplot)

cor_vars = data.frame(car_crash)
correlations = cor(cor_vars)
corrplot(correlations,method = "number")



#run on trianing data
my.model1<-(PERSONS.KILLED~PERSONS.INJURED+MOTORISTS.INJURED+MOTORISTS.KILLED+PERSONS.KILLED+PERSONS.INJURED)
glm.fits1=glm(my.model1,family=binomial,data=car_train)
summary(glm.fits1)



linear_clean <- lm(Total~., data=poke_train)
summary(linear_clean)
plot(linear_clean)