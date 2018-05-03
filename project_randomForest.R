# CS 513
# Final Project
# Using RandomForest to predict beer type based on brewing data. 

# First step is to clear the environment
rm(list=ls())

# Next we load our data
# Note that our working directory has been set to our lproject folder where this file, and the data are located
data <- read.csv(file = "recipeData.csv", na.strings=c("N/A"))
#View(data)

# Note that our KNN algorithm will not work with non-numeric values, so we can remove columns that we do not need 
# We must eliminate more columns this time to appease the random forest algorithm
#(we can't have a column with factors of more than 53 categories)
data$SugarScale <- NULL
data$BrewMethod <- NULL
data$PrimingMethod <- NULL
data$PrimingAmount <- NULL

# Its a LOT of data so we are simply going to remove any row with NA's
data <- na.omit(data)
# This cuts us from ~73000 observations to 7053 observations

# these are the extra removals
data$Name <- NULL
data$URL <- NULL
data$Style <- NULL
data$UserId <- NULL
data$BeerID <-NULL

# install and load packages
#install.packages('randomForest')
#install.packages("dplyr")
library(dplyr)
library(randomForest)

# separate data into bins so we can test with the random forest
bin1 <- filter(data, StyleID<=50)
bin2 <- filter(data, StyleID>50 & StyleID<=100)
bin3 <- filter(data, StyleID>100 & StyleID<=150)
bin4 <- filter(data, StyleID>150)

# check to make sure that we filtered properly, sum should equal 7053
sum(nrow(bin1),nrow(bin2),nrow(bin3),nrow(bin4))

# set seed for random forest
set.seed(456)

# get testing and training data for each bin
everyother1 <- seq(1, nrow(bin1), by=2)
testbin1 <- bin1[everyother1,]
trainbin1 <- bin1[-everyother1,]

everyother2 <- seq(1, nrow(bin2), by=2)
testbin2 <- bin2[everyother2,]
trainbin2 <- bin2[-everyother2,]

everyother3 <- seq(1, nrow(bin3), by=2)
testbin3 <- bin3[everyother3,]
trainbin3 <- bin3[-everyother3]

everyother4 <- seq(1, nrow(bin4), by=2)
testbin4 <- bin4[everyother4,]
trainbin4 <- bin4[-everyother4,]

# time to run the random forest on each of our bins

##################################################################################################
# bin 1
fit_bin1 <- randomForest(StyleID~., data = trainbin1[], importance=TRUE, ntree=1000)
importance(fit_bin1)
varImpPlot(fit_bin1)
prediction_bin1 <- predict(fit_bin1,testbin1)
# because we are predicting ID's we must round our predictions
prediction_bin1 <- round(prediction_bin1)
#table(actual=testbin1$StyleID, prediction_bin1)

#test accuracy of the algorithm
wrong_bin1 <- (testbin1[,2]!=prediction_bin1)
errorrate_bin1 <- sum(wrong_bin1)/length(wrong_bin1)
errorrate_bin1

##################################################################################################
# bin 2
fit_bin2 <- randomForest(StyleID~., data = trainbin2[], importance=TRUE, ntree=1000)
importance(fit_bin2)
varImpPlot(fit_bin2)
prediction_bin2 <- predict(fit_bin2,testbin2)
# because we are predicting ID's we must round our predictions
prediction_bin2 <- round(prediction_bin2)
#table(actual=testbin1$StyleID, prediction_bin2)

#test accuracy of the algorithm
wrong_bin2 <- (testbin2[,2]!=prediction_bin2)
errorrate_bin2 <- sum(wrong_bin2)/length(wrong_bin2)
errorrate_bin2

##################################################################################################
# bin 3
fit_bin3 <- randomForest(StyleID~., data = trainbin3[], importance=TRUE, ntree=1000)
importance(fit_bin3)
varImpPlot(fit_bin3)
prediction_bin3 <- predict(fit_bin3,testbin3)
# because we are predicting ID's we must round our predictions
prediction_bin3 <- round(prediction_bin3)
#table(actual=testbin1$StyleID, prediction_bin3)

#test accuracy of the algorithm
wrong_bin3 <- (testbin3[,2]!=prediction_bin3)
errorrate_bin3 <- sum(wrong_bin3)/length(wrong_bin3)
errorrate_bin3

##################################################################################################
# bin 4
fit_bin4 <- randomForest(StyleID~., data = trainbin4[], importance=TRUE, ntree=1000)
importance(fit_bin4)
varImpPlot(fit_bin4)
prediction_bin4 <- predict(fit_bin4,testbin4)
# because we are predicting ID's we must round our predictions
prediction_bin4 <- round(prediction_bin4)
#table(actual=testbin1$StyleID, prediction_bin4)

#test accuracy of the algorithm
wrong_bin4 <- (testbin4[,2]!=prediction_bin4)
errorrate_bin4 <- sum(wrong_bin4)/length(wrong_bin4)
errorrate_bin4

##################################################################################################
# all the data

every_other <- seq(1, nrow(data), by=2)
test_data <- data[every_other,]
train_data <- data[-every_other,]

fit_data <- randomForest(StyleID~., data = train_data[], importance=TRUE, ntree=1000)
importance(fit_data)
varImpPlot(fit_data, main = "Full Data Variable Importance")
prediction_data <- predict(fit_data, test_data)
prediction_data <- round(prediction_data)
wrong_data <- (test_data[,2]!=prediction_data)
errorrate_data <- sum(wrong_data)/length(wrong_data)
errorrate_data
