# CS 513
# Final Project
# Using RandomForest to predict beer type based on brewing data. 
# This part is the ANN prediction

# First step is to clear the environment
rm(list=ls())

# Next we load our data
# Note that our working directory has been set to our lproject folder where this file, and the data are located
data <- read.csv(file = "recipeData.csv", na.strings=c("N/A"))
#View(data)

# Note that our KNN algorithm will not work with non-numeric values, so we can remove columns that we do not need 
# We are using the same exact data set with our neural net as well, se we follow the same steps
data$SugarScale <- NULL
data$BrewMethod <- NULL
data$PrimingMethod <- NULL
data$PrimingAmount <- NULL

# Its a LOT of data so we are simply going to remove any row with NA's
data <- na.omit(data)
# This cuts us from ~73000 observations to 7053 observations

# Now we can separate the data into testing and training data
# We still have a lot of data, so we will be using a 50/50 split (taking every other observation)
every_other <- seq(1, nrow(data), by=2)
test_data <- data[every_other,]
train_data <- data[-every_other,]

# install and load packages
install.packages('randomForest')

library(randomForest)
set.seed(456)


fit <- randomForest( Style~., data=train_data, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test_data[,4],Prediction)


wrong<- (test[,4]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 


