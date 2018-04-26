# CS 513
# Final Project
# Using C5.0 to predict beer type based on brewing data. 

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

#install.packages('c50')
library('C50')

# c50 classification
C50_class <- C5.0( as.factor(Style)~.,data=train_data[,c(-1,-2,-3,-5,-19)] )
C50_class

# general information about the tree
summary(C50_class)
dev.off()
# plot took 45 minutes to run, do not try this at home
#plot(C50_class)

# check error rate, coincides with the returned error rate of the c5.0 function call
C50_predict<-predict( C50_class ,test_data , type="class" )
table(actual=test_data[,4],C50=C50_predict)
wrong<- (test_data[,4]!=C50_predict)
c50_rate<-sum(wrong)/length(test_data[,4])
c50_rate
