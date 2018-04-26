# CS 513
# Final Project
# Using kmeans clustering to predict beer type based on brewing data. 

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

?kmeans

set.seed(456)
kmeans<- kmeans(data[,-c(1,2,3,4,5,19)],5,nstart = 10)
kmeans

table(kmeans$cluster, data$Style)
