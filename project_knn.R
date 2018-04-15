# Nicholas Zubrycki
# CS 513
# Final Project
# Using KNN and ANN to predict beer type based on brewing data. 
# This part is the KNN prediction

# First step is to clear the environment
rm(list=ls())

# Next we load our data
# Note that our working directory has been set to our lproject folder where this file, and the data are located
data <- read.csv(file = "recipeData.csv", na.strings=c("N/A"))

# Note that our KNN algorithm will not work with non-numeric values, so we can remove columns that we do not need 
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

# Load library
library(class)

# Run knn algorithm to predict the Style column
# When we run it we are not taking the BeerID, Name, URL, Style, StyleID, or UserID into account 
# because they have nothing to do with the actual beer
knn5 <- knn(train_data[,c(-1,-2,-3,-4,-5,-19)], test_data[,c(-1,-2,-3,-4,-5,-19)], train_data[,4], k = 5)
#knn5

# Check our accuracy
wrong <- (test_data[,4] != knn5)
rate <-sum(wrong)/length(wrong)
rate

# Now we can look at more values for K by editing this for loop
for(i in seq(1, 100, by=1)){
  predict <- knn(train_data[,c(-1,-2,-3,-4,-5,-19)], test_data[,c(-1,-2,-3,-4,-5,-19)], train_data[,4], k = i)
  wrong <- (test_data[,4]!=predict)
  rate <- sum(wrong)/length(wrong)
  #print(i)
  #print(rate)
  # Here we are storing our accuracy rates into a vector so that we can plot it and find which values are good for K
  rates[i] <- rate * 100
}
max(rates)
plot(rates)
