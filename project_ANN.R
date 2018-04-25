# CS 513
# Final Project
# Using ANN to predict beer type based on brewing data. 
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

# install and load the neural network package
#install.packages("neuralnet")
library("neuralnet")
#?neuralnet()

#all_vars <- colnames(data[6:18])
#pred_vars <- all_vars[!all_vars%in%"StyleID"]
#pred_vars <- paste(pred_vars, collapse = "+")
#form= as.formula(paste("StyleID~", pred_vars, collapse = "+"))

#normalizing the test and train data
#scaled.test.data <- scale(test_data)
#scaled.train.data <- scale(train_data)

# Run the neural net to predict the StyleID based on the other features
net <- neuralnet(StyleID~Size.L.+OG+FG+ABV+IBU+Color+BoilSize+BoilTime+BoilGravity+Efficiency+MashThickness+PitchRate+PrimaryTemp, train_data, hidden = 10, threshold = .01)
plot(net)

net_results <- compute(net, test_data[,c(-1,-2,-3,-4,-5,-19)])
ANN=as.numeric(net_results$net.result)
ANN_round<-round(ANN)

wrong<- (test_data$StyleID!=ANN_round)
rate<-sum(wrong)/length(wrong)
rate

#net_bc21  <- neuralnet(formula = form, data=train_data, hidden=c(4,2), threshold=0.01)
#plot(net_bc21)

