#Andrew Newquist 0032120709

library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(dplyr)
library(caret)
library(car)
library(glmnet)

#Clustering algorithm used in part 5A-------------------------------------------
real_estate_data_raw <- read.csv("train.csv", header=TRUE)
real_estate_data <-select_if(real_estate_data_raw, is.numeric)
re_data_frame<- na.omit(real_estate_data)
re_data_frame<- re_data_frame[sample(nrow(re_data_frame),25),] #  <----- THIS IS AMOUNT OF PROPERTIES TO BE INCLUDED IN CLUSTERING
Var <- apply(re_data_frame, 2, var)
zero_var_col <- which (Var == 0)
re_data_frame<- re_data_frame[, -zero_var_col]

cluster3 <- kmeans(re_data_frame, centers = 4, nstart = 10)

plot3 <-fviz_cluster(cluster3, data = re_data_frame)
#-------------------------------------------------------------------------------

#Regression machine learning algorithm on all data for overall quality of
#property 5C--------------------------------------------------------------------

#Creating Training Data
training <- real_estate_data[1:730,]
training <- na.omit(training)
training <- training[1:550,]

#Creating Testing Data
testing <- real_estate_data[731:1460,]
testing <- na.omit(testing)
testing <- testing[1:550,]


#Fitting Model to predict overall quality
reg_model <- lm(training$OverallQual~.-training$Id -training$MSSubClass,data = training)

#Testing Model ability to predict overall quality
predictions <- predict(reg_model, newdata = testing)
Rsquare <- sqrt(mean((testing$OverallQual - predictions)^2))
print(paste0("R-squared value is: ", round(Rsquare, 3)))
