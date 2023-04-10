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
#re_data_frame<- re_data_frame[sample(nrow(re_data_frame),1000),] #  <----- THIS IS 
#AMOUNT OF PROPERTIES TO BE INCLUDED IN CLUSTERING, INCREASED FOR TRAINING OF REGRESSION MODELS

Var <- apply(re_data_frame, 2, var)
#zero_var_col <- which (Var == 0)
#re_data_frame<- re_data_frame[, -zero_var_col]
cluster_alg <- kmeans(re_data_frame, centers = 4, nstart = 10)
plot <-fviz_cluster(cluster_alg, data = re_data_frame)

#-------------------------------------------------------------------------------

#Regression machine learning algorithm on individual cluster data for overall 
#quality of property 5D------------------------------------------------------------

#Collect Individual Cluster Data
cluster1_data <- re_data_frame[cluster_alg$cluster == 1, ]
cluster2_data <- re_data_frame[cluster_alg$cluster == 2, ]
cluster3_data <- re_data_frame[cluster_alg$cluster == 3, ]
cluster4_data <- re_data_frame[cluster_alg$cluster == 4, ]


#Determine size of Test and Train data frames for each cluster. If even split. If odd ommit 1 and split

cluster11_counts <- nrow(cluster1_data)
if (cluster11_counts %% 2 == 1) {
  cluster12_counts <- floor((cluster11_counts - 1) / 2)
} else {
  cluster12_counts <- cluster11_counts / 2
}

cluster21_counts <- nrow(cluster2_data)
if (cluster21_counts %% 2 == 1) {
  cluster22_counts <- floor((cluster21_counts - 1) / 2)
} else {
  cluster22_counts <- cluster21_counts / 2
}

cluster31_counts <- nrow(cluster3_data)
if (cluster31_counts %% 2 == 1) {
  cluster32_counts <- floor((cluster31_counts - 1) / 2)
} else {
  cluster32_counts <- cluster31_counts / 2
}

cluster41_counts <- nrow(cluster4_data)
if (cluster41_counts %% 2 == 1) {
  cluster42_counts <- floor((cluster41_counts - 1) / 2)
} else {
  cluster42_counts <- cluster41_counts / 2
}

#Creating Training and Testing Data for clusters
training_c1 <- cluster1_data[1:cluster12_counts,]
testing_c1 <- cluster1_data[(cluster12_counts+1):cluster11_counts,]
training_c2 <- cluster2_data[1:cluster22_counts,]
testing_c2 <- cluster2_data[(cluster22_counts+1):cluster21_counts,]
training_c3 <- cluster3_data[1:cluster32_counts,]
testing_c3 <- cluster3_data[(cluster32_counts+1):cluster31_counts,]
training_c4 <- cluster4_data[1:cluster42_counts,]
testing_c4 <- cluster4_data[(cluster42_counts+1):cluster41_counts,]

#Fitting each cluster a regression model to predict overall quality
reg_model_c1 <- lm(training_c1$OverallQual~.,data = training_c1)
reg_model_c2 <- lm(training_c2$OverallQual~.,data = training_c2)
reg_model_c3 <- lm(training_c3$OverallQual~.,data = training_c3)
reg_model_c4 <- lm(training_c4$OverallQual~.,data = training_c4)

#Test the Models ability to predict overall quality
predictions_c1 <- predict(reg_model_c1, newdata = testing_c1)
Rsquare_c1 <- sqrt(mean((testing_c1$OverallQual - predictions_c1)^2))
print(paste0("R-squared value for the cluster 1 model is: ", round(Rsquare_c1, 3)))
predictions_c2 <- predict(reg_model_c2, newdata = testing_c2)
Rsquare_c2 <- sqrt(mean((testing_c2$OverallQual - predictions_c2)^2))
print(paste0("R-squared value for the cluster 2 model is: ", round(Rsquare_c2, 3)))
predictions_c3 <- predict(reg_model_c3, newdata = testing_c3)
Rsquare_c3 <- sqrt(mean((testing_c3$OverallQual - predictions_c3)^2))
print(paste0("R-squared value for the cluster 3 model is: ", round(Rsquare_c3, 3)))
predictions_c4 <- predict(reg_model_c4, newdata = testing_c4)
Rsquare_c4 <- sqrt(mean((testing_c4$OverallQual - predictions_c4)^2))
print(paste0("R-squared value for the cluster 4 model is: ", round(Rsquare_c4, 3)))
