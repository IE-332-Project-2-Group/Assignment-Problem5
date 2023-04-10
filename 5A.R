#Andrew Newquist 0032120709

#Packages to install 

#install.packages("cluster")
#install.packages("factoextra")
#install.packages("gridExtra")
#install.packages("dplyr")

library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(dplyr)

#Import Data. Ensure this .r file is in the same folder/working directory as the .csv
real_estate_data_raw <- read.csv("train.csv", header=TRUE)


#Clean data. Remove NAs, scale, and select only numerical columns. Remove Zero variance columns
real_estate_data <-select_if(real_estate_data_raw, is.numeric)
re_data_frame<- na.omit(real_estate_data)
re_data_frame<- re_data_frame[sample(nrow(re_data_frame),50),] #  <----- THIS IS AMOUNT OF PROPERTIES TO BE INCLUDED IN CLUSTERING
Var <- apply(re_data_frame, 2, var)
zero_var_col <- which (Var == 0)
re_data_frame<- re_data_frame[, -zero_var_col]

#Create Cluster.
cluster3 <- kmeans(re_data_frame, centers = 4, nstart = 5)
#Collect Individual Cluster Data
cluster1_data <- re_data_frame[cluster3$cluster == 1, ]
cluster2_data <- re_data_frame[cluster3$cluster == 2, ]
cluster3_data <- re_data_frame[cluster3$cluster == 3, ]
cluster4_data <- re_data_frame[cluster3$cluster == 4, ]
c1qual <- mean(cluster1_data$OverallQual)
c2qual <- mean(cluster2_data$OverallQual)
c3qual <- mean(cluster3_data$OverallQual)
c4qual <- mean(cluster4_data$OverallQual)
#Plots the Clusters. Prints Average Quality of cluster
plot3 <-fviz_cluster(cluster3, data = re_data_frame)
grid.arrange(plot3)
cat("The Average Overall Quality Rating of Cluster 1 is", sprintf("%.2f", c1qual))
cat("The Average Overall Quality Rating of Cluster 2 is", sprintf("%.2f", c2qual))
cat("The Average Overall Quality Rating of Cluster 3 is", sprintf("%.2f", c3qual))
cat("The Average Overall Quality Rating of Cluster 4 is", sprintf("%.2f", c4qual))
