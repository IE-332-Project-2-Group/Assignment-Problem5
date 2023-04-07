library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(dplyr)

#Import Data
real_estate_data_raw <- read.csv("train.csv", header=TRUE)


#Clean data. Remove NAs, scale, and select only numerical columns.
real_estate_data <-select_if(real_estate_data_raw, is.numeric)
data_frame<- na.omit(data_frame)
data_frame<- scale(data_frame)
data_frame<- data_frame[sample(nrow(data_frame),25),]. #.  <----- THIS IS AMOUNT OF PROPERTIES TO BE INCLUDED IN CLUSTERING


#Create Clusters. Lines commented out have different cluster amounts

#cluster1 <- kmeans(data_frame, centers = 2, nstart = 5)
#cluster2 <- kmeans(data_frame, centers = 3, nstart = 5)
cluster3 <- kmeans(data_frame, centers = 4, nstart = 5)
#cluster4 <- kmeans(data_frame, centers = 5, nstart = 5)


#Plots the Clusters. Lines commented out plot the above clusters. Cluster1=plot1.. etc

#plot1 <-fviz_cluster(cluster1, data = data_frame)
#plot2 <-fviz_cluster(cluster2, data = data_frame)
plot3 <-fviz_cluster(cluster3, data = data_frame)
#plot4 <-fviz_cluster(cluster4, data = data_frame)
grid.arrange(plot3)#,plot2,plot3,plot4)
