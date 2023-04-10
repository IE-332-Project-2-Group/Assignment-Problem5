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
re_data_frame<- re_data_frame[sample(nrow(re_data_frame),80),] #  <----- THIS IS AMOUNT OF PROPERTIES TO BE INCLUDED IN CLUSTERING
Var <- apply(re_data_frame, 2, var)
zero_var_col <- which (Var == 0)
re_data_frame<- re_data_frame[, -zero_var_col]

#Create Cluster.
cluster3 <- kmeans(re_data_frame, centers = 4, nstart = 5)

#Plots the Clusters.
plot3 <-fviz_cluster(cluster3, data = re_data_frame)
grid.arrange(plot3)
