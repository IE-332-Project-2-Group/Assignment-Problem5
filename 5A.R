library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(dplyr)
real_estate_data_raw <- read.csv("train.csv", header=TRUE)
real_estate_data <-select_if(real_estate_data_raw, is.numeric)

#real_estate_data <- real_estate_data %>% sample_n(100,replace=FALSE)
#real_estate_data <-real_estate_data_raw[,c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","TotalBsmtSF","1stFlrSF","2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath")]
data_frame<- real_estate_data
data_frame<- na.omit(data_frame)
data_frame<- scale(data_frame)
#daisy_output<- daisy(data_frame, metric = "gower")
#hclust_output <- hclust(as.dist(daisy_output))
#plot(hclust_output)
#head(data_frame)

kmeans2 <- kmeans(data_frame, centers = 4, nstart = 25)
str(kmeans2)

fviz_cluster(kmeans2, data = data_frame)