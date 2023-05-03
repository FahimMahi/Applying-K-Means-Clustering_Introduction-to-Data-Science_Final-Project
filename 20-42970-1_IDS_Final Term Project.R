Dataset <- read.csv("D:/Semester/10) Spring 2023/Introduction To Data Science/Final Term Project/Country-data.csv",header=TRUE,sep=",")
Dataset
names(Dataset)
summary(Dataset)
str(Dataset)
sum(is.na(Dataset))

head(Dataset)
nullre<- na.omit(Dataset)


install.packages("dplyr")
install.packages("stats")
install.packages("cluster")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("factoextra")
install.packages("tidyverse")
install.packages("gridExtra")


library(dplyr)
library(stats)
library(cluster)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(tidyverse)
library(gridExtra)


#Standardize continuous variables
data<- Dataset %>% select(-gdpp,-country) %>% scale() 
data


#Identify and display the optimal number of clusters by doing Plot of WSS Elbow Method, Silhouette Method and Gap Statistics Method  
fviz_nbclust(data, kmeans, method = "wss")
fviz_nbclust(data, kmeans, method = "silhouette")
fviz_nbclust(data, kmeans, method = "gap_stat")


#Create clusters with K-means
kmeans(data, centers = 2, iter.max = 100, nstart = 100)


#Create Hierarchical Clustering
d <- dist(data, method ="euclidean")
d

hc1 <- hclust(d, method ="complete") 
hc1

plot(hc1, cex = 0.6)


#Clustering Result
kR<- pam(data,k=2) 
kR
summary(kR) 

#Generate the Cluster and Silhouette Plot
plot(kR)


#Generate the Cluster Biplot
fviz_cluster(kmeans(data,centers = 2, iter.max = 100, nstart = 100), data=data)


#Create a cluster visualization using the original variables
clusters <- kmeans(data,centers = 2, iter.max = 100, nstart = 100)
clusters
Dataset1 <- Dataset |> mutate(cluster = clusters$cluster)
Dataset1


#Cluster Visualization
Dataset1 |> ggplot(aes(x = health, y = income, col = as.factor(cluster))) + geom_point()

#Cluster Center
kr1<- kmeans(data, centers = 2, iter.max = 100, nstart = 100)
kr1
kr1$centers
