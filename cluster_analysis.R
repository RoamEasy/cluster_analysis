
setwd("~/Documents/R/cluster_analysis")

data<- read.csv("Porto_taxi_data_test_partial_trajectories.csv")

#find the number of rows
dim(data)

# look at the first rows of the data
head(data)

summary(data)

# find average of each statistic
sapply(data, mean, na.rm=TRUE)

#Make pairwise scatterplots
library(GGally)
ggpairs(data[,c("ast", "fg", "trb")])

# makesclusters of the data
library(cluster)
set.seed(1)

#removes any non-numeric columns
isGoodCol <- function(col){
  sum(is.nan(col)) == 0 && sum(is.na(col)) == 0 && is.numeric(col) 
}
goodCols <- sapply(data, isGoodCol)
clusters <- kmeans(data[,goodCols], centers=3)
labels <- clusters$cluster

# plot data by cluster
data2d <- prcomp(data[,goodCols], center=TRUE)
twoColumns <- data2d$x[,1:2]
clusplot(twoColumns,labels,color=TRUE, shade=FALSE, labels= 1, lines=0)

