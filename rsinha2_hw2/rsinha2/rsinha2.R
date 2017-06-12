######
# Bisecting K-Means
#####
rm(list = ls(all = T))

library(RColorBrewer)

# set seed to ensure consistent results
set.seed(100)

# Read data file
# When	submitting, ensure that the path to this file is just "hw2-data.csv" 
# and doesn't point	to a path on your machine 
data.df <- read.csv('hw2-data.csv')

# TODO: Implement bisecting k means.
# Input:
# data.df: data frame based on hw2-data.csv
# trials.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Be mindful that there might be duplicates in the data.
# terminating condition: when k clusters have been found

bisectingkmeans <- function(data.df, iter.max, k){
SSE = matrix(nrow=k,ncol=1,-.Machine$integer.max)
CLUSTER = matrix(nrow=k,ncol=1,-.Machine$integer.max)
data.df
input <- data.df[,c(2:3)]
result  = kmeans(input,2)
CLUSTER = result$cluster;
SSE[1,] = result$withinss[1]
SSE[2,] = result$withinss[2]
numOfClusters=2
#till we get k clusters
while(k > numOfClusters) {

	clusterToSplit = which.max(SSE)
	ids = which(CLUSTER == clusterToSplit)
	inputM = data.df[ids,-nrow(data.df)]
	inputm <- inputM[,c(2,3)]
	count = 0
	resultSSE = 0
	#repeat iter.max number of times
	while(count<iter.max)
	{
		count = count+1
		points = which(data.df$ID %in% matrix(c(sample(ids, 2, replace=F)), nrow=5,  ncol=1))
		inputMat = data.df[points,-nrow(data.df)]
		inputmat <- inputMat[,c(2,3)]
		temp = kmeans(x=inputm,centers=inputmat)
		if (resultSSE<temp$withinss[1]+temp$withinss[2]){
			resultSSE = temp$withinss[1]+temp$withinss[2]
			result=temp
		}
	}
	numOfClusters <- numOfClusters +1
	SSE[numOfClusters,] = result$withinss[1]
	SSE[clusterToSplit] = result$withinss[2]	
	outResult <- result$cluster
	outResult[which(outResult == 1)] = -1
	outResult[which(outResult == 2)] = -2
	outResult[which(outResult == -1)] = clusterToSplit
	outResult[which(outResult == -2)] = numOfClusters
	
	for(i in 1:length(ids)){
			CLUSTER[ids[i]]=outResult[i]
	}
		
}

return(list(CLUSTER=CLUSTER,SSE=SSE))  
}

# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
	points = which(data.df$ID %in% matrix(c(210, 247, 265, 278, 288), nrow=5,  ncol=1))
	inputMat = data.df[points,-nrow(data.df)]
	inputm <- inputMat[,c(2:3)]
	data1.df <- data.df[,c(2:3)]
	result = kmeans(x=data1.df,centers=inputm)
	plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     	cex = 3)
}

# Don't edit anything beyond this line
# Please note, TA will test with different configurations of trails.max and k.
k=5
result <- bisectingkmeans(data.df, iter.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)
