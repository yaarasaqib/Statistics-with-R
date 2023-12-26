#### 11 Clustering #####

# Unsupervised learning 

#Clustering in R refers to the assimilation of the same kind of data in groups or
# cluster them in a group to distinguish one from the other


# AHC (Ascending Hierarchial Clustering) construct hierarchy of individuals
# that is graphically represented by a hierr tree also names as dendogram

# Step1: Read the data
# step2: Standardise the data if necessray (using scale function)
# step3: Construct the ascending hierarchial clustering.
# step4: Prune the hierrarchial tree.
# step5 : Characterise the clusters.

decath <- read.csv("decathlon.csv",header = TRUE,row.names = 1)
View(decath)
dim(decath)
library(cluster)
res.ahc <-agnes(scale(decath[,1:10]),method="ward")
plot(res.ahc,which.plots = 2,main="Dendogram",
     xlab="individuals")

res.ahc2 <- as.hclust(res.ahc)
plot(rev(res.ahc2$height),type="h",ylab="height")

clusters.hac <- cutree(res.ahc,k=4)
clusters.hac

library("FactoMineR")

clusters.hac <- as.factor(clusters.hac)
decath.comp  <- cbind.data.frame(decath,clusters.hac)
catdes(decath.comp,num.var=14)




############################
# using K-means algorithm

#1 Read the data
#2 Standardise the variables if necessary
#3 construct the partition
#4 Characterise the Clusters

# First  iris data

data("iris")
head(iris)

# using unsupervised learning
x=iris[,3:4] # using only petal width and length

model=kmeans(x,3) # for n=3 clusters
library(cluster)
model
clusplot(x,model$cluster)

clusplot(x,model$cluster,color=T,shade = T)


###############################################

decath<- read.csv("decathlon.csv",header=TRUE,
                  row.names = 1)

results.kmeans<- kmeans(scale(decath[,1:10]),centers=4)

results.kmeans

results.kmeans$cluster

library(FactoMineR)

decath.comp<- cbind.data.frame(decath,factor(results.kmeans$cluster
))

colnames(decath.comp)[14] <- "Cluster"

catdes(decath.comp, num.var=14)

# here the value greater than 2 means that cluster mean is
# significantly different from general mean.


# +ve and -ve sign  means v test indicate the mean of the cluster
# is superior or inferior wrt cluster.

