# 11-20-2019
# STA 141A : Lecture 15
# Codes


#######################################
# Clustering Methods
####################################


####################
## Simulated data, belonging to three clusters
n1 = 30
n2 = 30
n3 = 30
n = n1 + n2 + n3 # total number of observations

mu1 = c(0,0)  # Population mean for class 1
mu1
mu2 = c(4,5)  # Population mean for class 2
mu2
mu3 = c(4,-3)
mu3
Sigma = matrix(c(2,0.2,0.2,1),2,2)  # common population covariance matrix
Sigma
## Simulate data
Y = c(rep(1,n1),rep(2,n2),rep(3,n3)) # true cluster labels
Y
Sigma.eig = eigen(Sigma)  # spectral decmposition of Sigma1

set.seed(1000) # set the value of the seed for the random number generator
X = matrix(0,n,2)
X
X[1:n1,] = matrix(1,n1,1) %*% t(as.vector(mu1))  + matrix(rnorm(2*n1),n1,2) %*% diag(sqrt(Sigma.eig$val)) %*% t(Sigma.eig$vec)
X[(n1+1):(n1+n2),] =  matrix(1,n2,1) %*% t(as.vector(mu2))  + matrix(rnorm(2*n2),n2,2) %*% diag(sqrt(Sigma.eig$val)) %*% t(Sigma.eig$vec)
X[(n1+n2+1):n,] =  matrix(1,n3,1) %*% t(as.vector(mu3))  + matrix(rnorm(2*n3),n3,2) %*% diag(sqrt(Sigma.eig$val)) %*% t(Sigma.eig$vec)
X = round(X,3)
X
## Plotting the data
library(ggplot2)
dfxy = data.frame(X1=X[,1],X2=X[,2],cluster=as.factor(Y))
dfxy.pl = ggplot(dfxy,aes(X1,X2,color=cluster))
dfxy.pl + geom_point()

####################
## K-means Clustering
#################
#
#1. Randomly assign a number, from 1 to K, to each of the observations.
#   These serve as initial cluster assignments for the observations.
#
# 2. Iterate until the cluster assignments stop changing:
#  (a) For each of the K clusters, compute the cluster centroid. The
#      k-th cluster centroid is the vector of the p feature means for the
#      observations in the kth cluster.
#
#  (b) Assign each observation to the cluster whose centroid is closest
#      (where closest is defined using Euclidean distance).
####################

# Specify K=3 (true) initial clusters
# centers = K
# nstart = how many initial random clusters are to be chosen
dfxy.km = kmeans(dfxy[,1:2],centers=3, nstart = 20, iter.max = 10)
?kmeans
## Number of iteractions to convergence
dfxy.km$iter
### Cluster centers
dfxy.km$centers
### Cluster sizes
dfxy.km$iter
### Cluster labels
dfxy.km$cluster

library(ggplot2)

## plotting the clustering results
dfxy.pred = cbind(dfxy,cluster.pred=factor(dfxy.km$cluster))
dfxy.pred

dfxy.pred.pl = ggplot(dfxy.pred,aes(X1,X2,color=cluster))
dfxy.pred.pl + geom_point() + geom_text(aes(label=cluster.pred),nudge_x=0.1,nudge_y=0.1)

dfxy.clust.pl = ggplot(dfxy.pred,aes(X1,X2,color=cluster.pred))
dfxy.clust.pl + geom_point()

### Within cluster Sum of Squares
dfxy.km$withinss
### Between cluster Sum of Squares
dfxy.km$betweenss
### Total Sum of Squares
dfxy.km$totss

###############
## An alternative is to specify the initial K (=3) cluster
## centroids as K randomly chosen rows of the data matrix
dfxy.km.random = kmeans(dfxy[,1:2],centers=dfxy[sample(n,3),1:2], iter.max =10)
dfxy.km.random$cluster

## Choice of k studying the within variance

withinss <- function(n, x, centers, assignments) {
  p <- centers[n, ]
  m <- rbind(p, x[assignments==n, 1:2])
  sum((as.matrix(dist(m))[1, ])^2)
}

predict.kmeans <- function(object,
                           newdata,
                           method = c("centers", "classes")) {
  method <- match.arg(method)
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}

set.seed(12)
train_p=0.8;
N=100
n_clust=20
within_c_n=matrix(0,n_clust,N)
within_c=vector('numeric',0)
summary(dfxy)
attach(dfxy)

for(cl in 1:n_clust){
for(iter in 1:N){
train_1=sample(which(cluster==1),train_p*n/3)
train_2=sample(which(cluster==2),train_p*n/3)
train_3=sample(which(cluster==3),train_p*n/3)
train=sort(c(train_1,train_2,train_3))
test=sort(setdiff(1:n,train))
dfxy.km_train=kmeans(dfxy[train,1:2],centers=cl, nstart = 20, iter.max = 10)
centers <- dfxy.km_train$centers
assignments <-  as.numeric(row.names(predict.kmeans(dfxy.km_train, (dfxy[test,1:2]))))
within_c_n[cl,iter]=sum(sapply(seq(nrow(centers)), function(y){withinss(n=y,x = dfxy[test,1:2], centers = centers, assignments = assignments)}))
                }
within_c[cl]=(mean(within_c_n[cl,]))
            }
within_c
par(mfrow=c(1,1))
plot(within_c)
within_c[3]

#################################
############################
## Hierarchical Clustering
#############################
#
# 1. Begin with n observations and a measure (such as Euclidean distance)
#    of all the "n choose 2" = n(n−1)/2 pairwise dissimilarities. Treat each
#    observation as its own cluster.
#
# 2. For i = n, n − 1,...,2
#
#   (a) Examine all pairwise inter-cluster dissimilarities among the i
#       clusters and identify the pair of clusters that are least dissimilar
#       (that is, most similar). Fuse these two clusters. The dissimilarity
#       between these two clusters indicates the height in the dendrogram
#       at which the fusion should be placed.
#
#   (b) Compute the new pairwise inter-cluster dissimilarities among
#       the i − 1 remaining clusters.
################################################
##  Different types of "linkages"
#
# Complete: Maximal intercluster dissimilarity. Compute all pairwise
#           dissimilarities between the observations in cluster A and the
#           observations in cluster B, and record the largest of these
#           dissimilarities.
#
# Single:   Minimal intercluster dissimilarity. Compute all pairwise
#           dissimilarities between the observations in cluster A and the
#           observations in cluster B, and record the smallest of these
#           dissimilarities. Single linkage can result in extended, trailing
#           clusters in which single observations are fused one-at-a-time.
#
# Average:  Mean intercluster dissimilarity. Compute all pairwise dissimilarities
#           between the observations in cluster A and the observations in
#           cluster B, and record the average of these dissimilarities.
###################################################

datamat = X
?dist
hc.complete = hclust(dist(datamat), method = "complete")  # complete linkage
hc.average =  hclust(dist(datamat), method = "average") # avergae linkage
hc.single =  hclust(dist(datamat), method = "single") # single linkage

par(mfrow =c(1,3))
plot(hc.complete ,main = "Complete Linkage", xlab="", sub ="", cex =.9)
plot(hc.average , main = "Average Linkage", xlab="", sub ="", cex =.9)
plot(hc.single , main= "Single Linkage", xlab="", sub ="", cex =.9)

# To determine the cluster labels for each observation associated with a
# given cut of the dendrogram, we can use the cutree() function.

# cut the dendogram corresponding to hc.compete at level 3
cutree(hc.complete, 3)
hc.complete.pred = as.factor(cutree(hc.complete, 3))
dfxy.clust.hc.complete= ggplot(dfxy,aes(X1,X2,color=hc.complete.pred))
dfxy.clust.hc.complete + geom_point()

# cut the dendogram corresponding to hc.average at level 3
cutree(hc.average, 3)
?cutree
hc.average.pred = as.factor(cutree(hc.average, 3))
dfxy.clust.hc.average = ggplot(dfxy,aes(X1,X2,color=hc.average.pred))
dfxy.clust.hc.average + geom_point()

# cut the dendogram corresponding to hc.single at level 3
cutree(hc.single, 3)
hc.single.pred = as.factor(cutree(hc.single, 3))
dfxy.clust.hc.single = ggplot(dfxy,aes(X1,X2,color=hc.single.pred))
dfxy.clust.hc.single + geom_point()

##