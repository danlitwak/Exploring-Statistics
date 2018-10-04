##############################################################################################################################################################################################
## Ch. 10 Lab

## 10.4.1 PCA
library(ISLR)
names(USArrests)
states <- rownames(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, sd)

# The prcomp function will automatically scale variables to mean 0, and scale=TRUE sets SD to 1.
pr.out <- prcomp(USArrests, scale=TRUE)
names(pr.out)
summary(pr.out)
pr.out$center
pr.out$scale
# These are the principal components.
pr.out$rotation
# The prcomp function contains the principal component score vectors.
dim(pr.out$x)
biplot(pr.out, scale=0)
# To recreate the plots from earlier in the chapter, change the sign of the rotation matrix and score vectors.
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale=0)
# These are the standard deviations of the principal components.  We want this to get at the variance, which is needed to find proportion of variance explained (PVE)
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var / sum(pr.var)
pve
# The first principle component explains 62% of the variance, the second explains 24%, etc.
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0, 1), type="b")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0, 1), type="b")
# This is the scree plot that we search for an elbow in to determine when the principal components become "less important".

## 10.5 Clustering
set.seed(2)
# Create two clusters that are shifted relative to each other.
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

km.out <- kmeans(x, centers=2, nstart=20)
names(km.out)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering With K=2", xlab="", ylab="", pch=20, cex=2)
# What if we didn't know how many clusters there should be?
set.seed(4)
km.out <- kmeans(x, centers=3, nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering With K=3", xlab="", ylab="", pch=20, cex=2)

# nstart effectively performs the kmeans clustering nstart times, with a randomly chosen center.  Then, it reports the lowest total within-cluster sum of squares.
set.seed(3)
km.out <- kmeans(x, centers=3, nstart=1)
km.out$tot.withinss
km.out <- kmeans(x, centers=3, nstart=20)
km.out$tot.withinss

# 10.5.2 Hierarchical Clustering
# Create the model using hclust, the hierarchical clustering function
hc.complete <- hclust(dist(x), method="complete")
hc.average <- hclust(dist(x), method="average")
hc.single <- hclust(dist(x), method="single")
par(mfrow=c(1, 3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=0.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=0.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=0.9)

# Cut the tree of each function to cluster the observations
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

xsc=scale(x)
plot(hclust(dist( xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
#Correlation-based distance
x <- matrix(rnorm(30*3), ncol=3)
dd <- as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

# 10.6 NCI60 Data Example
library(ISLR)
?NCI60
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)
nci.labs[1:5]
table(nci.labs)

pr.out <- prcomp(nci.data, scale=TRUE)

Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1, 2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z3")
# Find the PVE in the summary and plot them. Bar height is SD^2.  More useful to plot PVE.
summary(pr.out)
plot(pr.out)
pve <- 100*pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow=c(1, 2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Compeonent", col="brown3")

pve
summary(pr.out)$importance
summary(pr.out)$importance[2,]
summary(pr.out)$importance[3,]


# 10.6.2 Clustering NCI60 Data
# Scale the data and use Complete, Average, and Single Linkage with Euclidean distance
sd.data <- scale(nci.data)
par(mfrow=c(1, 3))
data.dist <- dist(sd.data)
hc.complete <- hclust(data.dist, method="complete")
hc.average <- hclust(data.dist, method="average")
hc.single <- hclust(data.dist, method="single")
plot(hc.complete, labels=nci.labs, main="Complete Linkage", xlab="", sub="", ylab="")
plot(hc.average, labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab="")
plot(hc.single, labels=nci.labs, main="Single Linkage", xlab="", sub="", ylab="")

# Use Complete Linkage based on the hierarchical clustering plotted above.  Single Linkage results in many small clusters that are very different from the rest, and average yields somewhat similar results.
hc.out <- hc.complete
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
# Some types of cancer all fall into one cluster, like Leukemia or melanoma.  This is important because they were grouped *without regard for what type of cancer* known previously.
par(mfrow=c(1, 1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
hc.out

# Compare hierarchical clustering to kmeans clustering of the SAME cluster size=4
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart=20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
# These results are somewhat different.  Cluster 2 of kmeans has been recoded as cluster 3 of Hierarchical.  Others have been split up.  

# Perform hierarchical clustering on only the first few principal components
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
# Sometimes performing clustering on the principal components can "filter out the noise" and return better results than the full dataset.  

###########################################################################################
###########################################################################################
## Ch. 10 Exercises

## Q1

# (A) The left hand side of 10.12 says that for every feature p take the squared distance between all points in the cluster Ck.  The right hand side that for every feature p take the squared distance between each point and the average, and then double it.  Since we are using the summation to add all the points and then divide by Ck, this can be shown to become the mean (centroid) of the cluster.  We multiply by 2 because we add each permutation.
# (B) Since the centroid is computed, then observations are assigned to the _nearest_ centroid, it is apparent that the distance must always be decreasing overall for the cluster.  

## Q2
# (A)
dis.mat <- matrix(data=c(1, 0.3, 0.4, 0.7, 0.3, 1, 0.5, 0.8, 0.4, 0.5, 1, 0.45, 0.7, 0.8, 0.45, 1), nrow=4)
# The lowest dissimilarity scores will fuse first.  We only need consider the lower triangular half of the matrix.  THe dissimialrity between clusters indicates the height of fusion.  
# Complete: Compute all pairwise dissimilarities and record the LARGEST
# The 1st and 2nd observations fuse first at a height of 0.3.  Then 3&4 fuse.  Then both clusters fuse.
# See sketch for details
# (B) 
# Single: Minimal intercluster dissimilarity: compute pairwise dissimilarities and record the smallest
# 1&2 fuse first.  Then 3 fuses with them.  Then 4.
# See sketch for details
# (C)
# For complete linkage we end up with 2 clusters: 1&2 and 3&4.
# (D)
# For single linkage we end up with 2 clusters: 1&2&3 and 4.
# (E)The horizonal axis doesn't provide any meaning, so taking the mirror image over a vertical reflection line would result in a different dendrogram with the same meaning.

## Q3
# (A)
obs <- data.frame(x1 = c(1, 1, 0, 5, 6, 4), x2 = c(4, 3, 4, 1, 2, 0))
obs
plot(obs$x1, obs$x2, col="blue", pch=19)
# (B)
k <- 2
set.seed(1)
cluster <- sample(1:2, 6, replace=TRUE)
cluster
plot(obs$x1, obs$x2, col=obs$cluster, pch=19)
# (C)
obs$cluster <- cluster
centroids <- data.frame(x1cent = rep(NA, 2), x2cent = rep(NA, 2))
centroids[1,1] <- mean(obs$x1[obs$cluster==1])
centroids[1,2] <- mean(obs$x2[obs$cluster==1])
centroids[2,1] <- mean(obs$x1[obs$cluster==2])
centroids[2,2] <- mean(obs$x2[obs$cluster==2])
points(centroids, col="green", pch=19)
# (D)
obs$cluster <- ifelse((obs$x1 - centroids[1,1])^2 + (obs$x2 - centroids[1, 2])^2 < (obs$x1 - centroids[2,1])^2 + (obs$x2 - centroids[2, 2])^2, 1, 2)
obs
plot(obs$x1, obs$x2, col=obs$cluster, pch=19)
# (E)
centroids[1,1] <- mean(obs$x1[obs$cluster==1])
centroids[1,2] <- mean(obs$x2[obs$cluster==1])
centroids[2,1] <- mean(obs$x1[obs$cluster==2])
centroids[2,2] <- mean(obs$x2[obs$cluster==2])
points(centroids, col="green", pch=19)

obs$cluster <- ifelse((obs$x1 - centroids[1,1])^2 + (obs$x2 - centroids[1, 2])^2 < (obs$x1 - centroids[2,1])^2 + (obs$x2 - centroids[2, 2])^2, 1, 2)
plot(obs$x1, obs$x2, col=obs$cluster, pch=19)
centroids[1,1] <- mean(obs$x1[obs$cluster==1])
centroids[1,2] <- mean(obs$x2[obs$cluster==1])
centroids[2,1] <- mean(obs$x1[obs$cluster==2])
centroids[2,2] <- mean(obs$x2[obs$cluster==2])
points(centroids, col="green", pch=19)

# (F) See above

## Q4
# (A)
# It is possible that they fuse at the same height but more likely that the single linkage height will be lower than the complete.  This is because complete linkage seeks the LARGEST pairwise dissimilarity between the clusters and single linkage finds the smallest pairwise dissimilarity, which will define the fusion height.  
# (B)
# Since the distance between 5&6 will be both the MAX and MIN distance between the two single-observation clusters, they should fuse at the same height.  

## Q5
# Left Figure:  K-means clustering with K=2 would find one cluster of those who bought many items and one of those who bought fewer.  
# Center Figure: K-means clustering would find one cluster of people who bought BOTH socks and computers and one that bought only socks.
# Right Figure: K-means clustering would find one cluster with people who spent more than $1500 and one cluster of people who spent less.  AKA people who bought computers and those who didn't. 

## Q6
# (A)
# Projecting the dataset onto the first principle component would only account for 10% of variation found in the response, with 90% accounted for by other principal components.  Effectively, the most important difference amongst the data is which machine it was run on.
# (B) 
# The researcher is removing the "time/machine" factor from the data.  Perhaps instead the researcher could find the next n principal components that appear significant, based on identifying an elbow in the scree plot, and project the data onto those principal components, THEN perform a t-test.  This would be superior because in his initial idea he is still including the noise of small principal components in his t-test.  In the new idea not only is the 1st principal component removed, but also the noise.  He may also want to use ANOVA test first.
# (C)
set.seed(1)
genes <- data.frame(matrix(rnorm(1000*100), ncol=100))
genes[1,] <- seq(-15, 15, length=100)
pr.out <- prcomp(genes, scale=TRUE)
pve <- summary(pr.out)$importance[2,]
plot(pve)
# The first principal component accounts for almost 10% of variation.
# If we were to remove that from the data set (AKA create the same dataset without a linear trend) we will see a much small variation for the first principal component.
set.seed(1)
genes <- data.frame(matrix(rnorm(1000*100), ncol=100))
pr.out <- prcomp(genes, scale=TRUE)
pve <- summary(pr.out)$importance[2,]
plot(pve)
# This PVE is closer to 1.6% for the first principal component.  

## Q7
names(USArrests)
Eu.dist <- dist(scale(USArrests))^2
Cor.dist <- as.dist(1 - cor(t(scale(USArrests))))
Eu.dist / Cor.dist
summary(Eu.dist / Cor.dist)


## Q8
# (A)
USAscl <- scale(USArrests)
pr.out <- prcomp(USAscl)
sdev <- summary(pr.out)$sdev
pve1 <- sdev^2 / sum(sdev^2)
pve1
# (B)
phi <- pr.out$rotation
num <- apply((t(phi) %*% t(USAscl))^2, 1, sum)
denom <- sum(USAscl^2)
num/denom

## Q9
states <- rownames(USArrests)
# (A)
h.complete <- hclust(dist(USArrests), method="complete")
plot(h.complete)
# (B) 
cutree(h.complete, 3)
abline(h=130, col="red")
# (C)
USAscl <- scale(USArrests)
h.complete.scl <- hclust(dist(USAscl), method="complete")
plot(h.complete.scl)
cutree(h.complete, 3)
abline(h=4.3, col="red")
# (D)
par(mfrow=c(1, 2))
plot(h.complete)
abline(h=130, col="red")
plot(h.complete.scl)
abline(h=4.3, col="red")
# The effect of scaling causes the dissimilarities to be much lesser, evident from the scale.  Since each of the variables do not have necessarily similar frequencies and are not measured using the same units the variables should indeed be scaled.It provides a more accurate view of similarity between states arrest data.

## Q10
# (A)
par(mfrow=c(1, 1))
sim <- data.frame(matrix(rnorm(60*50), ncol=50))
sim[1:20,] <- sim[1:20,] + 3
sim[21:40,] <- sim[21:40,] - 3
clust <- c(rep(1, 20), rep(2, 20), rep(3, 20))
# (B)
pc.out <- prcomp(sim)
summary(pc.out)
pcvec <- pc.out$rotation[,1:2]
score1 <- t(pcvec[,1]) %*% t(sim)
score2 <- t(pcvec[,2]) %*% t(sim)
plot(score1, score2, col=(clust+1))
# (C)
km.out <- kmeans(sim, centers=3, nstart=20)
km.out$cluster
# The clusters correspond perfectly with the truth of the simulated clusters.  
table(clust, km.out$cluster)
# (D)
km.out <- kmeans(sim, centers=2, nstart=20)
km.out$cluster
# Now the 1st cluster is perfectly assigned and those that are ACTUALLY in the 2nd cluster are correctly assigned, but all points in cluster 3 have been assigned to cluster 2 out of necessity.  
table(clust, km.out$cluster)
# (E)
km.out <- kmeans(sim, centers=4, nstart=20)
km.out$cluster
table(clust, km.out$cluster)
# This time the 3rd cluster has been split into two separate clusters, while 1&2 are still fully clustered together.
# (F)
pc.score <- data.frame(t(score1), t(score2))
km.out <- kmeans(pc.score, centers=3, nstart=20)
km.out$cluster
table(clust, km.out$cluster)
# Again the clusters are properly assigned, and this time with only the first two principal components!  With only 2 of the 50 possible principal components the correct clustering was performed.  
# (G)
km.out <- kmeans(scale(sim), center=3, nstart=20)
km.out
table(clust, km.out$cluster)
# Even with scaled data the kmeans clusters them properly.  This should not be surprising as the initial data was drawn from a normal distribution of mean=0, sd=1 so it was scaled already.

## Q11
# (A)
ex11 <- read.csv("Ch10Ex11.csv", header=F)
fix(ex11)
dim(ex11)
# (B)
truth <- c(rep(1, 20), rep(2, 20))
genedist <- as.dist(1 - cor(ex11))
h.complete <- hclust(genedist, method="complete")
cutree(h.complete, 2)
plot(h.complete)
table(truth, cutree(h.complete, 2))

h.single <- hclust(genedist, method="single")
cutree(h.single, 2)
plot(h.single)
table(truth, cutree(h.single, 2))

h.ave <- hclust(genedist, method="average")
cutree(h.ave, 2)
plot(h.ave)
table(truth, cutree(h.ave, 2))

# The complete linkage performed best with a 75% true positive rate.  The average linkage performed second best, misclassifying only 1 extra observation compared to complete.

# (C)
# To determine which *tissue samples* differ the most we would examine the dendrograms and determine which genes have the greatest height difference.  
summary(h.complete)$height
which.max(h.complete$height)
which.min(h.complete$height)
# Thus the 1st and 39th sample are the most different in the complete linkage case. 

# However, to determine which GENES differ the most we could determine the Principal component score vectors for each gene and then compare them pairwise to determine which are the farthest away in euclidean space in the first n dimensions chosen.  
pc.out <- prcomp(t(ex11))
summary(pc.out)
pc.out$rotation
max(pc.out$rotation[,1])
which.max(pc.out$rotation[,1])
min(pc.out$rotation[,1])
which.min(pc.out$rotation[,1])
# So based on the 1st principal component, the 600th gene has the most impact and the 135th gene has the least impact.

# Alternatively, we could find the Euclidean distance between all genes pairwise and determine the maximum distance. 
diff <- dist(ex11)
max(diff)
which.max(diff)
str(diff)


