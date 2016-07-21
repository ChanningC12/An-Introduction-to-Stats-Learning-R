# Supervised Learning: The goal is to predict Y using X1,X2,...,Xp
# Unsupervised Learning: The goal is to discover interesting things about the measurements on X1,X2,...,Xp
# Principal Components Analysis and Clustering

# PCA on the USArrests data set
states = row.names(USArrests)
states
names(USArrests)
View(USArrests)
# examine the data
apply(USArrests,2,mean) # 2 means column 2, which is Murder
apply(USArrests,2,var)
# standardize before performing PCA
pr.out = prcomp(USArrests,scale=T) # by default, prcomp() function centers the variables to have mean zero. scale = T to make variables to have sd 1.
names(pr.out)
# center and scale components correspond to the means and standard deviations of the variables that were used for scaling prior to implementing PCA
pr.out$center
pr.out$scale
# the rotation matrix provides the principal components loading
pr.out$rotation
# four distinct components shown
dim(pr.out$x)
# plot the first two principal components
biplot(pr.out,scale=0) # scale = 0 ensures that the arrows are scaled to represent the loadings

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out,scale=0)

# prcomp() also outputs the standard deviation of each principal components
pr.out$sdev
# variance
pr.var = pr.out$sdev^2
pr.var

# compute the proportion of variance explained by each principal component
pve = pr.var / sum(pr.var)
round(pve,2)
# plot PVE
plot(pve,xlab="Principal Component", ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type='b')



# Clustering
# K-Means, function kmeans()
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]+4
km.out = kmeans(x,2,nstart=20)
km.out$cluster
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

# K=3
set.seed(4)
km.out = kmeans(x,3,nstart=20)
km.out
plot(x,col=(km.out$cluster+1),main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)
# recommend always running K-means clustering with a large value of nstart to minimize within cluster sum of squares

# Hierarchical Clustering: hclust()
# dist() function is used to compute the 50*50 inter-observation Euclidean matrix
hc.complete = hclust(dist(x),method="complete")
hc.average = hclust(dist(x),method="average")
hc.single = hclust(dist(x),method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",ylab="")
plot(hc.average,main="Average Linkage",xlab="",ylab="")
plot(hc.single,main="Single Linkage",xlab="",ylab="")
# To determine the cluster labels for each observation associated with a given cut of the dendrogram, we can use the cutree() function
cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)

cutree(hc.single,4)

# To scale variables before performing hierarchical clusering, use scale()
xsc = scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled Features")

# correlation-based distance can be computed using the as.dist() function, which converts an arbitraty square sysmmetric matrix into a form that the hclust() function recognizes as a distance matrix
x = matrix(rnorm(30*3),ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="Complete Linkage with Correlation-Based Distance",xlab="",sub="")







