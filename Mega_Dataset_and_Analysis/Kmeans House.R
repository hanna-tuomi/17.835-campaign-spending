# Try KMeans

spending_by_cand <- read.csv('mega_dataset_house.csv')
#print(colnames(spending_by_cand))

# get the matrix of spending percentages
X = spending_by_cand[,5:15]
X[is.na(X)] <- 0

# find ideal number of clusters
clusters = c(2:15)
within_ss <- c()

for (k in clusters){
  k_mean_test <- kmeans(X, k, nstart=50)
  within_ss <- append(within_ss, k_mean_test$tot.withinss)
}

plot(clusters, within_ss, main='K clusters vs Within Cluster Sum of Squares', 
     xlab='Number of Clusters', ylab='Within Cluster SS')

# The ideal number of clusters seems to be 7
mod <- kmeans(X, 7, nstart=50)

# get the PCA for graphing 
scaled_X <- scale(X)
X_PCA <- prcomp(scaled_X)
screeplot(X_PCA, main='PCA for House Campaign Spending 2010-2014', xlab='Component')
biplot(X_PCA, cex=0.6, main='PCA Biplot for House Campaign Spending 2010-2014' )

demean <- function(xmat){
  apply(xmat, 2, function(z) z - mean(z))
}

demean_X <- demean(X)
SVD_X<- svd(demean_X)

# use lecture slides to get the scores per country
v1 <- matrix(SVD_X$v[,1], ncol=1) 
X_first_score <- demean_X%*%v1
X_first_score <- c(X_first_score[,1])
v2 <- matrix(SVD_X$v[,2], ncol=1) 
X_second_score <- demean_X%*%v2
X_second_score <- c(X_second_score[,1])

# add clusters + scores to X
cs <- mod$cluster
X$cluster <- cs
X <- cbind(X, X_first_score, X_second_score)

# plot the clusters
plot(X_first_score, X_second_score, col='white',
     main='House Campaign Spending Strategy Clustering 2010-2014',
     xlab='PCA First Score', ylab='PCA Second Score')
cluster_1 <- X[X$cluster==1,]
cluster_2 <- X[X$cluster==2,]
cluster_3 <- X[X$cluster==3,]
cluster_4 <- X[X$cluster==4,]
cluster_5 <- X[X$cluster==5,]
cluster_6 <- X[X$cluster==6,]
cluster_7 <- X[X$cluster==7,]


text(cluster_1$X_first_score, cluster_1$X_second_score, 
     labels=rep(1,length(cluster_1)), col="#66C2A5", cex=1)
text(cluster_2$X_first_score, cluster_2$X_second_score, 
     labels=rep(2,length(cluster_2)), col="#FC8D62", cex=1)
text(cluster_3$X_first_score, cluster_3$X_second_score, 
     labels=rep(3,length(cluster_3)), col="#8DA0CB", cex=1)
text(cluster_4$X_first_score, cluster_4$X_second_score, 
     labels=rep(4,length(cluster_4)), col="#E78AC3", cex=1)
text(cluster_5$X_first_score, cluster_5$X_second_score, 
     labels=rep(5,length(cluster_5)), col="#A6D854", cex=1)
text(cluster_6$X_first_score, cluster_6$X_second_score, 
     labels=rep(6,length(cluster_6)), col="#FFD92F", cex=1)
text(cluster_7$X_first_score, cluster_7$X_second_score, 
     labels=rep(7,length(cluster_7)), col="#E5C494", cex=1)


spending_by_cand$cluster = cs
spending_by_cand$cluster.1 = replace(cs, which(cs != 1), 0)
clus.2 <- replace(cs, which(cs == 2), 1)
spending_by_cand$cluster.2 = replace(clus.2, which(cs != 2), 0)
clus.3 <- replace(cs, which(cs == 3), 1)
spending_by_cand$cluster.3 = replace(clus.3, which(cs != 3), 0)
clus.4 <- replace(cs, which(cs == 4), 1)
spending_by_cand$cluster.4 = replace(clus.4, which(cs != 4), 0)
clus.5 <- replace(cs, which(cs == 5), 1)
spending_by_cand$cluster.5 = replace(clus.5, which(cs != 5), 0)
clus.6 <- replace(cs, which(cs == 6), 1)
spending_by_cand$cluster.6 = replace(clus.6, which(cs != 6), 0)
clus.7 <- replace(cs, which(cs == 7), 1)
spending_by_cand$cluster.7 = replace(clus.7, which(cs != 7), 0)

write.csv(spending_by_cand,'mega_data_with_clusters.csv')



