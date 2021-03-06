
# clustering approach

dem.clusters <- clusters[,27:63]
dem.clusters[dem.clusters$incumbent == '(I)',]$incumbent  <- 1
dem.clusters[dem.clusters$incumbent == '',]$incumbent <- 0
dem.clusters[dem.clusters$PARTY == 'D',]$PARTY <- 1
dem.clusters[dem.clusters$PARTY == 'R',]$PARTY <- 0
dem.clusters[dem.clusters$PARTY != 1 & dem.clusters$PARTY != 0 ,]$PARTY <- 2
dem.clusters[dem.clusters$GE.perc == '',]$GE.perc <- 0
dem.clusters <- subset(dem.clusters, select = -c(2,3,4,6,8,9,10))
dem.clusters <- subset(dem.clusters, select = -c(3,4))

for (c in 1:ncol(dem.clusters)){
  dem.clusters[,c] <- as.numeric(gsub(x=dem.clusters[,c], pattern='%', replacement=''))
}

dem.clusters[is.na(dem.clusters)]<-0

# find ideal number of clusters
cs = c(2:15)
within_ss <- c()

for (k in cs){
  k_mean_test <- kmeans(dem.clusters, k)
  within_ss <- append(within_ss, k_mean_test$tot.withinss)
}

plot(cs, within_ss, main='K clusters vs Within Cluster Sum of Squares', 
     xlab='Number of Clusters', ylab='Within Cluster SS')

mod <- kmeans(dem.clusters, 7, nstart=50)

# get the PCA for graphing 
scaled_X <- scale(dem.clusters)
X_PCA <- prcomp(scaled_X)
screeplot(X_PCA, main='PCA for House Race Dem. 2010-2014', xlab='Component')
biplot(X_PCA, cex=0.6, main='PCA Biplot for House House Race Dem. 2010-2014' )

demean <- function(xmat){
  apply(xmat, 2, function(z) z - mean(z))
}

demean_X <- demean(dem.clusters)
SVD_X<- svd(demean_X)

# use lecture slides to get the scores per country
v1 <- matrix(SVD_X$v[,1], ncol=1) 
X_first_score <- demean_X%*%v1
X_first_score <- c(X_first_score[,1])
v2 <- matrix(SVD_X$v[,2], ncol=1) 
X_second_score <- demean_X%*%v2
X_second_score <- c(X_second_score[,1])

# add clusters + scores to X
CS <- mod$cluster
dem.clusters$cluster <- CS
dem.clusters<- cbind(dem.clusters, X_first_score, X_second_score)

# plot the clusters
plot(X_first_score, X_second_score, col='white',
     main='House Campaign Dem. Clustering 2010-2014',
     xlab='PCA First Score', ylab='PCA Second Score')
cluster_1 <- dem.clusters[clusters$cluster==1,]
cluster_2 <- dem.clusters[clusters$cluster==2,]
cluster_3 <- dem.clusters[clusters$cluster==3,]
cluster_4 <- dem.clusters[clusters$cluster==4,]
cluster_5 <- dem.clusters[clusters$cluster==5,]
cluster_6 <- dem.clusters[clusters$cluster==6,]
cluster_7 <- dem.clusters[clusters$cluster==7,]


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




