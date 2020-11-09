# Try Kmenas

spending_by_cand <- read.csv('expends_data/spending_by_candidate_and_category.csv')

X = spending_by_cand[,4:10]
X[is.na(X)] <- 0

# find ideal number of clusters
clusters = c(2:15)
within_ss <- c()

for (k in clusters){
  k_mean_test <- kmeans(X, k, nstart=50)
  within_ss <- append(within_ss, k_mean_test$tot.withinss)
}

plot(clusters, within_ss, main='K clusters vs Within Cluster Sum of Squares')

# get the ideal number of clusters
mod <- kmeans(X, 7, nstart=50)
cs <- mod$cluster
X$cluster <- cs

# get the PCA for graphing 
scaled_X <- scale(X)
X_PCA <- prcomp(scaled_X)
screeplot(X_PCA, main='PCA for Campaign Spending')
biplot(X_PCA, cex=0.6)

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

X <- cbind(X, X_first_score, X_second_score)

plot(X_first_score, X_second_score, col='white')
cluster_1 <- X[X$cluster==1,]
cluster_2 <- X[X$cluster==2,]
cluster_3 <- X[X$cluster==3,]
cluster_4 <- X[X$cluster==4,]
cluster_5 <- X[X$cluster==5,]
cluster_6 <- X[X$cluster==6,]
cluster_7 <- X[X$cluster==7,]



text(cluster_1$X_first_score, cluster_1$X_second_score, 
     labels=rep(1,length(cluster_1)), col='blue', cex=0.7)
text(cluster_2$X_first_score, cluster_2$X_second_score, 
     labels=rep(2,length(cluster_2)), col='red', cex=0.7)
text(cluster_3$X_first_score, cluster_3$X_second_score, 
     labels=rep(3,length(cluster_3)), col='purple', cex=0.7)
text(cluster_4$X_first_score, cluster_4$X_second_score, 
     labels=rep(4,length(cluster_4)), col='yellow', cex=0.7)
text(cluster_5$X_first_score, cluster_5$X_second_score, 
     labels=rep(5,length(cluster_5)), col='gray', cex=0.7)
text(cluster_6$X_first_score, cluster_6$X_second_score, 
     labels=rep(6,length(cluster_6)), col='green', cex=0.7)
text(cluster_7$X_first_score, cluster_7$X_second_score, 
     labels=rep(7,length(cluster_7)), col='orange', cex=0.7)




  




