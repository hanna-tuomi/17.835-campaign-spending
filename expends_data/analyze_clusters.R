# analyze the clusters

cluster.data <- read.csv('spending clusters.csv')

#cluster 1
cluster.1 <- cluster.data[cluster.data$cluster ==1,]
print(nrow(cluster.1))
print(summary(cluster.1[,4:14]))


#cluster 3
cluster.3 <- cluster.data[cluster.data$cluster ==3,]
print(nrow(cluster.3))
print(summary(cluster.3[,4:14]))


#cluster 5
cluster.5 <- cluster.data[cluster.data$cluster ==5,]
print(nrow(cluster.5))
print(summary(cluster.5[,4:14]))


