# Defining the clusters
library(ggplot2)
clusters <- read.csv('mega_data_with_clusters.csv')

# error bars 
library('matrixStats')

overall.spending <- colMeans(clusters[,6:16])
sd.spending <- colSds(as.matrix(clusters[,6:16]))

overall.perc.incumbent<- nrow(clusters[clusters$incumbent == '(I)',])/nrow(clusters)
is <- nrow(clusters[clusters$incumbent == '(I)',])
sd.perc.incumbent <- sqrt(((is-overall.perc.incumbent)^2 + 
                             ((nrow(clusters)-is)-overall.perc.incumbent)^2)/(nrow(clusters)-1))

overall.perc.D <- nrow(clusters[clusters$PARTY == 'D',])/nrow(clusters)
Ds <- nrow(clusters[clusters$PARTY == 'D',])
sd.perc.D <-sqrt(((Ds-overall.perc.D)^2 + 
                    ((nrow(clusters)-Ds)-overall.perc.D)^2)/(nrow(clusters)-1))

overall.perc.unopposed.primary <- nrow(clusters[clusters$PRIMARY == 'Unopposed',])/nrow(clusters)
unopposed <- nrow(clusters[clusters$PRIMARY == 'Unopposed',])
sd.perc.unopposed.primary <- sqrt(((unopposed-overall.perc.unopposed.primary)^2 + 
                                     ((nrow(clusters)-unopposed)-overall.perc.unopposed.primary)^2)/(nrow(clusters)-1))

overall.perc.woman <- mean(clusters$woman)
women <- nrow(clusters[clusters$woman == 1,])
sd.perc.woman <- sqrt(((women-overall.perc.woman)^2 + ((nrow(clusters)-women)-overall.perc.woman)^2)/(nrow(clusters)-1))

overall.demo.means <- colMeans(clusters[,39:63])
sd.demo.means <- colSds(as.matrix(clusters[,39:63]))

columns <- c('Cluster', colnames(clusters[,6:16]), 'perc.incumbent', 'perc.D', 'perc.prim.unopposed', 
             'perc.woman', colnames(clusters[,39:63]))
summaries <- c('Overall',overall.spending, overall.perc.incumbent, overall.perc.D, overall.perc.unopposed.primary,
               overall.perc.woman, overall.demo.means)

summary <- rbind(columns, summaries) 
colnames(summary) <- columns

sd.vector <- c('Overall', sd.spending, sd.perc.incumbent, sd.perc.D, sd.perc.unopposed.primary,
               sd.perc.woman, sd.demo.means)
sds<- rbind(columns, sd.vector)
sds <- as.data.frame(sds[2,2:41])

for (i in 1:7){
  cluster <- clusters[clusters$cluster ==i,]
  spending.averages <- colMeans(cluster[,6:16])
  perc.incumbent <- nrow(cluster[cluster$incumbent == '(I)',])/nrow(cluster)
  perc.D <- nrow(cluster[cluster$PARTY == 'D',])/nrow(cluster)
  perc.unopposed.primary <- nrow(cluster[cluster$PRIMARY == 'Unopposed',])/nrow(cluster)
  perc.woman <- mean(cluster$woman)
  demo.means <- colMeans(cluster[,39:63])
  new <- c(i,spending.averages, perc.incumbent, perc.D, perc.unopposed.primary,
                 perc.woman, demo.means)
  summary <- rbind(summary, new)
}

summary <- as.data.frame(summary[2:9,2:ncol(summary)])



# plotting the differences
distance_to_means <- data.frame()
for(i in 2:8) {
  distance_to_means <- rbind(distance_to_means, as.numeric(summary[i,]) - as.numeric(summary[1,]))
}
colnames(distance_to_means) <- colnames(summary)
rownames(distance_to_means) <- rownames(summary)[2:8]

var <- labels(distance_to_means)
var <- as.character(var)

# plot the differences to the mean
par(las=1, mar=c(5,10,5,3))
plot(as.matrix(distance_to_means[1,]),c(1:40), col='red', yaxt='n', pch=15,
     ylab='', xlab="Difference in Cluster to Overall Mean")
title(ylab="Variable", mgp=c(11, 0, 0))
axis(2, at=1:40, labels=colnames(distance_to_means), cex.axis=1)
points(as.matrix(distance_to_means[5,]),c(1:40), col='blue', pch=16)
points(as.matrix(distance_to_means[6,]), c(1:40), col='purple', pch=17)

abline(v=0)
abline(h=11.5, col='black')
abline(h=15.5, col='black')
legend(0.25,35, legend=c('Strategy 1', 'Strategy 5', 'Strategy 6'), 
       col=c('red', 'blue', 'purple'), pch=c(15, 16,17), cex=0.5)


for (i in 1:40){
  s <- as.numeric(sds[i,])
  arrows(x0=-s, y0=i, x1=s, y1=i, code=3, col="darkgray", lwd=0.3, angle=90, length=0.025)
}


