setwd("~/Desktop/17.835-campaign-spending/Mega_Dataset_and_Analysis")
library(MatchIt)
library(stringr)


dataset <- read.csv("mega_data_with_clusters.csv")
?MatchIt

match.cols <- dataset[, c("incumbent", "woman", "PARTY",
                          "Total.population",
                          "X18.years.and.over", "Under.20", "X20.to.44.years", "X45.to.64.years", "Over.65", "Median.age..years.",
                          "White", "Black.or.African.American", "American.Indian.and.Alaska.Native", "Asian", "Native.Hawaiian.and.Other.Pacific.Islander", "Some.other.race", "Two.or.more.races", "Hispanic.or.Latino..of.any.race.",
                          "Born.in.United.States", "Foreign.born",
                          "Percent.Over.16.In.labor.force", "Unemployment.Rate", "Median..dollars.", "Median.household.income..dollars.", "Mean.household.income..dollars.",
                          "All.people", "Percent.high.school.graduate.or.higher", "Percent.bachelor.s.degree.or.higher")]


for(i in 1:length(dataset$PARTY)){
  if((grepl("D", dataset$PARTY[i]) & !grepl("I", dataset$PARTY[i])) | grepl("Combined Parties", dataset$PARTY[i])) {
    dataset$PARTY[i] <- "D"
  }
  if(grepl("R", dataset$PARTY[i]) & !grepl("CRV", dataset$PARTY[i])) {
    dataset$PARTY[i] <- "R"
  }
  if(dataset$PARTY[i] != "D" & dataset$PARTY[i] != "R") {
    dataset$PARTY[i] <- "I"
  }
}
unique(dataset$PARTY)

# NOTE: MatchItSE was unavailable, but here is the code for the att() function that was on it
att <- function(obj, Y){
  stopifnot(methods::is(obj, "matchit"))
  ww <- obj$weights
  tt <- obj$treat
  mut <- stats::weighted.mean(Y[ww > 0 & tt == 1], ww[ww > 0 & tt == 1])
  muc <- stats::weighted.mean(Y[ww > 0 & tt == 0], ww[ww > 0 & tt == 0])
  mut - muc
}

as.numeric(substr(dataset$GE.perc[1], 1, 5))
as.numeric(63.60)
which(strsplit(dataset$GE.perc[1], "")[[1]]=="%")
dataset$GE.perc[1]

# Iterates through the General Election voteshares and stores in a list - if no GE is available, then uses the primary percentage
# Gets a consolidated lists of voteshares
voteshares <- c(1:665)
for(i in 1:655) {
  if(dataset$GE.perc[i] != "") {
    perc_index <- which(strsplit(dataset$GE.perc[i], "")[[1]]=="%")
    voteshares[i] <- substr(dataset$GE.perc[i], 1, perc_index-1)
  }
  else if(dataset$primary.perc[i] != "") {
    perc_index <- which(strsplit(dataset$primary.perc[i], "")[[1]]=="%")
    voteshares[i] <- substr(dataset$primary.perc[i], 1, perc_index-1)
  }
  else {
    voteshares[i] <- 0
  }
}

voteshares <- as.numeric(voteshares)

# Data match based on district demographics + candidate attributes
cluster1.match <- matchit(cluster.1 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster1 <- summary(cluster1.match)
plot(sum.cluster1,
     cex=0.5, main='Cluster 1')
c1.data <- match.data(cluster1.match)
dataset$GE.perc <- as.numeric(dataset$GE.perc)


cluster2.match <- matchit(cluster.2 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster2 <- summary(cluster2.match)
plot(sum.cluster2,
     cex=0.5, main='Cluster 2')


cluster3.match <- matchit(cluster.3 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster3 <- summary(cluster3.match)
plot(sum.cluster3,
     cex=0.5, main='Cluster 3')


cluster4.match <- matchit(cluster.4 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster4 <- summary(cluster4.match)
plot(sum.cluster4,
     cex=0.5, main='Cluster 4')


cluster5.match <- matchit(cluster.5 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster5 <- summary(cluster5.match)
plot(sum.cluster5,
     cex=0.5, main='Cluster 5')

cluster6.match <- matchit(cluster.6 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster6 <- summary(cluster6.match)
plot(sum.cluster6,
     cex=0.5, main='Cluster 6')

cluster7.match <- matchit(cluster.7 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster7 <- summary(cluster7.match)
plot(sum.cluster7,
     cex=0.5, main='Cluster 7')


cat('ATT cluster 1:', att(cluster1.match, Y=voteshares), '\n')
cat('ATT cluster 2:',att(cluster2.match, Y=voteshares), '\n')
cat('ATT cluster 3:',att(cluster3.match, Y=voteshares), '\n')
cat('ATT cluster 4:',att(cluster4.match, Y=voteshares), '\n')
cat('ATT cluster 5:',att(cluster5.match, Y=voteshares), '\n')
cat('ATT cluster 6:',att(cluster6.match, Y=voteshares), '\n')
cat('ATT cluster 7:',att(cluster7.match, Y=voteshares), '\n')

