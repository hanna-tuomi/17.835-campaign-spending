setwd("~/Desktop/17.835-campaign-spending/Mega_Dataset_and_Analysis")
library(MatchIt)

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

# Data match based on district demographics + candidate attributes
cluster1.match <- matchit(cluster.1 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster1 <- summary(cluster1.match)
plot(sum.cluster1,
     cex=0.5)



cluster2.match <- matchit(cluster.2 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster2 <- summary(cluster2.match)
plot(sum.cluster2,
     cex=0.5)


cluster3.match <- matchit(cluster.3 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster3 <- summary(cluster3.match)
plot(sum.cluster3,
     cex=0.5)


cluster4.match <- matchit(cluster.4 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster4 <- summary(cluster4.match)
plot(sum.cluster4,
     cex=0.5)


cluster5.match <- matchit(cluster.5 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster5 <- summary(cluster5.match)
plot(sum.cluster5,
     cex=0.5)


cluster6.match <- matchit(cluster.6 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster6 <- summary(cluster6.match)
plot(sum.cluster6,
     cex=0.5)


cluster7.match <- matchit(cluster.7 ~ Year + incumbent + woman + PARTY + Total.population + 
                            X18.years.and.over + Under.20 + X20.to.44.years + X45.to.64.years + Over.65 + Median.age..years. + 
                            White + Black.or.African.American + American.Indian.and.Alaska.Native + Asian + Native.Hawaiian.and.Other.Pacific.Islander + Some.other.race + Two.or.more.races + Hispanic.or.Latino..of.any.race. + 
                            Born.in.United.States + Foreign.born +
                            Percent.Over.16.In.labor.force + Unemployment.Rate + Median..dollars. + Median.household.income..dollars. + Mean.household.income..dollars. + 
                            All.people + Percent.high.school.graduate.or.higher + Percent.bachelor.s.degree.or.higher, data = dataset)
sum.cluster7 <- summary(cluster7.match)
plot(sum.cluster7,
     cex=0.5)



