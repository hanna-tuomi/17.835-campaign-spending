
expends <- read.csv("expenditure_data.csv")
selected <- expends[,c("Candid","Amount","Cycle","Expcode")]
aggregate(selected$Amount, by=list(selected$Candid), FUN=sum)
table <- aggregate(selected$Amount, by=list(selected$Candid, selected$Expcode, selected$Cycle), FUN=sum)
t_2008 <- table[table$Group.3=="2008",]
t_2010 <- table[table$Group.3=="2010",]
t_2012 <- table[table$Group.3=="2012",]
t_2014 <- table[table$Group.3=="2014",]
t_2008_sums <- aggregate(t_2008$x, by=list(t_2008$Group.1), FUN=sum )
t_2010_sums <- aggregate(t_2010$x, by=list(t_2010$Group.1), FUN=sum )
t_2012_sums <- aggregate(t_2012$x, by=list(t_2012$Group.1), FUN=sum )
t_2014_sums <- aggregate(t_2014$x, by=list(t_2014$Group.1), FUN=sum )
new = function (x,y) {
  lst=c()
  for (num in 1:NROW(x)){
    avg= x[num,"x"]/y[y$Group.1==x[num,"Group.1"], "x"]
    
    lst <- append(lst, avg)
    avg
  }
  return(lst)
}
t_2008$candavgs <- new(t_2008, t_2008_sums)
t_2010$candavgs <- new(t_2010, t_2010_sums)
t_2012$candavgs <- new(t_2012, t_2012_sums)
t_2014$candavgs <- new(t_2014, t_2014_sums)
mean2 <- function(x){
  return(mean(x, na.rm=TRUE))
}
props_2008 <- aggregate(t_2008$candavgs, by = list(t_2008$Group.2), FUN=mean2)
props_2010 <- aggregate(t_2010$candavgs, by = list(t_2010$Group.2), FUN=mean2)
props_2012 <- aggregate(t_2012$candavgs, by = list(t_2012$Group.2), FUN=mean2)
props_2014 <- aggregate(t_2014$candavgs, by = list(t_2014$Group.2), FUN=mean2)

x_axis <- c(2008,2008,2008,2008,2008,2008,2008, 2010,2010,2010,2010,2010,2010,2010,2012,2012,2012,2012,2012,2012,2012,2014,2014,2014,2014,2014,2014,2014)
props_2008
props_2010
props_2012
props_2014
plot(x_axis, c(props_2008$x, props_2010$x,props_2012$x,props_2014$x),main="Average Candidate Spending by Category over Time", xlab="Year",ylab="Percent Spent on Category")
lines(c(2008,2010,2012,2014),c(props_2008[1,"x"],props_2010[1,"x"],props_2012[1,"x"],props_2014[1,"x"]), col="Red")
lines(c(2008,2010,2012,2014),c(props_2008[2,"x"],props_2010[2,"x"],props_2012[2,"x"],props_2014[2,"x"]), col="Orange")
lines(c(2008,2010,2012,2014),c(props_2008[3,"x"],props_2010[3,"x"],props_2012[3,"x"],props_2014[3,"x"]), col="Yellow")
lines(c(2008,2010,2012,2014),c(props_2008[4,"x"],props_2010[4,"x"],props_2012[4,"x"],props_2014[4,"x"]), col="Green")
lines(c(2008,2010,2012,2014),c(props_2008[5,"x"],props_2010[5,"x"],props_2012[5,"x"],props_2014[5,"x"]), col="Blue")
lines(c(2008,2010,2012,2014),c(props_2008[6,"x"],props_2010[6,"x"],props_2012[6,"x"],props_2014[6,"x"]), col="Purple")
lines(c(2008,2010,2012,2014),c(props_2008[7,"x"],props_2010[7,"x"],props_2012[7,"x"],props_2014[7,"x"]), col="Brown")
cols=c("Red","Orange","Yellow",'Green',"Blue","Purple","Brown")
legend("topright",title="Categories", c("Campaing Mailings/Materials","Media Buys","Broadcast Ads","Print Ads","Web Ads","Media Production","Media Consulting"), fill=cols)
t_2008[t_2008$Group.2=="M30",]

