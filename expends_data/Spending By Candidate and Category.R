#kmeans clustering attempt

expends <- read.csv('expends_data/expenditure_data_new.csv')
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

codes <- unique(t_2008$Group.2)

cand_2008 <- unique(t_2008$Group.1)
cand_2010 <- unique(t_2010$Group.1)
cand_2012 <- unique(t_2012$Group.1)
cand_2014 <- unique(t_2014$Group.1)

spending_by_cand <- data.frame(Year=character(),
                               Candidate = character(),
                               C10=numeric(), 
                               M01=numeric(),
                               M10=numeric(),
                               M20=numeric(),
                               M30=numeric(),
                               M40=numeric(),
                               M50=numeric(),
                               S10=numeric(),
                               S20=numeric(),
                               S50=numeric(),
                               R10=numeric())

for (c in cand_2008){
  spending <- t_2008[t_2008$Group.1==c,]
  cand_codes <- unique(spending$Group.2)
  line_insert <- c('2008', c)
  for (code in codes){
    if (code %in% cand_codes){
      line_insert <- append(line_insert, spending[spending$Group.2 == code,]$candavgs)
    } else{
      line_insert <- append(line_insert, 0)
    }
  }
  spending_by_cand[nrow(spending_by_cand)+1,] <- line_insert
}

for (c in cand_2010){
  spending <- t_2010[t_2010$Group.1==c,]
  cand_codes <- unique(spending$Group.2)
  line_insert <- c('2010', c)
  for (code in codes){
    if (code %in% cand_codes){
      line_insert <- append(line_insert, spending[spending$Group.2 == code,]$candavgs)
    } else{
      line_insert <- append(line_insert, 0)
    }
  }
  spending_by_cand[nrow(spending_by_cand)+1,] <- line_insert
}

for (c in cand_2012){
  spending <- t_2012[t_2012$Group.1==c,]
  cand_codes <- unique(spending$Group.2)
  line_insert <- c('2012', c)
  for (code in codes){
    if (code %in% cand_codes){
      line_insert <- append(line_insert, spending[spending$Group.2 == code,]$candavgs)
    } else{
      line_insert <- append(line_insert, 0)
    }
  }
  spending_by_cand[nrow(spending_by_cand)+1,] <- line_insert
}

for (c in cand_2014){
  spending <- t_2014[t_2014$Group.1==c,]
  cand_codes <- unique(spending$Group.2)
  line_insert <- c('2014', c)
  for (code in codes){
    if (code %in% cand_codes){
      line_insert <- append(line_insert, spending[spending$Group.2 == code,]$candavgs)
    } else{
      line_insert <- append(line_insert, 0)
    }
  }
  spending_by_cand[nrow(spending_by_cand)+1,] <- line_insert
}

# only needed to run once
write.csv(spending_by_cand,'expends_data/spending_by_candidate_and_category_new.csv')


