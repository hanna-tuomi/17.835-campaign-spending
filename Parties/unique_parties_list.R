# Script to get all the unique party codes from all 4 elections

setwd('~/Desktop/17.835-campaign-spending/Parties')

parties2008 <- read.csv('parties08.csv')
parties2008 <- parties2008[5:nrow(parties2008),]
parties2008 <- subset(parties2008, select = -c(X, X.2))

parties2010 <- read.csv('parties10.csv')
parties2010 <- parties2010[5:nrow(parties2010),]
parties2010 <- subset(parties2010, select = -c(X, X.2))

parties2012 <- read.csv('parties12.csv')
parties2012 <- parties2012[5:nrow(parties2012),]
parties2012 <- subset(parties2012, select = -c(X, X.2, X.3, X.4, X.5, X.6, X.7, X.8,
                                               X.9, X.10, X.11, X.12, X.13, X.14, X.15,
                                               X.16, X.17))

parties2014 <- read.csv('parties14.csv')
parties2014 <- parties2014[5:nrow(parties2014),]
parties2014 <- subset(parties2014, select = -c(X, X.2, X.3, X.4, X.5, X.6, X.7, X.8,
                                               X.9, X.10, X.11, X.12, X.13, X.14, X.15,
                                               X.16, X.17, X.18, X.19, X.20, X.21, X.22,
                                               X.23, X.24, X.25))

parties <- rbind(parties2008, parties2010, parties2012, parties2014)
parties <- unique(parties)
write.csv(parties,'~/Desktop/17.835-campaign-spending/Parties/unique_party_list.csv')


  
  
  
  