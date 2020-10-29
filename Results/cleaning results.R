# script to clean federal election results

setwd('~/Desktop/17.835-campaign-spending/Results')

results.2008 <- read.csv('results08.csv')
results.2010 <- read.csv('results10.csv')
results.2012 <- read.csv('results12.csv')
results.2014.house <- read.csv('results14 house.csv')
results.2014.senate <- read.csv('results14 senate.csv')

#get rid of summary rows
results.2008 <- results.2008[results.2008$FEC.ID. != 'n/a',]
results.2008$year <- '2008'

names(results.2008)[6] = 'incumbent'
names(results.2008)[7] = 'candidate.first'
names(results.2008)[8] = 'candidate.last'
names(results.2008)[9] = 'candidate.name'
names(results.2008)[13] = 'primary.perc'
names(results.2008)[15] = 'runoff.perc'
names(results.2008)[17] = 'GE.perc'

results.2008.combine <- subset(results.2008, select=-c(18,19,20,21,22))
write.csv(results.2008,'~/Desktop/17.835-campaign-spending/Results/Clean/results08.csv')

# Standardize the main parties
results.2010 <- results.2010[results.2010$FEC.ID. != 'n/a',]
results.2010[results.2010$PARTY == 'REP',]$PARTY <- 'R'
results.2010[results.2010$PARTY == 'DEM',]$PARTY <- 'D'
results.2010$year <- '2010'

names(results.2010)[6] = 'incumbent'
names(results.2010)[7] = 'candidate.first'
names(results.2010)[8] = 'candidate.last'
names(results.2010)[9] = 'candidate.name'
names(results.2010)[13] = 'primary.perc'
names(results.2010)[15] = 'runoff.perc'
names(results.2010)[17] = 'GE.perc'

results.2010.combine <- subset(results.2010, select=-c(18,19,20,21,22,23))
write.csv(results.2010,'~/Desktop/17.835-campaign-spending/Results/Clean/results10.csv')

results.2012 <- results.2012[results.2012$FEC.ID. != 'n/a',]
results.2012$year <- '2012'

names(results.2012)[4] = 'DISTRICT'
names(results.2012)[6] = 'incumbent'
names(results.2012)[7] = 'candidate.first'
names(results.2012)[8] = 'candidate.last'
names(results.2012)[9] = 'candidate.name'
names(results.2012)[12] = 'PRIMARY'
names(results.2012)[13] = 'primary.perc'
names(results.2012)[14] = 'RUNOFF'
names(results.2012)[15] = 'runoff.perc'
names(results.2012)[16] = 'GENERAL'
names(results.2012)[17] = 'GE.perc'

results.2012.combine <- subset(results.2012, select=-c(18,19,20,21,22,23,24))
write.csv(results.2012,'~/Desktop/17.835-campaign-spending/Results/Clean/results12.csv')

results.2014.senate$D <- 'S'
results.2014 <- rbind(results.2014.house, results.2014.senate)
results.2014 <- results.2014[results.2014$FEC.ID. != 'n/a',]
results.2014$year <- '2014'

names(results.2014)[4] = 'DISTRICT'
names(results.2014)[6] = 'incumbent'
names(results.2014)[7] = 'candidate.first'
names(results.2014)[8] = 'candidate.last'
names(results.2014)[9] = 'candidate.name'
names(results.2014)[12] = 'PRIMARY'
names(results.2014)[13] = 'primary.perc'
names(results.2014)[14] = 'RUNOFF'
names(results.2014)[15] = 'runoff.perc'
names(results.2014)[16] = 'GENERAL'
names(results.2014)[17] = 'GE.perc'

results.2014.combine <- subset(results.2014, select=-c(18,19,20,21,22,23,24,25))
write.csv(results.2014,'~/Desktop/17.835-campaign-spending/Results/Clean/results14.csv')

federal.results <- rbind(results.2008.combine, results.2010.combine, 
                         results.2012.combine, results.2014.combine)
write.csv(federal.results,'~/Desktop/17.835-campaign-spending/Results/Clean/results_all.csv')


