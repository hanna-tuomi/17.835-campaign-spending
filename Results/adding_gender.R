# Add a column for women on the campaign data

women.2008 <- read.csv('Women/2008_women.csv')
colnames(women.2008) <- women.2008[1,]
senate <- which(women.2008$Office=="U.S. Senator")
women.2008$District[senate] <- 'S'
women.2008$woman = 1
women.2008 = women.2008[,c(1,6,13,14)]
colnames(women.2008)[1] <- 'STATE.ABBREVIATION'
colnames(women.2008)[2] <- 'candidate.last'
colnames(women.2008)[3] <- 'year'

women.2010 <- read.csv('Women/2010_women.csv')
colnames(women.2010) <- women.2010[1,]
senate <- which(women.2010$Office=="U.S. Senator")
women.2010$District[senate] <- 'S'
women.2010$woman = 1
women.2010 = women.2010[,c(1,6,13,14)]
colnames(women.2010)[1] <- 'STATE.ABBREVIATION'
colnames(women.2010)[2] <- 'candidate.last'
colnames(women.2010)[3] <- 'year'

women.2012 <- read.csv('Women/2012_women.csv')
colnames(women.2012) <- women.2012[1,]
senate <- which(women.2012$Office=="U.S. Senator")
women.2012$District[senate] <- 'S'
women.2012$woman = 1
women.2012 = women.2012[,c(1,6,13,14)]
colnames(women.2012)[1] <- 'STATE.ABBREVIATION'
colnames(women.2012)[2] <- 'candidate.last'
colnames(women.2012)[3] <- 'year'

women.2014 <- read.csv('Women/2014_women.csv')
colnames(women.2014) <- women.2014[1,]
senate <- which(women.2014$Office=="U.S. Senator")
women.2014$District[senate] <- 'S'
women.2014$woman = 1
women.2014 = women.2014[,c(1,6,13,14)]
colnames(women.2014)[1] <- 'STATE.ABBREVIATION'
colnames(women.2014)[2] <- 'candidate.last'
colnames(women.2014)[3] <- 'year'

results.2008 <- read.csv('Clean/results08.csv')
results.2010 <- read.csv('Clean/results10.csv')
results.2012 <- read.csv('Clean/results12.csv')
results.2014 <- read.csv('Clean/results14.csv')
all.results <- read.csv('Clean/results_all.csv')

all_women <- rbind(women.2008, women.2010, women.2012, women.2014)
cleaned_women <- all_women[all_women$candidate.last != '' & all_women$year != 'Year',]
cleaned_results <- all.results[all.results$STATE != '',]
    
added_women <- merge(cleaned_results, cleaned_women, by=c('STATE.ABBREVIATION', 
                          'candidate.last','year'), all.x = TRUE)
added_women$woman[is.na(added_women$woman)] <- 0  
write.csv(added_women,'Clean/women_allresults.csv')

