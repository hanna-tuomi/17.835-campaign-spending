# Create figure with total spending and and vote share 

setwd('~/Desktop/17.835-campaign-spending')
spending <- read.csv('expends_data/expenditure_data.csv')
results <- read.csv('Results/Clean/women_allresults.csv')

spending_subset <- subset(spending, select=c(Cycle, Amount, Candid))
spending_aggregate <- aggregate(x=spending$Amount, 
                      FUN=sum, by=list(spending$Cycle, spending$Candid))
colnames(spending_aggregate)[1] = 'year'
colnames(spending_aggregate)[2] = 'FEC.ID.'
colnames(spending_aggregate)[3] = 'Amount'

spending_votes <- merge(results, spending_aggregate, by=c('FEC.ID.'), all.x=TRUE)
plot_spending_votes <- spending_votes[!is.na(spending_votes$Amount) & !is.na(spending_votes$GE.perc)
                                      & spending_votes$GE.perc != '' & spending_votes$GE.perc != 'n/a',]

percentages <- as.numeric(sub("%","",plot_spending_votes$GE.perc))/100

plot(plot_spending_votes$Amount, percentages, xlab='Total Amount Spent',
     ylab='Percentage of General Election Vote Share',
     main = 'Total Spending vs GE Vote Share for all Candidates')
abline(h=0.5, col='red')

incumbents <- spending_votes[spending_votes$incumbent =='(I)' & !is.na(spending_votes$Amount) & 
                               !is.na(spending_votes$GE.perc) & spending_votes$GE.perc != '' 
                             & spending_votes$GE.perc != 'n/a',]
in_perc <- as.numeric(sub("%","",incumbents$GE.perc))/100
in_spending <- incumbents$Amount

plot(in_spending, in_perc, xlab='Incumbent Spending', ylab='General Election Vote Share',
     main='Total Incumbent Spending vs. GE Vote Share')



