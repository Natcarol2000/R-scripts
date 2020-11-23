

#deleting columns

#table=table[,c(6,8:9)]

#deleting rows
#table <- table[-c(1:2), ]



#changing columns names

#names(table) <- c("Raters", "RankingSystem", "RankingBaseline")

#View(table) #looks good!

# Descriptive -------------------------------------------------------------
table=Kantan9.Table.Ranking

library(psych)
summary(table)
describe(table)

t.test(table$FluencySystem,table$FluencyBaseline, paired = TRUE)

t.test(table$AdequacySystem, table$AdequacyBaseline, paired = TRUE)

# Ranking stats -----------------------------------------------------------

table_IAA=table[1:860,]


table$RankingSystem=as.numeric(table$RankingSystem)
table$RankingBaseline=as.numeric(table$RankingBaseline)

View(table)
is.numeric(table$RankingBaseline)
is.numeric(table$RankingSystem)

attach(table)



#creating column with condition
table$comp <- ifelse((RankingBaseline == 1) & (RankingSystem ==1), 'BOTH-',
                     ifelse((RankingBaseline==2) & (RankingSystem ==2), 'BOTH+',
                            ifelse((RankingBaseline==1) & (RankingSystem ==2), 'PS',
                                   ifelse((RankingBaseline==2) & (RankingSystem ==1), 'BS',
                                          as.character(table$comp)))))

View(table)

#table Comparison
table_rank_comp=table[,c(1,8)]
Rater1_rank_com=subset(table_rank_comp,Raters=='Rater1', select=c(comp))
Rater2_rank_com=subset(table_rank_comp,Raters=='Rater2', select=c(comp))
summary(Rater2_rank_com)
summary(Rater1_rank_com)
library(rowr)
raters=cbind.fill(Rater1_rank_com, Rater2_rank_com, fill = NA)
names(raters) <- c("Rater1", "Rater2")
View(raters)

getwd()
setwd('/Users/nataliaresende/Dropbox/iADAATPA _Analysis/Table-Ranking')

write.table(raters, 'table_overall_raninking_Tilde10.txt', sep='\t', row.names=FALSE)

table(raters)
