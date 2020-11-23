#script for IAA stats - iADAATPA project
Pangeanic2.IAA.Rtable <- read.csv("~/Dropbox/iADAATPA _Analysis/Tables/Pangeanic2-IAA-Rtable.csv")
table=Pangeanic2.IAA.Rtable


summary(table)

library(psych)

describe(table)

attach(table)
#subsetting data

#Rater 1 scores for system's fluency and adequacy
Flu_R1_sys=subset(table, Raters=="Rater1", select=c(FluencySystem))
Flu_R1_bas=subset(table, Raters=="Rater1", select=c(FluencyBaseline))
Adeq_R1_sys=subset(table, Raters=="Rater1", select=c(AdequacySystem))
Adeq_R1_bas=subset(table, Raters=="Rater1", select=c(AdequacyBaseline))
#Rater 2 scores for baseline's fluency and adequacy
Flu_R2_sys=subset(table, Raters=="Rater2", select=c(FluencySystem))
Flu_R2_bas=subset(table, Raters=="Rater2", select=c(FluencyBaseline))
Adeq_R2_sys=subset(table, Raters=="Rater2", select=c(AdequacySystem))
Adeq_R2_bas=subset(table, Raters=="Rater2", select=c(AdequacyBaseline))


#Building table for Fluency (system)
Table_fluency_System=cbind(Flu_R1_sys, Flu_R2_sys)

names(Table_fluency_System) <- c("Rater1-FS", "Rater2-FS")


#Building table for Fluency (baseline)

Table_fluency_bas=cbind(Flu_R1_bas, Flu_R2_bas)

names(Table_fluency_bas) <- c("Rater1-FB", "Rater2-FB")


#Building table for Adequacy (system)
Table_Adeq_System=cbind(Adeq_R1_sys, Adeq_R2_sys)

names(Table_Adeq_System) <- c("Rater1-AS", "Rater2-AS")


#Building table for Adequacy (baseline)

Table_Adeq_bas=cbind(Adeq_R1_bas, Adeq_R2_bas)

names(Table_Adeq_bas) <- c("Rater1-AB", "Rater2-AB")

#Summaries for adequacy and fluency per rater

View(Table_fluency_System)
summary(Table_fluency_System)
View(Table_fluency_bas)
summary(Table_fluency_bas)
View(Table_Adeq_System)
summary(Table_Adeq_System)
View(Table_Adeq_bas)
summary(Table_Adeq_bas)




#t-tests

attach(table)

t.test(table$FluencySystem,table$FluencyBaseline, paired = TRUE) #t = -5.8493, df = 261, p-value = 1.473e-08
t.test(table$AdequacySystem,table$AdequacyBaseline, paired = TRUE) #t = -5.8493, df = 261, p-value = 1.473e-08



library(irr)
#Kappa fluency
kappa2(Table_fluency_System, 'unweighted')
kappa2(Table_fluency_System, 'equal')
kappa2(Table_fluency_bas, 'unweighted')
kappa2(Table_fluency_bas, 'equal')


#Kappa adequacy
kappa2(Table_Adeq_System, 'unweighted')
kappa2(Table_Adeq_System, 'equal')
kappa2(Table_Adeq_bas, 'unweighted')
kappa2(Table_Adeq_bas, 'equal')



# Ranking -----------------------------------------------------------------


Pangeanic2.IAA.Rtable <- read.csv("~/Dropbox/iADAATPA _Analysis/Tables/Pangeanic2-IAA-Rtable.csv")
table=Pangeanic2.IAA.Rtable





#separating into columns
RankingSystem=as.factor(c(table$RankingSystem))
RankingBaseline=as.factor(c(table$RankingBaseline))



#creating column with condition
table$comp <- ifelse((RankingBaseline == 1) & (RankingSystem ==1), 'BOTH-',
                     ifelse((RankingBaseline==2) & (RankingSystem ==2), 'BOTH+',
                            ifelse((RankingBaseline==1) & (RankingSystem ==2), 'PS',
                                   ifelse((RankingBaseline==2) & (RankingSystem ==1), 'BS',
                                          as.character(table$comp)))))

View(table)

#table System
table_rank_sys=as.data.frame(table[,1:2], drop=FALSE)
Rater1_rank_sys=subset(table_rank_sys,Raters=='Rater1', select=c(RankingSystem))
Rater2_rank_sys=subset(table_rank_sys,Raters=='Rater2', select=c(RankingSystem))
table(table_rank_sys)
Table_Rank_Kappa_sys=cbind(Rater1_rank_sys,Rater2_rank_sys)
names(Table_Rank_Kappa_sys) <- c("Rater1-RankSys", "Rater2-RankSys")
View(Table_Rank_Kappa_sys)

#table Baseline
table_rank_bas=table[,c(1,3)]
Rater1_rank_bas=subset(table_rank_bas,Raters=='Rater1', select=c(RankingBaseline))
Rater2_rank_bas=subset(table_rank_bas,Raters=='Rater2', select=c(RankingBaseline))
table(table_rank_bas)
Table_Rank_Kappa_bas=cbind(Rater1_rank_bas,Rater2_rank_bas)
names(Table_Rank_Kappa_bas) <- c("Rater1-RankBas", "Rater2-RankBas")
View(Table_Rank_Kappa_bas)



#table Comparison
table_rank_comp=table[,c(1,9)]
Rater1_rank_com=subset(table_rank_comp,Raters=='Rater1', select=c(comp))
Rater2_rank_com=subset(table_rank_comp,Raters=='Rater2', select=c(comp))
View(table_rank_comp)
Table_Rank_Kappa_comp=cbind(Rater1_rank_com,Rater2_rank_com)
names(Table_Rank_Kappa_comp) <- c("Rater1", "Rater2")

View(Table_Rank_Kappa_comp)
Cmatrix=table(Table_Rank_Kappa_comp)

write.table(Table_Rank_Kappa_comp, 'Table_Rank_Kappa_comp.txt', sep='\t', row.names=FALSE)


#Kappa System

kappa2(Table_Rank_Kappa_sys,'equal')
kappa2(Table_Rank_Kappa_sys,'unweighted')

#Kappa Baseline
kappa2(Table_Rank_Kappa_bas,'equal')
kappa2(Table_Rank_Kappa_bas,'unweighted')



#Kappa Comparison

kappa2(Table_Rank_Kappa_comp,'equal')
kappa2(Table_Rank_Kappa_comp,'unweighted')




                                          