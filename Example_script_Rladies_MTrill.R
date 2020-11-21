
# 23-11-2020

# R script for the analyses of MTrill data for dataset with 19 subjects 
# This script is intended to inspire you how to deal with your own data! 

MTrill_data_19pp <- read.delim("~/Desktop/experimento_MTRILL/MTrill_data_19pp.csv")

# Preprocessing -----------------------------------------------------------


dataset=MTrill_data_19pp
dataset= MTrill_data_19pp[ -c(5) ] #drop column 5 as we won't analyse this variable
View(dataset)


#categorical variables as factor - continuous variable as numerical

dataset$Subject=as.factor(dataset$Subject)
dataset$Structures=as.factor(dataset$Structures)
dataset$items=as.factor(dataset$items)
dataset$prime=as.numeric(dataset$prime)
dataset$test=as.factor(dataset$test)
dataset$EnglishTestGrade=as.numeric(dataset$EnglishTestGrade)


# Get Descriptives ------------------------------------------------------------

#boxplots to check relation between language profiency and priming (syntactic repetition) effect



boxplot(EnglishTestGrade~Structures, data=dataset,col=c("light blue"), notch=FALSE, main="English proficiency vs.structures produced", ylab="English test score", outline=FALSE)




#comparing percentages of syntactic structures between pretest and priming test: NP structure (1) PNP structures (0)
table1 <- table(dataset$prime, dataset$test)
prop.table(table1)*100



library(psych) #for function describeBY

#check which trial (1-20) on average the NP structure seen on Google was more often repeated
#item 20 = higher mean compared to earlier trials meaning that at item 20 participants re-used the NP structure more often. 
with(dataset, describeBy(dataset$prime, group = list(dataset$items), mat = TRUE))
with(dataset, describeBy(dataset$prime, group = list(dataset$Structures), mat = TRUE))



# Contrasts setting: dummy coding -------------------------------------------------------
contr.treatment(2)
contrasts(dataset$test) = contr.treatment(2)

#more about contrasts setting: https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/#DEVIATION


# Models set-up -----------------------------------------------------------


library(lme4) #for lmers


#centering continuous variable 
dataset$EnglishTestGrade=scale(dataset$EnglishTestGrade, center = TRUE)

View(dataset)
#models

attach(dataset)

m1 <- glmer(prime ~ EnglishTestGrade+test+(1|Subject) + (1|items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optCtrl=list(maxfun=2000000)))
summary(m1)
#remove random intercept for items
m2 <- glmer(prime ~ EnglishTestGrade+test+(1|Subject),family=binomial(link = "logit"), data = dataset, control=glmerControl(optCtrl=list(maxfun=2000000)))
summary(m2) 
#add interaction with * to model 2
m3 <- glmer(prime ~ EnglishTestGrade*test+(1|Subject)+ (1|items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optCtrl=list(maxfun=2000000)))
summary(m3) 
#add random slope to model 3
m4 <- glmer(prime ~ EnglishTestGrade*test+(1+test|Subject)+ (1|items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optCtrl=list(maxfun=2000000)))

#add interaction as random slope to model 4
m5 <- glmer(prime ~ EnglishTestGrade*test+(1+EnglishTestGrade*test|Subject) + (1|items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optCtrl=list(maxfun=2000000))) #failed
#Warning message:
  #In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
     #Model failed to converge with max|grad| = 0.010786 (tol = 0.001, component 1)


#change optimizer to try convergence - random slope with interaction added

m5 <- glmer(prime ~ EnglishTestGrade*test+(1+EnglishTestGrade*test|Subject) + (1|items),family=binomial(link = "logit"), data = dataset, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m5) #worked!



# Comparing models:likelihood ratio test with anova() ---------------------

library(car) #for anova() and Anova()

anova(m1,m2)#no difference - keep the bigger model
anova(m1,m3) #interaction is significant 0.02767 *
anova(m3,m4) #no difference
anova(m4,m5) #no difference
anova(m3,m5) #no difference

#best option - model4 - smaller AIC



# Model diagnosis ---------------------------------------------------------

library(lattice) #for density plots

plot(m4) # for such logistic models, this is not so helpful...
densityplot(resid(m4)) # ok, looks symmetric, but quite leptokurtic, probably boostrapping for p values is a good idea
plot(dataset$prime, fitted(m4)) # since the original DV is either 0 or 1, this is also not so helpful...

library(arm)# for binnedplot() 

binnedplot(fitted(m4), resid(m4)) # 95% of the points should be within the lines and they are!

qqnorm(resid(m4)) # that doesn't look so bad



# Getting P-values --------------------------------------------------------

# so now we want to get p values

# Anova with KR-adjustment and F tests won't work, because that's only for gaussian models
# just to prove my point:
Anova_m4 <- Anova(m4, type = 3, test = 'F')
# Error in UseMethod("vcovAdj") : 
# no applicable method for 'vcovAdj' applied to an object of class "c('glmerMod', 'merMod')"

# the Anova with Chisquares should work. 
Anova_m4 <- Anova(m4, type = 3, test = 'Chisq')
Anova_m4

# There other options to get p-values: drop1, bootMer, and PBmodcomp



