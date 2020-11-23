# Miller et al data from FMF book
# Bernd Figner, Mix Mod 2014; 22 March 2014

# data are on BlackBoard; for a very brief description of the data, see Field, Miles, & Field book

# loading some libraries (not all of them needed; I load not all of them, I just copy/paste everthing here from an older script, in case I'll need it)

#libraries:
#library(gdata) #for drop.levels --> actually not necessary, as there is the function droplevels in base!!!
library(lme4) # for lmers
library(dataframes2xls) # to write xls file
library(ggplot2) # for densityplots
library(plyr) # for revalue (for densityplots)
library(reshape) # for melt
# library(gdata) # to read xls files; doesn't always work that neatly
library(reshape)
library(car)
library(lattice)
library(lme4)
library(pbkrtest)

library(multcomp)
library(boot)

library(psych)
#library(lsmeans)



options(scipen = 5) # 'penalize' scientific notation (e...)

# set contrasts to sum-to-zero (aka deviation coding) for unordered factors and polynomial for ordered factors
options(contrasts=c("contr.sum", "contr.poly"))



# load data


m1 <- read.delim("Miller et al. (2007).dat", header = TRUE)

head(m1)

# there is minimal information about the data in the book, thus I don't know how the variables are coded and whether they are continuous or not
# as I don't know (and don't have time to look it up in the original article), I code them as factors

m1$f_Contraceptive <- as.factor(m1$Contraceptive)
m1$f_Cyclephase <- as.factor(m1$Cyclephase)

# turn ID into an explicit factor
m1$f_ID <- as.factor(m1$ID)


# effect of Contraceptive and Cyclephase on Tip amount (however, without knowing what the codes are, this is not that meaningful...)
xyplot(Tips ~ Cyclephase | f_ID, groups = f_Contraceptive, data = m1, type = c('p', 'r'), auto.key = TRUE)
xyplot(Tips ~ Cyclephase  | f_Contraceptive, groups = f_ID, data = m1, type = c('p', 'r'), auto.key = FALSE)



# lmer models

# this is what FMF recommend in the book, i.e., to analyze the effect of Contraceptive, Cyclephase and the interaction. However, even with lots of iterations, this gives a non-convergence warning (I could imagine that they didn't use a maximal random effects structure...)
mod_1 <- lmer(Tips ~ f_Contraceptive * f_Cyclephase + (1 + f_Contraceptive * f_Cyclephase | f_ID), data = m1, control = lmerControl(optCtrl = list(maxfun = 1000000)))
summary(mod_1)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

# I use the non-factors to have a look at the structure of the data
with(m1, table(Contraceptive, Cyclephase))

with(m1, table(Contraceptive, Cyclephase, f_ID)) # OK, contraceptives is between-subjects, so no wonder mod_1 doesn't converge, f_Contraceptive should not be included as random effect


# same as before, but without random slope for f_Contraceptive; for that model the default number of iterations is sufficient
mod_1b <- lmer(Tips ~ f_Contraceptive * f_Cyclephase + (1 + f_Cyclephase | f_ID), data = m1)
summary(mod_1b) # --> the correlations between the random effects are all perfect (i.e., either + or - 1.0); thus, I could remove the slopes, for example; but as the model converged quickly, I don't think this is necessary

Anova(mod_1b, type = 3, test = 'F') # I use this method to quickly get p values

# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: Tips
                                    # F Df Df.res    Pr(>F)    
# (Intercept)                  203.3412  1 16.718 8.762e-11 ***
# f_Contraceptive                6.9779  1 16.718   0.01731 *  
# f_Cyclephase                  25.7867  2 19.650 3.201e-06 ***
# f_Contraceptive:f_Cyclephase   5.0811  2 19.650   0.01665 * 

# OK, so Contraceptive, Cyclephase, and their interaction have an effect on Tips. As I don't know the coding, I have no clue in which directions these effects go...

# we could also use lsmeans() to compute posthocs between Cyclephases, but again, since I don't know the coding, I'm not going to do that
# and I could recode the two factors (plus their interaction) into one single factor and then do all possible pairwise comparisons using lsmeans

