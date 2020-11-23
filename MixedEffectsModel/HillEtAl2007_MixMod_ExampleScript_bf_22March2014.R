# Hill et al data from FMF book
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
library(lsmeans)



options(scipen = 5) # 'penalize' scientific notation (e...)

# set contrasts to sum-to-zero (aka deviation coding) for unordered factors and polynomial for ordered factors
options(contrasts=c("contr.sum", "contr.poly"))



# load data


h1 <- read.delim("Hill et al. (2007).dat", header = TRUE)

head(h1) # ok, so it looks like each participant provided 1 row of data; there's not even a participant code in there



# turn class room into an explicit factor; this is the grouping variable
h1$f_Classroom <- as.factor(paste("class", h1$Classroom, sep = '_'))

# turn intervention into explicit factor
h1$f_Intervention <- as.factor(h1$Intervention)

# check data structure a bit
with (h1, table(f_Classroom, f_Intervention))
# ok, so each class got only 1 type of intervention (as it says in the book); and each intervention was administered to 5 or 6 classes. and in each class were 19 to 27 students or so


# the DV is Post_Exercise and FMF say one should in a second model check the the influence also of Pre_Exercise

# thus, scale Pre_Exercise
h1$s_Pre_Exercise <- scale(h1$Pre_Exercise, center = TRUE, scale = TRUE) 

# let's have a look at the DV
densityplot(h1$Post_Exercise) # hmm, not the most normal DV in the world; thus it might be a good idea to use bootstrapping for p values perhaps (but we need to check the residuals of course)


str(h1)


# effect of intervention on exercise levels (ignoring class)
boxplot(Post_Exercise ~ f_Intervention, data = h1)




# lmer models

# this is what FMF recommend in the book, i.e., to analyze the effect of Contraceptive, Cyclephase and the interaction. However, even with lots of iterations, this gives a non-convergence warning (I could imagine that they didn't use a maximal random effects structure...)
h_1 <- lmer(Post_Exercise ~ f_Intervention + (1 | f_Classroom), data = h1)
summary(h_1)

plot(h_1) # looks ok to me
densityplot(resid(h_1)) # looks also ok-ish (a slight bump in the lower values, as in the raw DV)
plot(h1$Post_Exercise, fitted(h_1)) # that's not so helpful due to the discrete values...


plot(h1$Post_Exercise, fitted(h_1), ylim = c(1.4,2.5), xlim = c(1,4)) # that's not so helpful due to the discrete values...
lines(lowess(h1$Post_Exercise, fitted(h_1)), col = 'blue', ylim = c(1.4,2.5), xlim = c(1,4))

# hmm, that dousn't look so fantastic

# but let's get some quick p values
Anova(h_1, type = 3, test = 'F')
# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: Post_Exercise
                       # F Df Df.res Pr(>F)    
# (Intercept)    1919.0621  1 17.855 <2e-16 ***
# f_Intervention    1.7038  3 17.884 0.2022 

#no significant effect of intervention



# ..........................................

# adding pre-intervention exercise as fixed effect and random slope varying over classroom
h_2 <- lmer(Post_Exercise ~ f_Intervention + s_Pre_Exercise + (1 + s_Pre_Exercise | f_Classroom), data = h1)
summary(h_2) # the random slope is perfectly correlated with the random intercept, so we could take it out

# Linear mixed model fit by REML ['lmerMod']
# Formula: Post_Exercise ~ f_Intervention + s_Pre_Exercise + (1 + s_Pre_Exercise |      f_Classroom) 
   # Data: h1 

# REML criterion at convergence: 401.8 

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -3.7617 -0.6021  0.0964  0.5139  3.3158 

# Random effects:
 # Groups      Name           Variance Std.Dev. Corr 
 # f_Classroom (Intercept)    0.002952 0.05433       
             # s_Pre_Exercise 0.001243 0.03525  -1.00
 # Residual                   0.121286 0.34826       
# Number of obs: 503, groups: f_Classroom, 22

# Fixed effects:
                # Estimate Std. Error t value
# (Intercept)      1.79154    0.01949   91.93
# f_Intervention1 -0.13909    0.03214   -4.33
# f_Intervention2 -0.01306    0.03220   -0.41
# f_Intervention3  0.07879    0.03292    2.39
# s_Pre_Exercise   0.43318    0.01781   24.32


plot(h_2) # hmm, looks a bit odd, but not superbad
densityplot(resid(h_2)) # looks better than before
plot(h1$Post_Exercise, fitted(h_2)) # that looks better than the first model

# same, but also adding a smoothed line
plot(h1$Post_Exercise, fitted(h_2)) # that looks pretty good
lines(lowess(h1$Post_Exercise, fitted(h_2)), col = 'blue')


# some quick way to get p values
Anova(h_2, type = 3, test = 'F')

# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: Post_Exercise
                       # F Df Df.res    Pr(>F)    
# (Intercept)    8242.5781  1 17.702 < 2.2e-16 ***
# f_Intervention    6.5718  3 16.841  0.003836 ** 
# s_Pre_Exercise  565.0240  1 18.808 1.719e-15 ***

# Aha, I guess that was the idea why FMS included that example: Once one controls for pre-intervention exercise levels (which are a significant predictor of post-intervention), intervention has a significant effect. That makes a lot of sense, of course.

# BUT: the different interventions might differ in their effect, so we should check that. I am going to use lsmeans for pairwise comparisons among the different interventions

lsmeans(h_2, pairwise ~ f_Intervention)

# leaflet alone is not better than the control group
# but both 'leaflet + quiz' and 'leaflet + plan' are better than control; the other pairwise comparisons are not significant
# $`f_Intervention lsmeans`
 # f_Intervention   lsmean         SE       df lower.CL upper.CL
        # Control 1.652443 0.03878603 17.00366 1.570613 1.734273
        # Leaflet 1.778477 0.04049945 10.30488 1.688599 1.868355
 # Leaflet + Plan 1.870330 0.04205962 11.31204 1.778068 1.962592
 # Leaflet + Quiz 1.864897 0.03923442 17.83948 1.782415 1.947378

# $`f_Intervention pairwise differences`
                                    # estimate         SE        df  t.ratio p.value
# Control - Leaflet               -0.126034025 0.05627888 11.591346 -2.23945 0.16958
# Control - Leaflet + Plan        -0.217886901 0.05741950 12.268705 -3.79465 0.01143
# Control - Leaflet + Quiz        -0.212453727 0.05553479 17.174291 -3.82560 0.00661
# Leaflet - Leaflet + Plan        -0.091852877 0.05866788  8.920416 -1.56564 0.44244
# Leaflet - Leaflet + Quiz        -0.086419702 0.05684973 13.065203 -1.52014 0.45401
# Leaflet + Plan - Leaflet + Quiz  0.005433174 0.05795796 13.769366  0.09374 0.99969
    # p values are adjusted using the tukey method for 4 means 