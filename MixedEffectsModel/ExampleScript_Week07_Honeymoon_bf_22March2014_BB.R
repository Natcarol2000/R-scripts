# Example Script Mix Mod 2014
# Course Week 7; 22 March 2014
# Uses the FMF data "Honeymoon Period.dat"


#libraries:
library(lme4) # for lmers
#library(ggplot2) # for densityplots
library(reshape) # for melt
library(car)
library(lattice)
library(pbkrtest)

library(multcomp)
library(boot)
library(psych)

options(scipen = 5) # 'penalize' scientific notation (e...)

# set contrasts to sum-to-zero (aka deviation coding) for unordered factors and polynomial for ordered factors
options(contrasts=c("contr.sum", "contr.poly"))



#Set the working directory
setwd("~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Classes/Week_07/Scripts_Data")

# read data
hm1 <- read.delim('Honeymoon Period.dat', header = TRUE)

head(hm1)
tail(hm1)

str(hm1) # all integer variables... (some should be factors; well, I'll do that after the melting)

unique(hm1$Person) # coded only with numerical entries



# the data file is in wide format, i.e., we'll have to reshape it!


hm2 <- melt(hm1, id = c('Person', 'Gender'), variable_name = 'Time')


# rename DV from 'value' to LifeSatisfaction
names(hm2)[4] <- 'LifeSatisfaction'

# turn Time into ordered factor: that's good for the plots and might be one easy way to model linear, quadratic, ... Time effects
hm2$o_Time <- ordered(hm2$Time, levels = c('Satisfaction_Base', 'Satisfaction_6_Months', 'Satisfaction_12_Months', 'Satisfaction_18_Months'))

head(hm2)


# create numeric variant of Time with entries 0, 6, 12, 18: mainly for plots; but perhaps also useful for analyses?

unique(hm2$Time)

hm2$num_TimeMonths <- as.numeric(as.character(with(hm2, recode(Time, "'Satisfaction_Base' = 0; 'Satisfaction_6_Months' = 6; 'Satisfaction_12_Months' = 12; 'Satisfaction_18_Months' = 18"))))


# create explicit factor f_Person that is non-numeric
hm2$f_Person <- as.factor(paste("pp", hm2$Person, sep = '_'))



# let's do some plots to understand the data a bit

# effect of o_Time on Life satisfaction, as a function of pp
xyplot(LifeSatisfaction ~ o_Time | f_Person, groups = f_Person, data = hm2, type = c('p', 'r'), auto.key = FALSE)

# use the numeric variable with Time entries in months (might look prettier)
xyplot(LifeSatisfaction ~ num_TimeMonths | f_Person, groups = f_Person, data = hm2, type = c('p', 'r'), auto.key = FALSE)



# Is there evidence for non-linear Time trends in the data?


# I try first the same as what we did for the valuation ratings
with(hm2, plot(num_TimeMonths, LifeSatisfaction))
with(hm2, abline(lm(num_TimeMonths ~ LifeSatisfaction), col = 'red'))
with(hm2, lines(lowess(LifeSatisfaction, num_TimeMonths), col = 'blue'))
# hmm, one the one hand side, this is not so useful due to the rather discrete values in the DV (thus hard to see how many points are overlapping) and also, I get an error message for the lowess.


# so I'm going to use a different approach: the library car has a function scatterplot() that can do cool things

scatterplot(hm2$LifeSatisfaction ~ hm2$num_TimeMonths, smoother = loessLine) # ok, here I still have the problem of the discrete values in the DV

# but, scatterplot() makes it easy to add "jitter" either to the values on the y or on the x axis, or both. I need it only on the y axis
# if you want to know more:
?scatterplot

# also, I want to suppress the scatterplots for this graph here
scatterplot(hm2$LifeSatisfaction ~ hm2$num_TimeMonths, smoother = loessLine, jitter = list(x = 0, y = 1), boxplots = FALSE)
# that looks pretty nice (at least good enough for me; for an actual paper, I would make it prettier)



# ....................................................................
# let's run some actual models
# I'm going to use mainly lme4


# First, I'm going to create 3 separate predictors, one each for the linear, quadratic, and cubic Time effect
# I use poly() to get uncorrelated predictors

hm2$Time_lin <- poly(hm2$num_TimeMonths, 3)[,1]
hm2$Time_quad <- poly(hm2$num_TimeMonths, 3)[,2]
hm2$Time_cub <- poly(hm2$num_TimeMonths, 3)[,3]



# let's try a maximal model with the ordered Time factor
lme4_1a <- lmer(LifeSatisfaction ~ o_Time + (1 + o_Time| f_Person), data = hm2)
summary(lme4_1a)
# Error in checkZdims(reTrms$Ztlist, n = n, control, allow.n = FALSE) : 
  # number of observations (=438) <= number of random effects (=460) for term (1 + o_Time | f_Person); the random-effects parameters and the residual variance (or scale parameter) are probably unidentifiable
  
# That doesn't work, because this is a similar case as we encountered with the only 1 observation per cell case earlier:
# If we fit a linear, quadratic, and cubic trend over only 4 data points per participant, there is no "repetition," i.e., it's not possible to also estimate an error term/residual


# Just to check that a model without random slopes work, I try this here (not a good idea in principle, it's really just a quick check here)
lme4_1b <- lmer(LifeSatisfaction ~ o_Time + (1 | f_Person), data = hm2)
summary(lme4_1b)
# ok, so that runs



# now, one thing I could do if I want a 'maximal' model is to remove the cubic time effect (which is overkill anyways, I think)
lme4_2 <- lmer(LifeSatisfaction ~ Time_lin + Time_quad + (1 + Time_lin + Time_quad | f_Person), data = hm2)
summary(lme4_2)
plot(lme4_2) # hmm, looks a bit funny, but not too bad
densityplot(resid(lme4_2)) # looks good
plot(hm2$LifeSatisfaction, fitted(lme4_2)) # that gives an error warning regarding different lengths! The reason is that the variable in the data frame has NA entries and is therefore longer than the fitted data...

# here's a not so elegant way to remove the NA entries from the DV...
plot(hm2$LifeSatisfaction[which(is.na(hm2$LifeSatisfaction) == FALSE)], fitted(lme4_2)) # that looks pretty good actually

# so let's do a quick way to get p values
Anova(lme4_2, type = 3, test = 'F')

# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: LifeSatisfaction
                   # F Df Df.res    Pr(>F)    
# (Intercept) 1017.967  1 113.84 < 2.2e-16 ***
# Time_lin     145.541  1 107.66 < 2.2e-16 ***
# Time_quad     66.551  1 109.50 6.243e-13 ***

# That worked and gives reasonable results




# Now, in the FMF book, they did some quite specific models that I want to recreate here. They did fit linear, quadratic, and cubic trends. And it worked there because they did not add all 3 of them as random slopes, but only added the linear one as random slope


# analogous to FMF lin, quad, cubic model in book; NOTE: in lme4, we cannot specify an autoregressive covariance structure.
# As they used in the book ML, I'm also using ML here
fmf_poly_lme4 <- lmer(LifeSatisfaction ~ Time_lin + Time_quad + Time_cub + (1 + Time_lin | f_Person), data = hm2,  REML = FALSE)
summary(fmf_poly_lme4)
plot(fmf_poly_lme4)

densityplot(resid(fmf_poly_lme4)) # looks good
plot(hm2$LifeSatisfaction[which(is.na(hm2$LifeSatisfaction) == FALSE)], fitted(fmf_poly_lme4)) # that looks a tiny bit worse than our lme4_2, it seems



# use bootstrapping for p values
t1 <- Sys.time()
boot_fmf_poly_lme4 <- bootMer(fmf_poly_lme4, FUN_bootMer, nsim = 1000, .progress = "txt", PBargs = list(style = 3), type = "parametric", parallel = 'multicore', ncpus = 3)
t2 <- Sys.time()
t2 - t1 # 30.00681 secs


head(as.data.frame(boot_fmf_poly_lme4))

library(boot)

# Intercept: Sig
boot_fmf_poly_lme4_CI_intercept_95 <- boot.ci(boot_fmf_poly_lme4, index = 1, conf = 0.95, type=c("norm", "basic", "perc")) #
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 1000 bootstrap replicates

# CALL : 
# boot.ci(boot.out = boot_fmf_poly_lme4, conf = 0.95, type = c("norm", 
    # "basic", "perc"), index = 1)

# Intervals : 
# Level      Normal              Basic              Percentile     
# 95%   ( 5.487,  6.214 )   ( 5.480,  6.243 )   ( 5.460,  6.223 )  
# Calculations and Intervals on Original Scale  

# Time linear: Sig
boot_fmf_poly_lme4_CI_Time_lin_95 <- boot.ci(boot_fmf_poly_lme4, index = 2, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   (-25.84, -18.80 )   (-25.70, -18.86 )   (-25.64, -18.80 )  

# Time quadratic: Sig
boot_fmf_poly_lme4_CI_Time_quad_95 <- boot.ci(boot_fmf_poly_lme4, index = 3, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   (-14.81,  -9.24 )   (-14.77,  -9.32 )   (-14.59,  -9.14 )  

# Time cubic: non-Sig
boot_fmf_poly_lme4_CI_Time_cub_95 <- boot.ci(boot_fmf_poly_lme4, index = 4, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   (-0.197,  5.095 )   (-0.244,  5.182 )   (-0.282,  5.143 )  



# analogous to FMF lin, quad, cubic model, BUT MAXIMAL RANDOM EFFECTS STRUCTURE: well, I tried that already above...
fmf_poly_lme4_max <- lmer(LifeSatisfaction ~ Time_lin + Time_quad + Time_cub + (1 + Time_lin + Time_quad + Time_cub | f_Person), data = hm2,  REML = FALSE)
# Error in checkZdims(reTrms$Ztlist, n = n, control, allow.n = FALSE) : 
  # number of observations (=438) <= number of random effects (=460) for term (1 + Time_lin + Time_quad + Time_cub | f_Person); the random-effects parameters and the residual variance (or scale parameter) are probably unidentifiable

# same, but only linear and quadratic
fmf_lin_quad_lme4_max <- lmer(LifeSatisfaction ~ Time_lin + Time_quad + (1 + Time_lin + Time_quad | f_Person), data = hm2,  REML = FALSE)
summary(fmf_lin_quad_lme4_max)
Anova(fmf_lin_quad_lme4_max, type = 3, test = 'F') # needs to be fit using REML!!
# Error in linearHypothesis.mer(model = model, hypothesis.matrix = hypothesis.matrix,  : 
  # F test available only for linear mixed model fit by REML


fmf_lin_quad_lme4_max_REML <- lmer(LifeSatisfaction ~ Time_lin + Time_quad + (1 + Time_lin + Time_quad | f_Person), data = hm2,  REML = TRUE)
summary(fmf_lin_quad_lme4_max_REML)
Anova(fmf_lin_quad_lme4_max_REML, type = 3, test = 'F') 

# Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)

# Response: LifeSatisfaction
                   # F Df Df.res    Pr(>F)    
# (Intercept) 1017.967  1 113.84 < 2.2e-16 ***
# Time_lin     145.541  1 107.66 < 2.2e-16 ***
# Time_quad     66.551  1 109.50 6.243e-13 ***






# using nlme
# FMF final model

library(nlme)

# that's the model in the book; this has time only as linar effect in there
ARModel <-lme(LifeSatisfaction ~ Time, random = ~Time|Person, correlation = corAR1(0, form = ~Time|Person), data = hm2, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
# doesn't run, I get an error message:
# Error in structure(res, levels = lv, names = nm, class = "factor") : 
  # 'names' attribute [323] must be the same length as the vector [0]


# hmm, perhaps I should use my explicit factors etc that I created?

ARModel_2 <-lme(LifeSatisfaction ~ num_TimeMonths, random = ~ num_TimeMonths | f_Person, correlation = corAR1(0, form = ~ num_TimeMonths | f_Person), data = hm2, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
# that worked!
summary(ARModel_2) # time is significant





# so what if we add now also the quadratic and cubic time trends?

# first, the way they do it in the book (random slope only for linear time)
poly_1 <-lme(LifeSatisfaction ~ Time_lin + Time_quad + Time_cub, random = ~ Time_lin | f_Person, correlation = corAR1(0, form = ~ Time_lin | f_Person), data = hm2, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
# that worked!
summary(poly_1) # time lin and quad are significant, cubic not

# what if we add the random slopes and autoregressive stuff also for quadratic and cubic?

poly_2 <-lme(LifeSatisfaction ~ Time_lin + Time_quad + Time_cub, random = ~ Time_lin + Time_quad + Time_cub| f_Person, correlation = corAR1(0, form = ~ Time_lin + Time_quad + Time_cub| f_Person), data = hm2, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
# I get an error that sounds a bit different than what I got from lme4, but I guess it tells us the same thing
# Error in solve.default(estimates[dimE[1] - (p:1), dimE[2] - (p:1), drop = FALSE]) : 
  # system is computationally singular: reciprocal condition number = 1.62924e-16
  
  
# So let's remove the cubic time trend, so we can compare it with our max model we fitted with lme4  
poly_3 <-lme(LifeSatisfaction ~ Time_lin + Time_quad, random = ~ Time_lin + Time_quad | f_Person, correlation = corAR1(0, form = ~ Time_lin + Time_quad | f_Person), data = hm2, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
summary(poly_3)
# that worked, and as in our lme4 model, time linear and quadratic are significant
plot(poly_3) # oops, that looks odd, I think the diagnostic plots in nlme require different commands than the ones in lme4?
densityplot(resid(poly_3)) # that works

# OK, so for now that shall suffice to show that we get the same results with lme4 as with nlme
# I still recommend to use lme4, unless there are good and specific reasons to use nlme


