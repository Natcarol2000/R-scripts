# Example Script "Generalized Linear Mixed Effects Models"
# for use with the intertemporal choice data frame
# created by Bernd Figner (April 7/8, 2013); updated March 22, 2014


# this script reads the "pre-processed" intertemp choice data file ICT_stacked_reduced_forBB_22March2014a.csv and then does a little bit more prepping and runs a bunch of glmer models (and graphs etc)




library(lme4) # for lmer, i.e., mixed models
library(dataframes2xls) # to write xls file
library(psych) # for describe.by (added in March 4, 2013 version)
library(languageR) # for pvals.fnc()
library(car) # for recode() and Anova()


options(scipen=5) # this has the effect that the serials are shown in regular, not scientific notation

# set contrasts to sum-to-zero (aka deviation coding) for unordered factors and polynomial for ordered factors
options(contrasts=c("contr.sum", "contr.poly"))


# saving/loading data frames

# most recent
#save.image('~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_glmer_23March2014a.RData')
#save.image('~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_glmer_23March2014b.RData')
#save.image('~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_glmer_23March2014c.RData')




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# these things here are just for myself (Bernd); students don't have the finished workspace, nor the version of the data frame to which the demographics data have been merged; but the code to do it is below
# save/load workspaces
# save
#save.image("~/Dropbox/Radboud/Teaching/MultilevelClass/MyClass/Data/ICT/ICT_Workspace_7April2013a.RData")

# load workspace
#load("~/Dropbox/Radboud/Teaching/MultilevelClass/MyClass/Data/ICT/ICT_Workspace_7April2013a.RData")

# final version of the data file is saved further below; search for write.csv(itc3, file = '~/Dropbox/Radboud/Teaching/MultilevelClass/MyClass/Data/ICT/ICT_stacked_Demographics_7April2013b.csv')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




ITC_NoPractTrials_Red <- read.csv("~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_reduced_forBB_22March2014a.csv")

head(ITC_NoPractTrials_Red)

# get rid of first column ('X'); and save data frame under shorter name

itc1 <- droplevels(ITC_NoPractTrials_Red[,-1])

head(itc1)




########################################################################################################################################
# MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE - MERGE

demo_1 <- read.csv('~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/demographics/demographics_cleaned_12March2014a.csv')

head(demo_1)

# hmm, in demo_1, each student has 2 rows, as the demographics variables were assessed twice, as part of each CCT
# thus, we first need to reduce the data frame to 1 row per participant

# create variable pp_code for merging
demo_1$pp_code <- as.factor(paste('pp', substring(demo_1$p_serial,9,10), sep = '_'))
demo_1$pp_code


# now, I select only the variables that I think might be relevant for ultimately merging it to the ITC data frame
demo_1b <- subset(demo_1, select = c(pp_code, d_gender, d_age, d_status, d_children, d_education, d_employment, d_income, d_smoke, d_quantitysmoke, d_lastsmoke, d_howlongsmoke))



# let's try to get rid of the redundant rows
demo_2 <- unique(demo_1b)

head(demo_2)
tail(demo_2)
nrow(demo_2) # 38, already better, but probably, some participants entered different things the two times the filled the stuff out...
ncol(demo_2) # 12

demo_2

with(demo_2, table(pp_code)) # pps 1, 12, 14, 19, and 40 have not completely identical entries, it seems



# let's sort the data frame by pp_code to see who it was (and whether we can get rid of some rows manually)

# this command here sorts data frame cct4, first according to pp_code and then according to serial_red; the sorted data frame is then saved into cct5
demo_3 <- demo_2[with(demo_2, order(pp_code)),]

demo_3 # ok, so now that participants with the same pp_code follow each other, it's easy to spot those with two entries and to see how their two entries differ

# now, there are different ways how to resolve this problem:
# 1. get rid of the variables that contain contradicting information (for the number of children entry of 54, it might be save to replace it with 0... but who konws)
# 2. take only one of the two contradicting entries (either the first one, or the one that you find more plausible, for example)
# 3. replace the contradicting values with NA

# Here, I'm going to choose option 2; taking only the first of two contradicting entires (which might not be the best solution in general or even in this specific case, but at least I can show how to do this; the eother things you already know how to do)

# this is not the most efficient way to do this, but it works...
delete1 <- with(demo_3, which(pp_code == 'pp_01'))
delete2 <- with(demo_3, which(pp_code == 'pp_12'))
delete3 <- with(demo_3, which(pp_code == 'pp_14'))
delete4 <- with(demo_3, which(pp_code == 'pp_19'))
delete5 <- with(demo_3, which(pp_code == 'pp_40'))

demo_4 <- demo_3[-c(delete1[2], delete2[2], delete3[2], delete4[2], delete5[2]),]

nrow(demo_4) #33, good!



# now let's try to merge the data frames

itc2 <- merge(itc1, demo_4, by.x = 'pp_code_ITC', by.y = 'pp_code')

# check number of rows and columns before and after merging
nrow(demo_4) #33
nrow(itc1) #992
nrow(itc2) #992 --> good!

itc2
head(itc2)
tail(itc2)




# now let's try to merge this data frame to the intertemp choice data frame

# let's save this combined data frame
write.csv(itc2, file = '~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_Demographics_22March2014a.csv')


# end of merging
########################################################################################################################################




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS ~ PREDICTORS


# preparing some of the predictors we want to use in the glmer analyses

itc3 <- itc2 # I'm saving the data frame under a new name...


# first look at what different values are in some of the variables

unique(itc3$SS_Time) #0 and 14 --> ok, the 0 trials are 'now' trials, the 14 trials are the 'not-now' trials

unique(itc3$LL_Time) #3, 14, 17, 28, 42


# let's compute the time difference between SS and LL

itc3$TimeDiff <- itc3$LL_Time - itc3$SS_Time

unique(itc3$TimeDiff) # 3, 14, 28

# what combinations of SS Time and Time Difference were presented? 
table(itc3$SS_Time, itc3$TimeDiff) # this is a command to show a 'cross-tabulation' with SS Time as 2 different rows; Time Difference as 3 different columns; and the numbers show how often each combination is in the data frame (hmm, odd that the 0/28 combination is in there more often than the others; perhaps partly due to the 'missing' 31 trials? not sure; would require a bit more detective work to figure that out; which I'm not going to do right now...)

# well, it's good that mixed models can handle well unbalanced designs, isn't it?

# so, how should we code things?
# As ususal, there are different ways how to do this (ordered/unordered factors, continuous, centered etc)


# Based on SS Time, I create a variable now/not-now that is coded as -0.5/+0.5
library(car)
itc3$t_Now_Notnow <- recode(itc3$SS_Time, "0 = -0.5; 14 = 0.5")

# well, I code it also as an ordered factor
itc3$o_Now_Notnow <- ordered(recode(itc3$SS_Time, "0 = 'now'; 14 = 'notnow'"), levels = c('now', 'notnow'))


# For time difference, I'm going to create 2 variables (since I'm not sure which might make more sense); a centered continous variable (that treats time as a linear effect; which might be a lot to expect from participants...), and an ordered factor (which, since it has 3 levels, it will check for linear and quadratic effects, which is quite plausible)

itc3$c_TimeDiff <- itc3$TimeDiff - mean(itc3$TimeDiff)

# i also create a scaled version
itc3$s_TimeDiff <- scale(itc3$TimeDiff, center = TRUE, scale = TRUE)


# here, i first recode the numerical 3, 14, 28 entries into categories (3days etc); then i turn it into an ordered factor
itc3$o_TimeDiff <- ordered(recode(itc3$TimeDiff, "3 = '3days'; 14 = '14days'; 28 = '28days'"), levels = c('3days', '14days', '28days'))



# SS Amount is relevant in the model to capture the 'magnitude' effect in intertemporal choice
# we can use it as continuous predictor, but it should be centered
itc3$c_SS_Amount <- itc3$SS_Amount - mean(itc3$SS_Amount)

# again a scaled version
itc3$s_SS_Amount <- scale(itc3$SS_Amount, center = TRUE, scale = TRUE)



# now, another very important predictor is the difference in amounts between the SS and the LL
# this difference can be computed either as absolute difference in amounts or in the relative difference in amounts (i.e., how much larger is the LL compared to the SS?)


# absolute difference
itc3$abs_Diff_Amounts <- itc3$LL_Amount - itc3$SS_Amount

# let's check the distribution, there might be some "catch" trials in there (where the SS is larger than the LL!)
library(lattice)
densityplot(itc3$abs_Diff_Amounts)
# yes, indeed, there's a bunch of trials, where the SS is approx. 10 Euros larger than the LL! this is to check whether participants paid attention
# we could either remove these catch trials for the glmer analyses, or we could just leave them in there (or do both and check whether it makes a difference)
# if i were to report these results in a paper, i would probably remove the catch trials and analyze them separately (to see who made the wrong choices on these catch trials and then perhaps remove these participants). for now, i leave them in there.

# just a quick check whether choosing the wrong option in these catch trials was very common:
with(itc3, table(abs_Diff_Amounts, choice_SS0_LL1)) # well, it wasn't very common it seems (only happened once)



# relative difference
itc3$rel_Diff_Amounts <- (itc3$LL_Amount - itc3$SS_Amount) / itc3$SS_Amount
densityplot(itc3$rel_Diff_Amounts)

# ok, so we see about 5 different 'bins' of relative differences:
# - -20% --> these are the catch trials
# - 5% --> everybody should be patient here
# - 10% --> there might be quite some individual (and frame) differences here
# - 20% --> same as 10%
# - 30% --> same, probably
# - 50% --> here, we probably would expect most individuals to choose the LL


# for the analyses, it's fine to use the exact relative differences in the model
# but: when we want to plot the choices as a function of the relative differences, it might be nice to 'bin' them in these, well, bins
# the recode command could again come in handy...

# to find the cutoffs for the different categories, i first look at the actual values that occur
sort(unique(itc3$rel_Diff_Amounts))


itc3$rounded_rel_Diff_Amounts <- ordered(recode(itc3$rel_Diff_Amounts, "-0.3:-0.1 = '1_negative'; 0.02:0.06 = '2_5%'; 0.07:0.12 = '3_10%'; 0.18:0.22 = '4_20%'; 0.28:0.32 = '5_30%'; 0.45:0.52 = '6_50%'"), levels = c('1_negative', '2_5%', '3_10%', '4_20%', '5_30%', '6_50%'))

# check whether our bin boundaries make sense, i.e., whether we created categories that contain equal number of trials
table(itc3$rounded_rel_Diff_Amounts, itc3$SS_Time)
# yep, looks good!

table(itc3$rounded_rel_Diff_Amounts, itc3$TimeDiff) # yep, looks still good!


# created centered and scaled version of reldiff amounts
itc3$c_rel_Diff_Amounts <- scale(itc3$rel_Diff_Amounts, center = TRUE, scale = FALSE)

itc3$s_rel_Diff_Amounts <- scale(itc3$rel_Diff_Amounts, center = TRUE, scale = TRUE)



# What else do we need for our models?

# We already have a pp_code, but we can make it explicitly into a factor (which it probably already is, but still...) 
itc3$f_pp_code_ITC <- as.factor(itc3$pp_code_ITC)

# recode d_smoke into a factor with nicer labels
itc3$f_smoking <- recode(itc3$d_smoke, "0 = 'non-smoker'; 1 = 'smoker'; else = NA")

# OK, I think we're ready to do some graphs and some models (there might be more variables that need to be brough in shape, but I'm sure you can do that on your own)


# let's save this final data frame
write.csv(itc3, file = '~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_Demographics_22March2014b.csv')



# some figures
head(itc3)

with(itc3, interaction.plot(rounded_rel_Diff_Amounts, o_TimeDiff, choice_SS0_LL1))


# hmm, I wonder whether the one negative rel diff might lead to problems later in the modeling

# thus, I create a new data frame from which these catch trials have been removed. which means that i have to re-center/re-scale all continuous predictors, ugh...

itc4 <- droplevels(itc3[itc3$abs_Diff_Amounts > 0,])

# let's check whether that worked
unique(itc4$abs_Diff_Amounts) # looks good

# check number of rows and columns before and after
nrow(itc3) #992
nrow(itc4) #960 --> thus 32 less, that looks about right to me

# now let's do the recentering/rescaling
itc4$c_rel_Diff_Amounts <- scale(itc4$rel_Diff_Amounts, center = TRUE, scale = FALSE)
itc4$s_rel_Diff_Amounts <- scale(itc4$rel_Diff_Amounts, center = TRUE, scale = TRUE)
itc4$c_TimeDiff <- itc4$TimeDiff - mean(itc4$TimeDiff)
itc4$s_TimeDiff <- scale(itc4$TimeDiff, center = TRUE, scale = TRUE)
itc4$c_SS_Amount <- itc4$SS_Amount - mean(itc4$SS_Amount)
itc4$s_SS_Amount <- scale(itc4$SS_Amount, center = TRUE, scale = TRUE)

head(itc4)

# ok, i think that's all of them now that have to be re-centered/scaled

# let's save that data file
write.csv(itc4, file = '~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_stacked_Demographics_NOCatchTrials_23March2014a.csv')


min(itc4$SS_Amount)
max(itc4$SS_Amount)

# let's do some plots

# simple interaction plot
with(itc4, interaction.plot(rounded_rel_Diff_Amounts, o_TimeDiff, choice_SS0_LL1, ylim = c(0,1)))



# proportion of LL choice as function of the rounded relative differences and the time difference between SS and LL (I use the rounded/categorical relative differences only for the plots, not the actual models)
with(itc4, interaction.plot(rounded_rel_Diff_Amounts, o_TimeDiff, choice_SS0_LL1))

# let's try some plots that show the individual differences

# let's try our usual xyplot first
xyplot(choice_SS0_LL1 ~ rounded_rel_Diff_Amounts | pp_code_ITC, groups = pp_code_ITC, data = itc4, type = c('p', 'r'), auto.key = FALSE)
# well, since the DV is always either 0 or 1, the data points are not very informative; at least the regression line shows a bit of the relationship between rel diff and choice (although this is linear in the probability space, which is not how it's modeled, but still, for some quick visual impression for ourselves it might be ok)

# same using the exact relative differences: that's actually better, because the circles are not exactly on top of each other
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = pp_code_ITC, data = itc4, type = c('p', 'r'), auto.key = FALSE)

xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = pp_code_ITC, data = itc4, type = c('p', 'smooth'), auto.key = FALSE)


# we could also show it separately for the different time differences, perhaps: ok, a bit complex, but still that's informative
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = TimeDiff, data = itc4, type = c('p', 'r'), auto.key = TRUE)

# separate for now and notnow
xyplot(choice_SS0_LL1 ~ rel_Diff_Amounts | pp_code_ITC, groups = o_Now_Notnow, data = itc4, type = c('p', 'r'), auto.key = TRUE)


# I also played around a bit with the function scatterplot, but couldn't produce results that looked that good; feel free to have a look and perhaps you can tweak it to make it more useful
scatterplot(itc4$choice_SS0_LL1 ~ itc4$rel_Diff_Amounts | itc4$o_Now_Notnow, smoother = loessLine, boxplots = FALSE)
scatterplot(itc4$choice_SS0_LL1 ~ itc4$rel_Diff_Amounts | itc4$o_Now_Notnow, smoother = loessLine,  jitter = list(x = 0, y = 0.5), boxplots = FALSE, ylim = c(-0.1, 1.1))

# same, but remove regression lines
scatterplot(itc4$choice_SS0_LL1 ~ itc4$rel_Diff_Amounts | itc4$o_Now_Notnow, smoother = loessLine,  jitter = list(x = 0, y = 0.5), boxplots = FALSE, ylim = c(-0.1, 1.1), reg.line = FALSE)


head(itc4)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...
# glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...glmer...

# some explanations: I first tried a bunch (well, a lot) of models, but they never converged. I tried to do these models with the data frame in which the catch trials were still in there (itc3). The next day, when I started working on it again, I realized that the problem might have been the catch trials. So I then created a data frame without the catch trials (itc4) and tried the models again.

# In the script below, the more recent models (using itc4) are on top; below them are the older models using itc3
# this stuff below I actually did first, before I removed the catch trials; as you an see below, I never managed to get the models to converge; therefore I am now trying it without the catch trials

# .........................................................
# models using data frame without catch trials

# a simple model investigating the effects of frame, now/notnow, time difference, ss amount (magnitude effect!?), and relative amount difference on choice
m2_1 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4)
summary(m2_1)
# Warning messages:
# 1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
  # failure to converge in 10000 evaluations
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.65226 (tol = 0.001)


# same, but with more iterations
m2_1b <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m2_1b)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.6522 (tol = 0.001)

# hmm, one warning less...
# probably further increasing the iterations doesn't help, but I'm curious and try it nevertheless
m2_1c <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000)))
summary(m2_1c)
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.6522 (tol = 0.001)

# ok, same warning, so I have to try something else

# as one can see in the summary output of all the models above, the correlation between the random intercept and the random slope for not/now is always perfectly correlated, therefore, I remove the random slope for now/notnow; I leave the number of iterations at this silly high number (i could reduce it...)
m2_1d <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000)))
summary(m2_1d)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.253484 (tol = 0.001)


# ok, still not converging; so i take out the random correlation terms

m2_1e <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts +
(1 |f_pp_code_ITC) + 
(0 + s_TimeDiff |f_pp_code_ITC) +
(0 + s_SS_Amount |f_pp_code_ITC) + 
(0 + s_rel_Diff_Amounts|f_pp_code_ITC),
family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000)))
summary(m2_1e)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.032318 (tol = 0.001)
# --> still not converging, darn


# OK, I try different optimizers now
m2_1e_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts +
(1 |f_pp_code_ITC) + 
(0 + s_TimeDiff |f_pp_code_ITC) +
(0 + s_SS_Amount |f_pp_code_ITC) + 
(0 + s_rel_Diff_Amounts|f_pp_code_ITC),
family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000), optimizer = "bobyqa"))
summary(m2_1e)

# that worked!!! ok, I'll try the more complicated starting model then with bobyqa


m2_1_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000), optimizer = "bobyqa"))
summary(m2_1_bobyqa)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.22175 (tol = 0.001)
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

# ok, that didn't work, but I'll go through the same simplification steps as before.
# Also: Note that the summary output always suggests the same results in all non-converged (and the one converged) model; so the whole procedure that I'm going through here is more for my conscience and probably won't change the results

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# remove random slope for o_Now_Notnow
m2_1d_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000), optimizer = "bobyqa"))
summary(m2_1d_bobyqa)
# great, that worked! hooray
# and the results are still the same

# ??????????????????????????????????????
# diagnostic plots?
# this is surprisingly(?) difficult for GLMMs...
# for those interested, see some links here (more on the slides...)
http://stats.stackexchange.com/questions/70783/how-to-assess-the-fit-of-a-binomial-glmm-fitted-with-lme4-1-0
http://www.r-bloggers.com/model-validation-interpreting-residual-plots/


plot(m2_1d_bobyqa) # for such logistic models, this is not so helpful...
densityplot(resid(m2_1d_bobyqa)) # ok, looks symmetric, but quite leptokurtic, probably boostrapping for p values is a good idea
plot(itc4$choice_SS0_LL1, fitted(m2_1d_bobyqa)) # since the original DV is either 0 or 1, this is also not so helpful...

library(arm)
binnedplot(fitted(m2_1d_bobyqa), resid(m2_1d_bobyqa)) # yep, should be in that order
?binnedplot # 95% of the points should be within the lines; this doesn't really seem to be the case

qqnorm(resid(m2_1d_bobyqa)) # that doesn't look so bad

boxplot(fitted(m2_1d_bobyqa) ~ itc4$choice_SS0_LL1) # that's more helpful and looks pretty ok, actually
densityplot(~ fitted(m2_1d_bobyqa) | itc4$choice_SS0_LL1) # it seems the model might be better at predicting LL choice than SS choice?

# ok, that whole diagnostic plot business is not yet really satisfying, i'm stopping for now.....
# ??????????????????????????????????????



# so now we want to get p values

# Anova with KR-adjustment and F tests won't work, because that's only for gaussian models
# just to prove my point:
Anova_3F_m2_1d_bobyqa <- Anova(m2_1d_bobyqa, type = 3, test = 'F')
# Error in UseMethod("vcovAdj") : 
  # no applicable method for 'vcovAdj' applied to an object of class "c('glmerMod', 'merMod')"

# the Anova with Chisquares should work
Anova_3Chisq_m2_1d_bobyqa <- Anova(m2_1d_bobyqa, type = 3, test = 'Chisq')
# Analysis of Deviance Table (Type III Wald chisquare tests)

# Response: choice_SS0_LL1
                     # Chisq Df Pr(>Chisq)    
# (Intercept)         4.8915  1    0.02699 *  
# o_Now_Notnow        0.2990  1    0.58450    
# s_TimeDiff         42.7547  1  6.205e-11 ***
# s_SS_Amount        32.1991  1  1.392e-08 ***
# s_rel_Diff_Amounts 48.1681  1  3.912e-12 ***

# however, these are the Wald chisquare tests and they are not trustworthy (they are the same tests as we get in the summary statement, except the ones in the summary statement are type 1 and this here is type 3)


# we still have the following options: drop1, bootMer, and PBmodcomp

# I do drop1 to get LRT tests 
drop1_m2_1d_bobyqa <- drop1(m2_1d_bobyqa, ~., test = 'Chisq')

# as they mention in Barr et al; there can be problems: some of the simpler ("smaller") models didn't seem to converge, thus we got an error warning
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 10.276 (tol = 0.001)
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model is nearly unidentifiable: large eigenvalue ratio
 # - Rescale variables?

# accordingly, the p values are probably not that trustworthy
drop1_m2_1d_bobyqa
# well, they still tell the same story: Time Diff, SS amount, and rel Diff are significant, now/notnow is not significant


# but let's do bootMer (that's what I would prefer for a paper anyway)
FUN_bootMer <- function(fit) {
	return(fixef(fit))
}

# I try it first with 3 simulations
t1 <- Sys.time()
boot_m2_1d_bobyqa <- bootMer(m2_1d_bobyqa, FUN_bootMer, nsim = 3, .progress = "txt", PBargs = list(style = 3), type = "parametric", parallel = 'multicore', ncpus = 3)
t2 <- Sys.time()
t2 - t1 # Time difference of 18.86104 secs
# ok, so i expect this to take around 1000/3*19/60 --> 105 minutes for 1000 simulations; which is more than I can afford right now, as I have to keep preparing the class...

# so for that reason, I'll do only 100 simulations (which is not a good idea in principle, as it's not that informative)
t3 <- Sys.time()
boot_m2_1d_bobyqa <- bootMer(m2_1d_bobyqa, FUN_bootMer, nsim = 100, .progress = "txt", PBargs = list(style = 3), type = "parametric", parallel = 'multicore', ncpus = 3)
t4 <- Sys.time()
t4 - t3 # Time difference of 9.85645 mins



as.data.frame(boot_m2_1d_bobyqa)
head(as.data.frame(boot_m2_1d_bobyqa))


library(boot)

# Intercept: Sig --> note that I get some warnings about the intervals being possibly unstable; this is because I used only 100 samples...
boot_m2_1d_bobyqa_CI_intercept_95 <- boot.ci(boot_m2_1d_bobyqa, index = 1, conf = 0.95, type=c("norm", "basic", "perc")) #
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
# boot.ci(boot.out = boot_m2_1d_bobyqa, conf = 0.95, type = c("norm", 
    # "basic", "perc"), index = 1)

# Intervals : 
# Level      Normal              Basic              Percentile     
# 95%   ( 0.369,  2.364 )   ( 0.310,  2.329 )   ( 0.456,  2.475 )  
# Calculations and Intervals on Original Scale
# Some basic intervals may be unstable
# Some percentile intervals may be unstable

# now, if we could want to back-transform that on the probability scale, we would use plogis()

# I use here the lower and upper estimates for the 95% CI from the 'Normal' Intervals
plogis(0.369) # 0.5912173 --> that means the lower CI bound estimate for the intercept is that the LL was chosen in 59% of the cases
plogis(2.364) # the upper bound is 0.9140406, i.e., 91%; this is a pretty wide range; this might be a bit more narrow if we had used more simulations


# o_Now_Notnow.L: Non-Sig
boot_m2_1d_bobyqa_CI_o_Now_Notnow.L_95 <- boot.ci(boot_m2_1d_bobyqa, index = 2, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   (-0.2243,  0.4084 )   (-0.2931,  0.4415 )   (-0.2712,  0.4633 )  
# --> consistent with what we've seen from the other methods to get p values, the now/notnow effect is not significant


# s_TimeDiff: Sig
boot_m2_1d_bobyqa_CI_s_TimeDiff_95 <- boot.ci(boot_m2_1d_bobyqa, index = 3, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   (-1.708, -0.902 )   (-1.700, -0.760 )   (-1.942, -1.003 )
# --> the negative coefficient says that the larger the time difference, the *less likely* the participants choose the LL; this is what we would expect

# s_SS_Amount: Sig
boot_m2_1d_bobyqa_CI_s_SS_Amount_95 <- boot.ci(boot_m2_1d_bobyqa, index = 4, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   ( 0.5397,  1.1433 )   ( 0.4786,  1.1256 )   ( 0.6167,  1.2638 ) 
# --> the positive coefficient says that the larger the SS amount (holding everything else constant), the *more likely* the participants choose the LL; this effect is known as the "magnitude effect" in the literature and is a very robust effect


# s_rel_Diff_Amounts: Sig
boot_m2_1d_bobyqa_CI_s_rel_Diff_Amounts_95 <- boot.ci(boot_m2_1d_bobyqa, index = 5, conf = 0.95, type=c("norm", "basic", "perc")) #
# Level      Normal              Basic              Percentile     
# 95%   ( 1.905,  3.154 )   ( 1.737,  3.081 )   ( 2.117,  3.461 ) 
# --> the positive coefficient says that the larger relative amount differences (i.e., the more I get for waiting), the *more likely* the participants choose the LL; this is again what we would expect



# '''''''''''' let's try PBmodcomp() just for fun
# I'm not going to use it for all the different effects, just for some, to demonstrate how we would do that

# so, our "Large" model was this here:
m2_1d_bobyqa <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000), optimizer = "bobyqa"))
summary(m2_1d_bobyqa)


# For the "Small" model, I'm going to remove the fixed effect of s_TimeDiff, so that we can compare the two models for a significance test

m2_1d_bobyqa_noTimeDiff <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000), optimizer = "bobyqa"))
summary(m2_1d_bobyqa_noTimeDiff)

# to make things go fast, I'll use 3 cores
# this needs a few commands

my3cluster <- makeCluster(rep("localhost", 3))


# Note, similar to the bootMer(), I'm doing only 100 simulations to avoid having to wait for too long, as I have to continue prepping the class...
t5 <- Sys.time()
PBmodcomp_TimeDiff <- PBmodcomp(m2_1d_bobyqa, m2_1d_bobyqa_noTimeDiff, nsim = 100, cl = my3cluster)
t6 <- Sys.time()
t6 - t5 # Time difference of 15.34087 mins
PBmodcomp_TimeDiff
# > PBmodcomp_TimeDiff
# Parametric bootstrap test; time: 920.44 sec; samples: 99 extremes: 0;
# Requested samples: 99 Used samples: 76 Extremes: 0
# large : choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + 
    # (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts | f_pp_code_ITC)
# small : choice_SS0_LL1 ~ o_Now_Notnow + s_SS_Amount + s_rel_Diff_Amounts + 
    # (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts | f_pp_code_ITC)
         # stat df   p.value    
# LRT    24.832  1 6.255e-07 ***
# PBtest 24.832      0.01299 *  
# --> Well, it seems it's still significant, although the p value is quite a bit larger for the boostrap test, compared to the LRT
# I wonder whether the PBtest p value would be smaller if we had used more than 100 samples...


# a second small model that differs from the large model in that i removed o_Now_Notnow
m2_1d_bobyqa_noNotnow <- glmer(choice_SS0_LL1 ~ s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc4, control = glmerControl(optCtrl = list(maxfun = 10000000), optimizer = "bobyqa"))
summary(m2_1d_bobyqa_noNotnow)


t7 <- Sys.time()
PBmodcomp_Notnow <- PBmodcomp(m2_1d_bobyqa, m2_1d_bobyqa_noNotnow, nsim = 100, cl = my3cluster)
t8 <- Sys.time()
t8 - t7 # Time difference of 13.44927 mins
PBmodcomp_Notnow
# Parametric bootstrap test; time: 806.95 sec; samples: 99 extremes: 59;
# Requested samples: 99 Used samples: 79 Extremes: 59
# large : choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + 
    # (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts | f_pp_code_ITC)
# small : choice_SS0_LL1 ~ s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + 
    # (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts | f_pp_code_ITC)
         # stat df p.value
# LRT    0.2996  1  0.5842
# PBtest 0.2996     0.7500
# --> as expected, both the LRT and the PBtest are non-significant

# ok, done with PBmodcomp for now, I'm going to stop the cluster
stopCluster(my3cluster)
















# -------------------------------------------------------------
# older older older: data frame WITH catch trials...
# this stuff below I actually did first, thus is older; these models below use the data frame before I removed the catch trials; as you an see below, I never managed particularly well to get the models to converge...

#[NOTE: also, I think some of my syntax when specifying a different optimizer might be wrong... the syntax above is correct, though]

# we could first do a kind of null model to get the overall intercept (i.e., how often participants chose the LL)
# I'm actually not a fan of these null models, but perhaps for didactic reasons they make sense (for tasks such as the CCT and the ITC, they don't make much sense, in my opinion)
m0 <- glmer(choice_SS0_LL1 ~ (1| f_pp_code_ITC), family=binomial(link = "logit"), data = itc3)
summary(m0)

# as one can see from the summary output, the fixed component of the intercept is 0.7350
# --> does that mean that participants chose the LL in 73.5% of the cases?
# --> no, this number is on the 'logit' scale; if we want the respective number on the probability scale (yes, that's what we want!), we have to back-transform using plogis()

plogis(0.3200) #0.5793243 --> i.e., participants chose the LL in about 58% of the cases

# let's compare that with the overall mean in the variable choice_SS0_LL1; that should be pretty similar
mean(itc3$choice_SS0_LL1) # it's 55, so not exactly the same, but pretty close, good!

# In case we would want to plot the intercept with the standard errors (e.g., in a bar graph with SEMs added), we always have first to sum up the numbers and then apply plogis()!

# I.e., upper SEM number:

# in this simple case, we could actually also use the term fixef(m0) to get the intercept...
# but let's keep it simple:

log_intercept <- 0.3200
log_sem <- 0.2335

prob_semUP <- plogis(log_intercept+log_sem) #0.6349472
prob_semDOWN <- plogis(log_intercept-log_sem) #0.5216115
prob_intercept <- plogis(log_intercept) #0.5793243

# with these 3 numbers, we could create a bar graph (well, just 1 bar) with SEM error bars; e.g., in excel or R or whatever...



# ok, on to some more interesting glmer models

# a simple model investigating the effects of frame, now/notnow, time difference, ss amount (magnitude effect!?), and relative amount difference on choice
m1 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3)
summary(m1)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.41972 (tol = 0.001)
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues


# model didn't converge, so let's try to increase the number of iterations (but I have a hunch we rather have to few data for such a complex model...)
m1b <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1b)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.41972 (tol = 0.001)
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

# use bobyqa as optimizer
m1c <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000), optimizer = "bobyqa"))
summary(m1c)
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.14769 (tol = 0.001)
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 2 negative eigenvalues

# Nelder-Mead
m1d <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000), optimizer = "Nelder_Mead"))
summary(m1d)
Warning message:
In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  Model failed to converge with max|grad| = 3.91617 (tol = 0.001)


# optimx from nlminb (you need to first install the package nlminb)
m1e <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000), list(method = 'nlminb'), optimizer = "optimx"))


test1 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial, data = itc3, control = glmerControl(optimizer = 'optimx', optCtrl = list(method = 'nlminb')))
# Warning messages:
# 1: In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
  # Parameters or bounds appear to have different scalings.
  # This can cause poor performance in optimization. 
  # It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.
# 2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.00102843 (tol = 0.001)
  
  
  
  test1 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial, data = itc3, control = glmerControl(optimizer = 'optimx', optCtrl = list(method = 'nlminb', maxit = 10)))



  test2 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial, data = itc3, verbose = 1)


glmerControl(optimizer="optimx",optCtrl=list(method="nlminb"))



gm1C <- update(gm1, control=glmerCtrl.optx(tolPwrss=1e-13, method="nlminb"))


install.packages("optimx")
library(optimx)

m1e <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000), optimizer = "nlminb"))

m1e <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000), optimizer = "nlminb", method = 'optimx'))



summary(m1e)
# Warning messages:
# 1: In (function (npt = min(n + 2L, 2L * n), rhobeg = NA, rhoend = NA,  :
  # unused control arguments ignored
# 2: In (function (iprint = 0L, maxfun = 10000L, FtolAbs = 1e-05, FtolRel = 1e-15,  :
  # unused control arguments ignored
# 3: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 4.41972 (tol = 0.001)
# 4: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
  
  
  
# OK, so none of this has worked, let's try something else

# let's have another look at our starting point, m1
m1 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3)
summary(m1)

# the random slope for now/notnow was perfectly correlated with the intercept, so I remove the random slope for now/notnow

m1_2 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3)
summary(m1_2)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.306847 (tol = 0.001)

# increase number of iterations
m1_2b <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1_2b)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.306847 (tol = 0.001)


# ok, that doesn't seem to help; so I can remove in addition the random covariances
m1_3 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + 
(1 |f_pp_code_ITC) +
(0 + s_TimeDiff |f_pp_code_ITC) +
(0 + s_SS_Amount |f_pp_code_ITC) +
(0 + s_rel_Diff_Amounts|f_pp_code_ITC)
, family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1_3)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.0442601 (tol = 0.001)  

# just for curiosity, i remove now the random intercept (this feels quite odd to do this, but if Barr says so...)
m1_4 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + 
(0 + s_TimeDiff |f_pp_code_ITC) +
(0 + s_SS_Amount |f_pp_code_ITC) +
(0 + s_rel_Diff_Amounts|f_pp_code_ITC)
, family=binomial(link = "logit"), data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1_4)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.00479357 (tol = 0.001)


# OK, so let's go back to m1 and do something else

# I follow the strategy of running separate models to test different things

# at first, I focus on now/notnow, timediff and rel-diff; thus I remove the random slope for ss amount
m1_5 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_TimeDiff + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial, data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1_5)
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.384356 (tol = 0.001)

# that still didn't work, now I also remove time diff from the random slopes
m1_6 <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 + o_Now_Notnow + s_rel_Diff_Amounts|f_pp_code_ITC), family=binomial, data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1_6)

# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.0187504 (tol = 0.001)

# dang, still nothing; hmm, perhaps i should try whether a model without any random slope even converges...
m1_noRanSlope <- glmer(choice_SS0_LL1 ~ o_Now_Notnow + s_TimeDiff + s_SS_Amount + s_rel_Diff_Amounts + (1 |f_pp_code_ITC), family=binomial, data = itc3, control = glmerControl(optCtrl = list(maxfun = 1000000)))
summary(m1_noRanSlope)
# OK, at least that works, good to now!

# ok, enough for now, i continue tomorrow

save.image('~/Dropbox/Radboud/Teaching/MultilevelClass/2014/Data/ICT_Choices/ICT_glmer_22March2014a.RData')





optimizer="optimx" with control=list(method="nlminb")

# optimx from nlminb


optimizer="Nelder_Mead"

?glmerControl


different optimizer (let's try bobyqa')
Warning messages:
1: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  Model failed to converge with max|grad| = 4.41972 (tol = 0.001)
2: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

Anova(m1, type = 3, test = 'F') # such F tests are not possible for generalized models!





drop1_m1 <- drop1(m1, ~., test = 'Chisq')



~~~~~~~~~~~~~

# Try Bootstrapping

FUN_bootMer <- function(fit) {
	return(fixef(fit))
}



time1 <- Sys.time()
boot_m9 <- bootMer(m9, FUN_bootMer, nsim = 1000, .progress = "txt", PBargs = list(style = 3), type = "parametric", parallel = 'multicore', ncpus = 3)
time2 <- Sys.time()
time2 - time1 # Time difference of 3.25661 hours
~~~~~~~~~~




# the more thorough way to test for significance is to use anova() to compare each a model with versus without the effect of interest!


# for example, if we want to test whether rel_Diff_Amounts is significant
m1b <- glmer(choice_SS0_LL1 ~ t_frame + t_Now_Notnow + c_TimeDiff + c_SS_Amount + (1|f_pp_code_ITC) + (1|f_pp_code_ITC_DelAcc), family=binomial(link = "logit"), data = itc3)
summary(m1b)

anova(m1b, m1) # --> yep, it's significant!


# Some plotting!!!

# let's try to graph the effect of the relative amounts difference on choices:
# we are going to use the rounded/binned relative differences for the plotting; and since i'm using interaction plot, i'm going to plot it for both frames (although that wasn't significant)

with(itc3, interaction.plot(rounded_rel_Diff_Amounts, Frame, choice_SS0_LL1)) # the spacing of the categories on the x axis is a bit odd (probably due to the fact that the first category name is longer than the other ones; this should be easy to fix, e.g., by having different category names...)

# what if we want to use the 'non-rounded' relative differences? and do something like a scatterplot showing the effect of the actual relative difference on choices?
# if we just use plot() to create a scatter plot, the problem is that the DV (choices) always are either 0 or 1; thus it is hard to see the actual effect
# what helps is to add something like a regression line, except that we're not using a linear regression approach, but a 'smoothing' approach, using the lowess smoothing (imagine it's something like a moving average; well, it's perhaps best to look up Loess or Lowess on wikipedia)

with(itc3, plot(rel_Diff_Amounts, choice_SS0_LL1)) # this creates a scatterplot (which is not very informative)
with(itc3, lines(lowess(rel_Diff_Amounts, choice_SS0_LL1), col = 'blue')) # this adds a smoothed line which really helps to see the relationship between the relative differences and choices!

# another option would be to make the little circles in the scatterplot a little transparent (as you did a while ago for the lines in the interaction plot); thus, if there are many circles on top of each other, one would see that because the circles then get darker.
with(itc3, plot(rel_Diff_Amounts, choice_SS0_LL1, col = rgb(255, 0, 0, 10, maxColorValue = 255)))
# well, that doesn't look that fantastic; one would have to play around with it some more; some additional tricks is to adjust the size of the circles according to many observations there are and/or to randomly 'jitter' the exact placement of the circles on the y axis a bit, so that they don't lie on exactly each other


# what if we want to compare the raw data to what the model predicts?

# let's try a scatterplot
plot(itc3$choice_SS0_LL1, fitted(m1)) # this creates a scatterplot (which is not very informative)

# perhaps we could try again the lowess trick? well, doesn't look that great
plot(itc3$choice_SS0_LL1, fitted(m1))
lines(lowess(itc3$choice_SS0_LL1, fitted(m1)), col = 'red')


# perhaps a box plot: that already has some more information
boxplot(fitted(m1) ~ itc3$choice_SS0_LL1)

# perhaps a densityplot?
densityplot(~fitted(m1) | itc3$choice_SS0_LL1) 
# that one shows two panels; the left one shows the distribution of the predicted probabilities when participants actually chose the SS (0); the right panel the same when participants actually chose the LL (1)

# we could also create a densityplot that shows the 2 curves in 1 panel
densityplot(~fitted(m1), groups = itc3$choice_SS0_LL1) 

# if we want to use the feature of an automatic (but rather shabby) legend
densityplot(~fitted(m1), groups = itc3$choice_SS0_LL1, auto.key=TRUE) 

# It seem the model is doing pretty well predicting LL choices, but not so well predicting SS choices, interestingly.





# ok, some more models with increasing complexity

# adding an interaction between frame (delay/accel) and now/not-now (in a different study, we found that only delay trials show a now-effect)
m2 <- glmer(choice_SS0_LL1 ~ t_frame * t_Now_Notnow + c_TimeDiff + c_SS_Amount + rel_Diff_Amounts + (1|f_pp_code_ITC) + (1|f_pp_code_ITC_DelAcc), family=binomial(link = "logit"), data = itc3)
summary(m2)

# as homework and exercise; use anova() to test for the significance of the interaction and the other fixed effects!


# adding smoking and some interactions of smoking
m3 <- glmer(choice_SS0_LL1 ~ d_smoke * t_frame * t_Now_Notnow + c_TimeDiff + c_SS_Amount + rel_Diff_Amounts + (1|f_pp_code_ITC) + (1|f_pp_code_ITC_DelAcc), family=binomial(link = "logit"), data = itc3)
summary(m3)

# hmm, from the summary p values, it looks like we get a significant 3-way interaction between smoking, frame, and now/not-now [well, we should thoroughly test for significance using the anova command!]


# so this is the same model, but without the 3 way interaction
m3b <- glmer(choice_SS0_LL1 ~ d_smoke * t_frame + d_smoke *  t_Now_Notnow + t_frame *  t_Now_Notnow + c_TimeDiff + c_SS_Amount + rel_Diff_Amounts + (1|f_pp_code_ITC) + (1|f_pp_code_ITC_DelAcc), family=binomial(link = "logit"), data = itc3)
summary(m3b)

anova(m3b, m3)

#--> OK, so the likelihood ratio test clearly tells us that the 3-way interaction is NOT significant!!! thus, the summary p value was misleading.


# since I'm curious, I still want to have a look what this 3-way interaction; so I'm getting descriptive values and do some plotting (that shows you some easy ways how to plot these things)
library(psych)
with(itc3, describe.by(choice_SS0_LL1, group = list(d_smoke, t_frame, t_Now_Notnow), mat = TRUE)) # hmm, i get a warning... and that I should use describeBy; which is what I am thus doing...
with(itc3, describeBy(choice_SS0_LL1, group = list(d_smoke, t_frame, t_Now_Notnow), mat = TRUE))

# Ok, having it as figures might also be nice
# this shows the non-significant two-way interaction between smoking and frame
with(itc3, interaction.plot(Frame, f_smoking, choice_SS0_LL1, ylim = c(0.5, 0.7)))

# this shows ONLY NOW trials, the effect of smoking and frame on choices
with(itc3[which(itc3$SS_Time == 0),], interaction.plot(Frame, f_smoking, choice_SS0_LL1, ylim = c(0.5, 0.7)))

# this shows the same for NOT-NOW trials
with(itc3[which(itc3$SS_Time > 0),], interaction.plot(Frame, f_smoking, choice_SS0_LL1, ylim = c(0.5, 0.7)))

# OK, it's not significant, but what I would conclude from it if it were:
# smokers seem to be more impatient than non-smokers in all conditions, except for Acceleration not-now trials (arguably these trials are the ones where we'd expect that it is easiest for all participants to be patient, as there are the least "temptations" having to resist any SSs...)




# so this is a pretty complex model; it's like m3b, but adds uncorrelated random slopes varying over participants crossed with frame
m4 <- glmer(choice_SS0_LL1 ~ d_smoke * t_frame + d_smoke *  t_Now_Notnow + t_frame *  t_Now_Notnow + c_TimeDiff + c_SS_Amount + rel_Diff_Amounts + (1|f_pp_code_ITC) + (1|f_pp_code_ITC_DelAcc) + (0+t_Now_Notnow|f_pp_code_ITC_DelAcc) + (0+t_frame|f_pp_code_ITC_DelAcc) + (0+c_TimeDiff|f_pp_code_ITC_DelAcc) + (0+rel_Diff_Amounts|f_pp_code_ITC_DelAcc) + (0+c_SS_Amount|f_pp_code_ITC_DelAcc), family=binomial(link = "logit"), data = itc3)
summary(m4)

# check distribution of residuals: well, not super-normal, but pretty symmetric around 0
densityplot(residuals(m4))

densityplot(fitted(m4)) # NOTE: the output of fitted() is already 'back-transformed' on the probability scale (i.e., values range between 0 and 1)!

# perhaps we could try again the lowess trick? well, doesn't look that great
plot(itc3$choice_SS0_LL1, fitted(m4))
lines(lowess(itc3$choice_SS0_LL1, fitted(m4)), col = 'red')
# that gives an error message: length differ!
# any idea why?
# this model contains d_smoke and there were some missing values for that. thus, the number of entries in fitted(m4) is less than the number of rows in itc3$choice_SS0_LL1

# it should work if we just use the entries in itc3$choice_SS0_LL1 for which the respective d_smoke entry is not NA;
# here's how to do that, we are using the command is.na() (look it up!)

plot(itc3$choice_SS0_LL1[which(is.na(itc3$d_smoke) == FALSE)], fitted(m4))
lines(lowess(itc3$choice_SS0_LL1[which(is.na(itc3$d_smoke) == FALSE)], fitted(m4)), col = 'red')

# the same trick can be used for the other types of plots
boxplot(fitted(m4) ~ itc3$choice_SS0_LL1[which(is.na(itc3$d_smoke) == FALSE)])
densityplot(~fitted(m4) | itc3$choice_SS0_LL1[which(is.na(itc3$d_smoke) == FALSE)]) 
densityplot(~fitted(m4), groups = itc3$choice_SS0_LL1[which(is.na(itc3$d_smoke) == FALSE)]) 
densityplot(~fitted(m4), groups = itc3$choice_SS0_LL1[which(is.na(itc3$d_smoke) == FALSE)], auto.key=TRUE) 



# Ok, we could some even more complex models. And YOU should do that, to learn and try out stuff and see how far you can go (also with random slopes and random correlations), until you get an error message that the model didn't converge!


