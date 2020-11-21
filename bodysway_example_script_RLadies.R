
# 23-11-2020

# R script for the analyses of MTrill data for dataset with 19 subjects 
# This script is intended to inspire you how to deal with your own data! 

Bodyway_R_course.2 <- read.csv("~/Dropbox/MixedEffectsModel/Bodyway_R_course 2.csv", sep=";")

# Preprocessing -----------------------------------------------------------

View(Bodyway_R_course.2)
bodysway=Bodyway_R_course.2
View(bodysway)

#make DV numeric

bodysway$SD_AP <- as.numeric(bodysway$SD_AP)

# for factors, it's best NOT to use numerical indicators for the factor levels (as this might lead to confusion whether it's numerical or not), so change that:

# subject
bodysway$Subject_f <- as.factor(bodysway$Subject_f)

# blockversion
bodysway$BlockVersion_f <- as.factor(bodysway$BlockVersion_f)
# PicCategory
bodysway$PicCategory_f <- as.factor(bodysway$PicCategory_f)

#PicIdentity, make factor variable out of it
bodysway$PicIdent_f <- as.factor(bodysway$PicIdent_f)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(Freeze_comb3$BlockVersion_f == 'V4' & Freeze_comb3$PicCategory_f == 'N', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V4' & Freeze_comb3$PicCategory_f == 'A', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V4' & Freeze_comb3$PicCategory_f == 'H', 3,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ifelse(Freeze_comb3$BlockVersion_f == 'V5' & Freeze_comb3$PicCategory_f == 'H', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V5' & Freeze_comb3$PicCategory_f == 'N', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V5' & Freeze_comb3$PicCategory_f == 'A', 3,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(Freeze_comb3$BlockVersion_f == 'V6' & Freeze_comb3$PicCategory_f == 'N', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V6' & Freeze_comb3$PicCategory_f == 'H', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V6' & Freeze_comb3$PicCategory_f == 'A', 3, NA)))))))))))))))))))

# get descriptives --------------------------------------------------------

# understand the raw data 
# look at descriptives
install.packages('ggplot2')
library(ggplot2) # for pretty plots;
library(psych) # for describBy

with(bodysway, describeBy(SD_AP, group = list(PicCategory_f), mat = TRUE))
with(bodysway, describeBy(SD_AP, group = list(gender_f), mat = TRUE))
with(bodysway, describeBy(SD_AP, group = list(BlockNr), mat = TRUE))



#picture category
barSD_AP <- ggplot(bodysway, aes(PicCategory_f, SD_AP)) 

barSD_AP+ stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary (fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + labs (x = "Picture Category", y = "Mean SD_AP") # add error bars

#gender
barSD_AP <- ggplot(bodysway, aes(gender_f, SD_AP)) 

barSD_AP+ stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary (fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + labs (x = "Gender", y = "Mean SD_AP") # add error bars

#block number
barSD_AP <- ggplot(bodysway, aes(BlockNr, SD_AP)) 

barSD_AP+ stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary (fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + labs (x = "Block", y = "Mean SD_AP") # add error bars

#density plots per participant
with(bodysway, densityplot(~SD_AP | Subject_f))
with(bodysway, densityplot(~SD_AP | PicIdent_f))
with(bodysway, densityplot(~SD_AP | PicNr))

# Data distribution -------------------------------------------------------
install.packages("lattice")
library(lattice) #for histograms


hist.SD_AP <- ggplot(bodysway, aes(SD_AP)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "SD_AP", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(bodysway$SD_AP, na.rm = TRUE), sd = sd(bodysway$SD_AP, na.rm = TRUE)), colour = "black", size = 1) 
hist.SD_AP
with(bodysway, densityplot(SD_AP)) 


#skewness and kurtosis
skew(bodysway$SD_AP) #0 - symmetric





#data transformation - centering data
bodysway$SD_APz <- scale(bodysway$SD_AP, scale = FALSE)
View(bodysway)



#checking distribution skewness
skew(bodysway$SD_APz) #skewness  0

#plots distribution DV centered 

hist.SD_APz <- ggplot(bodysway, aes(SD_APz)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "SD_AP", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(bodysway$SD_APz, na.rm = TRUE), sd = sd(bodysway$SD_APz, na.rm = TRUE)), colour = "black", size = 1) 
hist.SD_APz


hist(bodysway$SD_APz)

with(bodysway, densityplot(SD_APz)) # symmetric

# models ------------------------------------------------------------------


# set up mixed model

# grouping variables: Subject_f --> random per-participant intercept & PicIdentity_f --> random per-participant intercept
# within-unit effects: EmotionVsNeutral --> random participant 'EmotionVsNeutral' slope
# between-unit effects: gender
# Random effects: 
# per-participant intercept
# per-pic Identity intercept
# per-participant 'EmotionVsNeutral' * 'PicNr_c' * 'BlockNr_c' slope
# random correlation terms among all random effects


install.packages('lme4', repos = c("http://lme4.r-forge.r-project.org/repos", getOption("repos")))
library(lme4)
attach(bodysway)
names(bodysway)



# Angry and Happy as opposed to Neutral
bodysway$EmotionVsNeutral <- as.factor(ifelse(bodysway$PicCategory_f == 'A' | bodysway$PicCategory_f == 'H', 'Emotion', ifelse(bodysway$PicCategory_f == 'N', 'Neutral', NA)))


#Contrast setting - Deviation coding: compares the mean of the dependent variable for a given level to the overall mean of the dependent variable
options(contrasts = c("contr.sum", "contr.poly"))
#1) Save contrast matrix into a variable
Dev_cont_EmotionVsNeutral_f <- contrasts(bodysway$EmotionVsNeutral)
#2) Add a first column of 1's (for the intercept)"
Dev_cont_EmotionVsNeutral_f <- cbind(c(1,1), Dev_cont_EmotionVsNeutral_f)
#3)Invert (=solve) the matrix, to get the actual weights
solve(Dev_cont_EmotionVsNeutral_f)

bodysway$BlockNr_c <- scale(bodysway$BlockNr, center = TRUE, scale = TRUE)
with(bodysway, table (BlockNr, BlockNr_c))

bodysway$PicNr_c <- scale(bodysway$PicNr, center = TRUE, scale = TRUE)
with(bodysway, table(PicNr, PicNr_c))

View(bodysway)

#models

# Models set-up -----------------------------------------------------------compares the mean of the dependent variable for a given level to the overall mean of the dependent variable
attach(bodysway)
names(bodysway)

#backwards procedure 

m1 <- lmer(SD_APz ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f) + (1 | PicIdent_f), data = bodysway)

#increased number of interactions
m2 <- lmer(SD_APz ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f) + (1 | PicIdent_f), data = bodysway, control=lmerControl(optCtrl=list(maxfun=2000000)))
print(summary(m2), corr = FALSE) #converged

m3 <- lmer(SD_APz ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral | Subject_f) + (1 | PicIdent_f), data = bodysway, control=lmerControl(optCtrl=list(maxfun=2000000)))
print(summary(m3), corr = FALSE) 
#random intercept for PicIndent removed
m4 <- lmer(SD_APz ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral | Subject_f), data = bodysway, control=lmerControl(optCtrl=list(maxfun=2000000)))
print(summary(m4), corr = FALSE) #converged
m5 <- lmer(SD_APz ~  gender_f + EmotionVsNeutral +  PicNr_c + BlockNr_c + (1  | Subject_f), data = bodysway, control=lmerControl(optCtrl=list(maxfun=2000000)))
print(summary(m5), corr = FALSE) #converged

#models comparison - Likelihood ratio test
anova(m3,m2)#keep simpler model
anova(m3,m4)
anova(m3,m4) 
anova(m2,m4) 
anova(m5,m4) #interaction effects highly significant
#best model - model 4

# Model diagnosis ---------------------------------------------------------


plot(m4) # fitted vs. residuals

densityplot(resid(m4)) # distribution of the residuals

qqnorm(resid(m4))


# get p-values ------------------------------------------------------------

library(car)
Anova_m4 <- Anova(m4, type = 3, test = 'F')
Anova_m4
