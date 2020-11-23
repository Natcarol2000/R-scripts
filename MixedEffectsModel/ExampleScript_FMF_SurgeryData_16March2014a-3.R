# Example Script Mix-Mod 2014
# 16 March 2014, multilevel chapter (week 6)
# FMF book chapter 19

# Surgery data example

# not all needed here... I always paste lots of them here, but only load the ones that I then actually need
library(lme4) # for lmers
library(dataframes2xls) # to write xls file
library(ggplot2) # for densityplots and other data
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



##Load the data file into R. This is a tab-delimited file hence use of read.delim
surgeryData = read.delim("~/Dropbox/Radboud/Teaching/MultilevelClass/2014/FieldMilesField/MultilevelData/Cosmetic Surgery.dat",  header = TRUE)
head(surgeryData)
tail(surgeryData)

length(surgeryData$particnu)

# some preprocessing

surgeryData$pp_code <- as.factor(paste('pp', surgeryData$particnu, sep = '_'))
surgeryData$f_Clinic <- as.factor(paste('clinic', surgeryData$Clinic, sep = '_'))

Clinic

densityplot(surgeryData$Post_QoL) # looks pretty normal


# xyplot() showing the same as FMF book plot
xyplot(Post_QoL ~ Base_QoL | as.factor(Clinic), group = Surgery_Text, data = surgeryData, type = c('p', 'r'), auto.key = TRUE)

m0 <- lmer(Post_QoL ~ (1 | f_Clinic), data = surgeryData, REML = FALSE)
summary(m0)
ICC <- 39.03 / (39.03 + 52.40) # 0.426884

m00_lmer <- lmer(Post_QoL ~ 1, data = surgeryData, REML = FALSE)


library(nlme)

m0_gls <- gls(Post_QoL ~ 1, data = surgeryData, method = "ML")
summary(m0_gls)

randomInterceptOnly <-lme(Post_QoL~1, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptOnly)

logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2
anova(interceptOnly, randomInterceptOnly)



m1_nlme <- lme(Post_QoL~1, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(m1_nlme)

anova(m0_gls, m1_nlme)

anova(m0_gls, m0)
?anova()

# FMF final model
finalModel<-lme(Post_QoL~Surgery + Base_QoL + Reason + Reason:Surgery, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(finalModel)
intervals(finalModel, 0.95)

# lme4 variant
finalModel_lme4 <-lmer(Post_QoL~Surgery + Base_QoL + Reason + Reason:Surgery + (1 + Surgery|f_Clinic), data = surgeryData)
summary(finalModel_lme4)
Anova(finalModel_lme4, type = 3, test = 'F')

# keep it maximal model
finalModel_lme4_max <-lmer(Post_QoL ~ Surgery + Base_QoL + Reason + Reason:Surgery + (1 + Surgery + Base_QoL + Reason + Reason:Surgery|f_Clinic), data = surgeryData,  control = lmerControl(optCtrl = list(maxfun = 100000)))
summary(finalModel_lme4_max)
Anova(finalModel_lme4_max, type = 3, test = 'F')
drop1(finalModel_lme4_max, ~., test = 'Chisq')
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.155562 (tol = 0.001)

# simplify a bit: remove random correls
finalModel_lme4_max_simp1 <-lmer(Post_QoL ~ Surgery + Base_QoL + Reason + Reason:Surgery +
(1 |f_Clinic) + 
(0 + Surgery |f_Clinic) + 
(0 + Base_QoL |f_Clinic) +
(0 +  Reason |f_Clinic) +
(0 + Reason:Surgery |f_Clinic)
, data = surgeryData,  control = lmerControl(optCtrl = list(maxfun = 1000000)))
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.0475076 (tol = 0.001)
summary(finalModel_lme4_max_simp1)
Anova(finalModel_lme4_max_simp1, type = 3, test = 'F')

# simplify more: remove random intercept
finalModel_lme4_max_simp2 <-lmer(Post_QoL ~ Surgery + Base_QoL + Reason + Reason:Surgery +
(0 + Surgery |f_Clinic) + 
(0 + Base_QoL |f_Clinic) +
(0 +  Reason |f_Clinic) +
(0 + Reason:Surgery |f_Clinic)
, data = surgeryData,  control = lmerControl(optCtrl = list(maxfun = 1000000)))
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  :
  # Model failed to converge with max|grad| = 0.0475076 (tol = 0.001)
summary(finalModel_lme4_max_simp2)
Anova(finalModel_lme4_max_simp2, type = 3, test = 'F')



# re-code a couple of things

head(surgeryData)

contrasts(surgeryData$Surgery_Text)
surgeryData$s_Base_QoL <- scale(surgeryData$Base_QoL, center = TRUE, scale = TRUE)
contrasts(surgeryData$Reason)
surgeryData$f_Reason <- as.factor(surgeryData$Reason)
contrasts(surgeryData$f_Reason)

set.seed(100)
finalModel_lme4_max_V2 <-lmer(Post_QoL ~ Surgery_Text + s_Base_QoL + f_Reason + f_Reason:Surgery_Text + (1 + Surgery_Text + s_Base_QoL + f_Reason + f_Reason:Surgery_Text |f_Clinic), data = surgeryData,  control = lmerControl(optCtrl = list(maxfun = 10000)))
summary(finalModel_lme4_max_V2)
Anova(finalModel_lme4_max_V2, type = 3, test = 'F')
drop1(finalModel_lme4_max_V2, ~., test = 'Chisq')




library(car)


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FMF script

##Load the data file into R. This is a tab-delimited file hence use of read.delim
surgeryData = read.delim("Cosmetic Surgery.dat",  header = TRUE)

#Graph the data
pgrid <- ggplot(surgeryData, aes(Base_QoL, Post_QoL)) + opts(title="Quality of Life Pre-Post Surgery at 10 Clinics")
pgrid + geom_point(aes(colour = Surgery_Text)) + geom_smooth(aes(colour = Surgery_Text), method = "lm", se = F) + facet_wrap(~Clinic, ncol = 5) + labs(x = "Quality of Life (Baseline)", y = "Quality of Life (After Surgery)")

imageDirectory<-paste(Sys.getenv("HOME"),"/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Screenshots",sep="/")
imageFile <- paste(imageDirectory,"Surgery Data.png",sep="/")
ggsave(file = imageFile)


#Run an ANOVA
surgeryANOVA<-aov(Post_QoL~Surgery, data = surgeryData)
summary(surgeryANOVA)

# run the same model but using the linear models command
surgeryLinearModel<-lm(Post_QoL~Surgery, data = surgeryData)
summary(surgeryLinearModel)

#Run an ANCOVA
surgeryANCOVA<-aov(Post_QoL~Base_QoL + Surgery, data = surgeryData)
summary(surgeryANCOVA)
Anova(surgeryANCOVA, type = "III")

# run the same model but using the linear models command
surgeryLinearModel<-lm(Post_QoL~Surgery + Base_QoL, data = surgeryData)
summary(surgeryLinearModel)


##Fit baseline models
#Fit model with intercept only
interceptOnly <-gls(Post_QoL~1, data = surgeryData, method = "ML")
summary(interceptOnly)
#Fit model allowing intercepts to vary by clinic
randomInterceptOnly <-lme(Post_QoL~1, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptOnly)

logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2
anova(interceptOnly, randomInterceptOnly)

#Add surgery as a predictor
randomInterceptSurgery <-lme(Post_QoL~Surgery, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgery)

##Fit effect of surgery and baseline QoL- random intercepts across clinics
randomInterceptSurgeryQoL <-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgeryQoL)

anova(randomInterceptOnly, randomInterceptSurgery, randomInterceptSurgeryQoL)

##Fit effect of surgery and baseline QoL with interaction- random intercepts across clinics
#randomInterceptAll <-lme(Post_QoL~Surgery*Base_QoL, data = surgeryData, random = ~0|Clinic, method = "ML")
#summary(randomInterceptAll)
#anova(randomInterceptOnly, randomInterceptSurgery, randomIntercept2factors, randomInterceptAll)


##Fit effect of surgery and baseline QoL- random slopes across clinics
#trialSlopes<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~0+Surgery|Clinic, method = "ML")
#summary(trialSlopes)

##Fit effect of surgery and baseline QoL- random slopes and intercepts across clinics
addRandomSlope<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(addRandomSlope)
anova(randomInterceptSurgeryQoL,addRandomSlope)

plot(addRandomSlope)

##Fit effect of surgery and baseline QoL, Reason and Reason*Surgery Interaction- random slopes and intercepts across clinics

addReason<-lme(Post_QoL~Surgery + Base_QoL + Reason, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
addReason<-update(addRandomSlope, .~. + Reason)
summary(addReason)

finalModel<-lme(Post_QoL~Surgery + Base_QoL + Reason + Reason:Surgery, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(finalModel)
intervals(finalModel, 0.95)

anova(addRandomSlope, addReason, finalModel)

##Fit effect of surgery and baseline QoL seperately for the two Reason groups.

physicalSubset<- surgeryData$Reason==1 
cosmeticSubset<-surgeryData$Reason==0
print(surgeryData$Surgery);print(physicalSubset);print(cosmeticSubset)

physicalModel<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, subset= physicalSubset, method = "ML")
cosmeticModel<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, subset= cosmeticSubset, method = "ML")
summary(physicalModel)
summary(cosmeticModel)





# end of FMF script
~~~~~~~~~~~~~~~~~~~~~