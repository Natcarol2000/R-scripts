
# 24-03-2014

# R script for the analyses of body sway data for the complete data set. 
# You will just get a data set with half of the participants, so that results may be different then presenting in the script/powerpoint. 
# Additionally, I did several prepartory steps to get the data in the desired structure, which are included in the script but which are not relevant for you. Thus, some variables I have used are not included in the data set on blackboard. 
# This script is intended to inspire you how to deal with your own data! 
# You will have to adapt the script to the data set on blackboard in order to be able to run it. 

____________________________________________________________________________________________________
____________________________________________________________________________________________________

# check directory
getwd()

# adapt directory to 
setwd("/Users/hannahniermann/MixedModels/MixedModels_Course/FreezeDataAdolescentsOnly")

____________________________________________________________________________________________________
  
# load csv file of all participants with complete dataset (keep in mind that pp_13 has only 19 instead of 20 pictures of all picture categories, pp_37 --> dissy in (last) block of positive pictures, body sway to 10 pictures of positive block were excluded; pp_905 --> stepped from platform (beginning of angry face block), bodysway to 3 angry faces were excluded, which is included here)
  
bodysway <- read.table('SummaryMixedModelsFinal_ALL.csv', header = TRUE, sep = ";")

# change comma into dot of dependent variables
bodysway$Mean_ML <- gsub(",", ".", bodysway$Mean_ML)
bodysway$Mean_AP <- gsub(",", ".", bodysway$Mean_AP)
bodysway$SD_ML <- gsub(",", ".", bodysway$SD_ML)
bodysway$SD_AP <- gsub(",", ".", bodysway$SD_AP)
bodysway$Radius <- gsub(",", ".", bodysway$Radius)
bodysway$Area85 <- gsub(",", ".", bodysway$Area85)
bodysway$Area95 <- gsub(",", ".", bodysway$Area95)
bodysway$Swaypathlength <- gsub(",", ".", bodysway$Swaypathlength)

# make dependent variables numerical
bodysway$Mean_ML <- as.numeric(bodysway$Mean_ML)
bodysway$Mean_AP <- as.numeric(bodysway$Mean_AP)
bodysway$SD_ML <- as.numeric(bodysway$SD_ML)
bodysway$SD_AP <- as.numeric(bodysway$SD_AP)
bodysway$Radius <- as.numeric(bodysway$Radius)
bodysway$Area85 <- as.numeric(bodysway$Area85)
bodysway$Area95 <- as.numeric(bodysway$Area95)
bodysway$Swaypathlength <- as.numeric(bodysway$Swaypathlength)

# for factors, it's best NOT to use numerical indicators for the factor levels (as this might lead to confusion whether it's numerical or not), so change that:

# subject
bodysway$Subject_f <- as.factor(paste("pp", bodysway$Subject, sep = "_"))

# blockversion
bodysway$BlockVersion_f <- as.factor(ifelse(bodysway$BlockVersion == 1, 'V1', ifelse(bodysway$BlockVersion == 2, 'V2', ifelse(bodysway$BlockVersion == 3, 'V3', ifelse(bodysway$BlockVersion == 4, 'V4', ifelse(bodysway$BlockVersion == 5, 'V5',ifelse(bodysway$BlockVersion == 6, 'V6',NA)))))))

# PicCategory
bodysway$PicCategory_f <- as.factor(ifelse(bodysway$PicCategory == 1, 'L', ifelse(bodysway$PicCategory == 2, 'A', ifelse(bodysway$PicCategory == 3, 'H', ifelse(bodysway$PicCategory == 4, 'N', ifelse(bodysway$PicCategory == 5, 'NP',ifelse(bodysway$PicCategory == 6, 'PP',ifelse(bodysway$PicCategory == 7, 'NeuP', NA))))))))

#PicIdentity, make factor variable out of it
bodysway$PicIdent_f <- as.factor(bodysway$PicOrderNr)

# make matrix only across faces
bodyswayF <- droplevels(bodysway[which(bodysway$PicCategory_f == 'A' | bodysway$PicCategory_f == 'H' | bodysway$PicCategory_f == 'N'),])

# just use DV of SD_AP; remove other variables
bodyswayFSD_AP <- bodyswayF[c(-1, -2,-3,-5, -6,-7,-8,-10,-11,-12,-13)]

# remove pp_31 due to excessive movement & pp_905 due to stepping from the platform
with(bodyswayFSD_AP, table(PicCategory_f, Subject_f))
bodyswayFSD_AP1 <- subset(bodyswayFSD_AP, Subject_f != 'pp_31' & Subject_f != 'pp_905')
with(bodyswayFSD_AP1, table(PicCategory_f, Subject_f))

write.table(bodyswayFSD_AP1, file = 'SummaryMixedModelsFinalBodySwayF_SD_AP1.csv')

bodyswayFSD_AP1 <- read.table('SummaryMixedModelsFinalBodySwayF_SD_AP1.csv', header = TRUE)

__________________________________________________________________________________________________________

# get participants gender
Gender <- read.table('Gender_NLS_AdolFreeze.csv', header = TRUE, sep = ";")

# make factors
# subject ID
Gender$Subject_f <- as.factor(paste("pp", Gender$subj_id, sep = "_"))

# gender
Gender$gender_f <- as.factor(ifelse(Gender$gender == 0, 'm', ifelse(Gender$gender == 1, 'f', NA)))

# remove unnecessary variables
Gender1 <- Gender[c(-1, -2)]

___________________________________________________________________________________________________________

# merge files bodyswayFSD_AP and Gender
Freeze_comb <- merge(bodyswayFSD_AP1, Gender1, by = 'Subject_f')

# check whether merge went fine
with(Freeze_comb, table(PicCategory_f, Subject_f))
with(Freeze_comb, table(Subject_f, gender_f))

write.table(Freeze_comb, file = 'Freeze_Combined_PPAdolBodySway_Gender.csv')

Freeze_comb3 <- read.table('Freeze_Combined_PPAdolBodySway_Gender.csv', header = TRUE)

______________________________________________________________________________________________________________
# get BlockNr, indicating which picture type is presented as 1, 2, and 3 block

Freeze_comb3$BlockNr <- as.numeric(ifelse(Freeze_comb3$BlockVersion_f == 'V1' & Freeze_comb3$PicCategory_f == 'A', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V1' & Freeze_comb3$PicCategory_f == 'H', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V1' & Freeze_comb3$PicCategory_f == 'N', 3,
                                   ifelse(Freeze_comb3$BlockVersion_f == 'V2' & Freeze_comb3$PicCategory_f == 'A', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V2' & Freeze_comb3$PicCategory_f == 'N', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V2' & Freeze_comb3$PicCategory_f == 'H', 3,
                                   ifelse(Freeze_comb3$BlockVersion_f == 'V3' & Freeze_comb3$PicCategory_f == 'H', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V3' & Freeze_comb3$PicCategory_f == 'A', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V3' & Freeze_comb3$PicCategory_f == 'N', 3,
                                   ifelse(Freeze_comb3$BlockVersion_f == 'V4' & Freeze_comb3$PicCategory_f == 'N', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V4' & Freeze_comb3$PicCategory_f == 'A', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V4' & Freeze_comb3$PicCategory_f == 'H', 3,
                                   ifelse(Freeze_comb3$BlockVersion_f == 'V5' & Freeze_comb3$PicCategory_f == 'H', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V5' & Freeze_comb3$PicCategory_f == 'N', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V5' & Freeze_comb3$PicCategory_f == 'A', 3,
                                   ifelse(Freeze_comb3$BlockVersion_f == 'V6' & Freeze_comb3$PicCategory_f == 'N', 1, ifelse(Freeze_comb3$BlockVersion_f == 'V6' & Freeze_comb3$PicCategory_f == 'H', 2, ifelse(Freeze_comb3$BlockVersion_f == 'V6' & Freeze_comb3$PicCategory_f == 'A', 3, NA)))))))))))))))))))
# check whether it went fine
with(Freeze_comb3, table (BlockNr, PicCategory_f, BlockVersion_f))

_________________________________________________________________________________________________________________

_________________________________________________________________________________________________________________

# understand the raw data 
# look at descriptives
library(psych)
describe(Freeze_comb3)
with(Freeze_comb3, describeBy(SD_AP, group = list(PicCategory_f), mat = TRUE))

# mean       sd   median  trimmed      mad       min      max    range     skew  kurtosis
# 11    1      A    1 1579 2.487983 1.567388 2.113215 2.250528 1.085078 0.4443332 16.52949 16.08516 2.590069 12.127027
# 12    2      H    1 1579 2.514134 1.522925 2.105778 2.285240 1.073858 0.3777251 12.01518 11.63746 1.889186  5.196374
# 13    3      N    1 1579 2.665726 1.873101 2.201738 2.360275 1.162624 0.4597381 18.04508 17.58534 3.011783 14.167870

# means can be used for plotting 
# SEs are wrong! They don't take into account repeated-measures nature

# plot raw data
# distribution of DV

install.packages("lattice")
library(lattice)

with(Freeze_comb3, densityplot(SD_AP)) # positively skewed

library(psych)
skew(Freeze_comb3$SD_AP) # skewness 2.67
length(Freeze_comb3$SD_AP)

install.packages('pastecs')
library(pastecs)

stat.desc(Freeze_comb3$SD_AP, basic = FALSE, norm = TRUE)

# separate per face condition
with(Freeze_comb3, densityplot(~ SD_AP | PicCategory_f)) # all categories are positively skewed

library(ggplot2)
# same with the non-transformed DV: there is some difference in the peaks: it looks like for happy and angry the peak is more towards LESS body sway, compared to neutral, which IS consistent with the freeze idea
ggplot(Freeze_comb3, aes(x= SD_AP, fill = PicCategory_f)) + geom_density(alpha = 0.2)

# separate per participant
with(Freeze_comb3, densityplot(~ SD_AP | Subject_f))

# separate per face identiy condition
with(Freeze_comb3, densityplot(~ SD_AP | PicIdent_f))

_________________________________________________________________________________________________________________________

# get descriptives per gender
with(Freeze_comb3, describeBy(SD_AP, group = list(gender_f), mat = TRUE))

# item group1 vars    n     mean       sd   median  trimmed      mad       min      max    range     skew kurtosis
# 11    1      f    1 2400 2.270952 1.285675 1.963660 2.092648 1.004944 0.3777251 12.61594 12.23822 1.802539  5.48818
# 12    2      m    1 2337 2.848626 1.934443 2.344985 2.529614 1.231697 0.4803445 18.04508 17.56473 2.638205 10.80602

# separate per gender_f
with(Freeze_comb3, densityplot(~ SD_AP | gender_f))

# staffdiagram
bar_gender <- ggplot(Freeze_comb3, aes(gender_f, SD_AP)) + stat_summary (fun.y = mean, geom = "bar", fill = "Blue", colour = "Black") + labs(x = "Gender", y = "Mean body sway (SD_AP)") + coord_cartesian(ylim=c(2, 3)) 
bar_gender
_____________________________________________________________________________________________________________

# get descriptives per blockNr

with(Freeze_comb3, describeBy(SD_AP, group = list(BlockNr), mat = TRUE))

# 11    1      1    1 1579 2.390407 1.497616 2.013495 2.169970 1.038882 0.3777251 16.49060 16.11287 2.587023 13.005596
# 12    2      2    1 1579 2.534908 1.518607 2.144966 2.304491 1.082824 0.4051024 12.01518 11.61008 2.019590  6.013977
# 13    3      3    1 1579 2.742528 1.921256 2.254646 2.425129 1.211835 0.4803445 18.04508 17.56473 2.852962 12.675055

# bar
bar_BlockNr <- ggplot(Freeze_comb3, aes(BlockNr, SD_AP)) + stat_summary (fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Block Number", y = "Mean body sway (SD_AP)") 
bar_BlockNr

# bar
bar_BlockNr_PicNr <-ggplot(Freeze_comb3, aes(PicNr, SD_AP, fill = BlockNr)) + stat_summary (fun.y = mean, geom = "bar") + facet_wrap ( ~ BlockNr) + labs(x = "Number of Picture", y = "Mean body sway (SD_AP)") + opts(legend.position = "none") + coord_cartesian(ylim=c(2.0, 3.1)) 
bar_BlockNr_PicNr
______________________________________________________________________________________________________________

#histogram
install.packages('ggplot2')
library(ggplot2) # for pretty densityplots;

hist.SD_AP <- ggplot(Freeze_comb3, aes(SD_AP)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "SD_AP", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(Freeze_comb3$SD_AP, na.rm = TRUE), sd = sd(Freeze_comb3$SD_AP, na.rm = TRUE)), colour = "black", size = 1) 
hist.SD_AP

#qqplot
qqplot.SD_AP <- qplot(sample = Freeze_comb3$SD_AP, stat = "qq")
qqplot.SD_AP

# z-score to check for outliers
boxplot(Freeze_comb3$SD_AP, xlab = "Across Angry Happy Neutral", ylab = "SD_AP in mm")

Freeze_comb3$SD_APz <- scale(Freeze_comb3$SD_AP)
describe(Freeze_comb3$SD_APz)
stat.desc(Freeze_comb3$SD_APz)
sortBSFbySD_APz <- Freeze_comb3[order(Freeze_comb3$SD_APz),] # sort body sway of faces by z-score of SD_AP
table(Freeze_comb3$SD_APz > 4 | Freeze_comb3$SD_APz < -4) # n = 41 outliers 
tail(sortBSFbySD_APz, n = 41)

# do a transform?
# Log transformation and save as new variable
# SD_AP
Freeze_comb3$SD_APLOG <- log(1+Freeze_comb3$SD_AP) # looks best; skewness of 0.74
densityplot(Freeze_comb3$SD_APLOG) 
with(Freeze_comb3, densityplot(~ SD_APLOG | PicCategory_f)) 
ggplot(Freeze_comb3, aes(x= SD_APLOG, fill = PicCategory_f)) + geom_density(alpha = 0.2)
stat.desc(Freeze_comb3$SD_APLOG, basic = FALSE, norm = TRUE) # get skewness & kurtosis and significant levels
describe(Freeze_comb3$SD_APLOG)

#histogram
hist.SD_APLOG <- ggplot(Freeze_comb3, aes(SD_APLOG)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "SD_AP", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(Freeze_comb3$SD_APLOG, na.rm = TRUE), sd = sd(Freeze_comb3$SD_APLOG, na.rm = TRUE)), colour = "black", size = 1) 
hist.SD_APLOG

#qqplot
qqplot.SD_APLOG <- qplot(sample = Freeze_comb3$SD_APLOG, stat = "qq")
qqplot.SD_APLOG

#Square root transformation
Freeze_comb3$SD_AP_Sqrt <- sqrt(Freeze_comb3$SD_AP)
densityplot(Freeze_comb3$SD_AP_Sqrt) # still positively skewed of 1.26
with(Freeze_comb3, densityplot(~ SD_AP_Sqrt | PicCategory_f)) 
stat.desc(Freeze_comb3$SD_AP_Sqrt, basic = FALSE, norm = TRUE) # get skewness & kurtosis and significant levels
describe(Freeze_comb3$SD_AP_Sqrt)

#histogram
hist.SD_AP_Sqrt <- ggplot(Freeze_comb3, aes(SD_AP_Sqrt)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "SD_AP_Sqrt", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(Freeze_comb3$SD_AP_Sqrt, na.rm = TRUE), sd = sd(Freeze_comb3$SD_AP_Sqrt, na.rm = TRUE)), colour = "black", size = 1) 
hist.SD_AP_Sqrt

#qqplot
qqplot.SD_AP_Sqrt <- qplot(sample = Freeze_comb3$SD_AP_Sqrt, stat = "qq")
qqplot.SD_AP_Sqrt

#Square root transformation + 1
Freeze_comb3$SD_AP_Sqrt1 <- sqrt(1+Freeze_comb3$SD_AP)
densityplot(Freeze_comb3$SD_AP_Sqrt1) # still positively skewed of 1.55
with(Freeze_comb3, densityplot(~ SD_AP_Sqrt1 | PicCategory_f)) 
stat.desc(Freeze_comb3$SD_AP_Sqrt1, basic = FALSE, norm = TRUE)
describe(Freeze_comb3$SD_AP_Sqrt1)

#histogram
hist.SD_AP_Sqrt1 <- ggplot(Freeze_comb3, aes(SD_AP_Sqrt1)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "SD_AP_Sqrt1", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(Freeze_comb3$SD_AP_Sqrt1, na.rm = TRUE), sd = sd(Freeze_comb3$SD_AP_Sqrt1, na.rm = TRUE)), colour = "black", size = 1) 
hist.SD_AP_Sqrt1

#qqplot
qqplot.SD_AP_Sqrt1 <- qplot(sample = Freeze_comb3$SD_AP_Sqrt1, stat = "qq")
qqplot.SD_AP_Sqrt1

## Best transformation for SD_AP is log transformation

# check outliers in log transformation via z-scores
boxplot(Freeze_comb3$SD_APLOG, xlab = "Across Angry Happy Neutral", ylab = "SD_AP in mm (log)")

Freeze_comb3$SD_APLOGz <- scale(Freeze_comb3$SD_APLOG)
stat.desc(Freeze_comb3$SD_APLOGz)
describe(Freeze_comb3$SD_APLOGz)
sortBSFbySD_APLOGz <- Freeze_comb3[order(Freeze_comb3$SD_APLOGz),] # sort body sway of faces by z-score of SD_APLOG
table(Freeze_comb3$SD_APLOGz > 4 | Freeze_comb3$SD_APLOGz < -4) # n = 8 outliers 
tail(sortBSFbySD_APLOGz, n = 8)

# boxplots for dataframe Freeze_comb3 with DV SD_APLOG and group PicCategory_f
boxplot(Freeze_comb3$SD_APLOG ~ Freeze_comb3$PicCategory_f, xlab = "Picture Category", ylab = "SD_AP in mm")

install.packages('Hmisc')
library(Hmisc)

# bar chart for datframe Freeze_comb3 with DV SD_APLOG and group PicCategory_f
barSD_APLOG <- ggplot(Freeze_comb3, aes(PicCategory_f, SD_APLOG)) 
barSD_APLOG + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary (fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + labs (x = "Picture Category", y = "Mean SD_APLOG") # add error bars


# separate per face condition
with(Freeze_comb3, densityplot(~ SD_APLOG | PicCategory_f)) # all categories are positively skewed

# separate per participant
with(Freeze_comb3, densityplot(~ SD_APLOG | Subject_f))

# separate per face identity condition
with(Freeze_comb3, densityplot(~ SD_APLOG | PicIdent_n))

write.table(Freeze_comb3, file = 'SummaryFreeze_SD_APLOG_Combined.csv')

Freeze_comb3 <- read.table('SummaryFreeze_SD_APLOG_Combined.csv', header = TRUE)

______________________________________________________________________________________________________________________________

Freeze_comb3 <- read.table('SummaryFreeze_SD_APLOG_Combined.csv', header = TRUE)

Freeze_comb3$EmotionVsNeutral <- as.factor(ifelse(Freeze_comb3$PicCategory_f == 'A' | Freeze_comb3$PicCategory_f == 'H', 'Emotion', ifelse(Freeze_comb3$PicCategory_f == 'N', 'Neutral', NA)))

ggplot(Freeze_comb3, aes(x= SD_AP, fill = EmotionVsNeutral)) + geom_density(alpha = 0.2)

#Contrast setting
options(contrasts = c("contr.sum", "contr.poly"))
Dev_cont_EmotionVsNeutral_f <- contrasts(Freeze_comb3$EmotionVsNeutral)
Dev_cont_EmotionVsNeutral_f <- cbind(c(1,1), Dev_cont_EmotionVsNeutral_f)
solve(Dev_cont_EmotionVsNeutral_f)

# center BlockNr
Freeze_comb3$BlockNr_c <- scale(Freeze_comb3$BlockNr, center = TRUE, scale = FALSE)
with(Freeze_comb3, table (BlockNr, BlockNr_c))

# check whether centering went fine for PicNr_c

Freeze_comb3$PicNr_c <- scale(Freeze_comb3$PicNr, center = TRUE, scale = FALSE)
with(Freeze_comb3, table(PicNr, PicNr_c))

_______________________________________________________________________________________________________________________

# set up mixed model
# effects of interest: main effect of EmotionVsNeutral
# grouping variables: Subject_f --> random per-participant intercept & PicIdentity_f --> random per-participant intercept
# within-unit effects: EmotionVsNeutral --> random participant 'EmotionVsNeutral' slope
# between-unit effects: gender
# Random effects: 
# per-participant intercept
# per-pic Identity intercept
# per-participant 'EmotionVsNeutral' * 'PicNr_c' * 'BlockNr_c' slope
# random correlation terms among all random effects

install.packages('Matrix', repos = 'http://cran.us.r-project.org')
library(Matrix)
install.packages('pbkrtest', repos = 'http://cran.us.r-project.org')
library(pbkrtest)
install.packages('lme4', repos = c("http://lme4.r-forge.r-project.org/repos", getOption("repos")))
library(lme4)

_______________________________________________________________________________________________________________________

bodysway_1 <- lmer(SD_APLOG ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f) + (1 | PicIdent_f), data = Freeze_comb3)

# Warnmeldungen:
# 1: In commonArgs(par, fn, control, environment()) : maxfun < 10 * length(par)^2 is not recommended.
# 2: In optwrap(optimizer, devfun, getStart(start, rho$lower, rho$pp),  : convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
# 3: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  : Model failed to converge with max|grad| = 0.454166 (tol = 0.001)

print(summary(bodysway_1), corr = FALSE)

# Take PicIdent out as it explaing no variance. 

bodysway_2 <- lmer(SD_APLOG ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f), data = Freeze_comb3)

# Warnmeldungen:
# 1: In commonArgs(par, fn, control, environment()) : maxfun < 10 * length(par)^2 is not recommended.
# 2: In optwrap(optimizer, devfun, getStart(start, rho$lower, rho$pp),  : convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
# 3: In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  : Model failed to converge with max|grad| = 1.40823 (tol = 0.001)

print(summary(bodysway_2), corr = FALSE)

# Linear mixed model fit by REML ['lmerMod']
# Formula: SD_APLOG ~ gender_f + EmotionVsNeutral * PicNr_c * BlockNr_c +      (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f) 
# Data: Freeze_comb3 

# REML criterion at convergence: 2705.5 

# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8714 -0.6809 -0.0348  0.6317  4.3061 

# Random effects:
#   Groups    Name                    Variance  Std.Dev. Corr                                     
# Subject_f (Intercept)                4.213e-02 0.205267                                          
# EmotionVsNeutral1                   1.661e-03 0.040755 -0.21                                    
# PicNr_c                             1.254e-05 0.003541  0.34 -0.55                              
# BlockNr_c                           1.512e-03 0.038880  0.46 -0.55  0.66                        
# EmotionVsNeutral1:PicNr_c           1.225e-05 0.003500 -0.14 -0.51 -0.39 -0.23                  
# EmotionVsNeutral1:BlockNr_c         3.716e-03 0.060960 -0.18  0.49 -0.12  0.04 -0.59            
# PicNr_c:BlockNr_c                   1.300e-05 0.003606  0.38  0.29  0.20 -0.38 -0.30 -0.33      
# EmotionVsNeutral1:PicNr_c:BlockNr_c 1.732e-05 0.004162 -0.12 -0.34  0.72  0.03 -0.19 -0.10  0.35
# Residual                            9.338e-02 0.305575                                          
# Number of obs: 4737, groups: Subject_f, 79

# Fixed effects:
  #                                   Estimate Std. Error t value
# (Intercept)                          1.1943924  0.0238543   50.07
# gender_f1                           -0.0648089  0.0223346   -2.90
# EmotionVsNeutral1                   -0.0146141  0.0071024   -2.06
# PicNr_c                              0.0023296  0.0009393    2.48
# BlockNr_c                            0.0367046  0.0090285    4.07
# EmotionVsNeutral1:PicNr_c           -0.0011140  0.0009244   -1.21
# EmotionVsNeutral1:BlockNr_c          0.0104448  0.0120228    0.87
# PicNr_c:BlockNr_c                   -0.0011575  0.0011460   -1.01
# EmotionVsNeutral1:PicNr_c:BlockNr_c -0.0007293  0.0011666   -0.63

_______________________________________________________________________________________________________________________

# increase number of iterations

bodysway_3 <- lmer(SD_APLOG ~  gender_f + EmotionVsNeutral *  PicNr_c * BlockNr_c + (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f), data = Freeze_comb3 , control=lmerControl(optCtrl=list(maxfun=2000000)))

# Warnmeldung:
# In checkConv(attr(opt, "derivs"), opt$par, checkCtrl = control$checkConv,  : Model failed to converge with max|grad| = 0.00803627 (tol = 0.001)

print(summary(bodysway_3), corr = FALSE)

# Linear mixed model fit by REML ['lmerMod']
# Formula: SD_APLOG ~ gender_f + EmotionVsNeutral * PicNr_c * BlockNr_c +      (1 + EmotionVsNeutral * PicNr_c * BlockNr_c | Subject_f) 
# Data: Freeze_comb3 
# Control: lmerControl(optCtrl = list(maxfun = 2e+06)) 

# REML criterion at convergence: 2705.5 

# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.8674 -0.6803 -0.0350  0.6309  4.3066 

# Random effects:
#  Groups    Name                     Variance  Std.Dev. Corr                                     
# Subject_f (Intercept)               4.214e-02 0.205278                                          
# EmotionVsNeutral1                   1.669e-03 0.040851 -0.21                                    
# PicNr_c                             1.269e-05 0.003563  0.34 -0.54                              
# BlockNr_c                           1.499e-03 0.038712  0.46 -0.54  0.68                        
# EmotionVsNeutral1:PicNr_c           1.224e-05 0.003498 -0.14 -0.52 -0.39 -0.26                  
# EmotionVsNeutral1:BlockNr_c         3.678e-03 0.060643 -0.18  0.48 -0.15  0.05 -0.57            
# PicNr_c:BlockNr_c                   1.288e-05 0.003589  0.38  0.31  0.20 -0.37 -0.30 -0.35      
# EmotionVsNeutral1:PicNr_c:BlockNr_c 1.715e-05 0.004142 -0.11 -0.36  0.73  0.04 -0.20 -0.12  0.36
# Residual                            9.337e-02 0.305569                                          
# Number of obs: 4737, groups: Subject_f, 79

# Fixed effects:
#  Estimate Std. Error t value
# (Intercept)                          1.1943873  0.0238498   50.08
# gender_f1                           -0.0648211  0.0223423   -2.90
# EmotionVsNeutral1                   -0.0146031  0.0070979   -2.06
# PicNr_c                              0.0023320  0.0009398    2.48
# BlockNr_c                            0.0367042  0.0090297    4.06
# EmotionVsNeutral1:PicNr_c           -0.0011155  0.0009239   -1.21
# EmotionVsNeutral1:BlockNr_c          0.0104593  0.0120105    0.87
# PicNr_c:BlockNr_c                   -0.0011547  0.0011454   -1.01
# EmotionVsNeutral1:PicNr_c:BlockNr_c -0.0007323  0.0011664   -0.63

_______________________________________________________________________________________________________________________

# use standardized instead of centered PicNr and BlockNr

# standardize BlockNr
Freeze_comb3$BlockNr_s <- scale(Freeze_comb3$BlockNr, center = TRUE, scale = TRUE)
mean(Freeze_comb3$BlockNr_s)
sd(Freeze_comb3$BlockNr_s)

# standardize BlockNr
Freeze_comb3$PicNr_s <- scale(Freeze_comb3$PicNr, center = TRUE, scale = TRUE)
mean(Freeze_comb3$PicNr_s)
sd(Freeze_comb3$PicNr_s)

bodysway_4 <- lmer(SD_APLOG ~  gender_f + EmotionVsNeutral *  PicNr_s * BlockNr_s + (1 + EmotionVsNeutral * PicNr_s * BlockNr_s | Subject_f), data = Freeze_comb3 , control=lmerControl(optCtrl=list(maxfun=2000000)))
# no convergence problems

print(summary(bodysway_4), corr = FALSE)

# Linear mixed model fit by REML ['lmerMod']
# Formula: SD_APLOG ~ gender_f + EmotionVsNeutral * PicNr_s * BlockNr_s +      (1 + EmotionVsNeutral * PicNr_s * BlockNr_s | Subject_f) 
# Data: Freeze_comb3 
# Control: lmerControl(optCtrl = list(maxfun = 2e+06)) 

# REML criterion at convergence: 2693.1 

# Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
# -2.8674 -0.6804 -0.0350  0.6309  4.3066 

# Random effects:
#  Groups    Name                     Variance  Std.Dev. Corr                                     
# Subject_f (Intercept)               0.0421386 0.20528                                           
# EmotionVsNeutral1                   0.0016689 0.04085  -0.21                                    
# PicNr_s                             0.0004216 0.02053   0.34 -0.54                              
# BlockNr_s                           0.0009992 0.03161   0.46 -0.54  0.68                        
# EmotionVsNeutral1:PicNr_s           0.0004066 0.02016  -0.14 -0.52 -0.39 -0.26                  
# EmotionVsNeutral1:BlockNr_s         0.0024521 0.04952  -0.18  0.48 -0.15  0.05 -0.57            
# PicNr_s:BlockNr_s                   0.0002853 0.01689   0.38  0.31  0.20 -0.37 -0.30 -0.35      
# EmotionVsNeutral1:PicNr_s:BlockNr_s 0.0003800 0.01949  -0.11 -0.36  0.73  0.04 -0.20 -0.12  0.36
# Residual                            0.0933722 0.30557                                           
# Number of obs: 4737, groups: Subject_f, 79

# Fixed effects:
#                                     Estimate Std. Error t value
# (Intercept)                          1.194387   0.023850   50.08
# gender_f1                           -0.064821   0.022342   -2.90
# EmotionVsNeutral1                   -0.014603   0.007098   -2.06
# PicNr_s                              0.013441   0.005416    2.48
# BlockNr_s                            0.029972   0.007373    4.06
# EmotionVsNeutral1:PicNr_s           -0.006429   0.005325   -1.21
# EmotionVsNeutral1:BlockNr_s          0.008541   0.009807    0.87
# PicNr_s:BlockNr_s                   -0.005435   0.005391   -1.01
# EmotionVsNeutral1:PicNr_s:BlockNr_s -0.003447   0.005490   -0.63

_______________________________________________________________________________________________________________________

# make diagnostic plots of the model

plot(bodysway_4) # fitted vs. residuals
library(lattice)
densityplot(resid(bodysway_4)) # distribution of the resicuals
plot(Freeze_comb3$SD_APLOG, fitted(bodysway_4)) # raw vs. fitted

_______________________________________________________________________________________________________________________

# Significance testing

library(car)
Anova(bodysway_4, type = 3, test = 'F') # gives an error

Anova(bodysway_4, type = 3, test = 'Chisq')

# Analysis of Deviance Table (Type III Wald chisquare tests)

# Response: SD_APLOG
# Chisq Df Pr(>Chisq)    
# (Intercept)                        2507.9833  1  < 2.2e-16 ***
# gender_f                              8.4174  1   0.003716 ** 
# EmotionVsNeutral                      4.2329  1   0.039649 *  
# PicNr_s                               6.1580  1   0.013082 *  
# BlockNr_s                            16.5230  1  4.806e-05 ***
# EmotionVsNeutral:PicNr_s              1.4578  1   0.227288    
# EmotionVsNeutral:BlockNr_s            0.7584  1   0.383835    
# PicNr_s:BlockNr_s                     1.0164  1   0.313381    
# EmotionVsNeutral:PicNr_s:BlockNr_s    0.3941  1   0.530136    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# bars
library(ggplot2)
bar_EmotionVsNeutral<- ggplot(Freeze_comb3, aes(EmotionVsNeutral, SD_APLOG)) + stat_summary (fun.y = mean, geom = "bar", fill = "Red", colour = "Black", width = 0.2) + labs(x = "Picture Type", y = "Mean body sway (SD_AP log)") + coord_cartesian(ylim=c(1.1, 1.25)) 
bar_EmotionVsNeutral

bar_PicNr_s<- ggplot(Freeze_comb3, aes(PicNr_s, SD_APLOG)) + stat_summary (fun.y = mean, geom = "bar", fill = "Red", colour = "Black", width = 0.2) + labs(x = "Picture Number", y = "Mean body sway (SD_AP log)") + coord_cartesian(ylim=c(1.1, 1.25)) 
bar_PicNr_s

bar_BlockNr_s<- ggplot(Freeze_comb3, aes(BlockNr_s, SD_APLOG)) + stat_summary (fun.y = mean, geom = "bar", fill = "Red", colour = "Black", width = 0.2) + labs(x = "Block Number", y = "Mean body sway (SD_AP log)") + coord_cartesian(ylim=c(1.1, 1.25)) 
bar_BlockNr_s
