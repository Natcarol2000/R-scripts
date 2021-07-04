data_32_pp_Final <- read.csv("~/Dropbox/All desktop folders/experimento_MTRILL/data_32_pp_Final.csv", sep=";")
data_32_pp_Final_prime1_prime2 <- read.csv("~/Dropbox/All desktop folders/experimento_MTRILL/data_32_pp_Final_prime1_prime2.csv", sep=";")
data=data_32_pp_Final_prime1_prime2
data=data_32_pp_Final
View(data)
data=data[-c(7)]
names(data)

data$Participants=as.factor(data$Participants)
data$Structure=as.factor(data$Structure)
data$Items=as.numeric(data$Items)
data$Prime=as.numeric(data$Prime)
data$EnglishGrade=as.numeric(data$EnglishGrade)
data$Test=as.factor(data$Test)
data$Items_id=as.factor(data$Items_id)
data$prime1=as.factor(data$prime1)
data$prime2=as.factor(data$prime2)



summary(data)

library(psych)

describe(data)

with(data, describeBy(data$Prime, group = list(data$Test), mat = TRUE))
with(data, describeBy(data$Prime, group = list(data$Participants), mat = TRUE))
with(data, describeBy(data$Prime, group = list(data$Items), mat = TRUE))
with(data, describeBy(data$Prime, group = list(data$Items_id), mat = TRUE))
with(data, describeBy(data$Prime, group = list(data$prime1), mat = TRUE))
with(data, describeBy(data$Prime, group = list(data$prime2), mat = TRUE))

tapply(data$Prime, data$Test, mean)

contr.treatment(3)

#assigning the treatment contrasts to test
contrasts(data$Test) = contr.treatment(3)


#models

library(lme4)
library(car)

data$EnglishGrade=scale(data$EnglishGrade, center = TRUE)
data$Items=scale(data$Items, center = TRUE)


library(plyr)

data$Structure=mapvalues(data$Structure, from = c("OF", "NO"), to = c("PNP", "Other"))
View(data)

boxplot(data$Prime ~ data$Test, data = data, notch=TRUE, outline=TRUE)

boxplot(data$Prime ~ data$Items, data = data, notch=TRUE, outline=TRUE)
counts <- table(data$Test, data$Structure)
counts
df_counts=data.frame(counts)


barplot(counts, main="Count of structures produced per test phase",
        xlab="Number of Structures", col=c("black","light grey", "grey"),
        legend = rownames(counts), beside=TRUE)

posttest=subset(data, Test=="posttest")
View(posttest)
priming=subset(data, Test=="priming")
View(priming)

boxplot(priming$EnglishGrade ~ priming$Structure, data=priming,col=c("light gray"), notch=TRUE, main="English proficiency vs.structures produced", ylab="English test score", outline=FALSE)
boxplot(posttest$EnglishGrade ~ posttest$Structure, data=posttest,col=c("light gray"), notch=TRUE, main="English proficiency vs.structures produced", ylab="English test score", outline=FALSE)

boxplot(posttest$EnglishGrade ~ posttest$Structure, data=data,col=c("light gray"), notch=TRUE, main="English proficiency vs.structures produced", ylab="English test score", outline=FALSE)
boxplot(posttest$EnglishGrade ~ posttest$Prime, data=data,col=c("light gray"), notch=TRUE, main="English proficiency vs.structures produced", ylab="English test score", outline=FALSE)
boxplot(priming$EnglishGrade ~ primingt$Prime, data=data,col=c("light gray"), notch=TRUE, main="English proficiency vs.structures produced", ylab="English test score", outline=FALSE)



attach(data)
m1 <- glmer(Prime ~ EnglishGrade+Test+ Items+ (1|Participants) + (1|Items_id),family=binomial(link = "logit"), data = data, control=glmerControl(optCtrl=list(maxfun=2000000)))
summary(m1)
m2 <- glmer(Prime ~ EnglishGrade*Test+Items+(1|Participants) + (1|Items_id),family=binomial(link = "logit"), data = data, , control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m2)

#most significant model is model 3
names(data)
m3 <- glmer(Prime ~ EnglishGrade*Test+Items+(1+Test|Participants) + (1|Items_id),family=binomial(link = "logit"), data = data, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2000000)))
summary(m3)

m3.1 <- glmer(Prime ~ EnglishGrade*Test + Items+ prime1+prime2+(1+Test|Participants),family=binomial(link = "logit"), data = data, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m3.1) #best

m3.2 <- glmer(Prime ~ EnglishGrade*Test*Items+(1+Test|Participants),family=binomial(link = "logit"), data = data, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m3.2)

m4 <- glmer(Prime ~ EnglishGrade*Test+(1+EnglishGrade|Participants),family=binomial(link = "logit"), data = data, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m4)

m5 <- glmer(Prime ~ Test*EnglishGrade+ (1+EnglishGrade*Test|Participants),family=binomial(link = "logit"), data = data, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(m5)

#likelihood ratio test with anova()

anova(m1,m2) #m2 significant
anova(m2,m3) #m3 significant
anova(m3.1,m4) #m4 no significant
anova(m1,m3)
anova(m4,m5)
anova(m3,m4)
anova(m3,m5)
anova(m3,m3.1)
anova(m3.2,m3.1)

library(ggplot2)

attach(data)

interaction.plot(Test, Structure,EnglishGrade, xlab='Experimental phases', ylab='English test grades',leg.bty = "o", legend=TRUE, col = 1:5)


ggplot() +
  aes(x = Test, y = EnglishGrade, color = Structure) +
  geom_line(aes(group = Structure)) +
  geom_point()


ggplot() +
  aes(x = Test, color = Structure, group = Structure, y = EnglishGrade) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")