responses <- read.csv("~/Dropbox/All desktop folders/experimento_MTRILL/responses_survey_numbered_columns.csv", sep=";")
responses_survey_numbered_columns <- read.csv("~/Desktop/experimento_MTRILL/responses_survey_numbered_columns.csv", sep=";")
file_gender_nouns_categories_reg_trans_other <- read.csv("~/Dropbox/Gender_Model_project_tensorflow/file_gender_nouns_categories_reg_trans_other.csv", sep=";")
dt=file_gender_nouns_categories_reg_trans_other
dt=responses
dt=responses_survey_numbered_columns
dt= dt[-c(4)]
View(dt)

dt$Label=as.factor(dt$Label)
dt$Word=as.factor(dt$Word)
dt$Category=as.factor(dt$Category)


dt=responses
library(broom)
library(tibble)
library(ggplot2)
library(plyr)
ddply(dt,.(p1), function(x) with(x,data.frame(100*round(table(p2)/length(p2),2))))
attach(dt)
table1=table(dt$Label)
prop.table(table1)*100


table2 <- table(p2, p1)
prop.table(table2)*100
dt$p24=as.factor(dt$p24)
dt$p25=as.factor(dt$p25)
dt$p26=as.factor(dt$p26)
dt$p27=as.factor(dt$p27)
dt$p28=as.factor(dt$p28)
dt$p29=as.factor(dt$p29)
dt$p30=as.factor(dt$p30)
table1=table(dt$p4, dt$p24)
prop.table(table1)*100
table(p24)
dt$Category=revalue(dt$Category, c("Outro"="Other"))
mapvalues(dt$Category, from = c("Outro"), to = c("Other"))
ggplot(dt, aes(x = Category)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table3=table(p4)
table3
prop.table(table3)*100
attach(dt)
ggplot(dt, aes(x = Label)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table4=table(p3)
prop.table(table4)*100


ggplot(dt, aes(x = p21)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(dt, aes(x = reorder_size(Category))) +
  geom_bar() +
  xlab("Nouns forms") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(dt, aes(x = reorder_size(p2))) +
  geom_bar() +
  xlab("Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dt, aes(x = reorder_size(p4))) +
  geom_bar() +
  xlab("Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dt, aes(x = reorder_size(p1))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Age") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dt, aes(x = reorder_size(p2))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Sex") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dt, aes(x = reorder_size(p1))) +
  +     geom_bar(aes(y = (..count..)/sum(..count..))) +
  +     xlab(p1) +
  +     scale_y_continuous(labels = scales::percent, name = "Proportion") +
  +     facet_grid(~ p2) +
  +     theme(axis.text.x = element_text(angle = 45, hjust = 1))


table4=table(p7)
prop.table(table4)*100

ggplot(dt, aes(x = p7)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


table5=table(p21)
prop.table(table5)*100

table6=table(p14)
table6
prop.table(table6)*100

ggplot(dt, aes(x = reorder_size(p14))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Contry of residence") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

with(dt, describeBy(p23, group = list(p14)))

boxplot(p24,p25)
boxplot(p26,p27)
boxplot(p26,p27, p28)
boxplot(p28,p29)
table(p31)

with(dt, describeBy(p24, group = list(p4)))
with(dt, describeBy(p23, group = list(p4)))
with(dt, describeBy(p91, group = list(p4)))

attach(dt)
table(p4,p39)
table(p4,p23)
prop.table(table(p4,p39))*100

prop.table(table(p4,p40))*100


