#P600 
#data preprocessing
#set work directory
setwd("/Volumes/KINGSTON/export/P600/Data_DelimitedTab")
#uploading files
NVAO=read.table("NVAO_P600.txt", header=TRUE)
NVAT=read.table("NVAT_P600.txt", header=TRUE)
NVNO=read.table("NVNO_P600.txt", header=TRUE)
NVNT=read.table("NVNT_P600.txt", header=TRUE)
VAO=read.table("VAO_P600.txt", header=TRUE)
VAT=read.table("VAT_P600.txt", header=TRUE)
VNO=read.table("VNO_P600.txt", header=TRUE)
VNT=read.table("VNT_P600.txt", header=TRUE)
#reshaping data = from wide format to long format with melt() 
library(reshape2)
NVAO=melt(NVAO)
NVAT=melt(NVAT)
NVNO=melt(NVNO)
NVNT=melt(NVNT)
VAO=melt(VAO)
VAT=melt(VAT)
VNO=melt(VNO)
VNT=melt(VNT)

#binding files
tableP600=rbind(NVAO,NVAT,NVNO,NVNT,VAO,VAT,VNO,VNT)
#checking if binding went fine
str(tableP600) #looks good! 59 channels x 16 PPs x 8 conditions , total= 7552 obs. 

#checking names of the variables
names(tableP600)
attach(tableP600)
library(stringr)
#spliting strings of variable called 'variable' into two columns
list=str_split(variable, pattern = "[\ .]")

#adding strings into two different columns

col1 <- unlist(lapply(list, function(x) x[[1]]))
col2 <- unlist(lapply(list, function(x) x[[2]]))

#spliting variable 'col2' into 4 different colunms
list=str_split(col2, pattern = "[\ _]")

col3 <- unlist(lapply(list, function(x) x[[1]]))
col4 <- unlist(lapply(list, function(x) x[[2]]))
col5 <- unlist(lapply(list, function(x) x[[3]]))
col6 <- unlist(lapply(list, function(x) x[[4]]))

#checking if data split went fine
col3[1:100]#good
col4[1:100]#good
col5[1:100]#good

#joining all columns into a data frame
table_P600=data.frame(File, channel=col1, violation=col3, adj_noun=col4, transp_op=col5,value=value) #variable 'channel' still has "X"

#replacing "x" by "E"
attach(table_P600)

channel=str_replace_all(channel,"X","E")

#again joining all columns into a data frame

table_P600=data.frame(File, channel, violation=col3, adj_noun=col4, transp_op=col5,value=value) #looks good

#adding 1 column to data frame

table_P600["position"]=NA

#adding electrodes position= 'midline'

table_P600$position = as.factor(ifelse(table_P600$channel %in% c('E31','E58','E59','E60','E5','E18'), 'midline', 
                                       ifelse(table_P600$channel %in% c('E26','E27','E28','E29','E30','E31','E37','E50'), 'midline', 
                                              as.character(table_P600$position))))

#adding electrodes position=  'anterior', 'posterior'

table_P600$position = as.factor(ifelse(table_P600$channel %in% c('E34','E39','E46','E53', 'E38','E44', 'E45','E52','E57','E51','E56','E2', 'E6','E7','E14','E21','E13','E20','E25','E19','E12','E24'), 'anterior', 
                                       ifelse(table_P600$channel %in% c('E1', 'E4', 'E10', 'E17','E11', 'E3','E9','E16','E23', 'E22', 'E15','E8', 'E40','E47','E54','E35','E41', 'E48','E55','E33','E36','E42','E49','E43'), 'posterior', 
                                              as.character(table_P600$position))))

#adding 1 column to data frame
table_P600["left_right"]=NA
#adding hemisphere 
table_P600$left_right = as.factor(ifelse(table_P600$channel %in% c('E34','E39','E46','E53', 'E38','E45','E52','E57','E51','E56','E40','E47','E54','E35','E41', 'E48','E55','E33','E36','E42','E49','E43','E44'), 'left', 
                                         ifelse(table_P600$channel %in% c('E1', 'E4', 'E10', 'E17','E11', 'E3','E9','E16','E23', 'E22', 'E15','E8','E2', 'E6','E7','E14','E21','E13','E20','E25','E19','E12','E24'), 'right',
                                                ifelse(table_P600$channel %in% c('E31','E58','E59','E60','E26','E27','E28','E29','E30','E18','E5','E50','E37'), 'midline', 
                                                       as.character(table_P600$left_right)))))



#exporting the data
write.table(table_P600, 'table_P600.feitaemBH.txt', sep='\t', row.names=FALSE)

#P600 stats

#reading table
Table_P600=read.table("table_P600.feitaemBH.txt", header=TRUE)
Table_P600=table_P600.new
str(Table_P600) #checking table variables: everything ok
library(ez)
attach(Table_P600)
ezANOVA(
  data=Table_P600
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op","position"))

ezANOVA(
  data=subset(Table_P600, position=="posterior")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

ezANOVA(
  data=subset(Table_P600, position=="anterior")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

ezANOVA(
  data=subset(Table_P600, position=="anterior" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

ezANOVA(
  data=subset(Table_P600, position=="posterior" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op")) 

ezANOVA(
  data=subset(Table_P600, position=="anterior" & left_right=="right")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op")) 

ezANOVA(
  data=subset(Table_P600, position=="posterior" & left_right=="right")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

ezANOVA(
  data=subset(Table_P600, position=="midline" & left_right=="midline")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

#effects found: 
#full data = position/violation:position
#posterior (colapsed left - right) = violation / violation:transp_op
#Posterior left = violation/adj_noun/violation:transp_op

#t.tests

#subsetting data

# 8 conditions
#anterior left
Op_N_V=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
Op_N_NV=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="anterior" & left_right=="left", select=c(value))
Op_a_v=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
Op_a_Nv=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="anterior" & left_right=="left", select=c(value))
T_N_V=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
T_N_NV=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="anterior" & left_right=="left", select=c(value))
T_a_v=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
T_a_Nv=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="anterior" & left_right=="left", select=c(value))

#paired t tests for anterior left 
#Opaque x Transparents
t.test(Op_N_V,T_N_V, Paired= TRUE)
t.test(Op_N_NV,T_N_NV, Paired= TRUE)
t.test(Op_a_V,T_a_V, Paired= TRUE)
t.test(Op_a_Nv,T_a_Nv, Paired=TRUE) #p-value = 0.02764
#nouns x adjectives
t.test(T_N_V,T_a_v, Paired=TRUE)
t.test(Op_N_V,Op_a_v, Paired=TRUE) # p-value = 0.01464
t.test(T_N_NV,T_a_Nv, Paired=TRUE)
t.test(Op_N_NV,Op_a_Nv, Paired=TRUE)#p-value = 0.0005058
#violation x NonViolation
t.test(T_N_V,T_N_NV, Paired=TRUE) #p-value = 0.02017
t.test(T_a_v,T_a_Nv, Paired=TRUE)#p-value = 4.718e-05
t.test(Op_a_v,Op_a_Nv, Paired=TRUE)
t.test(Op_N_V,Op_N_NV, Paired=TRUE)


#anterior right

Op_N_V_AR=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="anterior" & left_right=="right", select=c(value))
Op_N_NV_AR=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="anterior" & left_right=="right", select=c(value))
Op_a_v_AR=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="anterior" & left_right=="right", select=c(value))
Op_a_Nv_AR=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="anterior" & left_right=="right", select=c(value))
T_N_V_AR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="anterior" & left_right=="right", select=c(value))
T_N_NV_AR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="anterior" & left_right=="right", select=c(value))
T_a_v_AR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="anterior" & left_right=="right", select=c(value))
T_a_Nv_AR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="anterior" & left_right=="right", select=c(value))

#paired t tests for anterior right 
#Opaque x Transparents
t.test(Op_N_V_AR,T_N_V_AR, Paired= TRUE) #p-value = 0.02418
t.test(Op_N_NV_AR,T_N_NV_AR, Paired= TRUE)
t.test(Op_a_v_AR,T_a_v_AR, Paired= TRUE)#p-value = 0.07148
t.test(Op_a_Nv_AR,T_a_Nv_AR, Paired=TRUE) 
#nouns x adjectives
t.test(T_N_V_AR,T_a_v_AR, Paired=TRUE)
t.test(Op_N_V_AR,Op_a_v_AR, Paired=TRUE)#p-value = 3.659e-05 
t.test(T_N_NV_AR,T_a_Nv_AR, Paired=TRUE)
t.test(Op_N_NV_AR,Op_a_Nv_AR, Paired=TRUE)#p-value = 2.342e-06
#violation x NonViolation
t.test(T_N_V_AR,T_N_NV_AR, Paired=TRUE)#p-value = 0.0001619
t.test(T_a_v_AR,T_a_Nv_AR, Paired=TRUE)#p-value = 0.007683
t.test(Op_a_v_AR,Op_a_Nv_AR, Paired=TRUE)#p-value = 0.003163
t.test(Op_N_V_AR,Op_N_NV_AR, Paired=TRUE)#p-value = 0.01115

#posterior left

Op_N_V_PL=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
Op_N_NV_PL=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="posterior" & left_right=="left", select=c(value))
Op_A_V_PL=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
Op_A_NV_PL=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="posterior" & left_right=="left", select=c(value))
T_N_V_PL=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
T_N_NV_PL=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="posterior" & left_right=="left", select=c(value))
T_A_V_PL=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
T_A_NV_PL=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="posterior" & left_right=="left", select=c(value))


#paired t tests for posterior left 
#Opaque x Transparents
t.test(Op_N_V_PL,T_N_V_PL, Paired= TRUE)#  p-value =0.004693
t.test(Op_N_NV_PL,T_N_NV_PL, Paired= TRUE)
t.test(Op_A_V_PL,T_A_V_PL, Paired= TRUE)
t.test(Op_A_NV_PL,T_A_NV_PL, Paired=TRUE)# p-value = 0.0008159 
#nouns x adjectives
t.test(T_N_V_PL,T_A_V_PL, Paired=TRUE)#p-value = 0.02324
t.test(Op_N_V_PL,Op_A_V_PL, Paired=TRUE)#p-value = 0.0002109
t.test(T_N_NV_PL,T_A_NV_PL, Paired=TRUE)#p-value = 0.02633
t.test(Op_N_NV_PL,Op_A_NV_PL, Paired=TRUE)#p-value = 5.154e-13
#violation x NonViolation
t.test(T_N_V_PL,T_N_NV_PL, Paired=TRUE)#p-value = 1.437e-10
t.test(T_A_V_PL,T_A_NV_PL, Paired=TRUE)#p-value = 1.783e-09
t.test(Op_A_V_PL,Op_A_NV_PL, Paired=TRUE)#p-value = 0.03387
t.test(Op_N_V_PL,Op_N_NV_PL, Paired=TRUE)#p-value = 0.0001407

# Posterior Right

Op_N_V_PR=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="posterior" & left_right=="right", select=c(value))
Op_N_NV_PR=subset(Table_P600, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="posterior" & left_right=="right", select=c(value))
Op_A_V_PR=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="posterior" & left_right=="right", select=c(value))
Op_A_NV_PR=subset(Table_P600, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="posterior" & left_right=="right", select=c(value))
T_N_V_PR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="posterior" & left_right=="right", select=c(value))
T_N_NV_PR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="posterior" & left_right=="right", select=c(value))
T_A_V_PR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="posterior" & left_right=="right", select=c(value))
T_A_NV_PR=subset(Table_P600, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="posterior" & left_right=="right", select=c(value))

#paired t tests for posterior right 
#Opaque x Transparents
t.test(Op_N_V_PR,T_N_V_PR, Paired= TRUE)#p-value = 0.01282
t.test(Op_N_NV_PR,T_N_NV_PR, Paired= TRUE)
t.test(Op_A_V_PR,T_A_V_PR, Paired= TRUE)
t.test(Op_A_NV_PR,T_A_NV_PR, Paired=TRUE)#p-value = 3.18e-05 
#nouns x adjectives
t.test(T_N_V_PR,T_A_V_PR, Paired=TRUE)
t.test(Op_N_V_PR,Op_A_V_PR, Paired=TRUE)#p-value = 5.199e-05
t.test(T_N_NV_PR,T_A_NV_PR, Paired=TRUE)#p-value = 0.01145
t.test(Op_N_NV_PR,Op_A_NV_PR, Paired=TRUE)#p-value = 1.382e-15
#violation x NonViolation
t.test(T_N_V_PR,T_N_NV_PR, Paired=TRUE)#p-value = 5.203e-09
t.test(T_A_V_PR,T_A_NV_PR, Paired=TRUE)#p-value = 7.102e-06
t.test(Op_A_V_PR,Op_A_NV_PR, Paired=TRUE)
t.test(Op_N_V_PR,Op_N_NV_PR, Paired=TRUE)#p-value = 0.000469

