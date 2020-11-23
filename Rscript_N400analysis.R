#N400 
#data preprocessing
#set work directory
setwd("/Volumes/KINGSTON/Estudo_EEG_MPI/export/N400/files_tabDLText")
#uploading files
NVAO=read.table("NVAO_N400.txt", header=TRUE)
NVAT=read.table("NVAT_N400.txt", header=TRUE)
NVNO=read.table("NVNO_N400.txt", header=TRUE)
NVNT=read.table("NVNT_N400.txt", header=TRUE)
VAO=read.table("VAO_N400.txt", header=TRUE)
VAT=read.table("VAT_N400.txt", header=TRUE)
VNO=read.table("VNO_N400.txt", header=TRUE)
VNT=read.table("VNT_N400.txt", header=TRUE)
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
tableN400=rbind(NVAO,NVAT,NVNO,NVNT,VAO,VAT,VNO,VNT)
#checking if binding went fine
str(tableN400) #looks good! 59 channels x 16 PPs x 8 conditions , total= 7552 obs. 

#checking names of the variables
names(tableN400)
attach(tableN400)
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
table_N400=data.frame(File, channel=col1, violation=col3, adj_noun=col4, transp_op=col5,value=value) #variable 'channel' still has "X"

#replacing "x" by "E"
attach(table_N400)

channel=str_replace_all(channel,"X","E")

#again joining all columns into a data frame

table_N400=data.frame(File, channel, violation=col3, adj_noun=col4, transp_op=col5,value=value) #looks good
str(table_N400)

#adding 1 column to data frame

table_N400["position"]=NA

#adding electrodes position= 'midline'
table_N400$position <- as.factor(ifelse(table_N400$channel %in% c('E31','E58','E59','E60','E5','E18'), 'midline', 
                                                          ifelse(table_N400$channel %in% c('E26','E27','E28','E29','E30','E31','E37','E50'), 'midline', 
                                                                 as.character(table_N400$position))))

#adding electrodes position=  'anterior', 'posterior'

table_N400$position = as.factor(ifelse(table_N400$channel %in% c('E34','E39','E46','E53', 'E38','E44', 'E45','E52','E57','E51','E56','E2', 'E6','E7','E14','E21','E13','E20','E25','E19','E12','E24'), 'anterior', 
                                       ifelse(table_N400$channel %in% c('E1', 'E4', 'E10', 'E17','E11', 'E3','E9','E16','E23', 'E22', 'E15','E8', 'E40','E47','E54','E35','E41', 'E48','E55','E33','E36','E42','E49','E43'), 'posterior', 
                                              as.character(table_N400$position))))

#adding 1 column to data frame
table_N400["left_right"]=NA
#adding hemisphere 
table_N400$left_right = as.factor(ifelse(table_N400$channel %in% c('E34','E39','E46','E53', 'E38','E45','E52','E57','E51','E56','E40','E47','E54','E35','E41', 'E48','E55','E33','E36','E42','E49','E43','E44','E50','E37'), 'left', 
                                         ifelse(table_N400$channel %in% c('E1', 'E4', 'E10', 'E17','E11', 'E3','E9','E16','E23', 'E22', 'E15','E8','E2', 'E6','E7','E14','E21','E13','E20','E25','E19','E12','E24','E18','E5'), 'right',
                                                ifelse(table_N400$channel %in% c('E31','E58','E59','E60','E26','E27','E28','E29','E30'), 'midline', 
                                                       as.character(table_N400$left_right)))))



#exporting the data

write.table(table_N400, 'table_N400.nova2.txt', sep='\t', row.names=FALSE)

#N400 stats

#reading table
Table_N400=read.table("table_N400.nova2.txt", header=TRUE)
Table_N400=table_N400.nova
str(Table_N400) #checking table variables: everything ok
library(ez)
attach(Table_N400)
aov1=ezANOVA(
  data=Table_N400
  , dv=value
  , wid=File
  , within = c("violation",  "transp_op","position"))

aov1

aov2=ezANOVA(
  data=subset(Table_N400, position=="midline" & left_right=="midline")
  , dv=value
  , wid=File
  , within= c("violation", "transp_op", 'adj_noun'))

aov2

aov3=ezANOVA(
  data=subset(Table_N400, position=="anterior")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

aov3

aov4=ezANOVA(
  data=subset(Table_N400, position=="anterior" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "transp_op","adj_noun"))

aov5=ezANOVA(
  data=subset(Table_N400, position=="posterior" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op")) 

aov6=ezANOVA(
  data=subset(Table_N400, position=="anterior" & left_right=="right")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op")) 

aov7=ezANOVA(
  data=subset(Table_N400, position=="posterior" & left_right=="right")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

aov8=ezANOVA(
  data=subset(Table_N400, position=="midline" & left_right=="midline")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

aov9=ezANOVA(
  data=subset(Table_N400,   position== "posterior" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

aov10=ezANOVA(
  data=subset(Table_N400,   position== "midline" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

aov11=ezANOVA(
  data=subset(Table_N400,   position== "anterior" & left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op"))

aov12=ezANOVA(
  data=subset(Table_N400, left_right=="left")
  , dv=value
  , wid=File
  , within = c("violation", "adj_noun", "transp_op", "position"))





#effects found (N400)
#Violation
#violation   1  15 4.757181615 0.0455060     * 3.754782e-02 (anterior left)




#t.tests (positions: midline, anterior left and posterior left)

#subsetting data

# 8 conditions
#anterior left
Op_N_V=subset(Table_N400, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
Op_N_NV=subset(Table_N400, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="anterior" & left_right=="left", select=c(value))
Op_a_v=subset(Table_N400, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
Op_a_Nv=subset(Table_N400, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="anterior" & left_right=="left", select=c(value))
T_N_V=subset(Table_N400, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
T_N_NV=subset(Table_N400, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="anterior" & left_right=="left", select=c(value))
T_a_v=subset(Table_N400, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="anterior" & left_right=="left", select=c(value))
T_a_Nv=subset(Table_N400, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="anterior" & left_right=="left", select=c(value))

 
#Opaque x Transparents
t.test(Op_N_V,T_N_V, Paired= TRUE)
t.test(Op_N_NV,T_N_NV, Paired= TRUE)
t.test(Op_a_v,T_a_v, Paired= TRUE)
t.test(Op_a_Nv,T_a_Nv, Paired=TRUE)
#nouns x adjectives
t.test(T_N_V,T_a_v, Paired=TRUE)#t = -4.2169, df = 345.139, p-value = 3.168e-05
t.test(Op_N_V,Op_a_v, Paired=TRUE)#t = -6.7921, df = 317.496, p-value = 5.453e-11
t.test(T_N_NV,T_a_Nv, Paired=TRUE)
t.test(Op_N_NV,Op_a_Nv, Paired=TRUE)#t = -4.0973, df = 346.001, p-value = 5.212e-05
#violation x NonViolation
t.test(T_N_V,T_N_NV, Paired=TRUE)#t = -4.4201, df = 346.628, p-value = 1.321e-05
t.test(T_a_v,T_a_Nv, Paired=TRUE)
t.test(Op_a_v,Op_a_Nv, Paired=TRUE)
t.test(Op_N_V,Op_N_NV, Paired=TRUE)#t = -4.5447, df = 334.489, p-value = 7.694e-06



#posterior left

Op_N_V_PL=subset(Table_N400, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
Op_N_NV_PL=subset(Table_N400, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="posterior" & left_right=="left", select=c(value))
Op_A_V_PL=subset(Table_N400, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
Op_A_NV_PL=subset(Table_N400, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="posterior" & left_right=="left", select=c(value))
T_N_V_PL=subset(Table_N400, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
T_N_NV_PL=subset(Table_N400, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="posterior" & left_right=="left", select=c(value))
T_A_V_PL=subset(Table_N400, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="posterior" & left_right=="left", select=c(value))
T_A_NV_PL=subset(Table_N400, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="posterior" & left_right=="left", select=c(value))



#Opaque x Transparents
t.test(Op_N_V_PL,T_N_V_PL, Paired= TRUE)#t = -2.8441, df = 381.976, p-value = 0.004693
t.test(Op_N_NV_PL,T_N_NV_PL, Paired= TRUE)
t.test(Op_A_V_PL,T_A_V_PL, Paired= TRUE)
t.test(Op_A_NV_PL,T_A_NV_PL, Paired=TRUE)#t = 3.3755, df = 364.883, p-value = 0.0008159
#nouns x adjectives
t.test(T_N_V_PL,T_A_V_PL, Paired=TRUE)#t = -2.2786, df = 381.11, p-value = 0.02324
t.test(Op_N_V_PL,Op_A_V_PL, Paired=TRUE)#t = -3.7417, df = 380.389, p-value = 0.0002109
t.test(T_N_NV_PL,T_A_NV_PL, Paired=TRUE)#t = -2.2309, df = 343.493, p-value = 0.02633
t.test(Op_N_NV_PL,Op_A_NV_PL, Paired=TRUE)#t = -7.4913, df = 366.347, p-value = 5.154e-13
#violation x NonViolation
t.test(T_N_V_PL,T_N_NV_PL, Paired=TRUE)#t = 6.6212, df = 331.525, p-value = 1.437e-10
t.test(T_A_V_PL,T_A_NV_PL, Paired=TRUE)#t = 6.1679, df = 377.294, p-value = 1.783e-09
t.test(Op_A_V_PL,Op_A_NV_PL, Paired=TRUE)#t = 2.1302, df = 341.887, p-value = 0.03387
t.test(Op_N_V_PL,Op_N_NV_PL, Paired=TRUE)#t = 3.8543, df = 314.494, p-value = 0.0001407

#midline

Op_N_V_M=subset(Table_N400, transp_op=="Op" & adj_noun=="Noun" & violation=="Violation" & position=="midline" & left_right=="midline", select=c(value))
Op_N_NV_M=subset(Table_N400, transp_op=="Op" & adj_noun=="Noun" & violation=="NonViolation" & position=="midline" & left_right=="midline", select=c(value))
Op_A_V_M=subset(Table_N400, transp_op=="Op" & adj_noun=="Adj" & violation=="Violation" & position=="midline" & left_right=="midline", select=c(value))
Op_A_NV_M=subset(Table_N400, transp_op=="Op" & adj_noun=="Adj" & violation=="NonViolation" & position=="midline" & left_right=="midline", select=c(value))
T_N_V_M=subset(Table_N400, transp_op=="Transp" & adj_noun=="Noun" & violation=="Violation" & position=="midline" & left_right=="midline", select=c(value))
T_N_NV_M=subset(Table_N400, transp_op=="Transp" & adj_noun=="Noun" & violation=="NonViolation"& position=="midline" & left_right=="midline", select=c(value))
T_A_V_M=subset(Table_N400, transp_op=="Transp" & adj_noun=="Adj" & violation=="Violation" & position=="midline" & left_right=="midline", select=c(value))
T_A_NV_M=subset(Table_N400, transp_op=="Transp" & adj_noun=="Adj" & violation=="NonViolation" & position=="midline" & left_right=="midline", select=c(value))

#Opaque x Transparents
t.test(Op_N_V_M,T_N_V_M, Paired= TRUE)#t = -1.2504, df = 285.273, p-value = 0.2122
t.test(Op_N_NV_M,T_N_NV_M, Paired= TRUE)#t = -1.7781, df = 275.932, p-value = 0.07648
t.test(Op_A_V_M,T_A_V_M, Paired= TRUE)#t = 1.7932, df = 271.339, p-value = 0.07406
t.test(Op_A_NV_M,T_A_NV_M, Paired=TRUE)#t = 1.8306, df = 279.666, p-value = 0.06822
#nouns x adjectives
t.test(T_N_V_M,T_A_V_M, Paired=TRUE)#t = -2.6932, df = 280.994, p-value = 0.007503
t.test(Op_N_V_M,Op_A_V_M, Paired=TRUE) #t = -5.8742, df = 246.847, p-value = 1.367e-08
t.test(T_N_NV_M,T_A_NV_M, Paired=TRUE)#t = -1.8896, df = 281.059, p-value = 0.05984
t.test(Op_N_NV_M,Op_A_NV_M, Paired=TRUE)#t = -6.2092, df = 283.632, p-value = 1.888e-09
#violation x NonViolation
t.test(T_N_V_M,T_N_NV_M, Paired=TRUE)
t.test(T_A_V_M,T_A_NV_M, Paired=TRUE)
t.test(Op_A_V_M,Op_A_NV_M, Paired=TRUE)
t.test(Op_N_V_M,Op_N_NV_M, Paired=TRUE)