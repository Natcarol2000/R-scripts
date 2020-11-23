PP=Pseudo_PP16
attach(PP)
names(PP)
#selecionando colunas
dataset=data.frame(Subject,TaskBlock1.RESP,TaskBlock1.RT,WordsBlock1)
View(dataset)

#excluindo linhas

Sujeito=Subject[-c(1:10)]
Resposta=TaskBlock1.RESP[-c(1:10)]
Latência=TaskBlock1.RT[-c(1:10)]
Itens=WordsBlock1[-c(1:10)]

#Criando data frame

dataset=data.frame(Sujeito,Itens,Resposta,Latência)
View(dataset)

#adicionando colunas 'Sexo' e 'OrdemTarefa'
dataset['Sexo']='Fem'
#ou
dataset['Sexo']='Masc'
dataset['OrdemTarefa']='Primeira'
#ou
dataset['OrdemTarefa']='Segunda'

View(dataset)

#adicionando colunas para variêveis
dataset['Categoria']=NA
dataset['FreqTerminaêêo']=NA
dataset['FreqPalavrasVizinhas']=NA
dataset['Grafemas']=NA
dataset['Terminaêêo']=NA
dataset['Dificuldade']=NA

dataset$Sujeito=ifelse(Sujeito==16,'PP16',
                       as.factor(dataset$Sujeito))

#adicionando terminaêêes das pseudo
dataset$Terminaêêo=as.factor(ifelse(dataset$Itens %in% c('Famida','Fotêcia','Eferta', 'Cetoda','Bamêlia','Detuca'),'-a',
                                  ifelse(dataset$Itens %in% c('Damume','Liume','Merfume','Ligume','Datume','Vostume'),'-ume',
                                         ifelse(dataset$Itens %in% c('Galodor','Fensador','Pobedor','Nor','Fanodor','Sor'), '-or',
                                                ifelse(dataset$Itens %in% c('Cetafo','Tomento','Damibo','Muturo','Cecuto','Fafeco'), '-o',
                                                       ifelse(dataset$Itens %in% c('Cetade','Lontade','Imade','Secade','Vomade','Vabade'), '-ade',
                                                              ifelse(dataset$Itens %in% c('Maragem','Voragem','Decagem','Vomagem','Cabagem','Tantagem'), '-agem',
                                                                     ifelse(dataset$Itens %in% c('Afenêêo','Ronstruêêo','Docotêo','Priaêêo','Fomadêo','Tetadêo'), '-êo',
                                                                            ifelse(dataset$Itens %in% c('Evema','Cilema','Sisfema','Cedema','Favema','Titema'), '-ema',
                                                                                   as.character(dataset$Itens))))))))))

#adicionando grau de dificuldade
dataset$Dificuldade=as.factor(ifelse(dataset$Itens %in% c('Fotêcia','Eferta','Bamêlia','Liume','Merfume','Ligume','Vostume','Fensador','Tomento','Muturo','Cetade','Lontade','Imade','Voragem','Maragem','Tantagem','Afenêêo','Ronstruêêo','Priaêêo','Cilema','Sisfema','Evema','Nor','Sor'),'fêcil',
                                    ifelse(dataset$Itens %in% c('Famida','Cetoda','Detuca','Damume','Datume','Galodor','Pobedor','Fanodor','Cetafo','Damibo','Cecuto','Fafeco','Secade','Vomade','Vabade','Decagem','Cabagem','Vomagem','Docotêo','Fomadêo','Tetadêo','Cedema','Favema','Titema'),'Difêcil',
                                           as.character(dataset$Itens))))

#adicionando nêmero de grafemas
dataset$Grafemas=as.numeric(ifelse(dataset$Itens %in% c('Famida'),6,
                                    ifelse(dataset$Itens %in% c('Fotêcia'),7,
                                           ifelse(dataset$Itens %in% c('Eferta'),6,
                                                  ifelse(dataset$Itens %in% c('Cetoda'),6,
                                                         ifelse(dataset$Itens %in% c('Bamêlia'),7,
                                                                ifelse(dataset$Itens %in% c('Detuca'),6,
                                                                       ifelse(dataset$Itens %in% c('Damume'),6,
                                                                              ifelse(dataset$Itens %in% c('Liume'),5,
                                                                                     ifelse(dataset$Itens %in% c('Merfume'),7,
                                                                                            ifelse(dataset$Itens %in% c('Ligume'),6,
                                                                                                   ifelse(dataset$Itens %in% c('Datume'),6,
                                                                                                          ifelse(dataset$Itens %in% c('Vostume'),7,
                                                                                                                 ifelse(dataset$Itens %in% c('Galodor'),7,
                                                                                                                        ifelse(dataset$Itens %in% c('Fensador'),8,
                                                                                                                               ifelse(dataset$Itens %in% c('Pobedor'),7,
                                                                                                                                      ifelse(dataset$Itens %in% c('Nor'),3,
                                                                                                                                             ifelse(dataset$Itens %in% c('Fanodor'),7,
                                                                                                                                                    ifelse(dataset$Itens %in% c('Sor'),3,
                                                                                                                                                           ifelse(dataset$Itens %in% c('Cetafo'),6,
                                                                                                                                                                  ifelse(dataset$Itens %in% c('Tomento'),7,
                                                                                                                                                                         ifelse(dataset$Itens %in% c('Damibo'),6,
                                                                                                                                                                                ifelse(dataset$Itens %in% c('Muturo'),6,
                                                                                                                                                                                       ifelse(dataset$Itens %in% c('Cecuto'),6,
                                                                                                                                                                                              ifelse(dataset$Itens %in% c('Fafeco'),6,
                                                                                                                                                                                                     ifelse(dataset$Itens %in% c('Cetade'),6,
                                                                                                                                                                                                            ifelse(dataset$Itens %in% c('Lontade'),7,
                                                                                                                                                                                                                   ifelse(dataset$Itens %in% c('Imade'),5,
                                                                                                                                                                                                                          ifelse(dataset$Itens %in% c('Secade'),6,
                                                                                                                                                                                                                                 ifelse(dataset$Itens %in% c('Vomade'),6,
                                                                                                                                                                                                                                        ifelse(dataset$Itens %in% c('Vabade'),6,
                                                                                                                                                                                                                                               ifelse(dataset$Itens %in% c('Maragem'),7,
                                                                                                                                                                                                                                                      ifelse(dataset$Itens %in% c('Voragem'),7,
                                                                                                                                                                                                                                                             ifelse(dataset$Itens %in% c('Decagem'),7,
                                                                                                                                                                                                                                                                    ifelse(dataset$Itens %in% c('Vomagem'),7,
                                                                                                                                                                                                                                                                           ifelse(dataset$Itens %in% c('Cabagem'),7,
                                                                                                                                                                                                                                                                                  ifelse(dataset$Itens %in% c('Tantagem'),8,
                                                                                                                                                                                                                                                                                         ifelse(dataset$Itens %in% c('Afenêêo'),7,
                                                                                                                                                                                                                                                                                                ifelse(dataset$Itens %in% c('Ronstruêêo'),10,
                                                                                                                                                                                                                                                                                                       ifelse(dataset$Itens %in% c('Docotêo'),7,
                                                                                                                                                                                                                                                                                                              ifelse(dataset$Itens %in% c('Priaêêo'),7,
                                                                                                                                                                                                                                                                                                                     ifelse(dataset$Itens %in% c('Fomadêo'),7,
                                                                                                                                                                                                                                                                                                                            ifelse(dataset$Itens %in% c('Tetadêo'),7,
                                                                                                                                                                                                                                                                                                                                   ifelse(dataset$Itens %in% c('Evema'),5,
                                                                                                                                                                                                                                                                                                                                          ifelse(dataset$Itens %in% c('Cilema'),6,
                                                                                                                                                                                                                                                                                                                                                 ifelse(dataset$Itens %in% c('Sisfema'),7,
                                                                                                                                                                                                                                                                                                                                                        ifelse(dataset$Itens %in% c('Cedema'),6,
                                                                                                                                                                                                                                                                                                                                                               ifelse(dataset$Itens %in% c('Favema'),6,
                                                                                                                                                                                                                                                                                                                                                                      ifelse(dataset$Itens %in% c('Titema'),6,
                                                                                                                                                                                                                                                                                                                                                                             as.numeric(dataset$Grafemas))))))))))))))))))))))))))))))))))))))))))))))))))

#adicionando categorias
dataset$Categoria=as.factor(ifelse(dataset$Itens %in% c('Famida','Fotêcia','Eferta', 'Cetoda','Bamêlia','Detuca'),'Reg',
                                    ifelse(dataset$Itens %in% c('Damume','Liume','Merfume','Ligume','Datume','Vostume'),'Transp/Masc',
                                           ifelse(dataset$Itens %in% c('Galodor','Fensador','Pobedor','Nor','Fanodor','Sor'), 'Transp/Masc',
                                                  ifelse(dataset$Itens %in% c('Cetafo','Tomento','Damibo','Muturo','Cecuto','Fafeco'), 'Reg',
                                                         ifelse(dataset$Itens %in% c('Cetade','Lontade','Imade','Secade','Vomade','Vabade'), 'Transp/fem',
                                                                ifelse(dataset$Itens %in% c('Maragem','Voragem','Decagem','Vomagem','Cabagem','Tantagem'), 'Transp/fem',
                                                                       ifelse(dataset$Itens %in% c('Afenêêo','Ronstruêêo','Docotêo','Priaêêo','Fomadêo','Tetadêo'), 'Transp/Irr',
                                                                              ifelse(dataset$Itens %in% c('Evema','Cilema','Sisfema','Cedema','Favema','Titema'), 'Transp/Irr',
                                                                                     as.character(dataset$Itens))))))))))

#adicionando frequência dos palavras 'vizinhas'
dataset$FreqPalavrasVizinhas=ifelse(dataset$Itens %in% c('Fotêcia'),179,
                                    ifelse(dataset$Itens %in% c('Eferta'),113,
                                           ifelse(dataset$Itens %in% c('Bamêlia'),571,
                                                  ifelse(dataset$Itens %in% c('Liume'),25,
                                                         ifelse(dataset$Itens %in% c('Merfume'),31,
                                                                ifelse(dataset$Itens %in% c('Ligume'),13,
                                                                       ifelse(dataset$Itens %in% c('Vostume'),80,
                                                                              ifelse(dataset$Itens %in% c('Fensador'),24,
                                                                                     ifelse(dataset$Itens %in% c('Nor'),223,
                                                                                            ifelse(dataset$Itens %in% c('Sor'),142,
                                                                                                   ifelse(dataset$Itens %in% c('Tomento'),7,
                                                                                                          ifelse(dataset$Itens %in% c('Muturo'),431,
                                                                                                                 ifelse(dataset$Itens %in% c('Lontade'),216,
                                                                                                                        ifelse(dataset$Itens %in% c('Imade'),218,
                                                                                                                               ifelse(dataset$Itens %in% c('Cetade'),172,
                                                                                                                                      ifelse(dataset$Itens %in% c('Voragem'),64,
                                                                                                                                             ifelse(dataset$Itens %in% c('Maragem'),4,
                                                                                                                                                    ifelse(dataset$Itens %in% c('Tantagem'),188,
                                                                                                                                                           ifelse(dataset$Itens %in% c('Afenêêo'),236,
                                                                                                                                                                  ifelse(dataset$Itens %in% c('Ronstruêêo'),248,
                                                                                                                                                                         ifelse(dataset$Itens %in% c('Priaêêo'),290,
                                                                                                                                                                                ifelse(dataset$Itens %in% c('Evema'),3,
                                                                                                                                                                                       ifelse(dataset$Itens %in% c('Cilema'),401,
                                                                                                                                                                                              ifelse(dataset$Itens %in% c('Sisfema'),731,
                                                                                                                                                                                                     ifelse(dataset$Itens %in% c('Favema'),128,
                                                                                                                                                                                                            as.numeric(dataset$FreqPalavrasVizinhas))))))))))))))))))))))))))



#adicionando frequência da terminaêêo
dataset$FreqTerminaêêo=ifelse(dataset$Itens %in% c('Famida','Fotêcia','Eferta','Cetoda','Bamêlia','Detuca'),70251,
                              ifelse(dataset$Itens %in% c('Damume','Liume','Merfume','Ligume','Datume','Vostume'),629,
                                     ifelse(dataset$Itens %in% c('Galodor','Fensador','Pobedor','Nor','Fanodor','Sor'),22178,
                                            ifelse(dataset$Itens %in% c('Cetafo','Tomento','Damibo','Muturo','Cecuto','Fafeco'),52478,
                                                   ifelse(dataset$Itens %in% c('Cetade','Lontade','Imade','Secade','Vomade','Vabade'),17005,
                                                          ifelse(dataset$Itens %in% c('Maragem','Voragem','Decagem','Vomagem','Cabagem','Tantagem'),3421,
                                                                 ifelse(dataset$Itens %in% c('Afenêêo','Ronstruêêo','Docotêo','Priaêêo','Fomadêo','Tetadêo'),51114,
                                                                        ifelse(dataset$Itens %in% c('Evema','Cilema','Sisfema','Cedema','Favema','Titema'),1254,
                                                                               as.numeric(dataset$FreqTerminaêêo)))))))))



View(dataset)  





setwd("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo")

write.table(dataset, file='Pseudo_PP16.txt', sep='\t', row.names=FALSE)

Data_Pseudo_complete=rbind(Pseudo_PP1,Pseudo_PP2,Pseudo_PP3,Pseudo_PP5,Pseudo_PP6,Pseudo_PP7,Pseudo_PP8,Pseudo_PP9,Pseudo_PP10,Pseudo_PP11,Pseudo_PP12,Pseudo_PP14,Pseudo_PP15,Pseudo_PP16,Pseudo_PP17,Pseudo_PP18,Pseudo_PP19,Pseudo_PP20,Pseudo_PP21,Pseudo_PP22,Pseudo_PP23,Pseudo_PP24,Pseudo_PP25,Pseudo_PP26,Pseudo_PP27,Pseudo_PP28,Pseudo_PP29,Pseudo_PP30)
write.table(Data_Pseudo_complete, file='Data_Pseudo_complete.txt',sep='\t', row.names=FALSE)
# estatêstica erros -------------------------------------------------------------------

Data_Pseudo_complete <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/Data_Pseudo_complete.txt")

dataset=Data_Pseudo_complete

attach(dataset)

#counting number of errors per item endings


aF=subset(dataset, Terminaêêo=='-a'& Dificuldade=='fêcil', select=c(Resposta)) 

aD=subset(dataset, Terminaêêo=='-a'& Dificuldade=='Difêcil', select=c(Resposta)) 
aD
oF=subset(dataset, Terminaêêo=='-o'& Dificuldade=='fêcil', select=c(Resposta))#19erros
oF
oD=subset(dataset, Terminaêêo=='-o'& Dificuldade=='Difêcil',select=c(Resposta))#19erros
oD


agemF=subset(dataset, Terminaêêo=='-agem'& Dificuldade=='fêcil', select=c(Resposta))#26erros
agemF
agemD=subset(dataset, Terminaêêo=='-agem'& Dificuldade=='Difêcil', select=c(Resposta))#26erros
agemD


adeF=subset(dataset, Terminaêêo=='-ade'& Dificuldade=='fêcil', select=c(Resposta))#42erros
adeF
adeD=subset(dataset, Terminaêêo=='-ade'& Dificuldade=='Difêcil', select=c(Resposta))#25erros
adeD
umeF=subset(dataset, Terminaêêo=='-ume'& Dificuldade=='fêcil', select=c(Resposta))#45erros
umeF 

umeD=subset(dataset, Terminaêêo=='-ume'& Dificuldade=='Difêcil', select=c(Resposta))#45erros
umeD 

orF=subset(dataset, Terminaêêo=='-or'& Dificuldade=='fêcil', select=c(Resposta))
orF 

orD=subset(dataset, Terminaêêo=='-or'& Dificuldade=='Difêcil', select=c(Resposta))
orD 





#counting number os entries for 'Fem' or 'Masc' 

êoF=subset(dataset, Terminaêêo=='-êo'& Dificuldade=='fêcil', select=c(Resposta)) #69 fem e #96 masculinas 3NA
êoF
êoD=subset(dataset, Terminaêêo=='-êo'& Dificuldade=='Difêcil', select=c(Resposta)) #69 fem e #96 masculinas 3NA
êoD

emaF=subset(dataset, Terminaêêo=='-ema'& Dificuldade=='fêcil',select=c(Resposta))#56masc e #112fem 2NA
emaF

emaD=subset(dataset, Terminaêêo=='-ema'& Dificuldade=='Difêcil', select=c(Resposta))#56masc e #112fem 2NA
emaD


#counting number os entries for 'Fem' or 'Masc' for each of the 'easy' pseudoword category

Fotêcia=subset(dataset, Itens=='Fotêcia', select=c(Resposta))
Fotêcia # 3 erros
Eferta=subset(dataset, Itens=='Eferta', select=c(Resposta))
Eferta # 3 erros
Bamêlia=subset(dataset, Itens=='Bamêlia', select=c(Resposta))
Bamêlia # 1 erro
Liume=subset(dataset, Itens=='Liume', select=c(Resposta))
Liume # 10 erros
Merfume=subset(dataset, Itens=='Merfume', select=c(Resposta))
Merfume #8 erros
Ligume=subset(dataset, Itens=='Ligume', select=c(Resposta))
Ligume #4 erros
Vostume=subset(dataset, Itens=='Vostume', select=c(Resposta))
Vostume #2 erros
Fensador=subset(dataset, Itens=='Fensador', select=c(Resposta))
Fensador # erros
Nor=subset(dataset, Itens=='Nor', select=c(Resposta))
Nor # 8 erros
Sor=subset(dataset, Itens=='Sor', select=c(Resposta))
Sor #7 erros

Tomento=subset(dataset, Itens=='Tomento', select=c(Resposta))
Tomento # 1 erro
Muturo=subset(dataset, Itens=='Muturo', select=c(Resposta))
Muturo # 0 erro
Cetade=subset(dataset, Itens=='Cetade', select=c(Resposta))
Cetade #7 erros
Lontade=subset(dataset, Itens=='Lontade', select=c(Resposta))
Lontade # 5 erros
Imade=subset(dataset, Itens=='Imade', select=c(Resposta))
Imade # 7 erros
Voragem=subset(dataset, Itens=='Voragem', select=c(Resposta))
Voragem # 6 erros
Maragem=subset(dataset, Itens=='Maragem', select=c(Resposta))
Maragem #3 erros
Tantagem=subset(dataset, Itens=='Tantagem', select=c(Resposta))
Tantagem #4 erros


Decagem=subset(dataset, Itens=='Decagem', select=c(Resposta))
Decagem # 3 erros
Vomagem=subset(dataset, Itens=='Vomagem', select=c(Resposta))
Vomagem #6 erros
Cabagem=subset(dataset, Itens=='Cabagem', select=c(Resposta))
Cabagem #4 erros





Afenêêo=subset(dataset, Itens=='Afenêêo', select=c(Resposta))
Afenêêo #4 erros
Ronstruêêo=subset(dataset, Itens=='Ronstruêêo', select=c(Resposta))
Ronstruêêo # 8 erros
Priaêêo=subset(dataset, Itens=='Priaêêo', select=c(Resposta))
Priaêêo #10 erros
Cilema=subset(dataset, Itens=='Cilema', select=c(Resposta))
Cilema #14 erros
Sisfema=subset(dataset, Itens=='Sisfema', select=c(Resposta))
Sisfema #16 erros
Evema=subset(dataset, Itens=='Evema', select=c(Resposta))
Evema #22 erros



# correlation -------------------------------------------------------------

data_correlation <- read.csv("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_correlation.csv", sep=";")
attach(data_correlation)
#excluding -ão e -ema
x=freq[c(1:6)]
y=erros[c(1:6)]
#excluding -ão
w=freq[c(1:7)]
z=erros[c(1:7)]

#plots
library(lattice)
library(psych)
library(car)
scatterplot(y ~ x, data=data_correlation, legend.coords="topleft")
scatterplot(erros ~ terminação, data=data_correlation)


boxplot(erros~categoria, data=data_correlation,
        col=(c("blue")),
        main="Quantidade de erros por categoria", xlab="Categorias", ylab="Quantidade de erros por categoria", outline=FALSE)



#correlation tests Erros*Frequência de terminaêêo
cor.test(x, y, method = "kendall") #0.02172 tau=-0.82 -excluding terminaêêes -êo e -ema
cor.test(z, w, method = "kendall") #0.015 tau=-0.78 - excluding terminaêêo em -êo

#data:PseudopalavrasFaceis

data_PseudoFaceis <- read.csv("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_PseudoFaceis.csv", sep=";")
data=data_PseudoFaceis
names(data)
attach(data)

#plots:PseudopalavrasFaceis

scatterplot(erros ~ categoria, data=data, legend.coords="topleft")
scatterplot(erros ~ terminação, data=data)


boxplot(erros~terminaêêo, data=data, notch=TRUE,
        col=(c("blue")),
        main="Quantidade de erros por terminaêêo (Fêceis)", xlab="Categorias", ylab="Quantidade de erros por terminaêêo (Fêceis)", outline=FALSE)

boxplot(erros~categoria, data=data, notch=TRUE,
        col=(c("blue")),
        main="Quantidade de erros por categoria (Fêceis)", xlab="Categorias", ylab="Quantidade de erros por categoria (Fêceis)", outline=FALSE)


#correlation tests Erros*Frequência de terminaêêo
cor.test(freqPalavra, erros, method = "kendall")
cor.test(Freqterminaêêo, erros, method = "kendall") 





# stats- velocidade de resposta -------------------------------------------

Data_Pseudo_complete <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/Data_Pseudo_complete.txt")
dataset=Data_Pseudo_complete
attach(dataset)
names(dataset)
#excluding 'NA' rows and Latencies below 300ms
dataset=subset(dataset, Latência>300)
dataset=subset(dataset, Resposta>0)

#amalgamando Categories 'Transp/masc' and 'Transp/fem' 

dataset$Categoria=ifelse(dataset$Categoria=='Transp/fem','Transp',
                         ifelse(dataset$Categoria=='Transp/Masc', 'Transp',
                                ifelse(dataset$Categoria=='Transp/Irr', 'Irr',
                                       as.character(dataset$Categoria))))

View(dataset)

#get descriptives

library(psych)
names(dataset)
with(dataset, describeBy(Latência, group=list(Categoria), mat=TRUE))
with(dataset, describeBy(Latência, group=list(Terminaêêo), mat=TRUE))
with(dataset, describeBy(Latência, group=list(Terminaêêo, Dificuldade), mat=TRUE))




#understanding distribution
library(lattice)
with(dataset, densityplot(Latência))#left skewed
library(ggplot2)
ggplotRT<- ggplot(dataset, aes(Latência)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Latência", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset$Latência, na.rm = TRUE), sd = sd(dataset$Latência, na.rm = TRUE)), colour = "black", size = 1) 
ggplotRT
skew(dataset$Latência)

qqplot.RT <- qplot(sample = dataset$Latência, stat = "qq")
qqplot.RT


#checking for outliers
library(pastecs)
stat.desc(dataset$Latência, basic = FALSE, norm = TRUE)

#z-score to check for outliers

dataset$Latênciaz<-scale(dataset$Latência)
describe(dataset$Latênciaz)
stat.desc(dataset$Latênciaz)
sortcategorybyLatênciaz <- dataset[order(dataset$Latênciaz),] #sort categories by z-score of RT
table(dataset$Latênciaz > 2.5 | dataset$Latênciaz < -2.5) #28 
tail(sortcategorybyLatênciaz, n = 28)

#log transformation

dataset$logLatência <- log(1+dataset$Latência)
skew(dataset$logLatência)
skew(dataset$Latência)
describe(dataset$logLatência)
densityplot(dataset$logLatência) #right skewed
hist.logLatência <- ggplot(dataset, aes(logLatência)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Latência (log)", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset$logLatência, na.rm = TRUE), sd = sd(dataset$logLatência, na.rm = TRUE)), colour = "red", size = 1) 
hist.logLatência #right skewed
with(dataset, densityplot(~ logLatência| Categoria)) 
ggplot(dataset, aes(x= logLatência, fill = Categoria)) + geom_density(alpha = 0.2)
stat.desc(dataset$logLatência, basic = FALSE, norm = TRUE)
describe(dataset$logLatência)
hist.logLatência<- ggplot(dataset, aes(logLatência)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Latência", y = "Densidade") + stat_function (fun = dnorm, args = list(mean = mean(dataset$logLatência, na.rm = TRUE), sd = sd(dataset$logLatência, na.rm = TRUE)), colour = "black", size = 1) 
hist.logLatência #much better

# check outliers in log transformation via z-scores
boxplot(dataset$logLatência, xlab = "Todas as categorias", ylab = "Latência (log)")

dataset$logLatênciaz<-scale(dataset$logLatência)
describe(dataset$logLatênciaz)
stat.desc(dataset$logLatênciaz)
sortcategorybylogLatênciaz <- dataset[order(dataset$logLatênciaz),] #sort categories by z-score of RT
table(dataset$logLatênciaz > 2.5 | dataset$logLatênciaz < -2.5) # 18
tail(sortcategorybylogLatênciaz, n = 18)



#removing outliers from variable 'logLatência' that are 2,5 sd away from mean
dataset_logtrimmed = dataset[abs(scale(dataset$logLatência)) < 2.5,] #10 outliers removed 
densityplot(dataset_logtrimmed$logLatência)
hist.logLatência <- ggplot(dataset_logtrimmed, aes(logLatência)) + theme(legend.position = "none") + geom_histogram(aes(y=..density..), colour = "black", fill = "white") +  labs(x = "Latência (log)", y = "Density") + stat_function (fun = dnorm, args = list(mean = mean(dataset_logtrimmed$logLatência, na.rm = TRUE), sd = sd(dataset_logtrimmed$logLatência, na.rm = TRUE)), colour = "red", size = 2) 
hist.logLatência #Great
skew(dataset_logtrimmed$logLatência)
attach(dataset_logtrimmed)

shapiro.test(dataset_logtrimmed$logLatência) 

library(Hmisc)
# separate per condition
with(dataset, densityplot(~ logLatência| Categoria)) # all categories are positively skewed


write.table(dataset_logtrimmed, file='data_logtrimmed_pseudo.txt', sep='\t', row.names=FALSE)




# lme4 models -------------------------------------------------------------
data_logtrimmed_pseudo <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_logtrimmed_pseudo.txt")
dataset=data_logtrimmed_pseudo

library(lme4)

names(dataset)

model1=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria+Terminação+Dificuldade+(1|Sujeito)+(1|Itens), data=dataset)
summary(model1, cor=FALSE)
model2=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria+Terminaêêo*Dificuldade+(1|Sujeito)+(1|Itens), data=dataset)
summary(model2, cor=FALSE)
model3=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria*Terminaêêo*Dificuldade+(1|Sujeito)+(1|Itens), data=dataset)
summary(model3, cor=FALSE)
model4=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria*Terminaêêo*Dificuldade+(1+Categoria|Sujeito)+(1|Itens), data=dataset)
summary(model4, cor=FALSE)
model5=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria*Dificuldade+Terminaêêo+(1+Categoria|Sujeito)+(1|Itens), data=dataset)
summary(model5, cor=FALSE) 
model6=lmer(logLatência~Sexo+Grafemas+Categoria*Dificuldade+Terminaêêo+OrdemTarefa+(1|Sujeito)+(1|Itens), data=dataset)
summary(model6, cor=FALSE)
model7=lmer(logLatência~Sexo+Grafemas+Categoria*Terminaêêo*Dificuldade*OrdemTarefa+(1+Dificuldade|Sujeito)+(1|Itens), data=dataset)
summary(model7, cor=FALSE)
model8=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria+Terminaêêo+Dificuldade+(1+Categoria|Sujeito)+(1|Itens), data=dataset)
summary(model8, cor=FALSE)


#models REML= FALSE

model1_REML=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria+Terminaêêo+FreqTerminaêêo+Dificuldade+(1|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model1, cor=FALSE)
model2_REML=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria+Terminaêêo*FreqTerminaêêo+Dificuldade+(1|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model2, cor=FALSE)
model3_REML=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria*FreqTerminaêêo*Terminaêêo*Dificuldade+(1|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model3, cor=FALSE)
model4_REML=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria*FreqTerminaêêo*Terminaêêo*Dificuldade+(1+Categoria|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model4, cor=FALSE)
model6_REML=lmer(logLatência~Sexo+OrdemTarefa+Grafemas+Categoria*FreqTerminaêêo*Terminaêêo*Dificuldade+(1+Dificuldade|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model6, cor=FALSE)
model7_REML=lmer(logLatência~Sexo+Grafemas+Categoria*FreqTerminaêêo*Terminaêêo*Dificuldade*OrdemTarefa+(1+Dificuldade|Sujeito)+(1|Itens), data=dataset,REML=FALSE)
summary(model7, cor=FALSE)


#likelihood ratio tests with anova() = Comparaêêo dos modelos

library(car)
anova(model1_REML,model2_REML)
anova(model1_REML,model3_REML)
anova(model2_REML,model3_REML)
anova(model1_REML,model4_REML)
anova(model2_REML,model4_REML)
anova(model1, model6)
anova(model1_REML, model7_REML)

#ANOVA para extraêêo do P-valor
library(car)


Anova(model6, type = 3, test = 'Chisq')

Anova(model1, type = 3, test = 'Chisq')

# make diagnostic plots of the model

plot(model1) # fitted vs. residuals
library(lattice)
plot(model1)
densityplot(resid(model1)) # distribution of the resicuals
plot(data_logtrimmed_pseudo$logLatência, fitted(model1)) # raw vs. fitted



# Plots -------------------------------------------------------------------
data_logtrimmed_pseudo <- read.delim("~/Dropbox/Files_Sujeitos_EFNovo/Experiment_Pseudo/data_logtrimmed_pseudo.txt")
dataset=data_logtrimmed_pseudo
attach(dataset)
View(dataset)
names(dataset)

boxplot(logLatência~Categoria, data=dataset, notch=TRUE,
        col=c('blue'),
        main="Boxplots da velocidade de resposta por categoria", xlab="Categorias", ylab="(log) Velocidade de Resposta", outline=FALSE)

boxplot(logLatência~Terminaêêo, data=dataset, notch=TRUE,
        col=c('blue'),
        main="Boxplots da velocidade de resposta por terminaêêo", xlab="Terminaêêes", ylab="(log) Velocidade de Resposta", outline=FALSE)

boxplot(logLatência~Terminação*Dificuldade, data=dataset, notch=TRUE,
        col=c('blue'),
        names=c('-a','-ade','-agem','-ão','-ema','-o','-or','-ume','-a','-ade','-agem','-ão','-ema','-o','-or','-ume'),
        main="Boxplots da velocidade de resposta por terminação", xlab="Dificuldade (Difícil x Fácil)", ylab="(log) Velocidade de Resposta", outline=FALSE)




interaction.plot(dataset$Dificuldade, Terminação,dataset$logLatência, xlab='Dificuldade (Difícil x Fácil)', ylab='(log) Velocidade de Resposta',leg.bty = "o", legend=TRUE, col = 1:5)
interaction.plot(dataset$Dificuldade, dataset$Categoria ,dataset$logLatência, xlab='Dificuldade (Difícil x Fácil)', ylab='(log) Velocidade de Resposta',leg.bty = "o", legend=TRUE, col = 1:5)





