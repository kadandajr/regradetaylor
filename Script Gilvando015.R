##Universidade Federal da Paraíba
##CCSA - Ciências Econômicas
##Introdução à Econometria
##Gilvando Gomes de Lima Júnior

##Bibliotecas Usadas

install.packages("fBasics")
install.packages("forecast")
install.packages("moments")
install.packages("mFilter")
install.packages("agricolae")
install.packages("stats")
install.packages("lmtest")
install.packages("het.test")

##Set Arquivos

Dados<-read.table("GILVANDO015.txt", header = T )


##Transformar TS

Gilvando<-Dados  
IPCA<-ts(Gilvando$IPCA, frequency = 12, start = c(2002,1), end = c(2017,12));IPCA
SELIC<-ts(Gilvando$SELIC, frequency = 12, start = c(2002,1), end = c(2017,12));SELIC
PIB<-ts(Gilvando$PIB, frequency = 12, start = c(2002,1), end= c(2017,12));PIB
EXPC<-ts(Gilvando$EXPC, frequency = 12, start = c(2002,1), end = c(2017,12));EXPC
META<-ts(Gilvando$META, frequency = 12, start = c(2002,1), end = c(2017,12));META
TXREAL<-((1+SELIC/100)/(1+IPCA/100)-1)*100;TXREAL
DESV<-(EXPC-META);DESV
library(mFilter)
pibdecom<-hpfilter(PIB, freq=14400, type = "lambda", drif = FALSE)
pib.t<-pibdecom$trend;pib.t
pib.c<-pibdecom$cycle;
PIBDECA<- decompose(PIB, type = "additive")
PIBDECA
PIBDECM <- decompose(PIB, type = "multiplicative")
PIBDECM

NINDC<-ts(Gilvando$NINDC, frequency = 12, start = c(2002,1), end= c(2017,12));NINDC
NINDC1<-((NINDC*100)/NINDC[192])
PIBDF<-((PIB*100)/NINDC1);PIBDF
library(mFilter)
pibdfdecom<-hpfilter(PIBDF, freq=14400, type = "lambda", drif = FALSE)
HIATOC<-pibdfdecom$cycle;
HIATOT<-pibdfdecom$trend;
PIBDFDECA<- decompose(PIBDF, type = "additive")
PIBDFDECM <- decompose(PIBDF, type = "multiplicative")

d<-list(IPCA,SELIC,PIBDF,EXPC,META,TXREAL,HIATOC,DESV)

##Títulos e Eixos

eixo <- c('%','%','Pib Real Acumulado','%','%','%','Hiato','%')
grafico <- c('Gráfico 1: IPCA Acumulado, 2002-2017','Gráfico 2: Selic Acumulada, 2002-2017','Gráfico 3: Pib Real, 2002-2017 em Milhões de R$','Gráfico 4: Expectativa Inflação, 2002-2017','Gráfico 5: Meta Inflação, 2002-2017','Gráfico 06: Taxa de Juros Real','Gráfico 07: Hiato do Produto','Gráfico 08: Desvio Inflação')
titulo <- c('Histograma do IPCA acumulado 12 meses','Histograma da Selic Acumulada 12 meses','Histograma do Pib Real','Histograma da Expectativa Inflação','Histograma da Meta de Inflação','Histograma da Taxa Real de Juros','Histograma do Hiato do Produto','Histograma do Desvio da Inflação')
med.ted <- c('ÍNDICE DE PREÇOS AO CONSUMIDOR AMPLO','TAXA DE JUROS SELIC','PRODUTO INTERNO BRUTO REAL','EXPECTATIVA DE INFLAÇÃO','META DE INFLAÇÃO','TAXA REAL DE JUROS','HIATO DO PRODUTO','DESVIO DA INFLAÇÃO')
titulos <- c('Média','Mediana','Mínimo','Máximo','Desvio Padrão','Coeficiente de Variação')
Curtose <- c('Curtose')
Assimetria <- c('Assimetria')
Jar.Bera <- c('Jarque Bera Teste')
Tx.Lin <- c('Taxa de Crescimento Linear')
Tx.Com <- c('Taxa Crescimento Composta')
n <- 192

##Código

cont <- 1;
for (x in d ){
   (par(mfrow=c(1,2)))
   (plot(x, main = grafico[c(cont)], type="l", col="darkgreen", xlab="Ano", ylab=eixo[c(cont)]))
   (grid(col="grey", lwd="1"))
   (abline(h=mean(x), col="red"))  
   (options(scipen = 999))
   (hist(x, breaks="sturges", col="darkgreen", main= titulo[c(cont)], ylab="Frequência", labels = T))
   print(med.ted[c(cont)])
   print (titulos[1])
   print (mean(x))
   print (titulos[2])
   print (median(x))
   print (titulos[3])
   print (min(x))
   print (titulos[4])
   print (max(x))
   print (titulos[5])
   print (sd(x))
   print (titulos[6])
   print ((sd(x)/mean(x)*100))
   print (Tx.Lin[1])
   print (TxC.Lin<-(((x[n]/x[1])-1)*100)/n)  
   print (Tx.Com[1])
   print (TxC.Com<-(((x[n]/x[1])^(1/n))-1)*100)
   print (Assimetria[1])
   library(moments)
   print (skewness(x))
   print (Curtose[c(1)])
   library(moments)
   print (kurtosis(x))
   print (Jar.Bera[1])
   library(fBasics)
   print (jarqueberaTest(x))
   cont <- cont+1;
}

##Gráficos

Grafico09<-cbind(PIBDF,SELIC,IPCA)
par(mfrow=c(3,1))
plot(Grafico09, main="Gráfico 11: PIB REAL, SELIC, IPCA", col="darkgreen")
Grafico10<-cbind(IPCA,EXPC,META)
par(mfrow=c(3,1))
plot(Grafico10, main="Gráfico 12: IPCA, EXPC, META", col="darkgreen") 
Grafico11<-cbind(TXREAL,SELIC,IPCA)
par(mfrow=c(3,1))
plot(Grafico11, main="Gráfico 12: TX REAL, SELIC, IPCA", col="darkgreen")

par(mfrow=c(1,1))
plot(PIBDF, type="l", col="blue", main="Gráfico 12: Pib Real e Pib Potencial Real")
lines(HIATOT, col="orange")
lines(HIATOT, col="orange")
legend("topleft", cbind(legend="Pib Real","Pib Potencial Real"),lty=2, col=c("orange","blue"),bty="n",lwd=1)
grid(col="grey", lwd="1")

par(mfrow=c(1,1))
plot(HIATOC, col="darkgreen", type="l", main="Gráfico 13: Ciclo")
grid(col="grey", lwd="1")

par(mfrow=c(1,1))
seasonplot(PIBDF)
plot(PIB, type="l", col="darkgreen", main="Gráfico: Pib Real e Pib Nominal")
lines(PIBDF, col="orange")
legend("topleft", cbind(legend="Pib Real","Pib Nominal"),lty=2, col=c("orange","darkgreen"),bty="n",lwd=1)
grid(col="grey", lwd="1")

plot(pibdfdecom)
plot(HIATOC)
plot(PIBDFDECA)
plot(PIBDFDECM)

##Coeficiente de Correlação

library("agricolae", "stats")
correlationTest(TXREAL,SELIC)
correlationTest(TXREAL,DESV)
correlationTest(TXREAL,HIATOC)
correlationTest(DESV,SELIC)
correlationTest(DESV,HIATOC)
correlationTest(SELIC,HIATOC)

correlationTest(IPCA,SELIC)
correlationTest(IPCA,PIBDF)
correlationTest(IPCA,EXPC)
correlationTest(IPCA,META)
correlationTest(IPCA,TXREAL)
correlationTest(SELIC,PIBDF)
correlationTest(SELIC,EXPC)
correlationTest(SELIC,META)
correlationTest(SELIC,TXREAL)
correlationTest(PIBDF,EXPC)
correlationTest(PIBDF,META)
correlationTest(PIBDF,TXREAL)
correlationTest(EXPC,META)
correlationTest(EXPC,TXREAL)
correlationTest(META,TXREAL)

##Regressão com o Hiato PIB Real Filtro HP

REGRESSDF<-lm(SELIC~TXREAL + DESV + HIATOC)
REGRESSDF
REGRESSDF$coefficients[1]
REGRESSDF$coefficients[2]
REGRESSDF$coefficients[3]
REGRESSDF$coefficients[4]
summary(REGRESSDF)

##Estimando Selic Estimada e Resíduo

SELICESTIMADA<-((REGRESSDF$coefficients[1]) + (REGRESSDF$coefficients[2] * TXREAL) + (REGRESSDF$coefficients[3] * DESV) + (REGRESSDF$coefficients[4] * HIATOC))
RESIDUO<-(SELIC-SELICESTIMADA);RESIDUO
summary(RESIDUO)
library(fBasics)
jarqueberaTest(RESIDUO)
summary(SELICESTIMADA)

par(mfrow=c(1,1))
plot(SELICESTIMADA, main = "Gráfico 9: Selic e Selic Estimada", type="l", col="darkgreen", xlab="Ano", ylab="")
grid(col="grey", lwd="1")
options(scipen = 999)
legend("topright", cbind(legend="Selic","Selic Estimada"),lty=2, col=c("orange","darkgreen"),bty="n",lwd=1)
lines(SELIC, col="orange")

par(mfrow=c(1,2))
plot(RESIDUO, main = "Gráfico 10: Resíduo da Regressão", type="l", col="darkgreen", xlab="Ano", ylab="")
grid(col="grey", lwd="1")
abline(h=mean(RESIDUO), col="red")
options(scipen = 999)
hist(RESIDUO, breaks="sturges", col="darkgreen", main= "Histograma do Resíduo da Regressão", ylab="Frequência", labels = T)

## TESTE PARK

ResLogPark<-log(RESIDUO^2)
Park01<-log(HIATOC+193500)
Park02<-log(DESV+1.2)
Park03<-log(TXREAL)
TestePark01<-lm(ResLogPark ~ Park01)
TestePark02<-lm(ResLogPark ~ Park02)
TestePark03<-lm(ResLogPark ~ Park03)
summary(TestePark01)
summary(TestePark02)
summary(TestePark03)

## TESTE DE GLEJSER

GLEJ01<-(HIATOC+193500)
GLEJ02<-(DESV+1.2)
GLEJ03<-(TXREAL)
RESIDUOG<-abs(RESIDUO)
GLEJSER1<-lm(RESIDUOG ~ GLEJ01)
GLEJSER2<-lm(RESIDUOG ~ GLEJ02)
GLEJSER3<-lm(RESIDUOG ~ GLEJ03)
summary(GLEJSER1)
summary(GLEJSER2)
summary(GLEJSER3)

GLEJ04<-sqrt(DESV + 1.2)
GLEJ05<-sqrt(HIATOC + 193500)
GLEJ06<-sqrt(TXREAL)
GLEJSER4<-lm(RESIDUOG ~ GLEJ04)
GLEJSER5<-lm(RESIDUOG ~ GLEJ05)
GLEJSER6<-lm(RESIDUOG ~ GLEJ06)
summary(GLEJSER4)
summary(GLEJSER5)
summary(GLEJSER6)

## TESTE DE BPG

library(lmtest)
bptest(REGRESSDF)

## TESTE DE GODFREY

library(lmtest)
gqtest(REGRESSDF)

## TESTE DE WHITE

residuow<-RESIDUO^2
desviow<-DESV^2
hiatow<-HIATOC^2
txrealw<-TXREAL^2
w1<-DESV*HIATOC
w2<-DESV*TXREAL
w3<-TXREAL*HIATOC
white<-lm(residuow ~ desviow + hiatow + txrealw + w1 + w2 + w3)
summary(white)
whitedistri<-192 * 0.317;whitedistri

##AutoCorrelação

##Teste de Durbin
dwtest(REGRESSDF)

bgtest(REGRESSDF, order=2)
bgtest(REGRESSDF, order=4)
bgtest(REGRESSDF, order=6)
bgtest(REGRESSDF, order=12)

install.packages("FinTS", repos="http://R-Forge.R-project.org")
library(FinTS)
ArchTest(RESIDUO)

##MULTICOLINEARIDADE

##FIV

install.packages("car")
library(car)
vif(REGRESSDF)

##KLEIN

as.matrix(cor(DESV, TXREAL))
DESV.TXREAL<-cbind(DESV, TXREAL)
plot(DESV.TXREAL)

correlationTest(TXREAL,DESV)
yt

as.matrix(cor(DESV, HIATOC))
DESV.HIATOC<-cbind(DESV, HIATOC)
plot(DESV.HIATOC)

correlationTest(DESV,HIATOC)
klein2<-(0.0086^2);klein2

as.matrix(cor(TXREAL, HIATOC))
TXREAL.HIATOC<-cbind(TXREAL, HIATOC)
plot(TXREAL.HIATOC)

correlationTest(TXREAL,HIATOC)
klein3<-(0.0513^2);klein3

## MEDIDAS CORRETIVAS LOG

par(mfrow=c(1,1))
logselic<-log(SELIC)
plot(logselic)
logtxreal<-log(TXREAL)
plot(logtxreal)
logdesv<-log(DESV + 1.2)
plot(logdesv)
loghiato<-log(HIATOC + 193500)
plot(loghiato)

REGRESSLOG<-lm(logselic ~ logtxreal + logdesv + loghiato)
summary(REGRESSLOG)

LOGSELICESTIMADA<-((REGRESSLOG$coefficients[1]) + (REGRESSLOG$coefficients[2] * logtxreal) + (REGRESSLOG$coefficients[3] * logdesv) + (REGRESSLOG$coefficients[4] * loghiato))
RESIDUOLOG<-(logselic - LOGSELICESTIMADA)
summary(RESIDUOLOG)
par(mfrow=c(1,1))
plot(RESIDUOLOG)

## TESTE DE BPG

library(lmtest)
bptest(REGRESSLOG)

## TESTE DE GODFREY

library(lmtest)
gqtest(REGRESSLOG)

##AUTOCORRELAÇÃO LOG

dwtest(REGRESSLOG)

bgtest(REGRESSLOG, order=2)
bgtest(REGRESSLOG, order=4)
bgtest(REGRESSLOG, order=6)
bgtest(REGRESSLOG, order=12)

library(FinTS)
ArchTest(RESIDUOLOG)

##CORRIGINDO AUTOCORELAÇÃO

library(dynlm)
REGRESSDYN<-dynlm(logselic ~ lag(logselic, -2) + lag(logselic, -1) + logtxreal + logdesv + loghiato)
summary(REGRESSDYN)
RESIDUODYN<-residuals(REGRESSDYN)

par(mfrow=c(1,2))
plot(RESIDUODYN, main = "Gráfico 13: Resíduo da Regressão Corretiva", type="l", col="darkgreen", xlab="Ano", ylab="")
grid(col="grey", lwd="1")
abline(h=mean(RESIDUODYN), col="red")
options(scipen = 999)
hist(RESIDUODYN, breaks="sturges", col="darkgreen", main= "Histograma do Resíduo da Regressão Corretiva", ylab="Frequência", labels = T)


library(lmtest)
bptest(REGRESSDYN)
library(lmtest)
gqtest(REGRESSDYN)
library(FinTS)
ArchTest(RESIDUODYN)
jarqueberaTest(RESIDUODYN)
dwtest(REGRESSDYN)


bgtest(REGRESSDYN, order=2)
bgtest(REGRESSDYN, order=4)
bgtest(REGRESSDYN, order=6)
bgtest(REGRESSDYN, order=12)


## TESTE DE BPG

library(lmtest)
bptest(REGRESSDYN)

## TESTE DE GODFREY

library(lmtest)
gqtest(REGRESSDYN)

library(FinTS)
ArchTest(RESIDUODYN)

## DIVIDINDO A SERIE DE DADOS

ipcfhc<-(window(IPCA, frequency=12, start=c(2002,1), end=c(2002,12)));ipcfhc
ipclula1<-(window(IPCA, frequency=12, start=c(2003,1), end=c(2006,12)));ipclula1
ipclula2<-(window(IPCA, frequency=12, start=c(2007,1), end=c(2010,12)));ipclula2
ipcdilma1<-(window(IPCA, frequency=12, start=c(2011,1), end=c(2014,12)));ipcdilma1
ipcdilma2<-(window(IPCA, frequency=12, start=c(2015,1), end=c(2016,8)));ipcdilma2
ipctemer<-(window(IPCA, frequency=12, start=c(2016,9), end=c(2017,12)));ipctemer  


selfhc<-(window(SELIC, frequency=12, start=c(2002,1), end=c(2002,12)));selfhc
sellula1<-(window(SELIC, frequency=12, start=c(2003,1), end=c(2006,12)));sellula1
sellula2<-(window(SELIC, frequency=12, start=c(2007,1), end=c(2010,12)));sellula2
seldilma1<-(window(SELIC, frequency=12, start=c(2011,1), end=c(2014,12)));seldilma1
seldilma2<-(window(SELIC, frequency=12, start=c(2015,1), end=c(2016,8)));seldilma2
seltemer<-(window(SELIC, frequency=12, start=c(2016,9), end=c(2017,12)));seltemer 

pibfhc<-(window(PIBDF, frequency=12, start=c(2002,1), end=c(2002,12)));pibfhc
piblula1<-(window(PIBDF, frequency=12, start=c(2003,1), end=c(2006,12)));piblula1
piblula2<-(window(PIBDF, frequency=12, start=c(2007,1), end=c(2010,12)));piblula2
pibdilma1<-(window(PIBDF, frequency=12, start=c(2011,1), end=c(2014,12)));pibdilma1
pibdilma2<-(window(PIBDF, frequency=12, start=c(2015,1), end=c(2016,8)));pibdilma2
pibtemer<-(window(PIBDF, frequency=12, start=c(2016,9), end=c(2017,12)));pibtemer 

expfhc<-(window(EXPC, frequency=12, start=c(2002,1), end=c(2002,12)));expfhc
explula1<-(window(EXPC, frequency=12, start=c(2003,1), end=c(2006,12)));explula1
explula2<-(window(EXPC, frequency=12, start=c(2007,1), end=c(2010,12)));explula2
expdilma1<-(window(EXPC, frequency=12, start=c(2011,1), end=c(2014,12)));expdilma1
expdilma2<-(window(EXPC, frequency=12, start=c(2015,1), end=c(2016,8)));expdilma2
exptemer<-(window(EXPC, frequency=12, start=c(2016,9), end=c(2017,12)));exptemer 

metafhc<-(window(META, frequency=12, start=c(2002,1), end=c(2002,12)));metafhc
metalula1<-(window(META, frequency=12, start=c(2003,1), end=c(2006,12)));metalula1
metalula2<-(window(META, frequency=12, start=c(2007,1), end=c(2010,12)));metalula2
metadilma1<-(window(META, frequency=12, start=c(2011,1), end=c(2014,12)));metadilma1
metadilma2<-(window(META, frequency=12, start=c(2015,1), end=c(2016,8)));metadilma2
metatemer<-(window(META, frequency=12, start=c(2016,9), end=c(2017,12)));metatemer 

jrfhc<-((1+selfhc/100)/(1+ipcfhc/100)-1)*100;jrfhc
jrlula1<-((1+sellula1/100)/(1+ipclula1/100)-1)*100;jrlula1
jrlula2<-((1+sellula2/100)/(1+ipclula2/100)-1)*100;jrlula2
jrdilma1<-((1+seldilma1/100)/(1+ipcdilma1/100)-1)*100;jrdilma1
jrdilma2<-((1+seldilma2/100)/(1+ipcdilma2/100)-1)*100;jrdilma2
jrtemer<-((1+seltemer/100)/(1+ipctemer/100)-1)*100;jrtemer

desvfhc<-(window(DESV, frequency=12, start=c(2002,1), end=c(2002,12)));desvfhc
desvlula1<-(window(DESV, frequency=12, start=c(2003,1), end=c(2006,12)));desvlula1
desvlula2<-(window(DESV, frequency=12, start=c(2007,1), end=c(2010,12)));desvlula2
desvdilma1<-(window(DESV, frequency=12, start=c(2011,1), end=c(2014,12)));desvdilma1
desvdilma2<-(window(DESV, frequency=12, start=c(2015,1), end=c(2016,8)));desvdilma2
desvtemer<-(window(DESV, frequency=12, start=c(2016,9), end=c(2017,12)));desvtemer 

hiafhc<-(window(HIATOC, frequency=12, start=c(2002,1), end=c(2002,12)));hiafhc
hialula1<-(window(HIATOC, frequency=12, start=c(2003,1), end=c(2006,12)));hialula1
hialula2<-(window(HIATOC, frequency=12, start=c(2007,1), end=c(2010,12)));hialula2
hiadilma1<-(window(HIATOC, frequency=12, start=c(2011,1), end=c(2014,12)));hiadilma1
hiadilma2<-(window(HIATOC, frequency=12, start=c(2015,1), end=c(2016,8)));hiadilma2
hiatemer<-(window(HIATOC, frequency=12, start=c(2016,9), end=c(2017,12)));hiatemer 

## REGRESSÃO FHC

rfhc<-lm(selfhc ~ jrfhc + desvfhc + hiafhc)
summary(rfhc)
res1<-residuals(rfhc)

library(lmtest)
bptest(rfhc)

library(lmtest)
gqtest(rfhc)

dwtest(rfhc)

library(FinTS)
ArchTest(res1)

library(dynlm)
rdfhc<-dynlm(log(selfhc) ~ lag(log(selfhc), -1) + log(jrfhc) + log(desvfhc) + log(hiafhc))
summary(rdfhc)
resdfhc<-residuals(rdfhc)

library(lmtest)
bptest(rdfhc)

library(lmtest)
gqtest(rdfhc)

dwtest(rdfhc)

library(FinTS)
ArchTest(resdfhc)

## REGRESSÃO LULA1

rlula1<-lm(sellula1 ~ jrlula1 + desvlula1 + hialula1)
summary(rlula1)

## REGRESSÃO LULA2

rlula2<-lm(sellula2 ~ jrlula2 + desvlula2 + hialula2)
summary(rlula2)

## REGRESSÃO DILMA1

rdilma1<-lm(seldilma1 ~ jrdilma1 + desvdilma1 + hiadilma1)
summary(rdilma1)

## REGRESSÃO DILMA2

rdilma2<-lm(seldilma2 ~ jrdilma2 + desvdilma2 + hiadilma2)
summary(rdilma2)

## REGRESSÃO TEMER

rtemer<-lm(seltemer ~ jrtemer + desvtemer + hiatemer)
summary(rtemer)