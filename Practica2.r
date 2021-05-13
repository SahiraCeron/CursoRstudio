#Practica 2 

#Paqueterias####
library(readxl)
library(car)
library(carData)
library(nortest)
library(normtest)

#Base y grupos####

Base1<-read_excel("C:\\Users\\86524\\Desktop\\Servicio\\Bases\\databaseR.xls")

GrupoControl <-Base1[Base1$Group==0,]
GrupoPreDiab <-Base1[Base1$Group==1,]
GrupoDiabetes <-Base1[Base1$Group==2,]

set.seed(123)

#Graficas Normalidad####

#Glucosa General

MGlucosa <-mean(Base1$GLU)
DGlucosa <- sd(Base1$GLU)

par(mfrow=c(1,2))
hist(Base1$GLU, freq=F, main="Histograma Glucosa")
lines(density(rnorm(50, mean = MGlucosa, sd=DGlucosa)))
qqPlot(Base1$GLU, main="Q-Q Plot Glucosa")

#Base1[46,"GLU"]      

#Glucosa Control
MCGlucosa <- mean(GrupoControl$GLU)
DCGlucosa <- sd(GrupoControl$GLU)

par(mfrow=c(1,2))
hist(GrupoControl$GLU, freq=F, main="Histograma Control Glucosa")
lines(density(rnorm(14, mean = MCGlucosa, sd=DCGlucosa)))
qqPlot(GrupoControl$GLU)

#GrupoControl[c(1,7),"GLU"]

#Glucosa Pre
MPGlucosa <- mean(GrupoPreDiab$GLU)
DPGlucosa <- sd(GrupoPreDiab$GLU)

par(mfrow=c(1,2))
hist(GrupoPreDiab$GLU, freq=F, main="Histograma Prediabetes Glucosa")
lines(density(rnorm(19, mean = MPGlucosa, sd=DPGlucosa)))
qqPlot(GrupoPreDiab$GLU)

#GrupoPreDiab[c(2,19),"GLU"]

#Glucosa Diabetes
MDGlucosa <- mean(GrupoDiabetes$GLU)
DDGlucosa <- sd(GrupoDiabetes$GLU)

par(mfrow=c(1,2))
hist(GrupoDiabetes$GLU, freq=F, main="Histograma Diabetes Glucosa")
lines(density(rnorm(14, mean = MDGlucosa, sd=DDGlucosa)))
qqPlot(GrupoDiabetes$GLU)

#GrupoDiabetes[c(5,15),"GLU"]

#Test Normalidad####

"H0:La muestra proviene de una distribución normal
 H1: La muestra no proviene de una distribución normal
 
Alfa: Nivel de significancia  
Alfa = 0.05

Si P < Alfa; se rechaza H0
Si P >= Alfa; NO se rechaza H0

"

#Glucosa General

#Para el test de kolmogorov-smirnov
ks.test(Base1$GLU,"pnorm" ,mean(Base1$GLU), sd(Base1$GLU))

#Para el test lillie que es una variante del kolmogorov-smirnov 
lillie.test(Base1$GLU)

#Para el test Shapiro Pequeñas
shapiro.test(Base1$GLU)

#Para el test Anderson Darling
ad.test(Base1$GLU)

#Para el test cramer-von-misses  Pequeñas
cvm.test(Base1$GLU)

#Para el test jarque-bera
jb.norm.test(Base1$GLU)


#Glucosa Control
shapiro.test(GrupoControl$GLU) 
cvm.test(GrupoControl$GLU)
#

#Glucosa Pre Diabetes
shapiro.test(GrupoPreDiab$GLU)
cvm.test(GrupoPreDiab$GLU)
#

#Glucosa Diabetes
shapiro.test(GrupoDiabetes$GLU)
cvm.test(GrupoDiabetes$GLU)

