##Practica 5#
#Paqueterias####
library(readxl)
library(car) #Para leveneTest
library(nortest)
library(normtest)
library(dunn.test)

#Base####
Base1<-read_excel("C:\\Users\\86524\\Desktop\\Servicio\\Bases\\databaseR.xls")
Control<-Base1[Base1$Group==0 ,c(3, 11:21,23)]
PreDiabetes<-Base1[Base1$Group==1 ,c(3, 11:21,23)]
Diabetes<-Base1[Base1$Group==2 ,c(3, 11:21,23)]
Base1$Group<-as.factor(Base1$Group)

##Normalidad####

PVALUES<-matrix(NA, 13, 9)
PVALUES<-as.data.frame(PVALUES)

for (i in 1:13) {
  
  cs <- shapiro.test(Control[[i]])
  cc <- cvm.test(Control[[i]])
  csp <- shapiro.test(PreDiabetes[[i]])
  ccp <-cvm.test(PreDiabetes[[i]])
  csd <-shapiro.test(Diabetes[[i]])
  ccd <-cvm.test(Diabetes[[i]])  
  
  PVALUES[i,1]<-round(as.numeric(cs$p.value),5)
  PVALUES[i,2]<-round(as.numeric(cc$p.value),5)
  PVALUES[i,4]<-round(as.numeric(csp$p.value),5)
  PVALUES[i,5]<-round(as.numeric(ccp$p.value),5)
  PVALUES[i,7]<-round(as.numeric(csd$p.value),5)
  PVALUES[i,8]<-round(as.numeric(ccd$p.value),5)
  
    
    if (PVALUES[i,1]> 0.05 & PVALUES[i,2]> 0.05) {
      PVALUES[i,3] <- "No Rechazo H0"
    } else{
      PVALUES[i,3] <- "Rechazo H0"
    }
    
    if (PVALUES[i,4]> 0.05 & PVALUES[i,5]> 0.05) {
      PVALUES[i,6] <- "No Rechazo H0"
    } else{
      PVALUES[i,6] <- "Rechazo H0"
    }  
  
    if (PVALUES[i,7]> 0.05 & PVALUES[i,8]> 0.05) {
      PVALUES[i,9] <- "No Rechazo H0"
    } else{
      PVALUES[i,9] <- "Rechazo H0"
    } 
}

rm(cs,cc,csp,ccp,csd,ccd,i)

#Por ultimo dar formato

#Le agregamos una columna para poner el nombre de las variables
PVALUES<-cbind(colnames(Control),PVALUES)

#Nombramos las columnas
colnames(PVALUES)<-c("Variables","Shapiro-C","Cramer-Von-Misses-C","Desicion-C",
                     "Shapiro-P","Cramer-Von-Misses-P","Desicion-P",
                     "Shapiro-D","Cramer-Von-Misses-D","Desicion-D")


##Para comparar 3 grupos####

#Anova debe cumplir normalidad y homogeneidad en las  varianzas
#Tomaremos el caso de la cintura (waist)

boxplot(WAIST~Group,data = Base1)

#Para la homogeneidad en las varianzas usamos:

leveneTest(Base1$WAIST,Base1$Group)

fligner.test(WAIST~Group,Base1)

#Si se cumple homogeneidad en las varianzas, entonces podemos hacer el ANOVA 

AnovaWaist <- aov(Base1$WAIST ~ Base1$Group)
summary(AnovaWaist)

#p= 0.018
#Hay diferencias significativas, realizamos un analisis Post-Hoc

TukeyHSD(AnovaWaist)
plot(TukeyHSD(AnovaWaist))

#H0:MEDIA PREDIABETES = MEDIA CONTROL
#H1:MEDIA PREDIABETES DISTINTA MEDIA CONTROL

pairwise.t.test(x= Base1$WAIST, g=Base1$Group,p.adjust.method = "holm")

#Al hacer uso de alternative=greater, se evaluan las hipotesis:
#H0:Media prediabetes < Media control H1:Media prediabetes > Media control
#H0:M2<M0 H1:M2>M0
#H0:M2<M1 H1:M2>M1
pairwise.t.test(x= Base1$WAIST, g=Base1$Group,p.adjust.method = "holm",
                alternative = "greater")

#Al usar alternative=less, se evaluan las hipotesis:
#H0:M1>M0 H1:M1<M0
#H0:M2>M0 H1:M2<M0
#H0:M1>M0 H1:M1<M0

pairwise.t.test(x= Base1$WAIST, g=Base1$Group,p.adjust.method = "holm",
                alternative = "less")

#POdemos ver que hay diferencia entre el grupo control y diabetes y el
#grupo control y prediabetes

#Más especifico la media del grupo diabetes y prediabetes son mayores que 
#el grupo control

#Kruskal-Wallis

#CHOL
#
boxplot(CHOL~Group,data = Base1)

#Para ver que tengan una distribución similar
hist(Control$CHOL)
hist(PreDiabetes$CHOL)
hist(Diabetes$CHOL)

#Para homogeneidad de la varianza
leveneTest(Base1$CHOL,Base1$Group)

#Hacemos el test Kruskal-Wallis
kruskal.test(CHOL~ Group, data = Base1)

#p= 0.36
#No hay diferencia significativa

#Hacemos un analisis Post-Hoc

dunn.test(Base1$CHOL,Base1$Group,method = "bonferroni")

pairwise.wilcox.test(Base1$CHOL,Base1$Group,p.adjust.method = "holm")

#También  podemos hacer uso de alternative = "less" o "greater"
pairwise.wilcox.test(Base1$CHOL,Base1$Group,p.adjust.method = "holm", 
                     alternative="greater")

pairwise.wilcox.test(Base1$CHOL,Base1$Group,p.adjust.method = "holm", 
                     alternative="less")
#exact=F