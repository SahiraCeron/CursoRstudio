#Practica 3

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


#Ejemplo para hacer pruebas de normalidad a todas las variables del grupo control####

#Primero seleccionamos las variables continuas.

ContinuasControl<-GrupoControl[ ,c(3, 11:21,23)]

#Creamos una matriz vacia donde el numero de filas es el numero de variables 
#El numero de columnas es el numero de pruebas que haremos

PVALUES<-matrix(NA, 13, 2)


for (i in 1:13) {
  
  cs<- shapiro.test(ContinuasControl[[i]])
  cc<- cvm.test(ContinuasControl[[i]])
  
  PVALUES[i,1]<-round(cs$p.value,5)
  PVALUES[i,2]<-round(cc$p.value,5)
}

#La convertimos en un data frame
PVALUES<-as.data.frame(PVALUES)

#Le agregamos una columna para poner el nombre de las variables
PVALUES<-cbind(colnames(ContinuasControl),PVALUES)

#Nombramos las columnas
colnames(PVALUES)<-c("Variables","Shapiro","Cramer-Von-Misses")


#Decisión####
#Podemos hacer un código para que ya nos ponga la decisión que tomamos.

#Agregamos una columna para poder poner los valores.

PVALUES <- cbind(PVALUES,"Desicion" = c(1:13))

#Creamos un ciclo for para que vaya poniendo la desición de cada variable
#Y un condiconal para poner la desisición

for (i in 1:13) {
  
  if (PVALUES[i,2]>0.05 & PVALUES[i,3]>0.05) {
    PVALUES[i,4] <- "No Rechazo H0"
  } else{
    PVALUES[i,4] <- "Rechazo H0"
  }
  
}

###Comparación 2 grupos####

"var.test() para comprobar la igualdad de varianzas entre dos grupos de datos
H0: x & y p tienen la misma varianza
H1: x & y NO tienen la misma varianza

si p>0.05 No se rechaza H0"

var.test(GrupoControl$GLU, GrupoDiabetes$GLU)

"t.test para comparar 2 muestras. Debe cumplir: normalidad
H0: MEDIA control = MEDIA diabetes  H1: MEDIA control DISTINTA MEDIA diabetes"

t.test(GrupoControl$GLU,GrupoDiabetes$GLU)

mean(GrupoControl$GLU)
mean(GrupoDiabetes$GLU)

"p<alpha  => Rechazo H0"

"H0:Media del control es mayor que la media del grupo diabetes
H1: Media del control es menor que la media del grupo diabetes
p < alpha rechazo H0"

t.test(GrupoControl$GLU,GrupoDiabetes$GLU, alternative = "less")

"Rechazo H0, La media del grupo control es menor que la media del grupo diabetes"

"SI importa el orden en que se ponen los grupos, en este caso:"

t.test(GrupoDiabetes$GLU, GrupoControl$GLU, alternative = "less")

#¿cómo estarian formuladas nuestras hiportesis?

##Respuesta##
"H0: Media del grupo diabetes es mayor que la media del grupo control
H1: Media del grupo diabetes es menor que la media del grupo control"


#Si tenemos grupos pareados: paired=T
#Si las varianzas de los grupos son iguales: var.equal=T
#t.test(x , y, var.equal = T, paired=T)


"Alternativa para t-test cuando no se distrubuyen normal. 
U-Mann-Whitney cuando son independientes y Wilcoxon cuando cuando son pareadas"

#Para U-Mann-Whitney se usa wilcox.test()

"H0:Mediana del grupo prediabetes es menor que la del grupo control
H1: Mediana del grupo prediabetes es mayor que la del grupo control"

wilcox.test(GrupoPreDiab$GLU, GrupoControl$GLU, alternative = "greater")


"Si tenemos datos pareados debemos espeficarlo con paired=T, asi ya estariamos 
usando la prueba de wilcoxon"

#wilcox.test(x, y, paired =T)


