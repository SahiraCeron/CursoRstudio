#Paqueterias####
library(readxl)
library(nortest)
library(normtest)

#Base####
Base1<-read_excel("C:\\Users\\86524\\Desktop\\Servicio\\Bases\\databaseR.xls")
Control<-Base1[Base1$Group==0 ,c(3, 11:21,23)]
PreDiabetes<-Base1[Base1$Group==1 ,c(3, 11:21,23)]
Diabetes<-Base1[Base1$Group==2 ,c(3, 11:21,23)]

#Estructuras de control Condicionales####
##IF##
x <- 4

if(x > 5){
  print("x es mayor que 5")
}

##IF-ELSE##

if(x>5){
  print("X es mayor que 5")
}else{
  print("x NO es mayor que 5")
}

##IF-ANIDADO
x<-4
y<-4

if (x > y) {
  x-y
  }else if (x<y) {
    y-x
  }



if (x>y) {
  x-y
  }else if (x<y) {
    y-x
    }else{
      x+y
    }

##ifelse##

ifelse(4>5,TRUE,FALSE)

##switch##

switch("aaa", 
       "uno"=1,
       "dos" = 2,
       "tres" = 3,
       "cuatro" = 4)

#Para especificar una instrucción por default ponemos la instrucción sin asiganar 
#un caso

switch("aaa", 
       "uno"=1,
       "dos" = 2,
       "tres" = 3,
       "Cuatro" = 4,
       "Prueba con otra palabra")

#Si no especificamos los casos y ponemos como objeto un numero pasa lo siguiente:

switch (5, "rojo","azul","verde")

#Estructuras de control Iterativas####
##FOR##
vec<-c(3,4,7)
for(i in vec){
    print(i)
}

##WHILE##
y<-1

while (y<5) {
    print(y)
    y=y+1
}

#Pruebas de normalidad con estructuras de control####

"Primero debemos pensar que es lo que necesitamos o queremos hacer. En este caso,
lo que queremos es una tabla que tenga los pvalues de 2 pruebas de normalidad 
aplicadas a cada variables continuas del grupo control"

PVALUES<-matrix(NA, 13, 2)

for (i in 1:13) {
  
  cs<- shapiro.test(Control[[i]])
  cc<- cvm.test(Control[[i]])
  
  PVALUES[i,1]<-round(cs$p.value,5)
  PVALUES[i,2]<-round(cc$p.value,5)
}

rm(i)

"También queremos la decisión que tomaremos es decir rechazar o no rechazar la H0
y una columna con los nombres de las variables"

PVALUES <- cbind(PVALUES,"Desicion" = c(1:13))

for (i in 1:13) {
    
if (PVALUES[i,1]>0.05 & PVALUES[i,2]>0.05) {
    PVALUES[i,3] <- "No Rechazo H0"
  } else{
    PVALUES[i,3] <- "Rechazo H0"
  }
  
}

rm(i)

#Por ultimo dar formato
#La convertimos en un data frame
PVALUES<-as.data.frame(PVALUES)

#Le agregamos una columna para poner el nombre de las variables
PVALUES<-cbind(colnames(Control),PVALUES)

#Nombramos las columnas
colnames(PVALUES)<-c("Variables","Shapiro","Cramer-Von-Misses","Desicion")

#EJERCICIOS####
"1. Usar solo un ciclo for para poner los Pvalues de las pruebas y poner la
decisión (Rechazo, No rechazo)"

"2. Se puede cambiar la estructura IF-ELSE  por SWITCH "
# 
