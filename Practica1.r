##Paquetes####

#install.packages("  ") 

library(readxl) #Para cargar excel
library(modeest) #Para calcular la moda
library(car) #para las lineas del qqplot
library(carData) #para las lineas del qqplot
#library(nortest) #para test lilie
#library(tseries) #para el test Jarque Bera


##Cargar Base####

#Cargamos la base
file.choose()

Base1<-read_excel("C:\\Users\\86524\\Desktop\\Servicio\\Bases\\databaseR.xls")

class(Base1)

#Base1 <- as.data.frame(Base1)

#class(Base1)

#Checammos que tipo de dato le puso a nuestras variables.

str(Base1)

Base1$Group <- as.factor(Base1$Group)

Base1$Gender <- as.factor(Base1$Gender)


##Semillas####

"Cuando generamos numeros 'aleatorios' en R en realidad son numeros pseudo
aleatorios ya que estos necesitan una semilla para generarse."

set.seed(123)

#Se usan las semillas cuando queremos que repliquen nuestro código.

### Pruebas Normalidad Graficas####

###Para la edad###

MediaAge <- mean(Base1$Age)
ModaAge <- mfv(Base1$Age)
MedianaAge <- median(Base1$Age)
DesvAge <-sd(Base1$Age)

##Histograma y curva de normalidad

hist(Base1$Age, freq = F, main= "Histograma Edad", xlab="edad", ylab ="Frecuencia", 
     col="darkseagreen") #Para  graficar el histograma

lines(density(rnorm(50, mean=MediaAge, sd=DesvAge)), 
      col="darkred") #para graficar la curva normal


#Cuantiles teoricos Q-Q PLOT

qqnorm(Base1$Age, ylab = "Edad") #Para graficar los puntos de nuestros datos
qqline(Base1$Age) #Para graficar la linea de la normal

#Existe un paquete "car" que con su función qqPlot nos marca unas lineas de confianza
#Estas estan basadas en el error standar

qqPlot(Base1$Age) #Para marcar las lineas de confianza

ESEdad<-DesvAge/sqrt(50)
Base1[15,"Age"]
Base1[42,"Age"]

"¿Qué harían para tener un dataframe con los registros de 
   cada grupo(0=control, 1=prediabetes, 2=diabetes)?"

###-GRUPOS####
GrupoControl <- Base1[Base1$Group == 0,]
GrupoPreDiabetes <- Base1[Base1$Group ==1,]
GrupoDiabetes <-Base1[Base1$Group==2,]