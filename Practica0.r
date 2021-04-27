#Practica Servicio 0

##Excel####
#Necesitamos usar paquetes para que R pueda leer archivos de excel los más comunes son:
# "readxl" y "xlsx"
#Para la paqueteria xlsx 
#library(xlsx)
#read.xlsx()

#install.packages("readxl")

library(readxl)

#Para leer un archivo excel usamos la función read_excel("rutadelarchivo")
#Hay dos formas de obtener la ruta la primera buscar el archivo en nuestra computadora
#acceder a propiedades y copiar la ruta

#??C:\Users\86524\Desktop\Servicio\Bases\databaseREjemplo.xls
#Hay que cambiar todos las diagonales

BaseEjemplo <- read_excel("C:/Users/86524/Desktop/Servicio/Bases/databaseREjemplo.xls")

#Otra forma es usar la función file.choose() 

file.choose()

BaseEjemplo2 <-read_excel("C:\\Users\\86524\\Desktop\\Servicio\\Bases\\databaseREjemplo.xls")

#para especificar que hoja queremos extraer de excel usamos la siguiente expresión

#read_excel("ruta del archivo", numero de la hoja)

#read_excel("ruta del archivo", sheet = 'NombreDeLaHoja')

rm(BaseEjemplo2)
#remove(BaseEjemplo2)

class(BaseEjemplo)

#En caso de que no se carguen nuestro datos como data.frame debemos indicarlo

BaseEjemplo <- as.data.frame(BaseEjemplo)

class(BaseEjemplo)


BaseEjemplo$`Gender (1:male, 2:female)`

mean(BaseEjemplo$`WEIGHT kg`)

BaseEjemplo[,"Gender (1:male, 2:female)"]

#Nos marca con una comilla los nombres porque tiene espacios 

#Para cambiar los nombres en R tendriamos que utilizar la función colnames()

colnames(BaseEjemplo)[2]<-"Group"

#El problema es que tendriamos que hacer uno por uno 

#Otra forma es escribir un vector con los 25 nombres nuevos

# 9
names(BaseEjemplo)<-c("Sample_ID", "Group", "Age", "Gender", "years_with_T2D", 
                      "Glibenclamide", "Metformin", "HAS", "HAS_Tx", "Cuales",
                      "WEIGHT")
#Lo tedioso aquí es que debemos escribir todos incluso los que no queremos cambiar

