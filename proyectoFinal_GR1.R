##################################################
# Análisis Multivariado y Modelos Estocásticos   #
#                 Grupo 1                        #
##################################################
#            PROYECTO FINAL                      #
##################################################
#    OPTIMIZACIÓN DE SUAVIZAMIENTO EXPONENCIAL   #
#             ALGORITMO GENÉTICO                 #
##################################################

#Recomendación:
  
#Para la ejecución del algoritmo genético se deben cargar y ejecutar los archivos
#en el siguiente orden:
  
#1. crear_poblacion
#2. calcular_fitness_individuo
#3. calcular_fitness_poblacion
#4. seleccionar_individuo
#5. cruzar_individuos
#6. mutar_individuo
#7. alg_gent

#Una vez cargado y ejecutado en ese orden puede proceder a utilizar el archivo:
#  * proyectoFinal_GR1

#Nota: cargar la función "pow(y,salto)" del algoritmo de fuerza bruta


##############################################################################################################
#Paquetes necesarios para la ejecuciÃ³n
# install.packages("readr") #Lectura csv
# install.packages("readxl") #Lectura excel
# install.packages("nortest") #Test de Normalidad - Kolmogorov Smirnov
# install.packages("Metrics")
# install.packages("tictoc")
# install.packages("GA")

#Carga de paquetes
library(readr)
library(readxl)
library(nortest)
library(Metrics)
library(tictoc)
library(GA)

#         DEFINICIÓN DE FUNCIÓN OBJETIVO
funcion <- function(x1){
  x1 =NULL
  sua = HoltWinters(y, alpha=x1, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
  s1 = sua$fitted[,1] #valores del suavizamiento exponencial
  err_pr_sua = y[2:(length(y))]-s1 #error del pronostico con suavizamiento
  err_pr_sua2 = err_pr_sua^2 
  CME_s = sum(err_pr_sua2)/length(s1)
}

#VECTOR DE PRUEBA DE FUNCIONAMIENTO DE ALGORITMO
y = c(17,21,19,23,18,16,20,18,22,20,15,22) 

#FUERZA BRUTA
pow(y,0.01)

#Método 1: Algoritmo genético
set.seed(3)
resultados_ga <- optimizar_ga(
  funcion_objetivo = funcion,
  n_variables      = 1,
  optimizacion     = "minimizar",
  limite_inf       = 0,
  limite_sup       = 1,
  n_poblacion      = 30,
  n_generaciones   = 500,
  elitismo         = 0.01,
  prob_mut         = 0.1,
  distribucion     = "normal",
  min_distribucion = -1,
  max_distribucion = 1,
  metodo_seleccion = "tournament",
  metodo_cruce     = "uniforme",
  parada_temprana  = TRUE,
  rondas_parada    = 10,
  tolerancia_parada = 10^-2,
  verbose          = 1,
  y = y
)

#Método 2: Algoritmo genético - R
tic()
GA <- ga(type="real-valued", fitness = funcion , lower = 0, upper = 1, maxiter = 1, seed = 123, elitism = 2)
print(paste("CME Óptimo = ",GA@fitnessValue))
toc()

##############################################################################################################
#1. CARGA DE DATOS DE DATASETS

# DATASET 1 <- 135 instancias
#Behavior of the urban traffic of the city of Sao Paulo in Brazil DataSet
#Es un dataset donde se toman muestras cada hora sobre el trafico
#y se va a predecir la variable Slowness in traffic
data1<- read_delim("UPS/6to_nivel/Analisis_Estocas/Grupo/Proyecto2/datasets/Behavior of the urban traffic of the city of Sao Paulo in Brazil.csv", 
                                                                               delim = ";", escape_double = FALSE, trim_ws = TRUE)
data1 <- data1[,-c(1)]

# DATASET 2 <- 522 instancias
#Hungarian Chickenpox Cases Data Set: El dataset se trata sobre los
#casos de varicela, se toman muestras cada 7 d ??ias desde el a ~no 2005
#hasta el 2014 y se va a predecir los casos en la ciudad de BUDAPEST
data2 <- read_csv("UPS/6to_nivel/Analisis_Estocas/Grupo/Proyecto2/datasets/hungary_chickenpox.csv")
data2 <- data2[,-c(1)]

# DATASET 3 <- 9357 instancias
#Air Quality Data Set: El dataset trata de la calidad del aire donde
#se toman diferentes muestras desde el 10/03/2004 a las 18:00 horas
#hasta las 14:00 del 04/04/2005 donde se va a predecir la variable
#PT08.S1(CO) (oxido de esta ~no)

data3 <- read_excel("UPS/6to_nivel/Analisis_Estocas/Grupo/Proyecto2/datasets/AirQualityUCI.xlsx")
data3 <- data3[,-c(1,2)]

##############################################################################################################
# 2. TRATAMIENTO DE DATOS
#
# 2.1 Reemplazo de datos faltantes por la media 

#Función para valores faltantes (NA)
datos_na <- function(x){
  if (sum(is.na(x))>0){
    x[is.na(x)] = mean(x)
    print("**Datos Faltantes (NA) reemplazados**")
  }else{
    print("**No tiene valores faltantes (NA)**")
  }
}

#Dataset 1
for( i in colnames(data1)){
  print(paste("Nombre Columna: ",colnames(data1[i])))
  datos_na(data1[[i]])
}
#Dataset 2
for( i in colnames(data2)){
  print(paste("Nombre Columna: ",colnames(data2[i])))
  datos_na(data2[[i]])
}
#Dataset 3
for( i in colnames(data3)){
  print(paste("Nombre Columna: ",colnames(data3[i])))
  datos_na(data3[[i]])
}


# 2.2 Extracción de variable con distribución normal de cada DATASET

#   2.2.1 Prueba de normalidad
#         Kolmogorov Smirnov (n>=50)
#         p-value > alpha (0.05) <- existe normalidad de los datos

var_normal <- function(x){
  kolg_srmi <- cbind()
  for( i in colnames(x)){
    k_test <- lillie.test(x[[i]])$p.value
    kolg_srmi <- cbind(kolg_srmi,c(k_test))
  }
  max_Kt=kolg_srmi[which.max(kolg_srmi)]
  indMax_Kt = which.max(kolg_srmi)
  column_nom <- colnames(x[indMax_Kt])
  print(paste("p-value max = ",max_Kt))
  print(paste("Índice de columna: ",indMax_Kt))
  print(paste("Nombre de columna: ",column_nom))
}

var_normal(data1)#DATASET 1
var_normal(data2)#DATASET 2
var_normal(data3)#DATASET 3

#Variables extraidas
y1 <- data1$`Slowness in traffic (%)`
y2 <- data2$BUDAPEST
y3 <- data3$`PT08.S5(O3)`

# 3. OPTIMIZACIÓN DE SUAVIZAMIENTO EXPONENCIAL

##DATASET 1
##Fuerza Bruta
pow(y1,0.01)

##Método 1: Algoritmo Genético

#         DEFINICIÓN DE FUNCIÓN OBJETIVO <- y1
funcion <- function(x1){
  x1 =NULL
  sua = HoltWinters(y1, alpha=x1, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
  s1 = sua$fitted[,1] #valores del suavizamiento exponencial
  err_pr_sua = y1[2:(length(y1))]-s1 #error del pronostico con suavizamiento
  err_pr_sua2 = err_pr_sua^2 
  CME_s = sum(err_pr_sua2)/length(s1)
}

set.seed(50)
resultados_ga <- optimizar_ga(
  funcion_objetivo = funcion,
  n_variables      = 1,
  optimizacion     = "minimizar",
  limite_inf       = 0,
  limite_sup       = 1,
  n_poblacion      = 30,
  n_generaciones   = 500,
  elitismo         = 0.01,
  prob_mut         = 0.1,
  distribucion     = "normal",
  min_distribucion = -1,
  max_distribucion = 1,
  metodo_seleccion = "tournament",
  metodo_cruce     = "uniforme",
  parada_temprana  = TRUE,
  rondas_parada    = 10,
  tolerancia_parada = 10^-4,
  verbose          = 1,
  y = y1
)

#Método 2: Algoritmo genético - R
tic()
GA <- ga(type="real-valued", fitness = funcion , lower = 0, upper = 1, maxiter = 1, seed = 123, elitism = 2)
print(paste("CME Óptimo = ",GA@fitnessValue))
toc()


##DATASET 2
##Fuerza Bruta
pow(y2,0.01)

##Método 1: Algoritmo Genético

#         DEFINICIÓN DE FUNCIÓN OBJETIVO <- y2
funcion <- function(x1){
  x1 =NULL
  sua = HoltWinters(y2, alpha=x1, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
  s1 = sua$fitted[,1] #valores del suavizamiento exponencial
  err_pr_sua = y2[2:(length(y2))]-s1 #error del pronostico con suavizamiento
  err_pr_sua2 = err_pr_sua^2 
  CME_s = sum(err_pr_sua2)/length(s1)
}

set.seed(38)
resultados_ga <- optimizar_ga(
  funcion_objetivo = funcion,
  n_variables      = 1,
  optimizacion     = "minimizar",
  limite_inf       = 0,
  limite_sup       = 1,
  n_poblacion      = 30,
  n_generaciones   = 500,
  elitismo         = 0.01,
  prob_mut         = 0.1,
  distribucion     = "normal",
  min_distribucion = -1,
  max_distribucion = 1,
  metodo_seleccion = "tournament",
  metodo_cruce     = "uniforme",
  parada_temprana  = TRUE,
  rondas_parada    = 10,
  tolerancia_parada = 10^-2,
  verbose          = 1,
  y = y2
)

#Método 2: Algoritmo genético - R
tic()
GA <- ga(type="real-valued", fitness = funcion , lower = 0, upper = 1, maxiter = 1, seed = 123, elitism = 2)
print(paste("CME Óptimo = ",GA@fitnessValue))
toc()

##DATASET 3
##Fuerza Bruta
pow(y3,0.01)

##Método 1: Algoritmo Genético

#         DEFINICIÓN DE FUNCIÓN OBJETIVO <- y1
funcion <- function(x1){
  x1 =NULL
  sua = HoltWinters(y3, alpha=x1, beta=FALSE, gamma=FALSE) #suavizamiento exponencial
  s1 = sua$fitted[,1] #valores del suavizamiento exponencial
  err_pr_sua = y3[2:(length(y3))]-s1 #error del pronostico con suavizamiento
  err_pr_sua2 = err_pr_sua^2 
  CME_s = sum(err_pr_sua2)/length(s1)
}

set.seed(7)
resultados_ga <- optimizar_ga(
  funcion_objetivo = funcion,
  n_variables      = 1,
  optimizacion     = "minimizar",
  limite_inf       = 0,
  limite_sup       = 1,
  n_poblacion      = 30,
  n_generaciones   = 500,
  elitismo         = 0.01,
  prob_mut         = 0.1,
  distribucion     = "normal",
  min_distribucion = -1,
  max_distribucion = 1,
  metodo_seleccion = "tournament",
  metodo_cruce     = "uniforme",
  parada_temprana  = TRUE,
  rondas_parada    = 10,
  tolerancia_parada = 10^-8,
  verbose          = 1,
  y = y3
)

#Método 2: Algoritmo genético - R
tic()
GA <- ga(type="real-valued", fitness = funcion , 
         lower = 0, upper = 1, maxiter = 1, seed = 123, elitism = 2)
print(paste("CME Óptimo = ",GA@fitnessValue))
toc()