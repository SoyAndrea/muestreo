#  TTM 2024 Muestreo Sistematico

# Universo bajo estudio: Población 2010 
# Parametro a estimar: Total de poblacion


# MUESTREO SISTEMATICO

# Se desea estimar el total de poblacion 2010 mediante una muestra sistematica 
# de radios censales de tamano (aproximado) n=100, ordenando el marco de muestreo
# segun codigo de radio
# Estimando el total mediante el pi-estimador:
# H-T(s): Sum Yi/pi_i  =   Sum Yi/ (1/I) = I*sum(yi)

# Se pide: 
# 1. Hallar el parametro poblacional
# 2. Seleccionar con R una muestra sistematica, de arranque aa=3
# 3. Calcular sesgo, CV y deff del estimador
#    seleccionando las I muestras posibles. 

# 4. Repetir probando otros tres ordenamientos: segun cantidad de viviendas 
#    en el radio, segun poblacion en el radio y en forma aleatoria.

# 4. Seleccionar una muestra con sampling (ordenando por codigo de radio)
# 5. Mediante survey, estimar el parametro poblacional, varianza, ds y cv del estimador
#    a partir de la muestra de radios seleccionada,suponiendo  que  se  trata  de  una  MAS



# Paquetes que necesitaremos
library(readxl)
library(writexl)
library(sampling)
library(survey)
library(ggplot2)
library(dplyr)
#library(tidyverse)

# Establecemos el Directorio de Trabajo
setwd("D:/Boulots/universi/maestria_generacion_info/teoria_tecnicas_muestreo_2024")

rm(list = ls())

# Lectura de las tablas
radios_sexo <- read_excel("cen2010_radios_sexo.xlsx")
radios_bienes <- read_excel("cen2010_radios_bienes.xlsx")
radios_tipo <- read_excel("cen2010_radios_tipo.xlsx")

#  Juntamos los archivos para unificarlos en un unico dataframe
radios_2010 = merge(radios_sexo, radios_bienes, by = "Codigo")
radios_2010 = merge(radios_2010, radios_tipo, by = "Codigo")

# Generamos nuevas variables en el dataframe que utilizaremos mas adelante
# Poblacion total en cada radio
radios_2010$Pob_radio <- radios_2010$Varon + radios_2010$Mujer 


# Cantidad de viviendas en cada radio
radios_2010$Viviendas <-  radios_2010$Casa + 
                          radios_2010$Rancho + 
                          radios_2010$Casilla + 
                          radios_2010$Departamento + 
                          radios_2010$Inquilinato + 
                          radios_2010$Hotel_pension


# Extraemos el codigo de provincia
radios_2010$prov <- floor(radios_2010$Codigo/10000000)


# Creamos etiqueta para las provincias
radios_2010$Provincia <- "X"
radios_2010 <- radios_2010 %>%
  mutate(Provincia = case_when(prov == 2 ~ 'CABA', prov == 6 ~ 'BsAs',
                               prov == 10 ~ 'Catamarca', prov == 14 ~ 'Cordoba',
                               prov == 18 ~ 'Corrientes', prov == 22 ~ 'Chaco',
                               prov == 26 ~ 'Chubut', prov == 30 ~ 'Entre Rios',
                               prov == 34 ~ 'Formosa', prov == 38 ~ 'Jujuy',
                               prov == 42 ~ 'La Pampa', prov == 46 ~ 'La Rioja',
                               prov == 50 ~ 'Mendoza', prov == 54 ~ 'Misiones',
                               prov == 58 ~ 'Neuquen', prov == 62 ~ 'Rio Negro',
                               prov == 66 ~ 'Salta', prov == 70 ~ 'San Juan',
                               prov == 74 ~ 'San Luis', prov == 78 ~ 'Santa Cruz',
                               prov == 82 ~ 'Santa Fe', prov == 86 ~ 'Santiago',
                               prov == 90 ~ 'Tucuman', prov == 94 ~ 'TdFuego'))


# Eliminamos los radios sin viviendas o poblacion
radios_2010 <- radios_2010[radios_2010$Viviendas>0,]
radios_2010 <- radios_2010[!is.na(radios_2010$Pob_radio),]


# N poblacional
N <- nrow(radios_2010)


# Tamano de la muestra
n <- 200

# Hallo intervalo de seleccion
I <- floor(N/n)

# Parametro poblacional
parametro <- sum(radios_2010$Pob_radio)



# Ordenamiento del marco de muestreo
# radios_2010 <- radios_2010[order(radios_2010$Codigo),]
# radios_2010$aleatorio <- runif(nrow(radios_2010),0,1)
radios_2010 <- radios_2010[order(radios_2010$Viviendas),]
# radios_2010 <- radios_2010[order(radios_2010$Pob_radio),]
# radios_2010 <- radios_2010[order(radios_2010$aleatorio),]



# Defino el arranque aleatorio
aa=3

s = radios_2010[ seq(aa,N,I), ]

estim <- I*sum(s$Pob_radio)   #H-T. Es insesgado este estimador

error_rel <- 100*(parametro - estim) /parametro

# Ahora calculamos varianza, emc, cv y deff 
# del estimador
# hallando las I estimaciones posibles


# Voy a calcular las I estimaciones posibles
# una para cada arranque aleatorio
# Definimos ahora una funcion que seleccione una muestra sistematica, 
# con el arranque aleatorio como variable independiente
# y devuelva la estimacion


estim_sistematico <- function(aa) {
  s = radios_2010[ seq(aa,N,I), ]
  estim <- I*sum(s$Pob_radio)
  return(c(estim))
}


# Ejemplo
estimacion <- estim_sistematico(2)



# Calculamos las I estimaciones posibles
lista_arranques <- 1:I

lista_estimaciones <- lapply(lista_arranques, estim_sistematico)

df_estim <- data.frame(matrix(unlist(lista_estimaciones),ncol=1, byrow=TRUE ) )


colnames(df_estim) <- c("Estimacion")

Esperanza   <-  mean(df_estim$Estimacion)
Sesgo    <- Esperanza - parametro
Varianza <-  var(df_estim$Estimacion)*(N-1)/N
DS       <- sqrt(Varianza)
CV        <- 100*DS/parametro
CV


# Comparar este CV con el CV de un MAS(100), estimando
# el total de poblacion mediante el estimador de H-T:
# N * y_media


# Que forma tiene el estimador?
# Es aproximadamente normal?
p<-ggplot(df_estim, aes(x=Estimacion)) + 
  geom_histogram(color="black", fill="white") + 
  geom_vline(xintercept = sum(radios_2010$Pob_radio))  
p



# Calculamos ahora el deff 
S2 = var(radios_2010$Pob_radio)
V_mas = N^2*(1-n/N)*S2/n

DS_mas = sqrt(V_mas)



deff= Varianza / V_mas

# Interpretacion deff
#     deff=2 ->  Mi diseno tiene el doble de varianza que el que
#     hubiera obtenido con un MAS de tamano similar
#     deff=0.5 -> Si hubiera seleccionado un MAS de igual tamano
#     hubiera obtenido un estimador del doble de varianza

#     deff=2 ->  Con la mitad de muestra hubiera con un MAS, obtenido la misma
#     precision



# Seleccionamos ahora una muestra sistematica con intervalo I, 
# mediante sampling con el comando UPsystematic. 

# Definimos el vector de probabilidades de seleccion
# Recordemos que en el muestreo sistematico cada muestra
# tiene probabilidad 1/I de ser seleccionada


radios_2010$pi_i <- 1/I


# Agrego total poblacional, por si lo necesito
# en survey
radios_2010$fpc <- N


# Seleccionamos la muestra con sampling y definimos 
# el factor de expansion
pik <- radios_2010$pi_i 

s= sampling :: UPsystematic(pik)
sum(s)

muestra_radios = radios_2010[s==1,]
muestra_radios$pondera <- 1/muestra_radios$pi_i


# Con survey realizo las estimaciones 
# Pero asumiendo que fue una MAS 
# Observar que le damos a survey la misma informacion 
# Agrego fpc para que survey incluya en el calculo de la varianza
# el factor de correccion por poblacion finita
# Survey en este caso sobreestima la varianza

diseno <- survey :: svydesign(id=~1,weights = ~pondera, 
                              data=muestra_radios,  fpc = ~fpc)
diseno

EstTotalPob <- survey :: svytotal(~Pob_radio, diseno, 
                                        deff=TRUE, cv=TRUE, ci=TRUE)
EstTotalPob
cv(EstTotalPob)
survey :: cv(EstTotalPob)
deff(EstTotalPob)
confint(EstTotalPob, level=0.99)

# Que CV si quisiera estimar total de hogares en rancho/casilla?

diseno2 <- update(diseno, Rancho_Casilla=muestra_radios$Rancho + muestra_radios$Casilla)

EstRanchoCasilla <- survey :: svytotal(~Rancho_Casilla, diseno2, 
                                  deff=TRUE, cv=TRUE, ci=TRUE)

survey :: cv(EstRanchoCasilla)

df_estimacion <- data.frame(EstTotalPob)
df_estimacion$cv <- 100*df_estimacion$Pob/df_estimacion$total
survey :: cv(EstTotalPob)


# Como le indicamos a survey que fue una MAS la estimacion del CV 
# es conservadora: sobreestimacion

# Estimacion de hogares en hotel pension
parametro <- sum(radios_2010$Hotel_pension)
estim_sistematico <- function(aa) {
  s=radios_2010[ seq(aa,N,I), ]
  estim1  <- N*mean(s$Hotel_pension)
  estim2 <- I*sum(s$Hotel_pension)
  return(c(estim1, estim2))
}


