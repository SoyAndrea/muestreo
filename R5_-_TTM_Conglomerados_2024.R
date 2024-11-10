# TTM 2024 Muestreo por conglomerados

library(readxl)
library(writexl)
library(sampling)
library(survey)
library(ggplot2)
library(dplyr)
library(fishmethods)

#setwd("D:/Boulots/universi/maestria_generacion_info/teoria_tecnicas_muestreo_2024")


# Universo bajo estudio: conjunto de alumnos último año secundario en 
# 2019 con puntaje calculado en matematica (TEM). 
# Y sin respuestas faltantes en ap02 (tenencia de automovil)
# De ambito urbano
# Cada colegio será considerado un conglomerado de alumnos

# Deseamos estimar, mediante una muestra aleatoria de 80 colegios: 
# Total y proporcion de alumnos segun tenencia de automoviles en el hogar 
# Puntaje promedio en matemática y lengua

# Evaluaremos dos estrategias:
# 1. Seleccion mediante Madow, con probabilidad de selección 
# proporcional a la cantidad de alumnos de la tabla. Estratificando 
# el marco de muestreo por Zona (CABA, Buenos Aires y Resto) y sector de gestion
# Ordenando el marco, dentro de cada estrato, por provincia.
# Asignacion de la muestra: proporcional a la cantidad de colegios



# Lectura de la tabla de datos

tabla_alumnos <- read.csv2("data/Bases usuarias Aprender 2019 CSV/Base estudiantes 5-6 año secundaria 2019 USUARIA (Censo L y M).csv")[,c(1:90, 230:240)]
tabla_alumnos <- tabla_alumnos[tabla_alumnos$ambito==1, ]
table(tabla_alumnos$ambito)

# Para este ejercicio no imputamos la NR en TEM y TEL
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$TEM),]
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$TEL),]

tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$ap11_02),]
tabla_alumnos <- tabla_alumnos[tabla_alumnos$ap11_02 >0,]



# Verificamos TEM y TEL numericos
class(tabla_alumnos$TEM)
class(tabla_alumnos$TEL)
# tabla_alumnos$TEM <- as.numeric(tabla_alumnos$TEM)
# tabla_alumnos$TEL <- as.numeric(tabla_alumnos$TEL)
options(scipen = 999)

# Los colegios seran los conglomerados de alumnos
# Encuestamos a todos los alumnos de los conglomerados seleccionados:
# muestreo por conglomerado monoetapico


# Creo una variable Colegio que numere los colegios (no los alumnos)
# en orden correlativo
# comenzando en 1

# Puedes corregir el error en la siguiente linea


tabla_alumnos <- tabla_alumnos %>% 
  group_by(cod_provincia, sector, ID1) %>%
  mutate(Colegio = cur_group_id()) # armar ud de colegio



# MAS de 40 colegios
n <- 80      

# Genero tabla de colegios con dplyr
# Agrego alumnos encuestados
tabla_colegios <- tabla_alumnos %>%
                  group_by(cod_provincia, sector, Colegio, ID1) %>%
                  summarise(Mi=n())
  
tabla_colegios$Colegio <- 1:nrow(tabla_colegios) 
tabla_colegios$ID1 <- NULL

# Estratificamos el marco de muestreo
# (en un caso real estratificariamos tambin por tamanio)
tabla_colegios <- tabla_colegios %>%
  mutate(Zona= case_when(cod_provincia==2~ "CABA",
                         cod_provincia==6 ~ "PBA",
                         TRUE ~ "Resto"))


table(tabla_colegios$Zona, useNA = "always")



# Asignacion de la muestra: proporcional a la cantidad de colegios
# Calculo total de colegios por estrato y total de matricula por estrato
# Mh lo utilizare para Madow
tabla_estratos <- tabla_colegios %>%
                  group_by(Zona, sector)  %>%   
                  summarise(Nh=n(), Mh=sum(Mi)) %>%
                  mutate(nh= round(n*Nh/nrow(tabla_colegios)))


sum(tabla_estratos$nh)

# SI HAY NH = 1 SUMAR 2 
tabla_estratos$nh <- ifelse(tabla_estratos$nh == 1, 2, tabla_estratos$nh)
tabla_estratos$Estrato <- 1:nrow(tabla_estratos) 


# Ordeno tabla estratos por Estrato
tabla_estratos <- tabla_estratos[order(tabla_estratos$Estrato),]


# MAS
# Seleccionamos una muestra con sampling

#Necesito pasar a tabla_colegios el nh y Mh
tabla_colegios <- merge(tabla_colegios, 
                        tabla_estratos[, c("Zona", "sector", "Estrato", "Nh", "nh")],
                        by=c("Zona", "sector"))


# Ordenamos el marco de muestreo por Estrato (lo pide sampling)
tabla_colegios <- tabla_colegios[order(tabla_colegios$Estrato,
                                       tabla_colegios$cod_provincia), ]



smpl_colegios = sampling::strata(data=tabla_colegios,
                                 stratanames="Estrato",
                                 method="srswor", #muestreo aleatorio simple
                                 size=tabla_estratos$nh) # tabla de estrato ordenda 

# recupero info de marco
muestra_colegios = sampling::getdata(tabla_colegios, smpl_colegios)


# Agregamos el factor de expansion para cada colegio
# Prob la genera sampling
muestra_colegios$pondera <-   1/muestra_colegios$Prob


# Genero muestra de alumnos ahora

muestra_alumnos <- merge(tabla_alumnos, 
                         muestra_colegios[, c("Colegio", "pondera", "Nh", "Estrato")],
                         by="Colegio")

# Definimos el diseno en survey 
# Le indicamos que fue un muestreo con reposicion

# Observar que ahora id = ~Colegio. Le indico a survey que la unidad de primera etapa
# es el colegio

diseno <- svydesign(id = ~Colegio, strata=~Estrato, weights = ~pondera, 
                    data=muestra_alumnos,fpc=~Nh)



# Estimaciones de totales y proporciones en tenencia de automoviles
class(muestra_alumnos$ap11_01)

muestra_alumnos$ap11_01 <- factor(muestra_alumnos$ap11_01)

#repetir
diseno <- svydesign(id = ~Colegio, strata=~Estrato, weights = ~pondera, 
                    data=muestra_alumnos,fpc=~Nh)

Est_totales <- svytotal(~ap11_02, diseno, deff=TRUE, ci=TRUE, na.rm=TRUE)
Est_totales

Est_prop <- svymean(~ap11_02, diseno, deff=TRUE, ci=TRUE, na.rm=TRUE)
Est_prop



# Estimaciones de puntaje medio
Est_medias <- svymean(~TEM+TEL, diseno, deff=TRUE, ci=TRUE)
Est_medias



# Interpretacion del deff: con un MAS de tamanio deff veces menor
# hubiera obtenido la misma precision

# El deff esta influenciado: 
# Tamaño variable de colegios
# Distribucion de muestra por estratos
# Etc.



# Madow
# Ordenamos el marco de muestreo por Estrato (lo pide sampling)
tabla_colegios <- tabla_colegios[order(tabla_colegios$Estrato,
                                       tabla_colegios$cod_provincia), ]

# Pasamos a la tabla de colegios la cantidad de colegios por estrato

tabla_colegios$Estrato <- NULL
tabla_colegios$Nh <- NULL
tabla_colegios$Mh <- NULL
tabla_colegios$nh <- NULL

tabla_colegios <- merge(tabla_colegios, 
                        tabla_estratos[, c("Zona", "sector", "Estrato", 
                                           "Nh", "Mh", "nh")],
                        by=c("Zona", "sector")) 

# Probabilidad de seleccion de cada colegio
tabla_colegios$pik <- tabla_colegios$nh*tabla_colegios$Mi/tabla_colegios$Mh 

sum(tabla_colegios$pik)


smpl_colegios = sampling::strata(data=tabla_colegios,
                                 stratanames="Estrato",
                                 method="systematic",
                                 size=tabla_estratos$nh, 
                                 pik=tabla_colegios$pik) # tamaño de muestra por estrato


muestra_colegios = sampling::getdata(tabla_colegios, smpl_colegios)


# Agregamos el factor de expansion para cada colegio
# Prob la genera sampling
muestra_colegios$pondera <-   1/muestra_colegios$Prob


# Recupero alumnos de esos colegios
muestra_alumnos$pondera <- NULL # PARA NO MEZCLAR PONDERADORES DE COLEGIOS Y ALUMNOS


REVISAR
# muestra_alumnos <- merge(muestra_alumnos <- merge(muestra_colegios["Colegio", "pondera"], tabla_alumnos,
#                          by="Colegio")



# Definimos el diseno en survey 
# Le indicamos que fue un muestreo con reposicion : omito fpc

# Observar que ahora id = ~Colegio. Le indico a survey que la unidad de primera etapa
# es el colegio

diseno <- svydesign(id = ~Colegio, weights = ~pondera, 
                    data=muestra_alumnos)



# Estimaciones de totales y proporciones en tenencia de automoviles
Est_totales <- svytotal(~ap11_02, diseno, deff=TRUE, ci=TRUE, na.rm=TRUE)
Est_totales

Est_prop <- svymean(~ap11_02, diseno, deff=TRUE, ci=TRUE, na.rm=TRUE)
Est_prop



# Estimaciones de puntaje medio
Est_medias <- svymean(~TEM+TEL, diseno, deff=TRUE, ci=TRUE)
Est_medias



# Interpretacion del deff: con un MAS de tamanio deff veces menor
# hubiera obtenido la misma precision

# El deff esta influenciado 
# por la homogeneidad de los colegios y la diferencia de tamaño de
# los colegios




##########################################################
# Estimacion de la homogeneidad de conglomerados

# Estimamos ahora la homogeneidad de los colegios
# respecto a algunas variables
# Utilizamos el paquete fishmethods
# Nos restringimos a colegios con una cantidad minima de alumnos
# y de tamaños similares en principio

# Estos indices van de 0 a 1. 1 maxima homogeneidad


tabla_alumnos <- read.csv2("D:/Boulots/aprender/2019/Bases usuarias Aprender 2019 CSV/Base estudiantes 5-6 anio secundaria 2019 USUARIA (Censo L y M).csv")[,c(1:90, 230:240)]
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$TEM),]
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$TEL),]

tabla_alumnos <- tabla_alumnos[tabla_alumnos$ap02>0,]
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$ap02),]

tabla_alumnos <- tabla_alumnos[tabla_alumnos$ap16>0,]
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$ap16),]


tabla_alumnos$Colegio <- tabla_alumnos$ID1


tabla_alumnos <- tabla_alumnos %>%
                 group_by(Colegio) %>%
                 mutate(Mi=n())


tabla_rho <- tabla_alumnos[(tabla_alumnos$Mi>=12) & (tabla_alumnos$Mi<=27) ,]
nrow(tabla_rho)


# Defino un data frame vacio para empezar
rho <- data.frame(row.names = c("Lohr rho", "Adjusted r-square", "ANOVA rho"))

# La variable cuya homogeneidad deseo calcular debe ser numerica
tabla_rho$varon <- ifelse(tabla_rho$ap02==1,1,0)
tabla_rho$mujer <- ifelse(tabla_rho$ap02==2,1,0)
tabla_rho$madre_univ <- ifelse(tabla_rho$ap16 >=6,1,0)
tabla_rho$Privado     <- ifelse(tabla_rho$sector ==2,1,0)
tabla_rho$Aleatorio   <- runif(nrow(tabla_rho,0,1))

for(i in c("TEM", "TEL", "varon", "mujer", "madre_univ", "Privado", "Aleatorio")){
  rho[[i]] <- fishmethods::clus.rho(popchar=tabla_rho[[i]] , 
                                    cluster = tabla_rho$Colegio,
                                    type = c(1,2,3), est = 0, nboot = 500)[[1]][,1]
}

rho


################################################################

