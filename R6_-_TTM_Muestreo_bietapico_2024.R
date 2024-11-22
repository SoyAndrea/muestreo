# MPE 2024 Muestreo polietapico

library(readxl)
library(writexl)
library(sampling)
library(survey)
library(ggplot2)
library(dplyr)
library(fishmethods)

setwd("D:/Boulots/universi/maestria_generacion_info/teoria_tecnicas_muestreo_2024")




# Deseamos seleccionar aleatoria bietapica 
# Para estimar media de puntaje en matemática, media de puntaje en lengua
# y proporción de alumnos segun variable ap11_02 (tenencia de auto)
# Para este ejemplo no imputaremos los valores faltanes. 

# La unidad de primera etapa seran los colegios.
# La unidad de segunda etapa los alumnos.

# En la primera etapa, muestreo estratificado, estratificando por sector
# MAS en cada estrato; n=120 en total
# Asignando la muestra en forma proporcional a la cantidad de colegios 
# MAS en cada estrato 
# En la segunda etapa,  m=5 alumnos en cada colegio seleccionado
# (o todos los alumnos si hay menos alumnos en el colegios).

# Lectura de la tabla de datos
tabla_alumnos <- read.csv2("D:/Boulots/aprender/2019/Bases usuarias Aprender 2019 CSV/Base estudiantes 5-6 anio secundaria 2019 USUARIA (Censo L y M).csv")
tabla_alumnos$Colegio <- tabla_alumnos$ID1 


# Calculo el total de colegios por estrato (fpc1) 
# y la cantidad de alumnos de cada colegio (fpc2)
# Lo agrego a la tabla de alumnos
tabla_alumnos <- tabla_alumnos %>%
                 group_by(Colegio) %>%
                 mutate(fpc2=n(), x=ifelse(row_number()==1,1,0),
                        Id_colegio=cur_group_id()) %>%
                 group_by(sector) %>% 
                 mutate(fpc1=sum(x)) 



# TEM y TEL a numerico
class(tabla_alumnos$TEM)
class(tabla_alumnos$TEL)

tabla_alumnos$TEM <- as.numeric(tabla_alumnos$TEM)
tabla_alumnos$TEL <- as.numeric(tabla_alumnos$TEL)

# Genero n y m, con los tamanos de muestra en cada etapa
n <- 120
m <- 5



# Seleccionamos ahora la muestra de colegios con sampling

tabla_colegios <- tabla_alumnos %>%
  group_by(sector, Colegio) %>%
  summarise(Mi=n())  


tabla_estratos <- tabla_colegios %>%
  group_by(sector) %>%
  summarise(Nh=n()) %>%
  mutate(nh=round(n*Nh/nrow(tabla_colegios)))


sum(tabla_estratos$nh)


# Selecciono muestra colegios con sampling

# Ordeno colegios

tabla_estratos <- tabla_estratos[order(tabla_estratos$sector),]
tabla_colegios <- tabla_colegios[order(tabla_colegios$sector),]

smplColegios <- sampling::strata(data=tabla_colegios, stratanames = "sector",
                                 size=tabla_estratos$nh,
                                 method="srswor")

muestra_colegios <- getdata(tabla_colegios, smplColegios)        
muestra_colegios$F1 <- 1/muestra_colegios$Prob


# Selecciono muestra de alumnos
# Creo marco alumnos
tabla_alumnos$Nh <- NULL
tabla_alumnos$Mi <- NULL
tabla_alumnos$F1 <- NULL
marco_alumnos <- merge(tabla_alumnos, muestra_colegios[, c("Colegio", "F1")],
                       by="Colegio")



# Ordeno por estrato
# Cada colegio es ahora un estrato de seleccion
marco_alumnos <- marco_alumnos[order(marco_alumnos$Colegio), ]


tabla_estratos2 <- marco_alumnos %>%
  group_by(Colegio) %>%
  summarise(nh=m, fpc2=mean(fpc2)) %>%
  mutate(nh=ifelse(nh>fpc2, fpc2, nh)) 


# Ordeno marco y tabla estratos
tabla_estratos2 <- tabla_estratos2[order(tabla_estratos2$Colegio), ]
marco_alumnos <- marco_alumnos[order(marco_alumnos$Colegio), ]

# Selecciono muestra alumnos
smplAlumnos <- sampling::strata(data=marco_alumnos, stratanames = "Colegio",
                                size=tabla_estratos2$nh,
                                method="srswor")

muestra_alumnos <- getdata(marco_alumnos, smplAlumnos)        
muestra_alumnos$F2 <- 1/muestra_alumnos$Prob

muestra_alumnos$pondera <- muestra_alumnos$F1 * muestra_alumnos$F2    


# Declaro diseno

# Necesito un  identificador de alumno
muestra_alumnos$id_alumno <- 1:nrow(muestra_alumnos)
muestra_alumnos$ap11_02 <- factor(muestra_alumnos$ap11_02) 
diseno <- svydesign(id=~Colegio+id_alumno, strata=~sector, weights = ~pondera,
                    fpc=~fpc1+fpc2, data=muestra_alumnos)


estimo_TEM <- svytotal(~TEM, diseno, na.rm=TRUE, cv=TRUE, deff=TRUE)

estimo_TEM

cv(estimo_TEM)

deff_TEM <- deff(estimo_TEM)



# Ahora media de alumnos en hogar según tenencia de auto
estimo_auto <- svymean(~ap11_02, diseno, na.rm=TRUE,cv=TRUE, deff=TRUE)

estimo_auto
cv(estimo_auto)






# Un ejemplo de diseno muestral mas realista:
# Muestra bietapica de alumnos
# Unidades de primera etapa: colegios. 120 colegios.
# Unidades de segunda etapa: alumnos. 5 alumnos por colegio

# En la primera etapa estratificacion por sector de gestion,  
# con asignacion proporcional a la matricula.

# En cada estrato seleccionamos mediante Madow la muestra de colegios con
# probabilidad de seleccion proporcional a la cantidad de colegios.

# Luego, en cada colegio seleccionado subseleccionamos una MAS de 5 alumnos 
# (o todos los alumnos).

# Ordenamos el marco de primera etapa por provincia, dentro de cada estrato 





# Muestra de colegios
# (agrego cod_provincia pues me piden ordenar tambin por provincia)
n=120
m=5

tabla_colegios <- tabla_alumnos %>%
  group_by(sector, cod_provincia, Colegio) %>%
  summarise(Mi=n())  


tabla_estratos <- tabla_colegios %>%
  group_by(sector) %>%
  summarise(Nh=n() ,  Mh=sum(Mi)) %>%
  mutate(nh=round(n*Nh/nrow(tabla_colegios)))


sum(tabla_estratos$nh)                  


# Paso nh y Mh a tabla colegios
tabla_colegios$nh <- NULL
tabla_colegios$Mh <- NULL

tabla_colegios <- merge(tabla_colegios, tabla_estratos[, c("sector", "nh", "Mh")],
                        by="sector")

# Genero probabilidad de seleccion en tabla colegios
tabla_colegios$pik <- tabla_colegios$nh * tabla_colegios$Mi / tabla_colegios$Mh 

sum(tabla_colegios$pik)


# Selecciono muestra colegios
# Ordeno colegios y estratos

tabla_estratos <- tabla_estratos[order(tabla_estratos$sector),]
tabla_colegios <- tabla_colegios[order(tabla_colegios$sector, tabla_colegios$cod_provincia),]

smplColegios <- sampling::strata(data=tabla_colegios, stratanames = "sector",
                                 size=tabla_estratos$nh,
                                 pik=tabla_colegios$pik,
                                 method="systematic")

muestra_colegios <- getdata(tabla_colegios, smplColegios)        
muestra_colegios$F1 <- 1/muestra_colegios$Prob



# Creo marco alumnos

marco_alumnos <- merge(tabla_alumnos, muestra_colegios[, c("Colegio", "Mi", "F1")],
                       by="Colegio")

# Ordeno por estrato
marco_alumnos <- marco_alumnos[order(marco_alumnos$Colegio), ]

tabla_estratos2 <- marco_alumnos %>%
  group_by(Colegio) %>%
  summarise(nh=m, Mi=mean(Mi))

tabla_estratos2$nh <- ifelse(tabla_estratos2$nh <= tabla_estratos2$Mi,
                             tabla_estratos2$nh, tabla_estratos2$Mi) 



# Selecciono muestra alumnos
smplAlumnos <- sampling::strata(data=marco_alumnos, stratanames = "Colegio",
                                size=tabla_estratos2$nh,
                                method="srswor")

muestra_alumnos <- getdata(marco_alumnos, smplAlumnos)        
muestra_alumnos$F2 <- 1/muestra_alumnos$Prob

muestra_alumnos$pondera <- muestra_alumnos$F1 * muestra_alumnos$F2    


# Declaro diseno
# Ahora va solo identificador de primer etapa
# y no va fpc ahora 
diseno <- svydesign(id=~Colegio, strata=~sector, weights = ~pondera,
                    data=muestra_alumnos)


estimo_TEM <- svytotal(~TEM, diseno, na.rm=TRUE, cv=TRUE, deff=TRUE)
estimo_TEM
cv_TEM <- 100*cv(estimo_TEM)
deff_TEM <- deff(estimo_TEM)



