# TTM 2024 Estratificacion

# Directorio de Trabajo
setwd("D:/Boulots/universi/maestria_generacion_info/teoria_tecnicas_muestreo_2024")

# Borramos la memoria
rm(list = ls())

# Paquetes que necesitaremos
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(sampling)
library(survey)




############################################################################

# Se desea estimar el total de hogares y proporcion de hogares 
# que habitan en Hotel-pension
# mediante una muestra estratificada 
# de radios censales de tamano total n=200, estratificando por 
# Zona: CABA - Resto, MAS en cada estrato.

# Se ensayaran tres asignaciones: 

# Uniforme, proporcional y optima

# Seleccionar una muestra con cada estrategia y estimar mediante ella 
# el parametro de interes,  IC(95%) , deff y CV del estimador


# Lectura de las tablas
radios_sexo <- read_excel("cen2010_radios_sexo.xlsx")
radios_bienes <- read_excel("cen2010_radios_bienes.xlsx")
radios_tipo <- read_excel("cen2010_radios_tipo.xlsx")

radios_2010 = merge(radios_sexo, radios_bienes, by = "Codigo")
radios_2010 = merge(radios_2010, radios_tipo, by = "Codigo")


# Generamos nuevas variables en el dataframe que utilizaremos mas adelante
# Poblacion total en cada radio

radios_2010$Pob_radio <- radios_2010$Varon + radios_2010$Mujer 

radios_2010$Hogares <- radios_2010$HeladeraSi + radios_2010$HeladeraNo 

# Cantidad de viviendas en cada radio
radios_2010$Viviendas <-  radios_2010$Casa + 
                          radios_2010$Rancho + 
                          radios_2010$Casilla + 
                          radios_2010$Departamento + 
                          radios_2010$Inquilinato + 
                          radios_2010$Hotel_pension


# Extraemos el codigo de provincia
radios_2010$prov <- floor(radios_2010$Codigo/10000000)

# Eliminamos los radios sin viviendas
radios_2010 <- radios_2010[radios_2010$Viviendas>0,]



# Parametro poblacional
TotalHotel <- sum(radios_2010$Hotel_pension)



# Tamano de la muestra
n <- 200
N <- nrow(radios_2010)

# Estratifico

radios_2010 <- radios_2010 %>%
               mutate(Zona = case_when(prov==2 ~ "CABA", 
                                            TRUE ~ "Resto") )

table(radios_2010$Zona, useNA = "always")                           

                           
# Cantidad de estratos
H=2


# Distribuyo la muestra por Zona
# con asignacion uniforme, proporciona y optima

# Agrego Xh, lo necesitare luego
tabla_estratos <- radios_2010 %>%
  group_by(Zona) %>%
  summarise(Nh=n(), Xh=sum(Viviendas), 
            Wh=Nh/N,
            Sh=sqrt(var(Hotel_pension))) %>%
  mutate(NhSh=Nh*Sh, 
         n_opt=round(n*NhSh/sum(NhSh)),
         n_unif=round(n/H), n_prop= round(n*Nh/sum(Nh))) 



# Control
sum(tabla_estratos$n_unif)
sum(tabla_estratos$n_prop)
sum(tabla_estratos$n_opt)



# Seleccion con sampling

# Agrego fpc pues selecciono MAS en cada estrato
radios_2010 <-  radios_2010 %>%
  group_by(Zona) %>%
  mutate(fpc=n())


#Ordeno el marco de muestreo por estrato (lo pide sampling)
radios_2010 <-  radios_2010[order(radios_2010$Zona),]


smplCasos = sampling::strata(radios_2010, stratanames = c("Zona") , 
                             size=tabla_estratos$n_prop , description=TRUE, 
                             method = "srswor")

# Recupero datos del marco de muestreo
muestra_casos <- sampling :: getdata(radios_2010,smplCasos)


# Calculo el factor de expansion utilizando probabilidad de seleccion calculada 
muestra_casos$pondera <- 1/muestra_casos$Prob



# Le indico a survey el diseno de muestra
# Indico fpc por estrato
diseno <- survey :: svydesign(id=~1, strata = ~Zona, 
                              weights = ~pondera,  data=muestra_casos,
                              fpc = ~fpc)
diseno


EstimTotalHotel <- survey :: svytotal(~Hotel_pension, diseno, deff=TRUE, cv=TRUE )
EstimTotalHotel
cv(EstimTotalHotel)
deff(EstimTotalHotel)
confint(EstimTotalHotel)


# Ahora la estimacion de la proporcion de hogares
# en hotel pension
Proporcion <- survey :: svyratio(~Hotel_pension , ~Hogares, diseno, deff=TRUE )
Proporcion
df_ratio <- data.frame(Proporcion[[1]])
df_SE    <- data.frame( sqrt(Proporcion[[2]]))
CV       <- survey::cv(Proporcion)


df_proporcion <- cbind(df_ratio, df_SE)

df_proporcion$CV  <- 100*CV[1,1] 
df_proporcion$deff  <- survey::deff(Proporcion) 
colnames(df_proporcion) <- c("Ratio", "SE", "CV", "deff")





#####################################################################
# Mejora el disenio si seleccionamos los radios con probabilidad
# proporcional a la cantidad de viviendas? 

# Seleccion pps sistematica en cada estrato con prob de seleccion
# proporcional a la cantidad de viviendas
# ordenando por codigo de radio



# Debo calcular la probabilidad de seleccion de cada radio
# le pego a la base de radios el valor nh
radios_2010$Nh <- NULL
marco_muestreo <- merge(radios_2010, tabla_estratos[, c("Zona", "n_prop", "Xh")], 
                        by="Zona")


# Genero probabilidad de seleccion de cada radio
marco_muestreo$pi_i <-  marco_muestreo$n_prop*marco_muestreo$Viviendas/marco_muestreo$Xh


# Controlo
sum(marco_muestreo$pi_i)


# Seleccionamos la muestra con sampling

# strata {
#     data        -> data.frame con los datos de la poblacion (ordenado por las variables estratificadoras)
#     stratanames -> vector con los nombres de las variables estratificadoras 
#     size        -> vector con los tamaños de muestra de cada estrato 
#     description -> Si es TRUE mostrara un mensaje con las unidades de seleccionadas
#     method      -> Metodo de seleccion dentro de cada estrato. Los valores posibles son: srswor, 
#                    srswr, poisson, systematic,
#     pik         -> prob de seleccion           
#   }


# Ordenamos el marco de muestreo por la variable estratificadora
# y dentro de cada estrato por Codigo

radios_2010 <- radios_2010[order(radios_2010$Zona, radios_2010$Codigo),]


# Selecciono la muestra
smplRadios = sampling::strata(marco_muestreo, stratanames = c("Zona") , 
                          size=tabla_estratos$n_prop , description=TRUE, 
                      method = "systematic", pik=marco_muestreo$pi_i)

# Recupero datos del marco de muestreo
muestra_radios <- sampling :: getdata(marco_muestreo,smplRadios)


# Calculo el factor de expansion utilizando probabilidad de seleccion calculada 
muestra_radios$pondera <- 1/muestra_radios$Prob



# Le indico a survey el diseno de muestra
# pero supongo muestreo con reposicion (omito fpc)
diseno <- survey :: svydesign(id=~1, strata=~Zona, 
                              weights=~pondera,  data=muestra_radios)
diseno


Totales <- survey :: svytotal(~Hotel_pension, diseno, deff=TRUE, cv=TRUE )
Totales
CV <- survey::cv(Totales)
deff <- deff(Totales)


# Paso a data frame
df_total_madow <- data.frame(Totales)
df_total_madow$CV_Madow <- 100*CV[1,1]
df_total_madow$deff <- deff



# Estimacion del total con asignacion optima y MAS en cada estrato

# Seleccion con sampling

# Agrego fpc pues selecciono MAS en cada estrato (ya estaba?)
radios_2010 <-  radios_2010 %>%
  group_by(Zona) %>%
  mutate(fpc=n())


#Ordeno el marco de muestreo por estrato (lo pide sampling)
radios_2010 <-  radios_2010[order(radios_2010$Zona),]


smplCasos = sampling::strata(radios_2010, stratanames = c("Zona") , 
                             size=tabla_estratos$n_opt , description=TRUE, 
                             method = "srswor")

# Recupero datos del marco de muestreo
muestra_casos_opt <- sampling :: getdata(radios_2010,smplCasos)


# Calculo el factor de expansion utilizando probabilidad de seleccion calculada 
muestra_casos_opt$pondera <- 1/muestra_casos_opt$Prob



# Le indico a survey el diseno de muestra
# Indico fpc por estrato
diseno <- survey :: svydesign(id=~1, strata = ~Zona, 
                              weights = ~pondera,  data=muestra_casos_opt,
                              fpc = ~fpc)
diseno


EstimTotalHotel_opt <- survey :: svytotal(~Hotel_pension, diseno, deff=TRUE, cv=TRUE )
EstimTotalHotel_opt
cv(EstimTotalHotel_opt)
deff(EstimTotalHotel_opt)



####################################################################
# Ejemplo con Aprender 2019


# Universo bajo estudio: conjunto de alumnos último año secundario en 
# 2019. Supondremos que coincide con el conjunto de alumnos evaluados 
# en Aprender 2019


# Deseamos estimar, con una muestra de 200 alumnos:

# Puntaje promedio en matemática (TEM)
# Puntaje promedio en lengua (TEL)
# Proporcion de alumnos que declaran tener hijos (ap29)
# Distribución de los alumnos según tenencia de automóviles (ap11_02)

# Disenio de la muestra:
# Muestreo estratificado, estratificando por sector y sexo 
# Asignación de la muestra: proporcional
# MAS en cada estrato


# Seleccionar con sampling la muestra, 
# calculando para cada etimación cv, IC(95%) y deff



# Lectura de la tabla de datos
# (consultar disenio de registro...)


tabla_alumnos <- read.csv2("D:/Boulots/aprender/2019/Bases usuarias Aprender 2019 CSV/Base estudiantes 5-6 anio secundaria 2019 USUARIA (Censo L y M).csv")

sum(is.na(tabla_alumnos$TEM))

table(tabla_alumnos$ap02, useNA = "always")



# Dejo solo alumnos con sexo declarado (no imputamos en este ejercicio)
tabla_alumnos <- tabla_alumnos[tabla_alumnos$ap02>0,]  

table(tabla_alumnos$ap02, useNA = "always")
tabla_alumnos <- tabla_alumnos[ !is.na(tabla_alumnos$ap02),]  


table(tabla_alumnos$ap02, useNA = "always")


# Dejo solo alumnos con ambos puntajes calculaods en Matematica y Lengua
tabla_alumnos <- tabla_alumnos[ !is.na(tabla_alumnos$TEM) & !is.na(tabla_alumnos$TEL),]

sum(is.na(tabla_alumnos$TEM))
sum(is.na(tabla_alumnos$TEL))

tabla_alumnos$ap11_02 <- ifelse(tabla_alumnos$ap11_02 < 0, NA,
                                tabla_alumnos$ap11_02)

table(tabla_alumnos$ap11_02, useNA = "always")

n=200

tabla_alumnos <- tabla_alumnos[order(tabla_alumnos$sector,
                                   tabla_alumnos$ap02),]

tabla_estratos <- tabla_alumnos %>%
                  group_by(sector, ap02) %>%
                  summarise(Nh=n()) %>%
                  mutate(nh= round(n*Nh/nrow(tabla_alumnos)))

tabla_estratos$Estrato <- 1:nrow(tabla_estratos) 
sum(tabla_estratos$nh)

# Pego estrato a marco de muestreo
tabla_alumnos$Estrato <- NULL 
tabla_alumnos <- merge(tabla_alumnos, tabla_estratos[, c("sector", "ap02","Estrato")],
                                                    by=c("sector", "ap02"))


s <- sampling:: strata(data=tabla_alumnos, 
                       stratanames = c("sector", "ap02"),
                       size=tabla_estratos$nh,
                       method="srswor") 

muestra <- getdata(tabla_alumnos,s)
muestra$pondera <- 1/muestra$Prob 



# Estimaciones
diseno <- survey::svydesign(id=~1, weights = ~pondera, 
                            strata = ~as.factor(sector)+ as.factor(ap02),
                            data=muestra)
summary(diseno)


diseno <- survey::svydesign(id=~1, weights = ~pondera, 
                            strata = ~Estrato,
                            data=muestra)
summary(diseno)


# Media de TEM y TEL
medias_TEM_TEL <- svymean(~TEM+TEL,diseno, deff=TRUE)
medias_TEM_TEL

df_medias_TEM_TEL <- data.frame(medias_TEM_TEL)
df_medias_TEM_TEL$CV <- 100*cv(medias_TEM_TEL)
df_medias_TEM_TEL$deff <- deff(medias_TEM_TEL)
df_medias_TEM_TEL$IC <- confint(medias_TEM_TEL)



# Distribución de alumnos según tenencia de automóviles

table(tabla_alumnos$ap11_02, useNA = "always")


table(tabla_alumnos$ap11_02, useNA = "always")


diseno <- update(diseno, Fap11_02 = factor(ap11_02))

distrib_autos <- svymean(~Fap11_02,diseno, na.rm=TRUE, deff=TRUE)
distrib_autos 
100*cv(distrib_autos)
100*deff(distrib_autos)
100*confint(distrib_autos)




