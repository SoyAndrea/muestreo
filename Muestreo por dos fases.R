library(tidyverse)
library(sampling)
library(survey)
tabla_alumnos <- read.csv2("C:/Users/Pablo/Desktop/Pablo/MGAIE/Teoria y técnicas de muestreo/Bases/Bases usuarias Aprender 2019 CSV/Base estudiantes 5-6 año secundaria 2019 USUARIA (Censo L y M).csv")

n= 120

### En primer etapa, estratificar por sector de gestion
### Unidad de la primer etapa es colegio, seleccionado por un MAS de tamano total 120

### En la segunda etapa, unidad de muestreo alumno
### Unidad en la segunda etapa, lo alumnos

tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$TEM),]
tabla_alumnos <- tabla_alumnos[!is.na(tabla_alumnos$TEL),]

## Armo tabla de colegios, agrupados por gestion

tabla_colegio <- tabla_alumnos %>% 
  group_by(sector, ID1) %>% 
  summarise(Mi = n())

### Estratifico los colegios

tabla_estrato <- tabla_colegio %>% 
                  group_by(sector) %>% 
                  summarise(Nh = n()) %>% 
                  mutate(nh = n*Nh/ floor(nrow(tabla_colegio)))

# Ordeno el df por sector

tabla_colegios_ordenado <- tabla_colegio[order(tabla_colegio$sector),]

s <- sampling :: strata(stratanames = "sector",
                        method = "srswor",
                        size = tabla_estrato$nh,
                        data = tabla_colegio)



## Recupero datos del marco de muestreo

muestra_colegios <- getdata(tabla_colegio, s)


## cuando sampling genera la muestra, calcula la probabilidad de selecicon. Y necesito calcular
## el factor de expansion 

muestra_colegios$FactExp1 <- 1/muestra_colegios$Prob


#### Ahora tengo que seleccionar los alumnos desde mi muestra de colegios

### Selecciono la muestra de segunda etapa

# Armo nueva tabla estrato

tabla_estrato1 <- muestra_colegios %>% 
                  select(ID1, Mi) %>% 
                  mutate(nh = ifelse(Mi >= 5, 5, Mi))

tabla_estrato1 <- tabla_estrato1[order(tabla_estrato1$ID1),]
# Armo nuevo marco de muestreo

marco_muestreo <- merge(tabla_alumnos, muestra_colegios[,c("ID1","FactExp1")],
                        by = "ID1")


## Genero la muestra por samlping

marco_muestro_ordenado <- marco_muestreo[order(marco_muestreo$ID1),]

s1 <- sampling :: strata(stratanames = "ID1",
                        method = "srswor",
                        size = tabla_estrato1$nh,
                        data = marco_muestro_ordenado)


## Calculo el factor de expansion y con survey armo el diseno para estimar el total
## de tel y tem

muestra_alumnos <- getdata(marco_muestro_ordenado, s1)

muestra_alumnos$FactExp2 <- 1/muestra_alumnos$Prob

FactExpF = muestra_alumnos$FactExp2 * muestra_colegios$FactExp1

# Le declaro el disno a Survey
# Como hay un MAS en las dos etapas, declaro el diseno de las dos 



muestra_alumnos <- merge(muestra_alumnos,
                               tabla_estrato[,c("sector","Nh")],
                                by = "sector")

muestra_final_alumnos <- merge(muestra_alumnos,
                               tabla_estrato1[,c("ID1","Mi")],
                               by = "ID1")

disenoEtapa1 <- svydesign(id = ~ ID1 + idalumno,
                          strata = ~sector,
                          weights = FactExpF,
                          data = muestra_final_alumnos,
                          fcp = ~ Nh + Mi)

estim_media_TEM <- svymean(~TEM, disenoEtapa1,
                           deff = TRUE,
                           cv = TRUE,
                           ci = TRUE)
CV = cv(estim_media_TEM)*100
Deff = deff(estim_media_TEM)
Ci = confint(estim_media_TEM)


class(muestra_final_alumnos$TEM)

