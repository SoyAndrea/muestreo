#  TTM2024  pps

rm(list = ls())

library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(sampling)
library(survey)
library(VIM)
library(tidyr)


setwd("D:/Boulots/universi/maestria_generacion_info/teoria_tecnicas_muestreo_2024")



#######################################################################

# Universo bajo estudio: Total de radios censales 2010 
# Parametros a estimar: 
#     Total de poblacion
#     Total de hogares que habitan en rancho o casilla
#     Proporción de hogares que habitan en rancho o casilla 
# (definicion censal)


# Diseno de muestra: seleccion de radios con probabilidades proporcionales 
# a una medida de tamano. Mediante Madow
# Tamaño de la muestra: 400 radios censales
# Ordenando el archivo por codigo de radio.

# En nuestro caso la medida de tamano sera la cantidad de viviendas
# del radio

# Estimando el total mediante el estimador de Horvitz-Thompson 


n=400



# La seleccion la haremos ahora con sampling

# Con el comando UPsystematic(pik)

# donde pik es el vector con las probabilidades de seleccion


# Se pide: 

# 1. Seleccionar una muestra mediante sampling
# 2. Mediante survey, estimar los parametros poblacionales, varianza, 
#    ds, cv y deff de los dos estimadores a partir de la muestra de radios seleccionada, 
#    (suponiendo  que  se  trata  de  una  muestra  seleccionada  con  
#    reposicion)
#    Seleccionar 12 (o mas) muestras y estimar con ellas CV del estimador
#    Graficar mediante un box-plot la serie de estimaciones y la serie de CV


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

# Hogares rancho- casilla
radios_2010$Rancho_casilla <- radios_2010$Rancho + radios_2010$Casilla 

# Cantidad de viviendas en cada radio
radios_2010$Viviendas <-  radios_2010$Casa + 
  radios_2010$Rancho + 
  radios_2010$Casilla + 
  radios_2010$Departamento + 
  radios_2010$Inquilinato + 
  radios_2010$Hotel_pension


# Total de hogares
radios_2010$Hogares <- radios_2010$HeladeraSi + radios_2010$HeladeraNo 

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
sum(radios_2010$Pob_radio)

# N poblacional
N <- nrow(radios_2010)

# Tamano de la muestra
n <- 400



# Definimos el vector de probabilidades de seleccion
radios_2010$pi_i <- n*radios_2010$Viviendas/sum(radios_2010$Viviendas)
max(radios_2010$pi_i)

# Ordenamos el marco de muestreo por codigo de radio
radios_2010 <-  radios_2010[order(radios_2010$Codigo),]


# Controlo que no haya pi_i mayores que 1, nulos ni missing
max(radios_2010$pi_i) 
min(radios_2010$pi_i)
sum( !is.finite(radios_2010$pi_i))


# Seleccionamos la muestra y definimos el factor de expansion

# Probabilidades de seleccion
pik <- radios_2010$pi_i

s=  sampling::UPsystematic(pik)

muestra_radios = radios_2010[s==1,]
muestra_radios$pondera <- 1/muestra_radios$pi_i

# Con survey realizo las estimaciones 
# (total de poblacion, cv, etc)
# survey asumira que el muestreo es con reposicion
# al no haber fpc


parametro <- sum(radios_2010$Rancho_casilla)

diseno <- svydesign(id = ~1, weights = ~pondera, data=muestra_radios)
diseno


# survey asume ahora muestreo con reposicion

# Estimacion del total de poblacion
EstTotalPob <- svytotal( ~Pob_radio , diseno, deff=TRUE, cv=TRUE, ci=TRUE)
EstTotalPob

100*cv(EstTotalPob)
survey :: cv(EstTotal)

df_estim_total <- data.frame(EstTotal)
colnames(df_estim_total) <- c("Estimacion", "SE", "deff")

df_estim_total$cv <- 100*df_estim_total$SE/df_estim_total$Estimacion


# Estimacion del total de hogares que habitan rancho casilla
EstTotalRancho <- svytotal( ~Rancho_casilla , diseno, deff=TRUE, cv=TRUE, ci=TRUE)
EstTotalRancho

100*cv(EstTotalRancho)
survey :: cv(EstTotalRancho)

df_estim_total <- data.frame(EstTotal)
colnames(df_estim_total) <- c("Estimacion", "SE", "deff")

df_estim_total$cv <- 100*df_estim_total$SE/df_estim_total$Estimacion




# Estimacion de la proporcion de hogares que habitan rancho casilla
EstRatio <- svyratio(~Rancho_casilla, ~Hogares, 
                     diseno, deff=TRUE, cv=TRUE, ci=TRUE)
EstRatio


100*survey::cv(EstRatio)

survey :: cv(EstRatio)
deff <- deff(EstRatio)
deff

IC  <- confint(EstRatio)
IC


# repetir diez veces con un for

estimo <- function(x){
  s=  sampling::UPsystematic(pik)
  muestra_radios = radios_2010[s==1,]
  muestra_radios$pondera <- 1/muestra_radios$pi_i
  EstRatio <- svyratio(~Rancho_casilla, ~Hogares, 
                       diseno, deff=TRUE, cv=TRUE, ci=TRUE)
  deff <- deff(EstRatio)
  cv   <- 100*survey::cv(EstRatio)
  
  caja <- c(EstRatio, deff, cv)
  return(caja)
}

lista_aa <- 1:10

lista_estimaciones <- lapply(lista_aa, estimo)

df_estimaciones <- data.frame(matrix(unlist(lista_estimaciones), 
                                     nrow=10, byrow=T))












df_estim_ratio <- data.frame(Estimacion = EstRatio[[1]],SE= sqrt(EstRatio[[2]])) 
df_estim_ratio$cv <- 100*df_estim_total$SE/df_estim_total$Estimacion
df_estim_ratio$deff <- deff 
colnames(df_estim_ratio) <- c("Estimacion", "SE", "cv", "deff")


###############################################################
# Seleccion de una muestra con reemplazo
# con prob de extraccion proporcional a la cantidad
# de viviendas


# Primero un ejemplo con un data frame reducido
Colegio <- c(  'a', 'b', 'c', 'd', 'e','f')
X       <- c( 20,30,40,50,60,200)
Y       <- c( 3, 2, 5, 6, 8, 12)
df <- data.frame(Colegio,X,Y)

N = nrow(df)

parametro <- sum(Y)

n = 3


# Probabilidades de extraccion
df$P <- df$X/sum(df$X)
sum(df$P)


# Con la funcion de R
Identificador <- sample(N,n,replace=TRUE, prob=df$P)
df_s <- data.frame(Identificador)

df$Identificador <- 1:N

muestra_con_reemplazo <- merge(df,df_s, by=c("Identificador"))






#############################################################
# Ahora una simulacion, para ver la forma de tres estategias para estimar
# la poblacion total:
# Muestreo aleatorio simple, Sitematico (ordenando el archivo por Codigo), 
# Madow (seleccion con probabilid proporcional
# a la cantidad de viviendas del radio, ordenando el archivo por Codigo) 

# Los tres con tamano medio similar

# Ordenamos el marco de muestreo por codigo de radio
radios_2010 <-  radios_2010[order(radios_2010$Codigo),]


parametro <- sum(radios_2010$Pob_radio)

# Estimacion a partir de una MAS
# La variable independiente de esta funcion, n, es el tamano de muestra


estimo_mas <- function(n){
  muestra <- radios_2010[sample(nrow(radios_2010),n),]
  estim <- N*mean(muestra$Pob_radio)  
  a <- estim
  return(a)
  
}


# Estimacion a partir de una muestra sistematica
# La variable independiente de esta funcion, x, es el arranque aleatorio
I = round(N/n)
estimo_sistematico <- function(aa) {
  s=radios_2010[ seq(aa,N,I), ]
  estim  <- I*sum(s$Pob_radio)
  
  return(estim)
  
}

# Madow x tamano de muestra
estimo_madow <- function(n){
  radios_2010$pi_i <- n*radios_2010$Viviendas/sum(radios_2010$Viviendas)
  pik <- radios_2010$pi_i
  s = UPsystematic(pik,eps=1e-6)
  muestra_radios = radios_2010[s==1,]
  muestra_radios$pondera <- 1/muestra_radios$pi_i
  estimacion <- sum(muestra_radios$Pob_radio/muestra_radios$pi_i)
  return(estimacion)
}





# Con lapply aplico la funcion selec_mas a cada elemento de la lista. 
# Por sencillez realizamos 1000 estimaciones para cada n, 
# deberiamos realizar mayor cantidad de estimaciones para un resultado mas preciso.


#MAS
lista <- rep(n,1000)
lista_estim <- lapply(lista, estimo_mas)

df_mas <- data.frame(matrix(unlist(lista_estim), nrow=length(lista_estim), byrow=TRUE))
df_mas$diseno <- "MAS"
colnames(df_mas) <- c("Estimacion", "diseno")


#Sistematico
I <- round(N/n)
# Genero ahora 1000 arranques aleatorios
lista <- sample(1:I, 1000, replace=T)

lista_estim <- lapply(lista, estimo_sistematico)

df_sistematico <- data.frame(matrix(unlist(lista_estim), nrow=length(lista_estim), byrow=TRUE))
df_sistematico$diseno <- "Sistematico"
colnames(df_sistematico) <- c("Estimacion", "diseno")


#Madow
lista <- rep(n,1000)
lista_estim <- lapply(lista, estimo_madow)

df_madow <- data.frame(matrix(unlist(lista_estim), nrow=length(lista_estim), byrow=TRUE))
df_madow$diseno <- "Madow"
colnames(df_madow) <- c("Estimacion", "diseno")




# Junto los tres data frame
df_todos <- rbind(df_mas, df_sistematico)
df_todos <- rbind(df_todos, df_madow)


# Graficamos ahora las densidades de las cuatro series de estimaciones
p <- ggplot(df_todos, aes(x=Estimacion, group=diseno, color=diseno)) + 
  geom_density(alpha=0.3) + 
  geom_vline(xintercept = sum(radios_2010$Pob_radio))  +
  facet_grid(rows=vars(diseno))

p


# Varianza, DS, CV, sesgo y EMC segun metodo

parametro <- sum(radios_2010$Pob_radio)
df_resumen <- df_todos %>%
              group_by(diseno) %>%
              summarise(Varianza=var(Estimacion), 
                        Esperanza=mean(Estimacion) ) %>%
              mutate(DS=sqrt(Varianza), 
                     CV=100*DS/Esperanza, sesgo = parametro - Esperanza) %>%
              mutate(EMC = Varianza + sesgo^2)



###########################################


# Ejercicio del TP I
# Hallar todas las muestras posibles y evaluar tres estimadores de la media 
# poblacional de Y: media, mediana y media truncada

df_tabla <- read_excel("tabla_muestras_posibles.xlsx")
df_muestras <- data.frame(matrix(unlist(combn(df_tabla$Y,10, simplify = FALSE)),
                                 ncol=10, byrow=TRUE))

# Para agregar a cada muestra la media muestral y el correspondiente histograma podemos hacer
# Calculo la media de cada muestra
media <- apply(df_muestras[,1:10],1,mean)
df_muestras$media <- media


# Histograma de la media
p <- ggplot(df_muestras, aes(x=media)) +
  geom_histogram(bins=30, color="black", fill="white")
p <- p+ geom_vline(aes(xintercept=mean(media)),
                   color="blue", linetype="dashed", size=1)
p



# La mediana podemos calcularla así
# Mediana
mediana <- apply(df_muestras[,1:10],1,median)
df_muestras$mediana <- mediana


# Y la media truncada así
# Media truncada
media_truncada <- function(x){
  mean(x, trim=0.1)
}
media_truncada <- apply(df_muestras[,1:10],1,media_truncada)
df_muestras$media_truncada <- media_truncada

parametro <- mean(df_tabla$Y)

# Histograma de la media
p <- ggplot(df_muestras, aes(x=media_truncada)) +
  geom_histogram(bins=30, color="black", fill="white")
p <- p+ geom_vline(aes(xintercept=parametro),
                   color="blue", linetype="dashed", size=1)
p


# Hallo EMC de cada estimador

df2 <- df_muestras[, c("media", "mediana", "media_truncada")]

df3 <- pivot_longer(df2, cols=c("media", "mediana", "media_truncada"),
                    names_to="Metodo")

parametro = mean(df_tabla$Y)
df_resumen2 <- df3 %>%
               group_by(Metodo) %>%
               summarise(Varianza=var(value), Esperanza=mean(value)) %>%
               mutate(sesgo=parametro-Esperanza, EMC=Varianza + sesgo^2) 
