# TTM 2024 Muestreo Aleatorio Simple


# Universo bajo estudio: Población 2010
# Marco de muestreo: Listado de radios censales 2010 (REDATAM)


# A)
# Parametros a estimar: 
# Total de población


# Mediante una MAS de radios censales
# de tamano n=200, estimando:
# mediante el estimador N*y_media


# 1. Hallar parametro poblacional
# 2. Calcular la varianza y CV del estimador
#    Recordar: CV = Desv.St(estimador)/Parametro a estimar
# 3. Seleccionar una muestra
# 4. Mediante survey, estimar el parametro poblacional, varianza, ds y 
#    cv del estimador a  partir de la muestra de radios seleccionada
# 5. Mediante una simulacion graficar la densidad del estimador 



# Directorio de Trabajo
# setwd("D:/Boulots/universi/maestria_generacion_info/teoria_tecnicas_muestreo_2024")

# Liberamos la memoria
rm(list = ls())

# Paquetes que necesitaremos
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(sampling)
library(survey)

options(scipen = 999)

# Lectura de las tablas
radios_sexo <- read_excel("cen2010_radios_sexo.xlsx")
radios_bienes <- read_excel("cen2010_radios_bienes.xlsx")
radios_tipo <- read_excel("cen2010_radios_tipo.xlsx")

# Juntamos los archivos para unificarlos en un unico dataframe
radios_2010 = merge(radios_sexo, radios_bienes, by = "Codigo")
radios_2010 = merge(radios_2010, radios_tipo, by = "Codigo")

# Generamos nuevas variables en el dataframe que utilizaremos mas adelante

# Poblacion total en cada radio
radios_2010$Pob_radio <- radios_2010$Varon + radios_2010$Mujer 

# Cantidad de viviendas en cada radio
radios_2010$Viv_radio <-  radios_2010$Casa + 
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


# Eliminamos los radios sin viviendas
radios_2010 <- radios_2010[radios_2010$Viv_radio>0,]



# N poblacional (total de unidades en el marco de muestreo)
N <- nrow(radios_2010)


# Parametro poblacional
TotalPob <- sum(radios_2010$Pob_radio)

# Tamano de la muestra
n <- 200

# hallo intervalo de seleccion

I <- floor(N/n)

# Calculamos la varianza para la estimacion del total:
# En el MAS 
# Var(y_media) = (1-n/N)*S^2/n
# Var(N*y_media) = N^2*(1-n/N)*S^2/n


S2 <- var(radios_2010$Pob_radio)
VarMAS_Media <- (1-n/N)*S2/n
VarMAS_Total <- N^2*VarMAS_Media   #Recordar: Var(k*X)= k^2*Var(X)
VarMAS_Total


# Calculamos el coeficiente de variacion
CVMAS_Total <- 100*sqrt(VarMAS_Total)/TotalPob

CVMAS_Total



# Intermedio:
# Seleccionar algunas filas de un data frame

df <- data.frame(X=c(7,2,11), Y=c(5, 1, 8))
df

s=c(2,3)
df_muestra <- df[s,]
nrow(df_muestra)

s=c(0, 1, 0)
df_muestra <- df[s,]
df_muestra

s=c(FALSE,TRUE, FALSE)
df_muestra <- df[s,]
df_muestra

s=c(0, 1, 0)
df_muestra <- df[s==1,]
df_muestra

df
df_muestra <- df[1:3 %% 3 ==0,]
df_muestra


# Seleccionamos ahora una muestra aleatoria simple con R
s_mas <- sample(N,n, replace=FALSE)

muestra_radios <- radios_2010[s_mas,]


# Agregamos al data frame el factor de expansion
# (recordar que seleccione una muestra aleatoria simple de radios)
muestra_radios$pondera <- N/n


# Cantidad total de unidades en el marco de muestreo
# lo necesitare luego para survey
muestra_radios$fpc <- N  


# Vamos a realizar las estimaciones con las funciones de survey
# El primer paso al trabajar con el paquete survey es definir el diseno de la 
# muestra, esto lo hacemos con la funcion svydesign 

# svydesign(
# ids,             -> identificacion de los conglomerados (~0 o ~1 si no hay)
# strata = NULL,   -> identificacion de los estratos 
# fpc = NULL,      -> Input para factor de correccion por poblacion finita: 
# solo es necesario indicar N o n/N (Nk o nk/Nk si estr.)
# weights = NULL,  -> peso de cada elemento
# data = NULL      -> datos de la muestra
# ...
# )

# El objeto 'diseno' contiene toda la informacion que sera empleada 
# para realizar las estimaciones.


diseno <- svydesign(id= ~1,weights=~pondera, data=muestra_radios, fpc=~fpc)
diseno
# (como es una muestra aleatoria simple ponemos fpc)

# Por ejemplo, si queremos extraer los pesos de un diseno podemos utilizar
pesos <- weights(diseno)


# Estimar un total poblacional
# svytotal(
#   x,             -> variable de la muestra sobre la que se va a calcular
#   design,        -> diseno de la muestra 
#   deff,          -> calcular el Deff (TRUE/FALSE)
#   ...
# )

# recordar que deff= Var(estim)/Var_MAS(estim*), MAS de tamano similar
EstTotalPob <- survey :: svytotal(~Pob_radio, diseno, deff=TRUE, cv=TRUE, ci=TRUE)
EstTotalPob

# Puedo ahora extraer diferentes valores:
survey :: cv(EstTotalPob)      # -> coeficiente de variacion
deff(EstTotalPob)    # -> efecto de diseno
SE(EstTotalPob)      # -> desvio estandar
confint(EstTotalPob) # -> intervalor de confianza (por defecto 95%)
cv(EstTotalPob)

# O pasar los resultados a un data frame
df_EstTotalPob <- as.data.frame(EstTotalPob)

# Quiero cambiar el nombre de las columnas
colnames(df_EstTotalPob) <- c("Estimacion", "SE", "deff")

# Calculemos ahora el intervalo de confianza con un 90% de confianza
# (suponemos que el estimador es aprox normal)
df_EstTotalPob$Li <- df_EstTotalPob$Estimacion-1.64*df_EstTotalPob$SE  
df_EstTotalPob$Ls <- df_EstTotalPob$Estimacion+1.64*df_EstTotalPob$SE  


# Ahora calculo el CV del estimador
df_EstTotalPob$CV <-  100*df_EstTotalPob$SE/df_EstTotalPob$Estimacion



# Que forma tiene el estimador?
# Creo una funcion que seleccione una muestra de tamanio x
# y estime total de poblacion
estimo_pob_mas <- function(x){
  muestra <- radios_2010[sample(nrow(radios_2010),x, replace=FALSE),]
  estim <- N*mean(muestra$Pob_radio)  
  a <- c(estim,x)
  return(a)
}


lista <- rep(200,2000)
lista_estim <- lapply(lista, estimo_pob_mas)

df_estim100 <- data.frame(matrix(unlist(lista_estim), 
                                 nrow=length(lista_estim), byrow=TRUE))

colnames(df_estim100) <- c("Estimacion", "n")


# Grafico la distribucion de las estimaciones
p <-ggplot(df_estim100, aes(x=Estimacion)) + 
  geom_histogram(color="black", fill="white")
p



