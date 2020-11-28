# Instituto Tecnológico de Santo Domingo, INTEC
# Diplomado: BigData para Negocios
# Profesor: José M. Aquino
# Author: Edwin Collado
# fuente de los datos https://www.kaggle.com/mczielinski/bitcoin-historical-data/download
# tambien esta disponible en mi repositorio github https://github.com/ecollado1/bigdata/

# Introducción a la ciencia de datos: Análisis de Precios de BTC

BTC <- read.csv(file="bitstampUSD_1-min_data_2012-01-01_to_2020-09-14.csv", head=TRUE, sep = ",") 
    ##lee un archivo csv
    ## file nombre del archivo
    ## head con encabezado
    ## sep separado por comas

View(BTC)

nrow(BTC) ## cantidad de filas que contiene el archivo
ncol(BTC) ## cantidad de columnas que contiene el archivo

sapply(BTC, class) ## donde mi data.frame es el nombre que esta utilizando
lapply(BTC, class) ## igual que sapply
str(BTC) ## mestra de forma compacta la estructura de mi objeto BTC

## Funcion que grafica en barra la cantidad de tipo de datos que tiene la estructura de mi objeto
## BTC
tipo_de_datos <- function(mi_data) {
  tipo <- lapply(mi_data, class)
  tipo_mi_data <- data.frame(unlist(tipo))
  barplot(table(tipo_mi_data), main="Tipo de Datos", col="steelblue", ylab="Cantidad")
}

tipo_de_datos(BTC) ## llamo la funcion y pase mi objeto BTC como parametro y me grafica...

View(BTC)

colnames(BTC) ## visualizando los nombres de las columnas

names(BTC)[names(BTC) == "Low"] <- "Baja"
names(BTC)[names(BTC) == "High"] <- "Alta"
names(BTC)[names(BTC) == "Close"] <- "Cerro"
names(BTC)[names(BTC) == "Open"] <- "Abrio"
names(BTC)[names(BTC) == "Volume_.BTC."] <- "Volumen_BTC"
names(BTC)[names(BTC) == "Volume_.Currency."] <- "Volumen_MONEDA"
names(BTC)[names(BTC) == "Weighted_Price"] <- "Precio_Ponderado"
names(BTC)[names(BTC) == "Timestamp"] <- "FechaHora"

colnames(BTC)
View(BTC)

## Analizando los datos con valores Nulos NaN
table(is.na(BTC$Precio_Ponderado)) ## determino cuanto registro tengo con valores nulos.
sum(is.na(BTC$Precio_Ponderado))
table(is.na(BTC$Abrio)) ## determino cuanto registro tengo con valores nulos.

nrow(BTC) #4572257
sum(is.na(BTC$Precio_Ponderado))#1241716

cant <- c(sum(is.na(BTC$Precio_Ponderado)),nrow(BTC))
diff(cant)

BTC <- na.omit(BTC) ## Elimina todas las filas que contenga algun valor nulos
BTC <- BTC[!is.na(BTC$Precio_Ponderado),] ## Elimina nulos columna Precio

nrow(BTC) #330451
sum(is.na(BTC)) #0

View(BTC)

## conversion de datos , convertir el tipo de datos FechaHora numerica a datetime fecha y hora
head(BTC$FechaHora)

library(lubridate) ##https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf 
                   ##https://lubridate.tidyverse.org/

BTC$FechaHora <- as_datetime(BTC$FechaHora) 

head(BTC$FechaHora)

#Después de crear el DataFrame en R, 
# ahora podemos aplicar algún análisis estadístico.

print(max(BTC$Precio_Ponderado))  ### Precio Maximo Ponderado
print(mean(BTC$Precio_Ponderado)) ### Media del Precio Ponderado
print(min(BTC$Precio_Ponderado)) ### Minimo del Precio Ponderado

## plot
library(ggplot2) ## https://rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf
library(dplyr)

BTC %>%   ##Lo siguiente que debemos hacer es especificar de donde viene los fuentes de datos
  tail(330451) %>% ## Indicando que tome limitando el techo de una cantidad de registro de datos
  ggplot( aes(x=FechaHora, y=Precio_Ponderado)) + ##Asingaciones las variables de los datos eje x y
  geom_line(color="red") + ##Agregar geometrias con geom_* 	simbolo + agrega componentes al plot
  #geom_point(alpha = 0.2) +
  ggtitle("Comportamiento del Precio de BitCoin")

BTC %>%
  tail(3330451) %>%
  ggplot(aes(x = log(Precio_Ponderado))) +
  geom_density(color="red")

