library(readr)
datos <- read_csv("Desktop/Especialización Datos/BUSINESS INTELLIGENCE SOLUTIONS/ Unit 1 - Phase 2 - Organization and design/Appendix 1 - Database Phase 2.csv")


head(datos)
str(datos)
datos$season2= as.numeric(datos$season)#convertir a numerica la variable season2

library(lubridate)

datos$date = as_datetime(datos$datetime, format = "%m/%d/%Y %H:%M")

View(datos)#ver datos

dim(datos)

str(datos)

summary(datos)#informacion de las variables

variables= c("season", "holiday", "workingday","weather", "temp", "atemp", "humidity", "windspeed", "casual", "registered","count", "sources","date")
tipos = c("numerica de intervalo", "numerica de intervalo", "numerica de intervalo","numerica de intervalo","numerica de razón","numerica de razón","numerica de razón","numerica de razón","numerica de razón","numerica de intervalo","numerica de intervalo","Categorica Nominal","Categorica Nominal")

table.datos= data.frame(variables,
                        tipos)
table.datos

View(table.datos)

#convertir variables
datos$season <- as.numeric(datos$season)
datos$sources <- as.factor(datos$sources)


#Separar columnas
library(lubridate)
datos$date = as_datetime(datos$datetime, format = "%m/%d/%Y %H:%M")
datos$date2 = as.Date(datos$date)
View(datos)

#Datos Faltantes
colSums(is.na(datos))

#imputar datos en humedad con la medida de la columa

datos$humidity = ifelse(is.na(datos$humidity), mean(datos$humidity, na.rm = TRUE), datos$humidity)

#Confirmar nombre de columnas
colnames(datos)

#Eliminar Columnas
datos$season = na.omit(datos)

# Reemplazar los NA por un valor específico, por ejemplo "Sin Información"
datos$sources=as.character(datos$sources)
datos$sources[is.na(datos$sources)] <-"Sin Información"
print(datos$sources)

# Filtrar datos donde a
datos_filtrados <- subset(datos, humidity > 80)
print(datos_filtrados)

#seleccionar columnas
datos_col <- subset(datos, select = c("temp","casual"))
print(datos_col)


#agregar datos
encuesta <- data.frame(
  edad = c(25, 30, 35, 40, 45),
  genero = c("M", "F", "M", "F", "M"),
  ingreso = c(50000, 60000, 70000, 80000, 90000))
View(encuesta)

archivo <- "encuesta.csv"
write.csv2(datos, file = archivo, row.names = FALSE)
