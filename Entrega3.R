library(readxl)
dataset <- read.csv("Appendix 2 - Database Phase 3 .csv")
View(dataset)

# Puntos 1 medidas de resumen

summary(dataset)

res = data.frame(media = mean (dataset$"Youtube"),
                 mediana = median (dataset$"Youtube"),
                 varianza = var (dataset$"Youtube"),
                 desviacion = sd (dataset$"Youtube"))

View(res)

boxplot(dataset$"Youtube",
        horizontal = TRUE,
        col = c("darkgreen"),
        main = "Diagrama de caja y bigotes")

install.packages(fdth)
library(fdth)

His = fdt(dataset$"Youtube",
                 breaks = "Sturges",
                 right = TRUE)

plot(His, type = "fh",
     main = "Histrograma",
     xlab = "Youtube",
     ylab = "Frecuencia",
     col = c("red", "blue", "green", "orange", "purple", "yellow", "cyan", "gray", "pink"))

# Puntos 2 variables categoricas vs variable cuantitativa

dataset$pop_density = as.factor(dataset$pop_density)

#Tabla contingencia categorica vs cuantitativa

data_youtube2= aggregate(dataset$Youtube ~ dataset$pop_density, data = dataset, FUN = sum)
View(data_youtube2)

#Crear variable costo como categorizada
library(dplyr)
dataset = dataset %>%
  mutate(categoria = case_when(
    dataset$Youtube < 100 ~ "bajo",
    dataset$Youtube < 200 ~ "Medio",
    TRUE ~ "Alto"))
View(dataset)

tabla = table(dataset$pop_density, dataset$categoria)

mosaicplot(tabla, 
           main = "Grafico de Mosaico",
           xlab = "Densidad pop",
           ylab = "Categoria",
           col = c("red", "yellow", "darkgreen"))

boxplot(dataset$Youtube ~ dataset$pop_density,
        horizontal = TRUE,
        col = c("darkgreen", "yellow", "red"),
        main = "Diagrama de cajas",
        xlab = "Categoria",
        ylab = "pop_density")

# Punto 3

# Graficar la relación entre las dos variables

plot(dataset$Youtube, dataset$revenues, 
     xlab = "Youtube", ylab = "Revenues",
     main = "Gráfico de dispersión entre Youtube y Revenues",
     col = c("darkgreen", "blue"))

tendencia = lm(dataset$revenues ~ dataset$Youtube)
abline(tendencia, col = "red")

# Calcular la correlación entre las dos variables
correlacion <- cor(dataset$Youtube, dataset$revenues)
print(paste("Correlación entre Youtube y Revenues:", correlacion))

#Tiene un correlación muy baja, casi nula de 0.06770365380187.

# Determinar la significancia de la correlación
test_correlacion <- cor.test(dataset$Youtube, dataset$revenues)
test_correlacion

# El p-value = 0.3775 es significativo ?

# Crear un gráfico de correlación de todas las variables
pairs(dataset, main = "Gráfico de correlación de todas las variables en la base de datos")

#Punto 4:

mod1 = lm(dataset$marketing_total ~ dataset$Youtube)
summary(mod1)



