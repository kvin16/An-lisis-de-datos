library(readxl)
data = read.csv("~/Desktop/Especialización Datos/BUSINESS INTELLIGENCE SOLUTIONS/Entrega 4/Appendix 2 - Database Phase 3 .csv")
View(data)

#marketing total y revenues: Variables a trabajar

# Punto 1

#Crear Grafico de dispersión
plot(data$marketing_total, data$revenues, main = "Grafico de dispersión",
     xlab = "Eje X (Marketing Total)" , ylab = "Eje Y (Ingresos)",
     col = "skyblue", pch = 16)

#Agregar linea de tendencia
tendencia = lm(data$revenues ~ data$marketing_total)
abline(tendencia, col = "red")

#Modelo de regresion Lineal
modrl = lm(revenues ~ marketing_total, data = data)
summary(modrl)

library(ggplot2)
library(gridExtra)
library(car)

#Calcular los residuos
residuos = resid(modrl)

#Histograma de residuos
histograma = ggplot(data.frame(residuos), aes(x = residuos)) + 
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +  
                 labs(title = "Histograma de Residuos", x = "Residuos", y = "Frecuencia")

#QQ plot de los residuos
qqplot = ggplot(data.frame(residuos), aes(sample = residuos)) + 
  stat_qq() + 
  stat_qq_line(color = "blue") + 
  labs (title = "QQ Plot de residuos", x = "Cuantiles teorios", y = "Cuantiles Observados" )

#Combinar los graficos
panelcombinado = grid.arrange(histograma, qqplot, ncol = 2)

#Obtener valores ajustados del modelo
valores_ajustado = predict(modrl)

#Df con los residuos y los valores ajustados
dataresiduos = data.frame(residuos, valores_ajustado)

#Grafico de dispersion de los residuos y los valores ajustados
scatter_plot = ggplot(dataresiduos, aes( x = valores_ajustado, y = residuos))+
  geom_point(color="blue")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  labs(titlte = "Residuos vs los valores ajustados", x = "Valores ajustados", y = "Residuos" )

#Graficos de diagnostico
install.packages("car")
library(car)

#Generar graficas de diagnostico
par(mfrow = c(2,2))

#Grafico de residuos vs valores ajustados
plot(modrl, which = 1)

#Grafico qq de los residuos
plot(modrl, which = 2)

#Grafico de dispersion parcial
crPlots(modrl)

#Grafico de influencia
influencePlot(modrl)

#------------------------------------------------------------------------------

# Ejercicio 3
summary(data$marketing_total)
rango = range(data$marketing_total)
diferencia= diff(rango)
print(diferencia)

#Nuevos valores para x, para predecir
x_new = data$marketing_total

#predecir valores de y usando el modelo
predicciones = predict(modrl, data = data)

#Mostrar Predicciones
print(predicciones)

#Calcular el intervalo de confianza 90%
intervaloDeConfianza = confint(modrl, level = 0.99)

summary(modrl)
print(intervaloDeConfianza)

#Calcular el intervalo de confianza 90%
intervaloDeConfianza = confint(modrl, level = 0.9)

summary(modrl)
print(intervaloDeConfianza)

#----------------------------------------------------------------
#Punto 4

data$pop_density = na.omit(data)#convertir a numerica la variable season2

matrizCorrelación = cor(data)

#Generar graficas de diagnostico
par(mfrow = c(1,1))

#Visualizar matriz de correlación
library(corrplot)
corrplot(matrizCorrelación, method = "circle", type = "upper",
        tl.col = "black", tl.srt = 45, tl.cex = 0.7)

#Modelo de regresion multiple
mrm = lm(data$revenues ~ data$marketing_total + data$google_adwords + data$facebook + data$Instagram +
           data$Youtube + data$twitter + data$marketing_total, data = data)

summary(mrm)

#Calcular los residuos
residuosmrm = resid(mrm)

#Calcular el MSE
mse = mean(residuosmrm^2)

#Calcula el RMSE
rmse = sqrt(mse)

#Graficos diagnosticos

#Obtener los residuos estandarizados
residuos_estandarizadosmrm = rstandard(mrm)

#Df de los valores ajustados y los residuos estandarizados
diagnosticos = data.frame(ValoresAjustados = predict(mrm),
                          residuos_estandarizadosmrm = residuos_estandarizadosmrm)

# Gráfico de residuos estandarizados vs valores ajustados
ggplot(diagnosticos, aes(x = ValoresAjustados, y = residuos_estandarizadosmrm)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos estandarizados vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos Estandarizados")

# Gráfico de cuantiles normales de los residuos estandarizados
qqPlot(residuos_estandarizadosmrm) # Valores atipicos 111 y 1072