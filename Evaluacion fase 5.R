#Punto 1
library(readxl)
data = read.csv("Appendix 3 - Database Phase 5.csv")
View(data)

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

#----------------------------------------

# Identificar y eliminar valores atípicos utilizando Cook's distance
cooksd <- cooks.distance(modrl)
influential <- as.numeric(names(cooksd)[(cooksd > 1 / nrow(data))])

# Eliminar valores atípicos
data_clean <- data[-influential, ]

# Construir un nuevo modelo lineal sin valores atípicos
modrl_clean <- lm(revenues ~ marketing_total, data = data_clean)
summary(modrl_clean)

# Volver a graficar y analizar el nuevo modelo

# Gráfico de dispersión sin valores atípicos
plot(data_clean$marketing_total, data_clean$revenues, main = "Gráfico de dispersión sin valores atípicos",
     xlab = "Eje X (Marketing Total)", ylab = "Eje Y (Ingresos)",
     col = "skyblue", pch = 16)

# Agregar línea de tendencia
tendencia_clean <- lm(data_clean$revenues ~ data_clean$marketing_total)
abline(tendencia_clean, col = "red")

# Calcular los residuos del nuevo modelo
residuos_clean <- resid(modrl_clean)

#Graficos de diagnostico
install.packages("car")
library(car)

#Generar graficas de diagnostico
par(mfrow = c(2,2))

#Grafico de residuos vs valores ajustados
plot(modrl_clean, which = 1)

#Grafico qq de los residuos
plot(modrl_clean, which = 2)

#Grafico de dispersion parcial
crPlots(modrl_clean)

#Grafico de influencia
influencePlot(modrl_clean)

#----------------------------------------

# Extraer la información relevante de los resúmenes
# Función para extraer la información relevante del resumen
extract_model_info <- function(summary_obj) {
  return(data.frame(
    Coefficient = summary_obj$coefficients[, "Estimate"],
    `Std. Error` = summary_obj$coefficients[, "Std. Error"],
    `t value` = summary_obj$coefficients[, "t value"],
    `Pr(>|t|)` = summary_obj$coefficients[, "Pr(>|t|)"],
    `Residual Standard Error` = summary_obj$sigma,
    `Multiple R-squared` = summary_obj$r.squared,
    `Adjusted R-squared` = summary_obj$adj.r.squared,
    `F-statistic` = summary_obj$fstatistic[1],
    `p-value` = summary_obj$fstatistic[3]
  ))
}

# Extraer la información de los dos modelos
summary_modrl <- summary(modrl)
summary_modrl_clean <- summary(modrl_clean)

model_info <- extract_model_info(summary_modrl)
model_info_clean <- extract_model_info(summary_modrl_clean)

# Agregar nombres de filas para diferenciar los coeficientes
row.names(model_info) <- paste("mrl1", row.names(model_info), sep = "_")
row.names(model_info_clean) <- paste("mrl2", row.names(model_info_clean), sep = "_")

# Crear una tabla de comparación
comparison_table <- rbind(model_info, model_info_clean)
View(comparison_table)

#----------------------------------------

#Punto 2

library(dplyr)

data$pop_density = na.omit(data)#convertir a numerica la variable season2

matrizCorrelación = cor(data)

#Generar graficas de diagnostico
par(mfrow = c(1,1))

#Visualizar matriz de correlación
library(corrplot)
corrplot(matrizCorrelación, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

#Modelo de regresion multiple
mrm = lm(data$revenues ~ data$marketing_total + data$google_adwords + data$facebook
         + data$twitter + data$marketing_total, data = data)

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
