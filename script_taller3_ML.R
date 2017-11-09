# Importación de librerías
library(tidyverse)
library(readxl)
library(MASS)
library(zoo)
library(lmtest)
source("macros_ML.R")

# Lectura de datos
data <- read_excel("data-table-B2.xlsx")
attach(data)


# Correlación de Pearson simples
cor(data)
pairs(data)
pairs(data[,-1])

# Modelamiento
fit <- lm(y~1+x1+x2+x3+x4+x5, data)
summary(fit)
# Modelo ajustado
fit
par(mfrow=c(2,2))
plot(fit)

# Correlaciones de Pearson simplesmediante función
par(mar=c(4,4,2,2))
Correlaciones(fit,3,2,1,"")

# Correlaciones parciales
Correlaciones.parcial(fit,3,2,1,"")

# Búsqueda del "mejor" modelo
ajuste.normal(fit,5)
ajuste.normal(fit,4)
ajuste.normal(fit,3)
ajuste.normal(fit,2)
ajuste.normal(fit,1)
fit1 <- lm(y ~ -1+x1+x2+x3+x4+x5, data)
summary(fit1)
vcov(fit1)
fitted(fit1)
qt(1-0.05/2,24)
qt(1-0.01/2,24)
qt(1-0.1/2,24)

# Identificando puntos de alto leverage
par(mfrow=c(1,1))
Leverage.normal(fit1,1,"")

# Residuos estandarizados
Residuos.normal(fit1,1,"")

# QQ Plot con sus bandas de confianza
qqplot.normal(fit1,500,0.01,1,"")

# Gráfico de distancia de Cook
par(mfrow=c(3,2))
Influence.normal(fit1,3,2,1,"")
bptest(fit1)
dwtest(fit1)

