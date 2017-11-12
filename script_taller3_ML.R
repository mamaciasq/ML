# Código Final

# Lectura de datos
library(readxl)
library(tidyverse)
library(MASS)
library(zoo)
library(lmtest)
library(xtable)
source("macros_ML.R")
Dataset <- read_excel("data-table-B2.xlsx")

# Correlación de Pearson simples
cor(Dataset)
pairs(Dataset)


# Ajuste de modelo lineal
RegModel.1 <- lm(y~x1+x2+x3+x4+x5, data=Dataset)
summary(RegModel.1)
RegModel.1
anova(RegModel.1)

# Análisis de residuales

# Estadísticas de observación
Dataset<- within(Dataset, {
  fitted.RegModel.1 <- fitted(RegModel.1)
  residuals.RegModel.1 <- residuals(RegModel.1)
  rstudent.RegModel.1 <- rstudent(RegModel.1)
  hatvalues.RegModel.1 <- hatvalues(RegModel.1)
  cooks.distance.RegModel.1 <- cooks.distance(RegModel.1)
  obsNumber <- 1:nrow(Dataset) 
})
View(Dataset)

# Gráficos de residuales
par(mfrow=c(2,2))
plot(RegModel.1)

# Identificando puntos de alto leverage
par(mfrow=c(1,1))
Leverage.normal(RegModel.1,3,"")

# Residuos estandarizados
Residuos.normal(RegModel.1,1,"")

# QQ Plot con sus bandas de confianza
qqplot.normal(RegModel.1,10000,0.01,1,"")

# Gráfico de distancia de Cook
par(mfrow=c(3,2))
Influence.normal(RegModel.1,3,2,1,"")

# Intervalos de confianza de las estimaciones
library(MASS)
confint(RegModel.1, level=0.95)

# Factores de inflación de varianza
library(car)
vif(RegModel.1)
round(cov2cor(vcov(RegModel.1)), 3) # Correlations of parameter estimates

# Test de heterocedasticidad y autocorrelación
shapiro.test(RegModel.1)
dwtest(RegModel.1)


# Test de Durbin-Watson para chequear correlación
library(zoo)
library(lmtest)
dwtest(y ~ x1 + x2 + x3 + x4, alternative="greater", data=Dataset)

# Gráficos
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(RegModel.1)
par(oldpar)
par(mfrow=c(1,1))
qqPlot(RegModel.1, simulate=TRUE, id.method="y", id.n=2)
crPlots(RegModel.1, span=0.5)
avPlots(RegModel.1, id.method="mahal", id.n=2)
influencePlot(RegModel.1, id.method="noteworthy", id.n=2)

# Selección de variables
library(leaps)
attach(Dataset)
vx = cbind(x1, x2, x3, x4, x5)
leaps(vx,y,int=T, method="Cp")
leaps(vx,y,int=T, method="adjr2")
