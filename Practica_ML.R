# Código Rcmdr para práctica en clase

# Lectura de datos
library(readxl)
Dataset <- read_excel("example-data/regmult.XLS")

# Ajuste de modelo lineal
RegModel.1 <- lm(y~x1+x2+x3+x4, data=Dataset)
summary(RegModel.1)

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

# Intervalos de confianza de las estimaciones
library(MASS)
confint(RegModel.1, level=0.95)

# Factores de inflación de varianza
library(car)
vif(RegModel.1)
round(cov2cor(vcov(RegModel.1)), 3) # Correlations of parameter estimates

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
vx = cbind(x1, x2, x3, x4)
leaps(vx,y,int=T, method="Cp")
leaps(vx,y,int=T, method="adjr2")
AIC(RegModel.1)

# Ajusto modelo sin x3 y reviso RCuadrado, RCUadradoAjustado, AIC y BIC
RegModel2 <- lm(y~x1+x2+x4, data=Dataset)
summary(RegModel2)
AIC(RegModel2)
BIC(RegModel2)

# Regresión Ridge

# Lectura de datos
hald <- read_excel("example-data//hald.XLS")

# Ajuste de modelo
RegModel.1 <- lm(y~x1+x2+x3+x4, data=hald)
summary(RegModel.1)

# Chequeo de AIC, correlación y VIF para ver problemas de Multicolinealidad
library(MASS)
library(leaps)
AIC(RegModel.1)
cor(hald[,c("x1","x2","x3","x4","y")], use="complete")
vif(RegModel.1)
round(cov2cor(vcov(RegModel.1)), 3) # Correlations of parameter estimates

# Ajustando regresión Ridge
m2r <- lm.ridge(y~x1+x2+x3+x4, data = hald, lambda = seq(0, 0.2, 0.01))

# Traza Ridge
plot(m2r)

# Se nota que el lambda más adecuado es 0.1, entonces se ajusta con ese valor
m2r <- lm.ridge(y~x1+x2+x3+x4, data = hald, lambda = 0.1)

# También podría hacerse con select(m2r)
MASS::select(m2r) # porque select también es una función de dplyr

# Para ver el modelo ajustado, revisamos las estimaciones de acuerdo al lambda 0.1 escogido
m2r

# o usamos esta otra librería (los resultados difieren un poquito)
library(ridge)
m2r <- linearRidge(y~x1+x2+x3+x4, data = hald)
m2r
summary(m2r)
