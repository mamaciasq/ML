library(readxl)
datos <- read_excel("~/Natalia/Nacional/Modelos Lineales/Talleres/Punto2.xlsx")
View(datos)

attach(datos)

instares<-as.factor(Instares)

fit<-aov(Consumo~instares)
summary(fit)
res<-residuals(fit)

qf(0.95,4,51)

#########Comparaciones multiples

tapply(Consumo, instares, mean)

tukey<-TukeyHSD(fit)
tukey
plot(tukey)

########Validacion

#Normalidad
qqnorm(res)
qqline(res)
shapiro.test(res)

#Homocedasticidad

#Ajustados vs residuales estandarizados
ajus<-fitted.values(fit)
plot(ajus,res)
abline(h=0)

bartlett.test(Consumo,ajus)
plot(seq(1,56,by=1),res, xlab="Observaciones", ylab="Residuales", main="Residuales estandarizados")
plot(fit$residuals)

###Gráficas
par(mfrow=c(1,2))
qqnorm(res)
qqline(res)
plot(seq(1,56,by=1),res, xlab="Observaciones", ylab="Residuales", main="Residuales estandarizados")

##independencia
library(lmtest)
dwtest(fit)


