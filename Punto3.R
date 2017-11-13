library(readxl)
datos<- read_excel("~/Natalia/Nacional/Modelos Lineales/Talleres/Punto3.xlsx")

attach(datos)

fert<-as.factor(Fertilizante)
var<-as.factor(Variedad)

fit<-aov(Rendimiento~fert*var)
summary(fit)
res<-residuals(fit)

fit1<-aov(Rendimiento~fert)
summary(fit1)
res1<-residuals(fit1)

qf(0.95,4,40)
qf(0.95,3,40)
qf(0.95,12,40)

#########Comparaciones multiples

#considerando el de dos vias
tukey1<-TukeyHSD(fit)
tukey1
plot(tukey1)

#considerando el de una via
tukey<-TukeyHSD(fit1)
tukey
plot(tukey)

########Validacion

##El de dos vias

#Normalidad
qqnorm(res)
qqline(res)
shapiro.test(res)

#Homocedasticidad

#Ajustados vs residuales estandarizados
plot(seq(1,60,by=1),res, xlab="Observaciones", ylab="Residuales", main="Residuales estandarizados")

ajus<-fitted.values(fit)
plot(ajus,res)

bartlett.test(Rendimiento,ajus)


dwtest(fit) #independencia


###Gráficas
par(mfrow=c(1,3))
qqnorm(res)
qqline(res)
plot(seq(1,60,by=1),res, xlab="Observaciones", ylab="Residuales", main="Residuales estandarizados")
plot(ajus,res)


##El de una vias

#Normalidad
qqnorm(res1)
qqline(res1)
shapiro.test(res1)

#Homocedasticidad

#Ajustados vs residuales estandarizados
plot(seq(1,60,by=1),res1, xlab="Observaciones", ylab="Residuales", main="Residuales estandarizados")

ajus1<-fitted.values(fit1)
plot(ajus1,res1)

bartlett.test(Rendimiento,ajus1)


dwtest(fit1) #independencia


###Gráficas
par(mfrow=c(1,3))
qqnorm(res1)
qqline(res1)
plot(seq(1,60,by=1),res1, xlab="Observaciones", ylab="Residuales", main="Residuales estandarizados")
plot(ajus1,res1)
