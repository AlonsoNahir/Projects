library(tidyr)
library(performance)
library(readxl)
library(lmtest)
library(knitr)
library(tidyr)
library(ggplot2)
library(dplyr)

#Cargamos los damaframe
CO<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/MaximosCOMensualZonaNoroeste.csv")
NO2<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/MaximosPM10MensualZonaNoroeste.csv")
O3<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/MaximosO3MensualZonaNoroeste.csv")
PM10<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/MaximosPM10MensualZonaNoroeste.csv")
PM25<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/MaximosPM25MensualZonaNoroeste.csv")
SO2<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/MaximosSO2MensualZonaNoroeste.csv")
C_m<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/Valor de producción total - Sector 23 Construcción (Miles de Pesos corrientes), Ciudad de México, 2023 Feb.csv")
B_m<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/Ingresos totales por suministro de bienes y servicios - 72 Servicios de alojamiento temporal y de preparación de alimentos y bebidas (Índice base 2013=100), Ciudad de México, 2023 Feb.csv")
B_p<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/Personal ocupado total - 72 Servicios de alojamiento temporal y de preparación de alimentos y bebidas (Índice base 2013=100), Ciudad de México, 2023 Feb.csv")
C_p<-read.csv("C:/Users/cacah/Downloads/Estancia de investigacion/Personal ocupado total - Sector 23 Construcción (Número de personas), Ciudad de México, 2023 Feb.csv")
T_m<-read_excel("C:/Users/cacah/Downloads/Estancia de investigacion/Ingresos por tránsito vehicular (Miles de pesos a precios corrientes).xls")

#C_mConstrucción en moneda


##Ajuste de los datos


#Filtrar los datos para el periodo necesario
#CO<-subset(CO, Year>=2019)
#C_m<-subset(C_m,Periodo>=2019 & Periodo<2022)
#Renombramos los vectores por practicidad
#names(C_m)[2]<-"Variacion"
#names(C_m)[4]<-"Lim.inf"
#names(C_m)[5]<-"Lim.Sup"
#Creamos un vector con año y mes (3 primeras letras):
#Periodo <- sapply(CO$Mes, function(x) substr(x, 1, 3))
#CO$Periodo<-Periodo
#CO<-unite(CO,Periodo,c(Year,Periodo),sep = " ")
#CO<-subset(CO, select = -Mes)

#Unimos CO y Const_men un mismo dataframe
#CO_C_m<-merge(CO,C_m, by="Periodo")

#Creamos el modelo lineal
#ml_CO_C_m<-lm(Nivel~Variacion,CO_C_m)
#plot(CO_C_m$Variacion,CO_C_m$Nivel)
#summary(ml_CO_C_m)




#Replicamos para todas las bases de datos

CO<-subset(CO, Year>=2019 & Year<2022)
Periodo <- sapply(CO$Mes, function(x) substr(x, 1, 3))
CO$Periodo<-Periodo
CO<-unite(CO,Periodo,c(Year,Periodo),sep = " ")
CO<-subset(CO, select = -Mes)
names(CO)[2]<-"Nivel.CO"

NO2<-subset(NO2, Year>=2019 & Year<2022)
Periodo <- sapply(NO2$Mes, function(x) substr(x, 1, 3))
NO2$Periodo<-Periodo
NO2<-unite(NO2,Periodo,c(Year,Periodo),sep = " ")
NO2<-subset(NO2, select = -Mes)
names(NO2)[2]<-"Nivel.NO2"

O3<-subset(O3, Year>=2019 & Year<2022)
Periodo <- sapply(O3$Mes, function(x) substr(x, 1, 3))
O3$Periodo<-Periodo
O3<-unite(O3,Periodo,c(Year,Periodo),sep = " ")
O3<-subset(O3, select = -Mes)
names(O3)[2]<-"Nivel.O3"

PM10<-subset(PM10, Year>=2019 & Year<2022)
Periodo <- sapply(PM10$Mes, function(x) substr(x, 1, 3))
PM10$Periodo<-Periodo
PM10<-unite(PM10,Periodo,c(Year,Periodo),sep = " ")
PM10<-subset(PM10, select = -Mes)
names(PM10)[2]<-"Nivel.PM10"

PM25<-subset(PM25, Year>=2019 & Year<2022)
Periodo <- sapply(PM25$Mes, function(x) substr(x, 1, 3))
PM25$Periodo<-Periodo
PM25<-unite(PM25,Periodo,c(Year,Periodo),sep = " ")
PM25<-subset(PM25, select = -Mes)
names(PM25)[2]<-"Nivel.PM25"

SO2<-subset(SO2, Year>=2019 & Year<2022)
Periodo <- sapply(SO2$Mes, function(x) substr(x, 1, 3))
SO2$Periodo<-Periodo
SO2<-unite(SO2,Periodo,c(Year,Periodo),sep = " ")
SO2<-subset(SO2, select = -Mes)
names(SO2)[2]<-"Nivel.SO2"

#cambiamos el nombre de algunos vectores económicos para hacer una matriz con todos los datos
C_m<-subset(C_m,Periodo>=2019 & Periodo<2022)
names(C_m)[2]<-"Variacion.C_m"
names(C_m)[3]<-"Error.estandar.C_m"
names(C_m)[4]<-"Lim.inf.C_m"
names(C_m)[5]<-"Lim.Sup.C_m"

C_p<-subset(C_p,Periodo>=2019 & Periodo<2022)
names(C_p)[2]<-"Variacion.C_p"
names(C_p)[3]<-"Error.estandar.C_p"
names(C_p)[4]<-"Lim.inf.C_p"
names(C_p)[5]<-"Lim.Sup.C_p"

B_m<-subset(B_m,Periodo>=2019 & Periodo<2022)
names(B_m)[2]<-"Variacion.B_m"
names(B_m)[3]<-"Error.estandar.B_m"
names(B_m)[4]<-"Lim.inf.B_m"
names(B_m)[5]<-"Lim.Sup.B_m"

B_p<-subset(B_p,Periodo>=2019 & Periodo<2022)
names(B_p)[2]<-"Variacion.B_p"
names(B_p)[3]<-"Error.estandar.B_p"
names(B_p)[4]<-"Lim.inf.B_p"
names(B_p)[5]<-"Lim.Sup.B_p"

#hacemos la matriz con todos los datos de particulas y desarrollo económico.
Matriz<-merge(CO,NO2, by="Periodo")
Matriz<-merge(O3, Matriz, by="Periodo")
Matriz<-merge(PM10, Matriz, by="Periodo")
Matriz<-merge(PM25, Matriz, by="Periodo")
Matriz<-merge(SO2, Matriz, by="Periodo")
Matriz<-merge(C_m, Matriz, by="Periodo")
Matriz<-merge(C_p, Matriz, by="Periodo")
Matriz<-merge(B_m, Matriz, by="Periodo")
Matriz<-merge(B_p, Matriz, by="Periodo")
Matriz<-merge(T_m, Matriz, by="Periodo")
Matriz<-select(Matriz, -Variacion.B_p,-Error.estandar.B_p,-Variacion.B_m,-Error.estandar.B_m,
               -Variacion.C_p,-Error.estandar.C_p,-Variacion.C_m,-Error.estandar.C_m)

##Fin del ajuste de los datos



mod.CO.B_m<-lm(Nivel.CO~Variacion.B_m,Matriz)
mod.O3.B_m<-lm(Nivel.O3~Variacion.B_m,Matriz)
summary(mod.CO.B_m)
summary(mod.O3.B_m)
plot(Matriz$Nivel.CO,Matriz$Variacion.B_m, asp = 1)
plot(residuals(mod.CO.B_m),Matriz$Nivel.CO)
plot(log(residuals(mod.CO.B_m)),Matriz$Nivel.CO)
plot((residuals(mod.CO.B_m))^2,Matriz$Nivel.CO)

#Graficos de los vectores con desarrollo historico
x<-seq(1,36)

plot(x,Matriz$Variacion.B_p)
plot(x,Matriz$Error.estandar.B_p)
plot(x,Matriz$Lim.inf.B_p)
plot(x,Matriz$Lim.Sup.B_p)
plot(x,Matriz$Variacion.B_m)
plot(x,Matriz$Error.estandar.B_m)
plot(x,Matriz$Lim.inf.B_m)
plot(x,Matriz$Lim.Sup.B_m)
plot(x,Matriz$Variacion.C_p)
plot(x,Matriz$Error.estandar.C_p)
plot(x,Matriz$Lim.inf.C_p)
plot(x,Matriz$Lim.Sup.C_p)
plot(x,Matriz$Variacion.C_m)
plot(x,Matriz$Error.estandar.C_m)
plot(x,Matriz$Lim.inf.C_m)
plot(x,Matriz$Lim.Sup.C_m)
plot(x,Matriz$Nivel.SO2)
plot(x,Matriz$Nivel.PM25)
plot(x,Matriz$Nivel.PM10)
plot(x,Matriz$Nivel.O3)
plot(x,Matriz$Nivel.CO)
plot(x,Matriz$Nivel.NO2)
plot(x,Matriz$T_m)

#Graficos de las variables económicas (X) con las variables particulas (Y)

plot(Matriz$Lim.inf.B_p,Matriz$Nivel.SO2)
plot(Matriz$Lim.Sup.B_p,Matriz$Nivel.SO2)
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.SO2)
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.SO2)

plot(Matriz$Lim.inf.B_p,Matriz$Nivel.PM25)
plot(Matriz$Lim.Sup.B_p,Matriz$Nivel.PM25)
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.PM25)
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.PM25)

plot(Matriz$Lim.inf.B_p,Matriz$Nivel.PM10)
plot(Matriz$Lim.Sup.B_p,Matriz$Nivel.PM10)
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.PM10)
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.PM10)

plot(Matriz$Lim.inf.B_p,Matriz$Nivel.O3)
plot(Matriz$Lim.Sup.B_p,Matriz$Nivel.O3)
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.O3)
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.O3)

plot(Matriz$Lim.inf.B_p,Matriz$Nivel.CO)
plot(Matriz$Lim.Sup.B_p,Matriz$Nivel.CO)
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.CO)
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.CO)

plot(Matriz$Lim.inf.B_p,Matriz$Nivel.NO2)
plot(Matriz$Lim.Sup.B_p,Matriz$Nivel.NO2)
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.NO2)
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.NO2)

#Modelos lineales de las variables económicas por bienes (X) contra las variables particulas (Y)

m1<-lm(Nivel.SO2~Lim.inf.B_p,Matriz)
m2<-lm(Nivel.SO2~Lim.Sup.B_p,Matriz)
m3<-lm(Nivel.SO2~Lim.inf.B_m,Matriz)
m4<-lm(Nivel.SO2~Lim.Sup.B_m,Matriz)

m5<-lm(Nivel.PM25~Lim.inf.B_p,Matriz)
m6<-lm(Nivel.PM25~Lim.Sup.B_p,Matriz)
m7<-lm(Nivel.PM25~Lim.inf.B_m,Matriz)
m8<-lm(Nivel.PM25~Lim.Sup.B_m,Matriz)

m9<-lm(Nivel.PM10~Lim.inf.B_p,Matriz)
m10<-lm(Nivel.PM10~Lim.Sup.B_p,Matriz)
m11<-lm(Nivel.PM10~Lim.inf.B_m,Matriz)
m12<-lm(Nivel.PM10~Lim.Sup.B_m,Matriz)

m13<-lm(Nivel.O3~Lim.inf.B_p,Matriz)
m14<-lm(Nivel.O3~Lim.Sup.B_p,Matriz)
m15<-lm(Nivel.O3~Lim.inf.B_m,Matriz)
m16<-lm(Nivel.O3~Lim.Sup.B_m,Matriz)

m17<-lm(Nivel.CO~Lim.inf.B_p,Matriz)
m18<-lm(Nivel.CO~Lim.Sup.B_p,Matriz)
m19<-lm(Nivel.CO~Lim.inf.B_m,Matriz) #R^2=0.436
m20<-lm(Nivel.CO~Lim.Sup.B_m,Matriz) #R^2=0.430

m21<-lm(Nivel.NO2~Lim.inf.B_p,Matriz)
m22<-lm(Nivel.NO2~Lim.Sup.B_p,Matriz)
m23<-lm(Nivel.NO2~Lim.inf.B_m,Matriz)
m24<-lm(Nivel.NO2~Lim.Sup.B_m,Matriz)

#comparamos los modelos
compare_performance(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,
                    m16,m17,m18,m19,m20,m21,m22,m23,m24)

#Vemos los 2 mejores modelos:
plot(Matriz$Lim.inf.B_m,Matriz$Nivel.CO)
abline(m19,col="red")
plot(Matriz$Lim.Sup.B_m,Matriz$Nivel.CO)
abline(m20,col="red")

#Modelos lineales de las variables económicas por construcción (X) contra las variables particulas (Y)

mc1<-lm(Nivel.SO2~Lim.inf.C_p,Matriz)
mc2<-lm(Nivel.SO2~Lim.Sup.C_p,Matriz)
mc3<-lm(Nivel.SO2~Lim.inf.C_m,Matriz)
mc4<-lm(Nivel.SO2~Lim.Sup.C_m,Matriz)

mc5<-lm(Nivel.PM25~Lim.inf.C_p,Matriz)
mc6<-lm(Nivel.PM25~Lim.Sup.C_p,Matriz)
mc7<-lm(Nivel.PM25~Lim.inf.C_m,Matriz)
mc8<-lm(Nivel.PM25~Lim.Sup.C_m,Matriz)

mc9<-lm(Nivel.PM10~Lim.inf.C_p,Matriz)
mc10<-lm(Nivel.PM10~Lim.Sup.C_p,Matriz)
mc11<-lm(Nivel.PM10~Lim.inf.C_m,Matriz)
mc12<-lm(Nivel.PM10~Lim.Sup.C_m,Matriz)

mc13<-lm(Nivel.O3~Lim.inf.C_p,Matriz)
mc14<-lm(Nivel.O3~Lim.Sup.C_p,Matriz)
mc15<-lm(Nivel.O3~Lim.inf.C_m,Matriz)
mc16<-lm(Nivel.O3~Lim.Sup.C_m,Matriz)

mc17<-lm(Nivel.CO~Lim.inf.C_p,Matriz)
mc18<-lm(Nivel.CO~Lim.Sup.C_p,Matriz)
mc19<-lm(Nivel.CO~Lim.inf.C_m,Matriz) #R^2=0.188
mc20<-lm(Nivel.CO~Lim.Sup.C_m,Matriz) #R^2=0.242

mc21<-lm(Nivel.NO2~Lim.inf.C_p,Matriz)
mc22<-lm(Nivel.NO2~Lim.Sup.C_p,Matriz)
mc23<-lm(Nivel.NO2~Lim.inf.C_m,Matriz)
mc24<-lm(Nivel.NO2~Lim.Sup.C_m,Matriz)

#comparamos los modelos
compare_performance(mc1,mc2,mc3,mc4,mc5,mc6,mc7,mc8,mc9,mc10,mc11,mc12,mc13,mc14,mc15,
                    mc16,mc17,mc18,mc19,mc20,mc21,mc22,mc23,mc24)


plot(Matriz$Lim.inf.C_m,Matriz$Nivel.CO)
abline(mc19,col="red")
plot(Matriz$Lim.Sup.C_m,Matriz$Nivel.CO)
abline(mc20,col="red")

#Modelos lineales de las variables económicas por transporte (X) contra las variables particulas (Y)

mt1<-lm(Nivel.SO2~T_m,Matriz)

mt2<-lm(Nivel.PM25~T_m,Matriz)

mt3<-lm(Nivel.PM10~T_m,Matriz)

mt4<-lm(Nivel.O3~T_m,Matriz)

mt5<-lm(Nivel.CO~T_m,Matriz)

mt6<-lm(Nivel.NO2~T_m,Matriz)

#comparamos los modelos
compare_performance(mt1,mt2,mt3,mt4,mt5,mt6)


plot(Matriz$T_m,Matriz$Nivel.CO)
abline(mt5,col="red")

#modelos multiparamétricos

mp1<-lm(Nivel.CO+Nivel.SO2+Nivel.O3+Nivel.PM10+Nivel.NO2~T_m+Lim.Sup.B_m+Lim.Sup.C_m,Matriz)
mp2<-lm(Nivel.CO~T_m+Lim.Sup.B_m+Lim.Sup.C_m,Matriz)
mp3<-lm(Nivel.CO~T_m+Lim.Sup.B_m,Matriz)

summary(mp1)
summary(mp2)
summary(mp3)

#retomando el mejor modelo m19
 
 #residuales
plot(residuals(m19),Matriz$Lim.inf.B_m)

 #Prueba Goldfeld-Quandt
prueba<- gqtest(m19, point=0.5, alternative="two.sided",
                order.by=Matriz$Lim.inf.B_m)

prueba

#Revisar simat para variables de contaminación.

#Graficas de caja
Transporte<-Matriz$T_m/1000
Construccion<-Matriz$Lim.Sup.C_m/100000
Bienes_Servicios<-Matriz$Lim.inf.B_m
grupo<-data.frame(Transporte,Construccion,Bienes_Servicios)
boxplot(grupo, col = rainbow(3), main="Variables económicas")

Ozono<-Matriz$Nivel.O3/100
Monoxido_Carbono<-Matriz$Nivel.CO
grupo2<-data.frame(Ozono,Monoxido_Carbono)
boxplot(grupo2, col=rainbow(2), main="Variables ambientales")
