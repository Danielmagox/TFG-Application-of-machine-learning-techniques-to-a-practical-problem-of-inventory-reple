setwd("C:/Users/Daniel/Desktop/TFG/R")
#Prophet 
#Arreglar la función
#install.packages("RSQLite")
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "../Datos/Database/ProyectoFinal.db")

#data = dbGetQuery(con,"Select FechaCierreH,  sum(UnidadesCierre) Unidades from CierresAlmacen where 
#                  FamiliaDesc NOT IN ('COMBUSTIBLES','ADITIVOS PARA LA COMBUSTION','LUBRICANTES'
#                  ,'LÍQUIDOS REFRIGERANTES','CASCOS (NO INVENTARIABLE)','UTILES DE PERSONAL','UNIFORMES') 
#                  AND FechaCierreH > '2008-01-01' AND Descripcion NOT IN ('no existe viene mal de PICK',
#                  'PASTA MANOS',' ...........  no usar ...............', '..', 'PASTA MANOS ZORKIL-C35') 
#                  AND FamiliaDesc = 'SCANIA L94' AND Descripcion = 'JUEGO PASTILLA FRENO    ( VER 1112703 )' 
#                  group by FechaCierreH")

#install.packages("forecast")
#install.packages("readxl")

#library(readxl)
#library(forecast)

#Creamos la serie temporal
#tsdata<-ts(data$Unidades,frequency=12,start=c(2008,1))
#Dibujamos la misma
#plot(tsdata)
#creamos el modelo
#modeloArima<-auto.arima(tsdata,D=1)
#la predicción a 12 meses
#forecast0<-forecast(modeloArima,h=2)

#forecast0
#dibujamos la predicción a 12 meses
#plot(forecast0)

#plot(forecast1$residuals)
#Sumario
#summary(modeloArima)

#accuracy(modeloArima)

#plot(forecast0,include=12)


####Predicción a futuro
data2 = dbGetQuery(con,"Select Fecha, sum(Existencias) TotalExistencias from Movimientos where 
                   FamiliaDesc NOT IN ('COMBUSTIBLES','ADITIVOS PARA LA COMBUSTION','LUBRICANTES',
                   'LÍQUIDOS REFRIGERANTES','CASCOS (NO INVENTARIABLE)','UTILES DE PERSONAL'
                   ,'UNIFORMES') AND Fecha > '2008-01-01' AND ArticuloDesc NOT IN 
                   ('no existe viene mal de PICK','PASTA MANOS',' ...........  no usar ...............', '..', 
                   'PASTA MANOS ZORKIL-C35') AND FamiliaDesc = 'BOMBILLOS Y LAMPARAS' AND ArticuloDesc = 
                   'BOMBILLO 24V-5W ZEPELIN (N109)7552' group by Fecha")


#install.packages("forecast")
#install.packages("readxl")

library(readxl)
library(forecast)

#Creamos la serie temporal
tsdata<-ts(data2$TotalExistencias,frequency=365,start=c(2008,01,02), end=c(2020,05,27))
#Dibujamos la misma
#plot(tail(tsdata,300))
#creamos el modelo
modeloArima<-auto.arima(tsdata,D=1)
#la predicción a 30 días
forecast1<-forecast(modeloArima,h=30)

forecast1

#dibujamos la predicción a 2 meses
#plot(forecast1)
#plot(tail(forecast1,100))

#plot(forecast1$residuals)
#Sumario
summary(modeloArima)

accuracy(modeloArima)

plot(forecast1,include=180)




### Ahora con train y test set para ver la eficacia
data3 = dbGetQuery(con,"Select Fecha, sum(Existencias) TotalExistencias from Movimientos where 
                   FamiliaDesc NOT IN ('COMBUSTIBLES','ADITIVOS PARA LA COMBUSTION','LUBRICANTES',
                   'LÍQUIDOS REFRIGERANTES','CASCOS (NO INVENTARIABLE)','UTILES DE PERSONAL'
                   ,'UNIFORMES') AND Fecha > '2008-01-01' AND ArticuloDesc NOT IN 
                   ('no existe viene mal de PICK','PASTA MANOS',' ...........  no usar ...............', '..', 
                   'PASTA MANOS ZORKIL-C35') AND FamiliaDesc = 'BOMBILLOS Y LAMPARAS' AND ArticuloDesc = 
                   'BOMBILLO 24V-5W ZEPELIN (N109)7552' group by Fecha")

index_size <- floor(0.90 * nrow(data3))


train <- data3[1:index_size,]
test <- data3[index_size:nrow(data3),]


#install.packages("forecast")
#install.packages("readxl")

library(readxl)
library(forecast)

#Creamos la serie temporal
tsdata_all<-ts(data3$TotalExistencias,frequency=365,start=c(2008,01,02),end=c(2020,05,27))
tsdata_train<-ts(train$TotalExistencias,frequency=365,start=c(2008,01,02),end=c(2018,12,17))
#fecha_test = head(test,1)[0]
tsdata_test<-ts(test$TotalExistencias,frequency=365,start=c(2018,12,17),end=c(2020,05,27))
#Dibujamos la misma
#creamos el modelo
modeloArima1<-auto.arima(tsdata_train,D=1)
#la predicción a 2 años
forecast2<-forecast(modeloArima1,h=60)

#dibujamos la predicción a 2 meses
plot(head(tsdata_test,75))
#####-----------------------------####
plot(forecast2,include=20)



#plot(forecast1$residuals)
#Sumario
summary(modeloArima1)

accuracy(modeloArima1)





#------------------------------------NEUMATICOS----------------------------------
####Predicción a futuro
data4 = dbGetQuery(con,"Select Fecha, sum(Existencias) TotalExistencias from Movimientos
                        where FamiliaDesc NOT IN ('COMBUSTIBLES','ADITIVOS PARA LA COMBUSTION','LUBRICANTES',
                        'LÍQUIDOS REFRIGERANTES','CASCOS (NO INVENTARIABLE)','UTILES DE PERSONAL','UNIFORMES')
                        AND Fecha > '2008-01-01'
                        AND ArticuloDesc NOT IN ('no existe viene mal de PICK','PASTA MANOS',
                        ' ...........  no usar ...............', '..', 'PASTA MANOS ZORKIL-C35')
                        AND FamiliaDesc = 'NEUMATICOS NUEVOS' AND ArticuloDesc = 'CUBIERTA NUEVA 275/70 R-22.5 METROPOLITANO/URBANO'
                        group by Fecha")


#install.packages("forecast")
#install.packages("readxl")

library(readxl)
library(forecast)

#Creamos la serie temporal
tsdata1<-ts(data4$TotalExistencias,frequency=365,start=c(2012,09,10), end=c(2019,08,12))
#Dibujamos la misma
#plot(tail(tsdata,300))
#creamos el modelo
modeloArima2<-auto.arima(tsdata1,D=1)
#la predicción a 30 días
forecast3<-forecast(modeloArima2,h=30)

forecast3

#dibujamos la predicción a 2 meses
#plot(forecast1)
#plot(tail(forecast1,100))

#plot(forecast1$residuals)
#Sumario
summary(modeloArima2)

accuracy(modeloArima2)

plot(forecast3,include=180)




### Ahora con train y test set para ver la eficacia
data5 = dbGetQuery(con,"Select Fecha, sum(Existencias) TotalExistencias from Movimientos
                        where FamiliaDesc NOT IN ('COMBUSTIBLES','ADITIVOS PARA LA COMBUSTION','LUBRICANTES',
                        'LÍQUIDOS REFRIGERANTES','CASCOS (NO INVENTARIABLE)','UTILES DE PERSONAL','UNIFORMES')
                        AND Fecha > '2008-01-01'
                        AND ArticuloDesc NOT IN ('no existe viene mal de PICK','PASTA MANOS',
                        ' ...........  no usar ...............', '..', 'PASTA MANOS ZORKIL-C35')
                        AND FamiliaDesc = 'NEUMATICOS NUEVOS' AND ArticuloDesc = 'CUBIERTA NUEVA 275/70 R-22.5 METROPOLITANO/URBANO'
                        group by Fecha")

index_size1 <- floor(0.90 * nrow(data5))


train1 <- data5[1:index_size1,]
test1 <- data5[index_size1:nrow(data5),]


#install.packages("forecast")
#install.packages("readxl")

library(readxl)
library(forecast)

#Creamos la serie temporal
tsdata_all1<-ts(data5$TotalExistencias,frequency=365,start=c(2012,09,10),end=c(2019,08,12))
tsdata_train1<-ts(train1$TotalExistencias,frequency=365,start=c(2012,09,10),end=c(2018,11,26))
#fecha_test = head(test,1)[0]
tsdata_test1<-ts(test1$TotalExistencias,frequency=365,start=c(2018,11,26),end=c(2019,08,12))
#Dibujamos la misma
#creamos el modelo
modeloArima3<-auto.arima(tsdata_train1,D=1)
#la predicción a 2 años
forecast4<-forecast(modeloArima3,h=60)

#dibujamos la predicción a 2 meses
plot(head(tsdata_test1,75))
#####-----------------------------####
plot(forecast4,include=30)



#plot(forecast1$residuals)
#Sumario
summary(modeloArima3)

accuracy(modeloArima3)

