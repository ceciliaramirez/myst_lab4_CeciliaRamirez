# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()

# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 17
# Uso horario
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "EUR_USD"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
# S5, S10, S30, M1, M5, M15, M30, H1, H4, H8, D, M
OA_Pr <- "M1"
# Multiplicador de precios para convertir a PIPS
MultPip_MT1 <- 10000

#############################

suppressMessages(library(openxlsx))
Unemployment_Rate<- read.csv("history.csv")


Unemployment_Rate[,1] <- as.POSIXct(as.character(Unemployment_Rate[,1]),
                                    format = "%m/%d/%Y %H:%M",
                                    origin = "America/Guadalajara")

UR <- data.frame("Date"= Unemployment_Rate[,1],
                 "Actual"= Unemployment_Rate[,2],
                 "Consensus"= Unemployment_Rate[,3],
                 "Previous"= Unemployment_Rate[,4],
                 "Clasificacion" = 0)


#Clasificacion de observaciones
for(i in 1:length(UR$Date)){
  
  if(UR$Actual[i]>=UR$Consensus[i] & UR$Consensus[i]>=UR$Previous[i])
  {UR$Clasificacion[i] <-"A"}
  
  if(UR$Actual[i]>=UR$Consensus[i] & UR$Consensus[i]<UR$Previous[i])
  {UR$Clasificacion[i] <-"B"}
  
  if(UR$Actual[i]<UR$Consensus[i] & UR$Consensus[i]>=UR$Previous[i])
  {UR$Clasificacion[i] <-"C"}
  
  if(UR$Actual[i]<UR$Consensus[i] & UR$Consensus[i]<UR$Previous[i])
  {UR$Clasificacion[i] <-"D"}
  
  
}

###########################
Datos <- list()


for (i in 1:36){
  
  Fecha_Ejemplo <- UR$Date[[i]]
  aux <- UR$Clasificacion[[i]]
  
  
# Opcion 2 para convertir a "YYYY-MM-DD"
F2 <- as.Date(substr(Fecha_Ejemplo,1,10))
  
  
  if(wday(F2) != 1) 
  {
    
    Fecha1 <- F2
    Fecha2 <- F2+1
    Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                               DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                               Instrument = OA_In, 
                               Start = Fecha1, End = Fecha2, Count = NULL)
  } else {
    
    Fecha1 <- F2-2
    Fecha2 <- F2+1
    Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                               DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                               Instrument = OA_In, 
                               Start = Fecha1, End = Fecha2, Count = NULL)
  }
  
  Precios_Oanda$TimeStamp <- as.character(as.POSIXct(Precios_Oanda$TimeStamp,format = "%m/%d/%Y %H:%M:%S"))
  
  
  ind <- which(Precios_Oanda$TimeStamp == UR$Date[[i]])
  
  
  
  Datos[[i]] <- list("Clasificacion" = UR$Clasificacion[i],"Precios" = Precios_Oanda[(ind-15):(ind+15),])
  
  
  Calculos$Rend[[i]]<-Datos[,list(mean = mean(Datos[[i]]$Precios$Close)), by = Datos$Clasificacion]
  
  Calculos$Desv_s[[i]]<-Datos[,list(desv = stdev(Datos[[i]]$Precios$Close)), by = Datos$Clasificacion]
  Calculos$Dif[[i]]<-Datos[,list(dif= Datos$Precios$Close[[1]] - Datos$Precios$Close[[31]]), by = Datos$Clasificacion]
  Calculos$max[[i]]<-Datos[,list(max = max(Datos$Precios$Close)-min(Datos$Precios$Close)),by = Datos$Clasificacion] 
  
  
}
