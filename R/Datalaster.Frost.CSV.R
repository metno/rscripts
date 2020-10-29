#https://ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c@frost.met.no/observations/v0.csv?sources=sn18700&referencetime=R4/2009-12-01T06/2009-12-01T06/P1D&elements=air_temperature,min(air_temperature%20PT1H),max(air_temperature%20PT1H)&levels=default&timeoffsets=default&fields=sourceId,referenceTime,value,elementId

#Døgn
#https://ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c@frost.met.no/observations/v0.csv?sources=sn700&referencetime=2008-01-01/2008-01-05&elements=mean(air_temperature%20P1D)&levels=default&timeoffsets=default&fields=sourceId,referenceTime,value,elementId
Dgn.Laster <- function(StNr=18700,start="1990-01-01",slutt="2100-12-31",elementer="mean(air_temperature%20P1D)"){
  URL="https://"
  Bruker <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  Del1 <- "@frost.met.no/observations/v0.csv?sources=sn"
  #18700
  Del2 <- "&referencetime="
  Datoer<-paste(start,"/",slutt,sep="")
  Del3 <- "&elements="
  #mean(air_temperature%20P1M)
  Del4 <-"&levels=default&timeoffsets=default&levels=default&fields=sourceId,referenceTime,value,elementId"
  URL<-paste(URL,Bruker,Del1,StNr,Del2,Datoer,Del3,elementer,Del4,sep="")
  print(URL)
  Data <- read.table(URL,header=T,sep=",",dec=".")
  Data <- cbind(as.numeric(substr(Data[,2],1,4)),as.numeric(substr(Data[,2],6,7)),as.numeric(substr(Data[,2],9,10)),Data[,3])
  Data
}


#Måned
#https://ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c@frost.met.no/observations/v0.csv?sources=sn18700&referencetime=R3/2001-07-01/2001-09-01/P1Y&elements=mean(air_temperature%20P1M),min(air_temperature%20P1M),max(air_temperature%20P1M)&levels=default&timeoffsets=default&fields=sourceId,referenceTime,value,elementId
Mnd.Laster <- function(StNr=18700,start="1800-01-01",slutt="2100-12-31",elementer="mean(air_temperature%20P1M)"){
  URL="https://"
  Bruker <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  Del1 <- "@frost.met.no/observations/v0.csv?sources=sn"
  #18700
  Del2 <- "&referencetime=R3/"
  Datoer<-paste(start,"/",slutt,sep="")
  Del3 <- "/P1Y&elements="
  #mean(air_temperature%20P1M)
  Del4 <-"&levels=default&timeoffsets=default&levels=default&fields=sourceId,referenceTime,value,elementId"
  URL<-paste(URL,Bruker,Del1,StNr,Del2,Datoer,Del3,elementer,Del4,sep="")
  #print(URL)
  Data <- read.table(URL,header=T,sep=",",dec=".")
  Data <- cbind(as.numeric(substr(Data[,2],1,4)),as.numeric(substr(Data[,2],6,7)),Data[,3])
  Data
}
