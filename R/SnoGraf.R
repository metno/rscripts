
SnoGraf <- function(StNr=18700, FAar=NA,TAar=NA,Aar=2018,AntAar=30){
  if (is.na(TAar)){TAar<-Aar-1}
  if (is.na(FAar)){FAar<-TAar-AntAar}
  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(TAar,"-12-31",sep="")
  MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F)[,4])
  FD <- paste(FAar-1,"-08-01",sep="")
  TD <- paste(FAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind((Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"))/86400,Data[,4])
  #print(FD)
  #print(D2)
  plot(D2[,1],D2[,2],type="l",col="gray",xlim=c(1,366),ylim=c(0,MSnow),ylab="Snødybde [cm]",xlab="",main=StNr)
  lines(c(0,0),c(-10,10000))#Aug
  lines(c(31,31),c(-10,10000))#Aug
  lines(c(61,61),c(-10,10000))#Sep
  lines(c(92,92),c(-10,10000))#Okt
  lines(c(122,122),c(-10,10000))#Nov
  lines(c(153,153),c(-10,10000))#Des
  lines(c(184,184),c(-10,10000),lwd=2)#Jan
  lines(c(212,212),c(-10,10000))#Feb
  lines(c(244,244),c(-10,10000))#Mar
  lines(c(274,274),c(-10,10000))#Apr
  lines(c(305,305),c(-10,10000))#Mai
  lines(c(335,335),c(-10,10000))#Jun
  lines(c(365,365),c(-10,10000))#Jun
  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind((Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"))/86400,Data[,4])
    lines(D2[,1],D2[,2],col="gray")
  }
  FD <- paste(Aar-1,"-08-01",sep="")
  TD <- paste(Aar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"),Data[,4])
  lines(D2[,1],D2[,2],col="red",lwd=2)
}

SnoGraf2 <- function(StNr=18700, FAar=NA,TAar=NA,Aar=2018,RefAar=NA,AntAar=30){
  if (is.na(TAar)){TAar<-Aar-1}
  if (is.na(FAar)){FAar<-TAar-AntAar}
  if (is.na(RefAar)){RefAar<-TAar}
  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(Aar,"-12-31",sep="") #Kan endres til TAar
  MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F)[,4])

  FD <- paste(FAar-1,"-08-01",sep="")
  TD <- paste(FAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind((Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"))/86400,Data[,4])
  plot(D2[,1],D2[,2],type="l",col="gray",xlim=c(1,366),ylim=c(0,MSnow),ylab="Snødybde [cm]",xlab="",main=StNr,
       sub=paste("Graå streker: ", FAar," - ", TAar, ", Blå strek: ", RefAar, ", Rød strek: ",Aar,sep=""))

  lines(c(0,0),c(-10,10000))#Aug
  lines(c(31,31),c(-10,10000))#Aug
  lines(c(61,61),c(-10,10000))#Sep
  lines(c(92,92),c(-10,10000))#Okt
  lines(c(122,122),c(-10,10000))#Nov
  lines(c(153,153),c(-10,10000),lwd=2)#Des
  lines(c(184,184),c(-10,10000))#Jan
  lines(c(212,212),c(-10,10000))#Feb
  lines(c(244,244),c(-10,10000))#Mar
  lines(c(274,274),c(-10,10000))#Apr
  lines(c(305,305),c(-10,10000))#Mai
  lines(c(335,335),c(-10,10000))#Jun
  lines(c(365,365),c(-10,10000))#Jun
  text(10,2,"Aug")
  text(41,2,"Sep")
  text(71,2,"Okt")
  text(103,2,"Nov")
  text(133,2,"Des")
  text(165,2,"Jan")
  text(195,2,"Feb")
  text(223,2,"Mar")
  text(255,2,"Apr")
  text(285,2,"Mai")
  text(316,2,"Jun")
  text(346,2,"Jul")


  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind((Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"))/86400,Data[,4])
    lines(D2[,1],D2[,2],col="gray")
  }
  text(10,2,"Aug")
  text(41,2,"Sep")
  text(71,2,"Okt")
  text(103,2,"Nov")
  text(133,2,"Des")
  text(165,2,"Jan")
  text(195,2,"Feb")
  text(223,2,"Mar")
  text(255,2,"Apr")
  text(285,2,"Mai")
  text(316,2,"Jun")
  text(346,2,"Jul")

  FD <- paste(TAar-1,"-08-01",sep="")
  TD <- paste(TAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind((Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"))/86400,Data[,4])
  lines(D2[,1],D2[,2],col="blue",lwd=2)

  FD <- paste(Aar-1,"-08-01",sep="")
  TD <- paste(Aar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"),Data[,4])
  lines(D2[,1],D2[,2],col="red",lwd=2)
}

#rm(list=ls())
library("httr")

Dogn.Laster <- function(start = "2017-01-01",
                        stop = "2017-01-31",
                        elements = c("sum(precipitation_amount P1D)"),
                        StNr = 18700,
                        Daglig=T){
  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  #sources    <- c("SN18700")
  #elements <- c("sum(precipitation_amount P1D)")  # Døgnnedbør RR
  #elements <- c("mean(air_temperature P1D)")     # Døgnetemperatur TAM
  #elements <- c("surface_snow_thickness P1D")     # Snødybde SA
  #start_date <- "2013-01-01T00:00"
  #stop_date  <- "2017-12-31T23:00"
  start_date <- paste(start,"T00:00",sep="")
  stop_date  <- paste(stop,"T23:00",sep="")
  sources <- paste("SN",StNr,sep="")

  url        <- paste("https://", client_id, "@frost.met.no/observations/v0.jsonld?",
                      "sources=", paste(sources,collapse=",") ,
                      "&referencetime=", start_date, "/", stop_date,
                      "&elements=", paste(elements,collapse=","),
                      sep = "", collapse= "")
  if(Daglig){url<-paste(url,"&timeresolutions=P1D",sep="")}


  system.time({
    xs <- scan(url, what="")
    print(object.size(xs))
    k  <- grep("referenceTime", xs)
    DataTable <- data.frame(Stnr=xs[k-2],param=xs[k+10], time=xs[k+2], value=as.numeric(gsub(",", "", xs[k+14])))
  })

  # Mer lesbart datoformat
  datetime <- strptime(x = as.character(DataTable$time),
                       format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
  DataTable$time <- datetime
  DataTable
}
