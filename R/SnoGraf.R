
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
  MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)[,4])

  FD <- paste(FAar-1,"-08-01",sep="")
  TD <- paste(FAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr,Daglig=F)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
  plot(D2[,1],D2[,2],type="l",col="gray",xlim=c(1,366),ylim=c(0,MSnow),ylab="Snødybde [cm]",xlab="",main=StNr,
       sub=paste("Grå streker: ", FAar," - ", TAar, ", Blå strek: ", RefAar, ", Rød strek: ",Aar,sep=""))

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


  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
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

  FD <- paste(RefAar-1,"-08-01",sep="")
  TD <- paste(RefAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
  lines(D2[,1],D2[,2],col="blue",lwd=2)

  FD <- paste(Aar-1,"-08-01",sep="")
  TD <- paste(Aar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
  lines(D2[,1],D2[,2],col="red",lwd=2)
}

SnoGraf2Ski <- function(StNr=18700, FAar=NA,TAar=NA,Aar=2018,RefAar=NA,AntAar=30,SkiFore=25){
  if (is.na(TAar)){TAar<-Aar-1}
  if (is.na(FAar)){FAar<-TAar-AntAar}
  if (is.na(RefAar)){RefAar<-TAar}
  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(Aar,"-12-31",sep="") #Kan endres til TAar
  MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)[,4])

  FD <- paste(FAar-1,"-08-01",sep="")
  TD <- paste(FAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr,Daglig=F)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
  plot(D2[,1],D2[,2],type="l",col="gray",xlim=c(1,366),ylim=c(0,MSnow),ylab="Snødybde [cm]",xlab="",main=StNr,
       sub=paste("Grå streker: ", FAar," - ", TAar, ", Blå strek: ", RefAar, ", Rød strek: ",Aar,sep=""))

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


  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    try(SnoGraf2Ski.Plotter(FD=FD,TD=TD,StNr=StNr),silent=T)
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

  FD <- paste(RefAar-1,"-08-01",sep="")
  TD <- paste(RefAar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
  lines(D2[,1],D2[,2],col="blue",lwd=2)

  FD <- paste(Aar-1,"-08-01",sep="")
  TD <- paste(Aar,"-07-31",sep="")
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
  Data[Data[,4]<0,4]<-NA
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
  lines(D2[,1],D2[,2],col="red",lwd=2)
  if(!is.na(SkiFore)){
    lines(c(-1000,1000),c(SkiFore,SkiFore),col="antiquewhite",lwd=2)
    text(c(30),c(25),"Skiføre (25 cm)",pos=3,offset = 0)
  }
}


SnoGraf2Ski.Plotter <- function(FD,TD,StNr){
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr);
  Data[Data[,4]<0,4]<-NA;
  D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4]);
  lines(D2[,1],D2[,2],col="gray")
}


SnoGraf.MaxSno <- function(StNr=18700,FAar=1961,TAar=2018,Tittel="Årlig maks snødybde på Oslo-Blindern",FokusAar=2018){

  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(TAar,"-12-31",sep="") #Kan endres til TAar
  MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)[,4])

  plot(c(-1000,10000),c(50,50),xlim = c(FAar,TAar),ylim=c(0,MSnow),main=Tittel,xlab="",ylab="",type = "l")


  Resultat <- c()
  for (Aar in FAar:TAar){
    FD <- paste(Aar,"-01-01",sep="")
    TD <- paste(Aar,"-12-31",sep="") #Kan endres til TAar
    MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)[,4])
    Resultat <- rbind(Resultat,
                      c(Aar,MSnow))
    PX <- c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)
    PY <- c(0,         MSnow,     MSnow,      0,         0)
    polygon(PX,PY,col = "antiquewhite")
  }

  MSnow <- Resultat[Resultat[,1]==FokusAar,2]
  Aar <- FokusAar

  PX <- c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)
  PY <- c(0,         MSnow,     MSnow,      0,         0)
  polygon(PX,PY,col = "antiquewhite",border="darkred",lwd=2)

  Resultat
}

SnoGraf.MaxSno2 <- function(StNr=18700,FAar=1961,TAar=2018,Tittel="Årlig maks snødybde på Oslo-Blindern",FokusAar=2018){

  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(TAar,"-12-31",sep="") #Kan endres til TAar
  MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)[,4])

  plot(c(-1000,10000),c(50,50),xlim = c(FAar,TAar),ylim=c(0,MSnow),main=Tittel,xlab="",ylab="",type = "l")


  Resultat <- c()
  for (Aar in FAar:TAar){
    FD <- paste(Aar,"-01-01",sep="")
    TD <- paste(Aar,"-12-31",sep="") #Kan endres til TAar
    MSnow <- max(Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)[,4])
    Resultat <- rbind(Resultat,
                      c(Aar,MSnow))
    PX <- c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)
    PY <- c(0,         MSnow,     MSnow,      0,         0)
    polygon(PX,PY,col = "antiquewhite")
    text(Aar,10,paste(Aar,": ",MSnow),srt=90)
  }

  MSnow <- Resultat[Resultat[,1]==FokusAar,2]
  Aar <- FokusAar

  PX <- c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)
  PY <- c(0,         MSnow,     MSnow,      0,         0)
  polygon(PX,PY,col = "antiquewhite",border="darkred",lwd=2)
  text(Aar,10,paste(Aar,": ",MSnow),srt=90,col="DarkRed")
  Resultat
}


SnoGraf.Klima <- function(StNr=18700, FAar=1961,TAar=2018){
  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(TAar,"-12-31",sep="") #Kan endres til TAar
  MSnow <- Dogn.Laster(elements = "max(surface_snow_thickness P1M)",start = FD,stop = TD,Daglig=F,StNr=StNr)
  MSnow <- cbind(as.numeric(strftime(MSnow[,3],format="%Y")),as.numeric(strftime(MSnow[,3],format="%m")),MSnow[,4])
  MMSnow <- max(MSnow[,3])
  print(MSnow)
  plot(MSnow[MSnow[,2]==1,1],MSnow[MSnow[,2]==1,3],type="l",ylim=c(0,MMSnow))
  farver <- rainbow(12)
  for(m in 1:12){
    lines(MSnow[MSnow[,2]==m,1],MSnow[MSnow[,2]==m,3],col=farver[m])
  }

}


SnoGraf.Klima2 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=c(1,10,25)){
  Resultat <- c()
  for(aar in FAar:TAar){
    R2 <- aar
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    for(SL in SLim){
      R2 <- c(R2, length(Data[Data[,4]>=SL & !is.na(Data[,4]),4]))
    }
    Resultat <- rbind(Resultat,R2)
  }
  YGrenser <- c(0, max(Resultat[,2]))
  Farver=rainbow(length(SLim))
  plot(Resultat[,1],Resultat[,2],type = "l",ylim=YGrenser,ylab = "Dager",xlab="",main=StNr,lwd=2)
  for(n in 1:length(SLim)){
    lines(Resultat[,1],Resultat[,n+1],col=Farver[n],lwd=2)
  }
  Resultat
}

SnoGraf.Klima3 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=1){
  Resultat <- c()
  plot(-1000,-1000,xlim = c(0,365),ylim = c(FAar,TAar),xlab="",ylab = "", main=StNr)
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

  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
    D2[D2[,2]<SLim] <- NA
    D2[,2] <- D2[,2]/D2[,2]*aar

    lines(D2[,1],D2[,2],col="blue",lwd=2)
  }
  text(10,FAar,"Aug")
  text(41,FAar,"Sep")
  text(71,FAar,"Okt")
  text(103,FAar,"Nov")
  text(133,FAar,"Des")
  text(165,FAar,"Jan")
  text(195,FAar,"Feb")
  text(223,FAar,"Mar")
  text(255,FAar,"Apr")
  text(285,FAar,"Mai")
  text(316,FAar,"Jun")
  text(346,FAar,"Jul")

}

SnoGraf.Klima4 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=1){
  Resultat <- c()
  plot(-1000,-1000,xlim = c(0,365),ylim = c(FAar,TAar),xlab="",ylab = "", main=StNr)
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

  for(aar in FAar:TAar){

    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")

    Data <- Dogn.Laster(elements = "mean(air_temperature P1D)",start = FD,stop = TD,StNr=StNr)
    D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
    D2[D2[,2]<0] <- NA
    D2[,2] <- D2[,2]/D2[,2]*(aar-0.3)
    lines(c(1,365),c((aar-0.3),(aar-0.3)),col="blue",lwd=2)
    lines(D2[,1],D2[,2],col="red",lwd=2)

    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
    D2[D2[,2]<SLim] <- NA
    D2[,2] <- D2[,2]/D2[,2]*aar
    lines(c(1,365),c((aar),(aar)),col="darkgreen",lwd=4)
    lines(D2[,1],D2[,2],col="antiquewhite",lwd=2)

  }
  text(10,FAar,"Aug")
  text(41,FAar,"Sep")
  text(71,FAar,"Okt")
  text(103,FAar,"Nov")
  text(133,FAar,"Des")
  text(165,FAar,"Jan")
  text(195,FAar,"Feb")
  text(223,FAar,"Mar")
  text(255,FAar,"Apr")
  text(285,FAar,"Mai")
  text(316,FAar,"Jun")
  text(346,FAar,"Jul")
}

SnoGraf.Klima5 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=1){
  Resultat <- c()
  plot(-1000,-1000,xlim = c(0,365),ylim = c(FAar,TAar),xlab="",ylab = "", main=StNr)
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
  R2 <- c()
  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
    D2[D2[,2]<SLim] <- NA
    D2[,2] <- D2[,2]/D2[,2]*aar

    lines(D2[,1],D2[,2],col="blue",lwd=2)

    R2 <- rbind(R2,
                c(aar,sum(D2[,2],na.rm = T)/aar))

  }
  lines(R2[,2],R2[,1],col="purple",lwd=2)

  text(10,FAar,"Aug")
  text(41,FAar,"Sep")
  text(71,FAar,"Okt")
  text(103,FAar,"Nov")
  text(133,FAar,"Des")
  text(165,FAar,"Jan")
  text(195,FAar,"Feb")
  text(223,FAar,"Mar")
  text(255,FAar,"Apr")
  text(285,FAar,"Mai")
  text(316,FAar,"Jun")
  text(346,FAar,"Jul")

}

SnoGraf.Klima6 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=1){
  Resultat <- c()
  plot(-1000,-1000,xlim = c(0,365),ylim = c(FAar,TAar),xlab="",ylab = "", main=StNr)
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
  R2 <- c()
  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    Data[Data[,4]<0,4]<-NA
    D2 <- cbind(as.numeric(Data[,3]-strptime(FD,format="%Y-%m-%d",tz="UTC"), units = "days"),Data[,4])
    D2[D2[,2]<SLim] <- NA
    D2[,2] <- D2[,2]/D2[,2]*aar

    R <- c(aar,sum(D2[,2],na.rm = T)/aar)
    R2 <- rbind(R2,R)
    PY <- c(R[1]-0.5, R[1]-0.5, R[1]+0.5, R[1]+0.5, R[1]-0.5)
    PX <- c(0,         R[2],     R[2],      0,         0)
    polygon(PX,PY,col = "antiquewhite")

    lines(D2[,1],D2[,2],col="blue",lwd=2)

  }
  #lines(R2[,2],R2[,1],col="purple",lwd=2)

  text(10,FAar,"Aug")
  text(41,FAar,"Sep")
  text(71,FAar,"Okt")
  text(103,FAar,"Nov")
  text(133,FAar,"Des")
  text(165,FAar,"Jan")
  text(195,FAar,"Feb")
  text(223,FAar,"Mar")
  text(255,FAar,"Apr")
  text(285,FAar,"Mai")
  text(316,FAar,"Jun")
  text(346,FAar,"Jul")

  R2
}

SnoGraf.Klima7 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=1){
  Resultat <- c()
  AAr<-TAar-FAar
  plot(-1000,-1000,xlim = c(0,365),ylim = c(0,AAr),
       xlab="",ylab = "Antalle ganger med snø over grense på gitt dato", main=StNr, xaxp = c(-100,10000,1))
  polygon(c(-100,-100,3650,3650,-100),c(0,AAr,AAr,0,0),col="DarkGreen")
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
  R2 <- c()
  D2 <- c()

  for(aar in FAar:TAar){
    FD <- paste(aar-1,"-08-01",sep="")
    TD <- paste(aar,"-07-31",sep="")
    Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
    Data[Data[,4]<SLim,4]<-0
    Data[Data[,4]>=SLim,4]<-1
    D2 <- rbind(D2,Data[1:365,4])
  }
  D2[is.na(D2)]<-0
  R2 <- colSums(D2)
  lines(1:365,R2)
  polygon(c(1:365,1),R2[c(1:365,1)],col = "wheat")

  text(10,3,"Aug")
  text(41,3,"Sep")
  text(71,3,"Okt")
  text(103,3,"Nov")
  text(133,3,"Des")
  text(165,3,"Jan")
  text(195,3,"Feb")
  text(223,3,"Mar")
  text(255,3,"Apr")
  text(285,3,"Mai")
  text(316,3,"Jun")
  text(346,3,"Jul")
  R2
}

SnoGraf.Klima8 <- function(StNr=18700, FAar=1961,TAar=2018, SLim=c(1,10,25)){
  Resultat <- c()
  AAr<-TAar-FAar+1
  plot(-1000,-1000,xlim = c(0,365),ylim = c(0,AAr),
       xlab="",ylab = "Antalle ganger med snø over grense på gitt dato",
       main=StNr, xaxp = c(-100,10000,1),
       sub=paste("01.08.",FAar-1," - 31.07.", TAar, sep="" ))
  polygon(c(-100,-100,3650,3650,-100),c(0,AAr,AAr,0,0),col="DarkGreen")
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
  farver <- c("wheat","antiquewhite","snow","white")
#  R2 <- c()
  for(n in 1:length(SLim)){
    D2 <- c()
    for(aar in FAar:TAar){
      FD <- paste(aar-1,"-08-01",sep="")
      TD <- paste(aar,"-07-31",sep="")
      Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
      Data<-cbind(Data,Data[,4])
      Data[Data[,4]<SLim[n],4]<-0
      Data[Data[,4]>=SLim[n],4]<-1
      D2 <- rbind(D2,Data[1:365,4])
    }
    D2[is.na(D2)]<-0
    R2 <- colSums(D2)
    lines(1:365,R2)
    polygon(c(0:365,0),c(0,R2[c(1:365)],0),col = farver[n])
  }
  text(10,3,"Aug")
  text(41,3,"Sep")
  text(71,3,"Okt")
  text(103,3,"Nov")
  text(133,3,"Des")
  text(165,3,"Jan")
  text(195,3,"Feb")
  text(223,3,"Mar")
  text(255,3,"Apr")
  text(285,3,"Mai")
  text(316,3,"Jun")
  text(346,3,"Jul")

  legend("topleft",
         legend=c(paste(">=",SLim," cm",sep="")),
         fill=c(farver[1:length(SLim)]),
         title="Tegnforklaring")
  R2
}



SoGraf.Snofall <- function(StNr=18700, FAar=1961,TAar=2018){
  FD <- paste(FAar,"-01-01",sep="")
  TD <- paste(TAar,"-12-31",sep="") #Kan endres til TAar
  Data <- Dogn.Laster(elements = "surface_snow_thickness",start = FD,stop = TD,StNr=StNr)
  LD <- length(Data[,4])
  SDiff <- Data[,4]-c(NA,Data[1:(LD-1),4])
  Data <- cbind(Data,SDiff)
  hist(Data[,5])
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
