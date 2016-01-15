#require(maps)
#require(mapdata)
#require(mapproj)
#require(mapplots)
#require(rgl)
#require(rgdal)
#require(sp)
require(RgoogleMaps)

setwd("~/SKI_lek")

#Funksjonene bruker parametrene Y, M, D, D1, D2, H, Senter, Zoom:
# Y: Året, fire siffer
# M: Månednummeret
# D: Dagen i måneden
# D1: Første dag i en periode (Brukes kun i Lynkart_Flere_Dogn)
# D2: Siste dag i en periode (Brukes kun i Lynkart_Flere_Dogn)
# H: Time (Brukes kun i Lynkart_Time)
# Senter: Senterpunktet
# Zoom: zoom, heltall fra 0 og oppover

Lynkart_time_leser <- function(Y=2016,M=1,D=15,H=6){
  Sti <- c("E://lyndata/UALF/")
  if(M<10){M <- paste("0",M,sep="")}
  if(D<10){D <- paste("0",D,sep="")}
  if(H<10){H <- paste("0",H,sep="")}
  Sti <- paste(Sti,Y,"/",M,"/",sep="")
  fil <- paste("lyndata",Y,M,D,H,sep="")
  filnavn <- paste(Sti,fil,sep="")
#  print(filnavn)
  Data <- read.table(filnavn)
}

Lynkart_time <- function(Y=2016,M=1,D=15,H=6,Senter=c(61, 8),zoom=6){
  Data <- c()
  try(Data <- Lynkart_time_leser(Y=Y,M=M,D=D,H=H),silent=TRUE)
  #print(Data[,9:10])
  if(is.null(dim(Data))){}
  if(!is.null(dim(Data))){
    #Posisjoner <- Data[,c(9,10)]
    Kart <- GetMap(center=Senter, zoom=zoom, destfile = "kart.png")
    PlotOnStaticMap(Kart,lon=Data[,10],lat=Data[,9],destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE, col="red")
  }
}

Lynkart_Dogn <- function(Y=2016,M=1,D=15,Senter=c(61, 8),zoom=6){
  Kart <- GetMap(center=Senter, zoom=zoom, destfile = "kart.png")
  PlotOnStaticMap(Kart,lon=0,lat=0,destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE, col="red")
  for (H in 0:23){
    Data <- c()
    try(Data <- Lynkart_time_leser(Y=Y,M=M,D=D,H=H),silent=TRUE)
    #print(Data[,9:10])
    if(is.null(dim(Data))){}
    if(!is.null(dim(Data))){
      #Posisjoner <- Data[,c(9,10)]
      PlotOnStaticMap(Kart,lon=Data[,10],lat=Data[,9],destfile = "Kart1.png", cex=1.5,pch=20, add=TRUE, col="red")
    }
  }
}

Lynkart_Flere_Dogn <- function(Y=2016,M=1,D1=11,D2=14,Senter=c(61, 8),zoom=6){
  Kart <- GetMap(center=Senter, zoom=zoom, destfile = "kart.png")
  PlotOnStaticMap(Kart,lon=0,lat=0,destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE, col="red")
  for(D in D1:D2){
    for (H in 0:23){
      Data <- c()
      try(Data <- Lynkart_time_leser(Y=Y,M=M,D=D,H=H),silent=TRUE)
      #print(Data[,9:10])
      if(is.null(dim(Data))){}
      if(!is.null(dim(Data))){
        #Posisjoner <- Data[,c(9,10)]
        PlotOnStaticMap(Kart,lon=Data[,10],lat=Data[,9],destfile = "Kart1.png", cex=1.5,pch=20, add=TRUE, col="red")
      }
    }
  }
}
