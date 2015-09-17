require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/DataLaster.R")
require(maps)
require(mapdata)
require(mapproj)
require(mapplots)
require(rgl)
require(rgdal)
require(sp)
require(RgoogleMaps)

#Sources i R via kommandoen:
#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/VindKart.R")

#For a set of tiles that give a national coverage the setup for extreme weather "Ole" might be used:
#Southern Norway
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(61,8),elementer="FFX")
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(61,8),elementer="FGX")
#Nordland
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(66,14),elementer="FGX")
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(66,14),elementer="FFX")
#Troms
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(69,18),elementer="FFX")
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(69,18),elementer="FGX")
#Finnmark
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(69,25),elementer="FGX")
#VindKart(FD="07.02.2015",TD="08.02.2015",center=c(69,25),elementer="FFX")

VindKart <- function(FD="15.11.2013",TD="17.11.2013",elementer="FFX",Stasjoner=NA,center= c(61, 8),zoom=6,MinGrense=NA){ 
  #FFX gir høyeste middelvind, mens FGX gir høyeste vindkast
  #center= c(64.5, 10) #Trøndelag
  #center= c(61, 8) #Sørnorge
  #Stasjoner kan settes eksplisitt om en ønsker et spesielt utvalg, ellers hentes det dynamisk inn for alle stasjoner som har observert på tidspunktet
  #zoom=6 #Kan justeres for å sette ulike utsnitt, men er ikk trinnløs

  Aar <- substr(FD,7,10)
  if (is.na(Stasjoner)){Stasjoner<- Stasjon.Laster(elementer=elementer,FY=Aar,TY=Aar)
    Sperrede <- c(89920, 86070) #put station nuber of stations that shall be excluded here
    LS <- length(Sperrede)
    for (n in LS){
      Stasjoner <- Stasjoner[Stasjoner[,6]!=Sperrede[n],]
    }
    Stasjoner <- Stasjoner[Stasjoner[,6]<=99999,]
  }
  St <- Stasjoner[,c(6,10,11)]
  ls <- length(St[,1])
  Kart <- GetMap(center=center, zoom=zoom, destfile = "kart.png")
  tmp <- PlotOnStaticMap(Kart, lat = 0, lon = 0, destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE)
  
  for (n in 1:ls){
    #print(St[n,1])
    Data<-c()
    if(elementer=="FFX"){try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=c("FFX","FXX")),silent=TRUE)}
    if(elementer=="FGX"){try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=c("FGX")),silent=TRUE)}
    if(elementer=="RR"){try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=c("RR")),silent=TRUE)}
    #print(St[n,1])
    #print(length(Data))
    if(length(Data>0)){
      if(elementer=="FFX"){
        VindMaks <- max(c(max(c(as.numeric(Data[,5]),na.rm=TRUE)),max(c(as.numeric(Data[,6]),na.rm=TRUE))),na.rm=TRUE)
      #Farver settes her for middelvind, grenser i m/s
        Farge<-"gray19"
        if (VindMaks > 32.6){Farge="darkred"}
        if (VindMaks > 28.4 & VindMaks<32.7){Farge="Purple"}
        if (VindMaks > 24.4 & VindMaks<28.5){Farge="darkblue"}
        if (VindMaks > 20.7 & VindMaks<24.5){Farge="darkgreen"}
      print(c(St[n,1],Farge,VindMaks))
      }
      if(elementer=="FGX"){
        VindMaks <- max(c(max(as.numeric(Data[,5]),na.rm=TRUE)),na.rm=TRUE)
        #Farver  settes her for vindkast, grenser i m/s
        Farge<-"gray19"
        #      if (VindMaks > 32.6*1.3){Farge="darkred"}
        #      if (VindMaks > 28.4*1.3 & VindMaks<32.7*1.3){Farge="Purple"}
        #      if (VindMaks > 24.4*1.3 & VindMaks<28.5*1.3){Farge="darkblue"}
        #      if (VindMaks > 20.7*1.3 & VindMaks<24.5*1.3){Farge="darkgreen"}
        if (VindMaks > 44.9){Farge="darkred"}
        if (VindMaks > 39.9 & VindMaks<45){Farge="Purple"}
        if (VindMaks > 34.9 & VindMaks<40){Farge="darkblue"}
        if (VindMaks > 29.9 & VindMaks<35){Farge="darkgreen"}
      }
      if(elementer=="RR"){
        Data[is.na(Data[,5]),5]<-0
        Data[Data[,5]<0,5]<-0
        VindMaks <- sum(c(sum(as.numeric(Data[,5]),na.rm=TRUE)),na.rm=TRUE)
        #Farver  settes her for vindkast, grenser i m/s
        Farge<-"gray19"
        #      if (VindMaks > 32.6*1.3){Farge="darkred"}
        #      if (VindMaks > 28.4*1.3 & VindMaks<32.7*1.3){Farge="Purple"}
        #      if (VindMaks > 24.4*1.3 & VindMaks<28.5*1.3){Farge="darkblue"}
        #      if (VindMaks > 20.7*1.3 & VindMaks<24.5*1.3){Farge="darkgreen"}
        Startpunkt <- 0
        if(!is.na(MinGrense)){Startpunkt<-MinGrense}
        if (VindMaks > (100+Startpunkt)){Farge="black"}
        if (VindMaks >= (75+Startpunkt) & VindMaks<(100+Startpunkt)){Farge="Purple"}
        if (VindMaks >= (50+Startpunkt) & VindMaks<(75+Startpunkt)){Farge="darkblue"}
        if (VindMaks >= (25+Startpunkt) & VindMaks<(50+Startpunkt)){Farge="darkgreen"}
      }
#      TextOnStaticMap(Kart,lon=as.numeric(St$LON_DEC[n]),lat=as.numeric(St$LAT_DEC[n]),col='red',labels=VindMaks,adj=c(+1.5),cex=0.5,add=TRUE)
      if(is.finite(VindMaks)){
        if(is.na(MinGrense)|VindMaks>=MinGrense){
          TextOnStaticMap(Kart,lon=as.numeric(St$LON_DEC[n]),lat=as.numeric(St$LAT_DEC[n]),col=Farge,labels=VindMaks,cex=0.7,add=TRUE)
        }
      }
    }
  }
}
