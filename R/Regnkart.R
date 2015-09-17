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

RegnKart <- function(FD="28.10.2014",TD="29.10.2014",elementer="RR",Stasjoner=NA){
  Aar <- substr(FD,7,10)
  if (is.na(Stasjoner)){Stasjoner<- Stasjon.Laster(elementer=elementer,FY=Aar,TY=Aar)}
  Stasjoner <- Stasjoner[Stasjoner[,6]!=Sperrede[n],]
  St <- Stasjoner[,c(6,10,11)]
  print(St)
  ls <- length(St[,1])
  #center= c(64.5, 10) #Trøndelag
  center= c(61, 8) #Sørnorge
  center= c(61, 5) #Sørnorge
  zoom=7
  Kart <- GetMap(center=center, zoom=zoom, destfile = "kart.png")
  tmp <- PlotOnStaticMap(Kart, lat = 0, lon = 0, destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE)
  
  for (n in 1:ls){
    #print(St[n,1])
    Data<-c()
    try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=elementer),silent=TRUE)
    #print(St[n,1])
    #print(length(Data))
    if(length(Data>0)){
      RegnMaks <- max(c(as.numeric(Data[,5]),na.rm=TRUE))
      
      print(RegnMaks)
      #Farver
      Farge<-"darkblue"
      if(is.finite(RegnMaks)){if (RegnMaks < 50){Farge="gray"}}
      #if (RegnMaks > 32.6){Farge="darkred"}
      #if (RegnMaks > 28.4 & RegnMaks<32.7){Farge="Purple"}
      #if (RegnMaks > 24.4 & RegnMaks<28.5){Farge="darkblue"}
      #if (RegnMaks > 20.7 & RegnMaks<24.5){Farge="darkgreen"}
      
#      TextOnStaticMap(Kart,lon=as.numeric(St$LON_DEC[n]),lat=as.numeric(St$LAT_DEC[n]),col='red',labels=RegnMaks,adj=c(+1.5),cex=0.5,add=TRUE)
      if(is.finite(RegnMaks)){
        TextOnStaticMap(Kart,lon=as.numeric(St$LON_DEC[n]),lat=as.numeric(St$LAT_DEC[n]),col=Farge,labels=RegnMaks,cex=0.7,add=TRUE)
      }
    }
  }
}

