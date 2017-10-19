require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/DataLaster.R")
#require(maps)
#require(mapdata)
#require(mapproj)
#require(mapplots)
#require(rgl)
#require(rgdal)
require(sp)
require(RgoogleMaps)

#Sources i R via kommandoen:
#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/RegnKart.R")

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

ElementKart <- function(FD="15.11.2013",TD="17.11.2013",elementer="FFX",Stasjoner=NA,center= c(61, 8),zoom=6,MinGrense=NA,FS=NA,TS=NA){ 
  #FFX gir hÃ¸yeste middelvind, mens FGX gir hÃ¸yeste vindkast, RR gir nedbÃ¸rmengden, RR_R gir returperioden for nedbÃ¸rmengden, TAN og TAX gir min og max temp.
  #center= c(64.5, 10) #TrÃ¸ndelag
  #center= c(61, 8) #SÃ¸rnorge
  #Stasjoner kan settes eksplisitt om en Ã¸nsker et spesielt utvalg, ellers hentes det dynamisk inn for alle stasjoner som har observert pÃ¥ tidspunktet
  #zoom=6 #Kan justeres for Ã¥ sette ulike utsnitt, men er ikk trinnlÃ¸s
  
  Aar <- substr(FD,7,10)
  if (is.na(Stasjoner)){Stasjoner<- Stasjon.Laster(elementer=elementer,FY=Aar,TY=Aar)
  Sperrede <- c(89920, 86070) #put station number of stations that shall be excluded here
  LS <- length(Sperrede)
  for (n in LS){
    Stasjoner <- Stasjoner[Stasjoner[,6]!=Sperrede[n],]
  }
  if (!is.na(FS)){Stasjoner <- Stasjoner[Stasjoner[,6]>=FS,]}
  if (!is.na(TS)){Stasjoner <- Stasjoner[Stasjoner[,6]<=TS,]}
  Stasjoner <- Stasjoner[Stasjoner[,6]<=99989,]
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
    if(elementer=="RR_R"){try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=c("RR")),silent=TRUE)}
    if(elementer=="TAN"){try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=c("TAN")),silent=TRUE)}
    if(elementer=="TAX"){try(Data <- Dogn.Laster(StNr=St[n,1],FD=FD,TD=TD,elementer=c("TAX")),silent=TRUE)}
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
        if (VindMaks > (100+Startpunkt)){Farge="darkred"}
        if (VindMaks >= (75+Startpunkt) & VindMaks<(100+Startpunkt)){Farge="Purple"}
        if (VindMaks >= (50+Startpunkt) & VindMaks<(75+Startpunkt)){Farge="darkblue"}
        if (VindMaks >= (25+Startpunkt) & VindMaks<(50+Startpunkt)){Farge="darkgreen"}
      }
      if(elementer=="TAN"){
#        Data[is.na(Data[,5]),5]<-0
#        Data[Data[,5]<0,5]<-0
        VindMaks <- min(c(min(as.numeric(Data[,5]),na.rm=TRUE)),na.rm=TRUE)
        #Farver  settes her for vindkast, grenser i m/s
        Farge<-"gray19"
        #      if (VindMaks > 32.6*1.3){Farge="darkred"}
        #      if (VindMaks > 28.4*1.3 & VindMaks<32.7*1.3){Farge="Purple"}
        #      if (VindMaks > 24.4*1.3 & VindMaks<28.5*1.3){Farge="darkblue"}
        #      if (VindMaks > 20.7*1.3 & VindMaks<24.5*1.3){Farge="darkgreen"}
        Startpunkt <- 0
        if(!is.na(MinGrense)){Startpunkt<-MinGrense}
        if (VindMaks >  0){Farge="darkred"}
        if (VindMaks == 0){Farge="darkgreen"}
        if (VindMaks <  0){Farge="darkblue"}
      }
      if(elementer=="TAX"){
#        Data[is.na(Data[,5]),5]<-0
#        Data[Data[,5]<0,5]<-0
        VindMaks <- max(c(max(as.numeric(Data[,5]),na.rm=TRUE)),na.rm=TRUE)
        #Farver  settes her for vindkast, grenser i m/s
        Farge<-"gray19"
        #      if (VindMaks > 32.6*1.3){Farge="darkred"}
        #      if (VindMaks > 28.4*1.3 & VindMaks<32.7*1.3){Farge="Purple"}
        #      if (VindMaks > 24.4*1.3 & VindMaks<28.5*1.3){Farge="darkblue"}
        #      if (VindMaks > 20.7*1.3 & VindMaks<24.5*1.3){Farge="darkgreen"}
        Startpunkt <- 0
        if(!is.na(MinGrense)){Startpunkt<-MinGrense}
        if (VindMaks >  0){Farge="darkred"}
        if (VindMaks == 0){Farge="darkgreen"}
        if (VindMaks <  0){Farge="darkblue"}
      }
      if(elementer=="RR_R"){
        Data[is.na(Data[,5]),5]<-0
        Data[Data[,5]<0,5]<-0
        LD <- length(Data[,5])
        Nedbor.Mengde <- NA
        NedborMengde <- sum(c(sum(as.numeric(Data[,5]),na.rm=TRUE)),na.rm=TRUE)
        VindMaks <- NA
        if(is.finite(NedborMengde)){
          print(St[n,1])
          NyUrl <- "http://klapp/kdvhpub/production/Report?re=51&head=&ct=text/plain&del=space&ddel=comma&no=0&nod=blank&nob=dot&flag=7&sflag=-1&qa=4&la=no&co=NO&m1=0&m1=1&m1=11&m2=2&m2=3&m2=4&m3=5&m3=6&m3=7&m4=8&m4=9&m4=10&num=10&fac=1&nmt=1&lastperiod=true&pid=9&val="
          NyUrl <- paste(NyUrl,NedborMengde,"&s=",St[n,1],"&nd=",LD,sep="")
          #          NyUrl <- "http://klapp/kdvhpub/production/Report?re=51&head=&ct=text/plain&del=space&ddel=comma&no=0&nod=blank&nob=dot&flag=7&sflag=-1&qa=4&la=no&co=NO&m1=0&m1=1&m1=11&m2=2&m2=3&m2=4&m3=5&m3=6&m3=7&m4=8&m4=9&m4=10&num=10&nd=1&fac=1&nmt=1&lastperiod=true&pid=9&val="
          #          NyUrl <- paste(NyUrl,NedborMengde,"&s=",St[n,1],sep="")
          VindMaks <- NA
          try(VindMaks<-scan(file=NyUrl,skip=12,n=2)[2],silent=TRUE)
        }
        print(VindMaks) 
        
        #Farver  settes her for vindkast, grenser i m/s
        Farge<-"gray19"
        #      if (VindMaks > 32.6*1.3){Farge="darkred"}
        #      if (VindMaks > 28.4*1.3 & VindMaks<32.7*1.3){Farge="Purple"}
        #      if (VindMaks > 24.4*1.3 & VindMaks<28.5*1.3){Farge="darkblue"}
        #      if (VindMaks > 20.7*1.3 & VindMaks<24.5*1.3){Farge="darkgreen"}
        if(is.finite(VindMaks)){
          Startpunkt <- 0
          if(!is.na(MinGrense)){Startpunkt<-MinGrense}
          if (VindMaks > (100+Startpunkt)){Farge="darkred"}
          if (VindMaks >= (75+Startpunkt) & VindMaks<(100+Startpunkt)){Farge="Purple"}
          if (VindMaks >= (50+Startpunkt) & VindMaks<(75+Startpunkt)){Farge="darkblue"}
          if (VindMaks >= (25+Startpunkt) & VindMaks<(50+Startpunkt)){Farge="darkgreen"}
        }
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


