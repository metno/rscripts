
#register_google(key="AIzaSyAcpkXcEV9cGmF0NsvGQw30NmJTQiHkwnU",write = T) Skaff din egen API-key til Googlemaps
require(RgoogleMaps)

Lynkart <- function(center= c(61, 8),zoom=6, StartDato = "11.07.2020",Dager=7){
  
  Kart <- GetMap(center=center, zoom=zoom, destfile = "kart.png")
  tmp <- PlotOnStaticMap(Kart, lat = 0, lon = 0, destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE)
  
  Datoer <- seq.Date(as.Date(StartDato,"%d.%m.%Y"),by="day",len=(Dager+1))
  
  for(D in 1:Dager){
    print(Datoer[D])
    Lenke <- paste("https://ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c@frost.met.no/lightning/v0.ualf?referencetime=", Datoer[D],"%2F",Datoer[D+1],sep="")
    Data <- c()
    try(Data <- read.table(Lenke,sep=" ",dec=".",header=F),silent = T)
    #https://frost.met.no/lightning/v0.ualf?referencetime=2020-07-28%2F2020-07-29
    if(length(Data>0)){
      tmp <- PlotOnStaticMap(Kart, lat = Data[,9], lon = Data[,10], destfile = "Kart1.png",pch=20, add=TRUE)
    }
  }
}

Lynkart.farve <- function(center= c(61, 8),zoom=6, StartDato = "11.07.2020",Dager=7,MndNavn=T){
  
  Kart <- GetMap(center=center, zoom=zoom, destfile = "kart.png")
  tmp <- PlotOnStaticMap(Kart, lat = 0, lon = 0, destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE)
  
  Datoer <- seq.Date(as.Date(StartDato,"%d.%m.%Y"),by="day",len=(Dager+1))
  
  farver<-rainbow(12)
  
  for(D in 1:Dager){
    print(Datoer[D])
    Lenke <- paste("https://ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c@frost.met.no/lightning/v0.ualf?referencetime=", Datoer[D],"%2F",Datoer[D+1],sep="")
    Data <- c()
    try(Data <- read.table(Lenke,sep=" ",dec=".",header=F),silent = T)
    #https://frost.met.no/lightning/v0.ualf?referencetime=2020-07-28%2F2020-07-29
    if(length(Data>0)){
      tmp <- PlotOnStaticMap(Kart, lat = Data[,9], lon = Data[,10], destfile = "Kart1.png",pch=20, add=TRUE, col=farver[Data[,3]])
    }
    Maaneder=c("Januar","Februar","Mars","April","Mai","Juni","Juli","August","September","Oktober","November","Desember")
    if (MndNavn & zoom>=4){
      Mnd <- as.numeric(substr(StartDato,4,5))
      TextOnStaticMap(Kart,lat=71,lon=0,labels=Maaneder[Mnd],add=TRUE,cex=2,col=farver[Mnd])
    }
  }
  
}


#Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.01.2019",Dager=31);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.02.2019",Dager=28);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.03.2019",Dager=31);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.04.2019",Dager=30);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.05.2019",Dager=31);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.06.2019",Dager=30);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.07.2019",Dager=31);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.08.2019",Dager=31);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.09.2019",Dager=30);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.10.2019",Dager=31);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.11.2019",Dager=30);Lynkart.farve(zoom=4,center=c(63,10),StartDato = "01.12.2019",Dager=31)
