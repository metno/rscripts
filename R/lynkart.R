
#register_google(< Skaff din egen API-key til Googlemaps>) Skaff din egen API-key til Googlemaps
require(RgoogleMaps)

Lynkart <- function(center= c(61, 8),zoom=6, StartDato = "11.07.2020",Dager=7){
  
  Kart <- GetMap(center=center, zoom=zoom, destfile = "kart.png")
  tmp <- PlotOnStaticMap(Kart, lat = 0, lon = 0, destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE)
  
  Datoer <- seq.Date(as.Date(StartDato,"%d.%m.%Y"),by="day",len=(Dager+1))
  
  for(D in 1:Dager){
    print(Datoer[D])
    Lenke <- paste("https://frost.met.no/lightning/v0.ualf?referencetime=", Datoer[D],"%2F",Datoer[D+1],sep="")
    Data <- c()
    try(Data <- read.table(Lenke,sep=" ",dec=".",header=F),silent = T)
    #https://frost.met.no/lightning/v0.ualf?referencetime=2020-07-28%2F2020-07-29
    if(length(Data>0)){
      tmp <- PlotOnStaticMap(Kart, lat = Data[,9], lon = Data[,10], destfile = "Kart1.png",pch=20, add=TRUE)
    }
  }
}
