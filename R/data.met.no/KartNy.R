library("httr")
require(RgoogleMaps)

client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
url1 <- "https://data.met.no/sources/v0.jsonld?types=SensorSystem&country=NO&fields=geometry"
xs1 <- scan(url1, what="")
k1  <- grep("id", xs1)
data.frame(Stnr=as.character(xs1[k1+2]),E=as.numeric(gsub(",", "", xs1[k1+14])),N=as.numeric(gsub(",", "", xs1[k1+15]))) -> Datasett
Datasett <- cbind(Datasett,NA,1)

Datasett <- Datasett[!is.na(Datasett[,2]),]
Datasett <- Datasett[!is.na(Datasett[,1]),]
LD <- length(Datasett[,1])

elements <- c("sum(precipitation_amount P1D)");
start_date <- "2017-10-01T00:00"
stop_date  <- "2017-10-01T23:00"

farver <- c("darkgray","black","darkgreen","darkblue","purple","darkred")

for (n in 1:LD){
  print(Datasett[n,1])
  url        <- paste("https://", client_id, "@data.met.no/observations/v0.jsonld?",
                    "sources=", paste(Datasett[n,1],collapse=",") ,
                    "&referencetime=", start_date, "/", stop_date,
                    "&elements=", paste(elements,collapse=","),
                    sep = "", collapse= "")
  xs <- NA
  try(xs <- scan(url, what=""),silent=T)
  if (!is.na(xs)){
    k <- grep("value", xs)
    verdi <- as.numeric(gsub(",", "", xs[k+2]))
    if (length(verdi>1)){verdi <- verdi[1]}
    Datasett[n,4] <- verdi}
}
Datasett <- Datasett[!is.na(Datasett[,4]),]
Datasett[Datasett[,4]<0,4]<-0



Kart <- GetMap(center=c(61,8), zoom=7, destfile = "kart.png")

Datasett[Datasett[,4]>0,5]<-2
Datasett[Datasett[,4]>25,5]<-3
Datasett[Datasett[,4]>50,5]<-4
Datasett[Datasett[,4]>75,5]<-5
Datasett[Datasett[,4]>100,5]<-6
TextOnStaticMap(Kart,lon=Datasett[,2],lat=Datasett[,3],col=farver[Datasett[,5]],labels=Datasett[,4],cex=0.7,add=FALSE)

