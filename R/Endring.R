require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.R")
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/dognnormaler.R")

Endring <- function(StNr=18700,FAar1=1961,FAar2=1991,TAar1=1990,TAar2=2020){
#  if (is.na(Navn)){Navn<-StNr}
#  Maaneder <- c("Januar","Februar","Mars","April","Mai","Juni","Juli","August","September","Oktober","November","Desember")
#  Overskrift <- paste(Navn,", ",Maaneder[Mnd],sep="")
#  Mnd<-Mnd+1
  start <- paste(FAar1,"-01-01",sep="")
  stopp <- paste(TAar2,"-12-31",sep="")
  Data <- Data.Laster(StNr=StNr,start=start,stop=stopp,elements = "mean(air_temperature P1M)",Daglig=F)
  Data<-cbind(as.numeric(format(Data[,3],format="%m")), as.numeric(format(Data[,3],format="%Y")),Data[,3:4])
  D1 <- c()
  D2 <- c()
  for(n in 1:12){
    D1 <- c(D1, mean(Data[Data[,1]==n & Data[,2]>=FAar1 & Data[,2]<=TAar1,4]))
    D2 <- c(D2, mean(Data[Data[,1]==n & Data[,2]>=FAar2 & Data[,2]<=TAar2,4]))
  }
  #print(D2)
  Dogn1 <- Lag.Dogn.Normal(D1)
  Dogn2 <- Lag.Dogn.Normal(D2)
  ygrense <- c(range(c(Dogn1,Dogn2)))
  plot(c(1:365),Dogn1,type="l",col="Darkgreen",ylim = ygrense,main=StNr,xlab="Dager",ylab="Temperatur")
  polygon(c(-100,-100,500,500,-100),c(10,100,100,10,10),col="palegreen",border = "springgreen")
  polygon(c(-100,-100,500,500,-100),c(10,0,0,10,10),col="wheat1",border = "wheat1")
  polygon(c(-100,-100,500,500,-100),c(0,-100,-100,0,0),col="paleturquoise",border = "paleturquoise")
  #lines(c(-1000,1000),c(10,10),lty=3)
  lines(c(1:365),Dogn1,col="Darkgreen",lwd=2)
  lines(c(1:365),Dogn2,col="Darkred",lwd=2)
  print(c("Dager med sommer",length(Dogn1[Dogn1>10]),length(Dogn2[Dogn2>10])))
  print(c("Dager med vinter",length(Dogn1[Dogn1<0]),length(Dogn2[Dogn2<0])))
  print(c("Makstemp.",max(round(Dogn1,1)),round(max(Dogn2),1)))
  print(c("Mintemp.",min(round(Dogn1,1)),round(min(Dogn2),1)))
  
}
