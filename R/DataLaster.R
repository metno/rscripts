# A series of functions to get data from Klimadatabasen into R
#
#Original by Hans Olav Hygen

#Commands to use this directly from GIT:
#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/DataLaster.R")


#https://dokit.met.no/klima/userservices/urlinterface
#https://dokit.met.no/klima/userservices/urlinterface/brukerdok

Dogn.Laster <- function(StNr=18700, elementer = c("TAM","TAX","TAN"), FD = "01.01.2003", TD = "31.12.2003", Nod="NA"){
#Reads daily values
#http://klapp.oslo.dnmi.no/metnopub/production/metno?re=14&p=TAM&p=TAN&p=TAX&fd=13.11.2002&td=12.12.2004&ddel=dot&del=space&ct=text/plain&s=18700&nod=NA&split=1
  if (is.numeric(TD)){TD = paste("31.12.",TD,sep="")}
  if (is.numeric(FD)){FD = paste("01.01.",FD,sep="")}
  del1 <- "http://klapp.oslo.dnmi.no/metnopub/production/metno?re=14&ct=text/plain&del=semicolon&ddel=comma&split=1"
  del1 <- paste(del1,"&nod=",Nod,sep="")
  for (n in 1:length(StNr)){del1 <- paste(del1,"&s=",StNr[n],sep="")}
  del1 <- paste(del1,"&fd=",FD,"&td=",TD,sep="")
  for (n in 1:length(elementer)){del1 <- paste(del1,"&p=",elementer[n],sep="")}
#  print(del1)
#  Dataserie <- read.table(del1, header = TRUE, sep = ";",  dec = ",", na.strings = "NA", skip=1, colClasses="numeric")
  Dataserie <- read.table(del1, header = TRUE, sep = ";",  dec = ",", na.strings = "NA", skip=0, colClasses="numeric", stringsAsFactors=FALSE)
  Dataserie
}

Dogn.Laster2 <- function(StNr=18700, elementer = c("TAM","TAX","TAN"), FD = "01.01.2003", TD = "31.12.2003", Nod="NA"){
#Reads daily values
#http://klapp.oslo.dnmi.no/metnopub/production/metno?re=14&p=TAM&p=TAN&p=TAX&fd=13.11.2002&td=12.12.2004&ddel=dot&del=space&ct=text/plain&s=18700&nod=NA&split=1
  Dataserie <- c()
  if (!is.numeric(TD)){TDAar <- as.numeric(substr(TD,7,10))}
  if (!is.numeric(FD)){FDAar <- as.numeric(substr(FD,7,10))}
  if ( is.numeric(TD)){TDAar <- TD}
  if ( is.numeric(FD)){FDAar <- FD}
  for (Aar in FDAar:TDAar){TD <- paste("31.12.",Aar,sep="");
                           FD <- paste("01.01.",Aar,sep="")
                           del1 <- "http://klapp.oslo.dnmi.no/metnopub/production/metno?re=14&ct=text/plain&del=semicolon&ddel=comma&split=1"
                           del1 <- paste(del1,"&nod=",Nod,sep="")
                           for (n in 1:length(StNr)){del1 <- paste(del1,"&s=",StNr[n],sep="")}
                           del1 <- paste(del1,"&fd=",FD,"&td=",TD,sep="")
                           for (n in 1:length(elementer)){del1 <- paste(del1,"&p=",elementer[n],sep="")}
                           D2 <- read.table(del1, header = TRUE, sep = ";",  dec = ",", na.strings = "NA", skip=1, stringsAsFactors=FALSE)
#                           print(c(Aar,dim(D2),dim(Dataserie)))
                           Dataserie <- rbind(Dataserie,D2)}
  Dataserie
}

Maaned.Laster <- function(StNr=18700, elementer = c("TAM","TAX","TAN"), FD = "01.01.2003", TD = "31.12.2003"){
#Reads monthly values
#http://klapp.oslo.dnmi.no/metnopub/production/metno?re=15&p=TAM&p=TAN&p=TAX&fd=13.11.2002&td=12.12.2004&ddel=dot&del=space&ct=text/plain&s=18700&nod=NA&split=1
  if (is.numeric(TD)){TD = paste("31.12.",TD,sep="")}
  if (is.numeric(FD)){FD = paste("01.01.",FD,sep="")}
  del1 <- "http://klapp.oslo.dnmi.no/metnopub/production/metno?re=15&&ct=text/plain&del=semicolon&ddel=comma&nod=NA&split=1"
  for (n in 1:length(StNr)){del1 <- paste(del1,"&s=",StNr[n],sep="")}
  del1 <- paste(del1,"&fd=",FD,"&td=",TD,sep="")
  for (n in 1:length(elementer)){del1 <- paste(del1,"&p=",elementer[n],sep="")}
  #print(del1)
  Datasett <- read.table(del1, header = TRUE, sep = ";",  dec = ",", na.strings = "NA", stringsAsFactors=FALSE)

  Datasett
}

Stasjon.Laster <- function(elementer="TAM", FY = 1971, TY = 2000){
#Reads metadata about the stations
  #http://klapp.oslo.dnmi.no/metnopub/production/metno?re=16&ct=text/plain&p=TAM&TY=2000&FY=1971
  #http://klapp.oslo.dnmi.no/metnopub/production/metno?re=16&ct=text/plain&p=TAM&FY=1961&TY=1990
  del1 <- "http://klapp.oslo.dnmi.no/metnopub/production/metno?re=16&ct=text/plain&del=semicolon"
  for (n in 1:length(elementer)){del1 <- paste(del1,"&p=",elementer[n],sep="")}
  #del1 <- paste(del1,"&fy=",FY,sep="")
  del1 <- paste(del1,"&FY=",FY,"&TY=",TY,sep="")
  #print(del1)
  Datasett <- read.table(del1, header = TRUE, sep = ";",  dec = ",", na.strings = "NA", skip=1, stringsAsFactors=FALSE)
  Datasett
}

Obs.Laster <- function(StNr=18700, elementer = c("TA"), FD = "01.01.2003", TD = "31.12.2003", tider=c(0,6,12,18), Auto=1){
#Reads the observations
  #http://klapp.oslo.dnmi.no/metnopub/production/metno?re=17&p=VV&fd=13.11.2003&td=12.12.2003&h=0&h=6&nmt=0&ddel=dot&del=;&ct=text/plain&x=1&s=18700&nod=NAN
  del1 <- "http://klapp.oslo.dnmi.no/metnopub/production/metno?re=17&nmt=0&ddel=dot&del=semicolon&ct=text/plain&x=1&nod=NA"
  if (is.numeric(TD)){TD = paste("31.12.",TD,sep="")}
  if (is.numeric(FD)){FD = paste("01.01.",FD,sep="")}
  if (Auto==1){del1 <- paste(del1,"&dup=A",sep="")}
  if (Auto!=1){del1 <- paste(del1,"&dup=V",sep="")}
  for (n in 1:length(StNr)){del1 <- paste(del1,"&s=",StNr[n],sep="")}
  del1 <- paste(del1,"&fd=",FD,"&td=",TD,sep="")
  for (n in 1:length(elementer)){del1 <- paste(del1,"&p=",elementer[n],sep="")}
  for (n in 1:length(tider)){del1 <- paste(del1,"&h=",tider[n],sep="")}
#  print(del1)
#  Datasett <- read.table(del1, header = TRUE, sep = ";",  dec = ".", na.strings = "NA", skip=2,colClasses="numeric")
  Datasett <- read.table(del1, header = TRUE, sep = ";",  dec = ".", na.strings = "NA", skip=0,colClasses="numeric", stringsAsFactors=FALSE)
#  Datasett <- read.table(del1, header = TRUE, sep = ";",  dec = ".", na.strings = "NA", skip=3,colClasses="numeric")
  #print(del1)
  if (Datasett[1,1]=="Stnr"){LD <- length(Datasett[,1]);Datasett <- Datasett[2:LD,]}
  Datasett
}

Obs.Laster.Stor <- function(StNr=18700, elementer = c("TA"), FD = 2005, TD = 2008, tider=0:23, Auto=0){
#Reads observations if the above function fails due to size
  Datasett <- c()
  for (Aar in FD:TD){
    try(Datasett <- rbind(Datasett,Obs.Laster(StNr=StNr,elementer=elementer,FD=Aar,TD=Aar,tider=tider,Auto=Auto)),silent=TRUE)
  }
  Datasett
}

DognNormal.Laster <- function(StNr=18700){
#Reads the daily normals
  filnavn <- paste("http://klapp.oslo.dnmi.no/kdvhpub/production/Report?re=58&head=&ct=text/plain&del=semicolon&ddel=comma&no=0&nod=blank&nob=dot&flag=7&sflag=-1&qa=5&la=no&co=NO&p=TAM&nmt=1&s=",StNr,sep="")
  Datasett <- read.table(file=filnavn, skip = 14, nrows=366,header=FALSE,sep=";",dec=",",stringsAsFactors=FALSE)
  Datasett
}

Region.Laster <- function(region=0,rType="GR",m=c(0,1:12,21:24),FAar=1901,TAar=2010,element="TAMA"){
#Reads the regional data
  #http://klapp/image/landsdelkart.jpg
  #http://klapp/metnopub/production/metno?re=20&ct=text/plain&del=semicolon&ddel=dot&p=TAMA&fy=1950&ty=2000&m=0&m=21&m=22&m=23&m=24&r_type=TR&r_no=1
  #http://klapp/metnopub/production/metno?re=20&ct=text/plain&del=semicolon&ddel=dot&p=TAMA&fy=1901&ty=2010&m=1&r_type=TR&r_no=0
  Lenke <- "http://klapp/metnopub/production/metno?re=20&ct=text/plain&del=semicolon&ddel=dot"
  Lenke <- paste(Lenke,"&p=",element,sep="")
  Lenke <- paste(Lenke,"&fy=",FAar,sep="")
  Lenke <- paste(Lenke,"&ty=",TAar,sep="")
  for(n in 1:length(m)){
    Lenke <- paste(Lenke,"&m=",m[n],sep="")
  }
  Lenke <- paste(Lenke,"&r_type=",rType,sep="")
  Lenke <- paste(Lenke,"&r_no=",region,sep="")
  #print(Lenke)
  Datasett <- read.table(file=Lenke,header=TRUE,skip=1,sep=";",dec=".",stringsAsFactors=FALSE)
  Datasett
}
