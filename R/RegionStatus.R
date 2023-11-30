setwd("~/Rscript/RegionStatus")
source("RegionHenter.R")

RegStatus <- function(Data = NA, TAM = F, RR = T, Reg=0, Aar = 2022, Mnd=11, Prog=T, NormPeriode = c(1991,2020) ){
  if (is.na(Data)){Data<-RegionHenter()}
  if (TAM == T & RR == F){RegStatus.Temperatur(Data=Data,Reg=Reg,Aar=Aar,Mnd=Mnd,Prog=Prog,TAM=TAM,RR=RR)}
  if (TAM == F & RR == T){RegStatus.Nedbor(Data=Data,Reg=Reg,Aar=Aar,Mnd=Mnd,Prog=Prog,TAM=TAM,RR=RR)}
  if (TAM == T & RR == T){RegStatus.TempRegn(Data=Data,Reg=Reg,Aar=Aar,Mnd=Mnd,Prog=Prog,TAM=TAM,RR=RR)}
}

RegStatus.Temperatur <- function(Data, Reg=0, Aar = 2022, Mnd=11, Prog=T, NormPeriode = c(1991,2020),TAM=T,RR=F){
  Data <- Data[Data[,1]==Reg,c(2,3,4)]
  D1 <- RegStatus.Omformer(Data)
  D2 <- RegStatus.Temperatur.Beregner(D1)
  if(Prog){
    D3 <- RegStatus.Temperatur.Prognose(D1,D2,Aar,Mnd)
  }
  #print("test")
  Normal <- RegStatus.NormBeregner(D2,NormPeriode)
  RegStatus.Plotter(D2, D3, Normal, Reg=Reg, Aar=Aar, Prog=Prog, TAM=TAM, RR=RR)
  
}

RegStatus.Nedbor <- function(Data, Reg=0, Aar = 2022, Mnd=11, Prog=T, NormPeriode = c(1991,2020),TAM=F,RR=T){
  Data <- Data[Data[,1]==Reg,c(2,3,5)]
  D1 <- RegStatus.Omformer(Data)
  D2 <- RegStatus.Nedbor.Beregner(D1)
  if(Prog){
    D3 <- RegStatus.Nedbor.Prognose(D1,D2,Aar,Mnd)
  }
  #print("test")
  Normal <- RegStatus.NormBeregner(D2,NormPeriode)
  RegStatus.Plotter(D2, D3, Normal, Reg=Reg, Aar=Aar, Prog=Prog,TAM=TAM,RR=RR)
}

RegStatus.TempRegn <- function(Data, Reg=0, Aar = 2022, Mnd=11, Prog=T, NormPeriode=c(1991,2020), TAM=T, RR=T){
  
  #print(Reg)
  if (Reg == 0){Regionnavn <- "GR0 - Norge"}
  #print("A")
  if (Reg == 1){Regionnavn <- "GR1 - Østlandet"}
  #print("A")
  if (Reg == 2){Regionnavn <- "GR2 - Sørlandet"}
  #print("A")
  if (Reg == 3){Regionnavn <- "GR3 - Vestlandet"}
  #print("A")
  if (Reg == 4){Regionnavn <- "GR4 - Midt-Norge"}
  #print("A")
  if (Reg == 5){Regionnavn <- "GR5 - Nord-Norge"}
  
  Temperatur <- Data[Data[,1]==Reg,c(2,3,4)]
  Regn       <- Data[Data[,1]==Reg,c(2,3,5)]
  
  R1 <- RegStatus.Omformer(Regn)
  T1 <- RegStatus.Omformer(Temperatur)
  
  T2 <- RegStatus.Temperatur.Beregner(T1)
  R2 <- RegStatus.Nedbor.Beregner(R1)
  
  if(Prog){
    T3 <- RegStatus.Temperatur.Prognose(T1,T2,Aar,Mnd)
    R3 <- RegStatus.Nedbor.Prognose(R1,R2,Aar,Mnd)
  }
  
  TN <- RegStatus.NormBeregner(T2,NormPeriode)
  RN <- RegStatus.NormBeregner(R2,NormPeriode)
  
  TR <- range(T2[,2:13],na.rm=T)
  RR <- range(R2[,2:13],na.rm=T)
  plot(T2[,13],R2[,13], pch=16, col="gray", ylab="Nedbør mm", xlab="Temperatur C", main=Regionnavn)
  polygon(TN[c(3,1,1,3,3),12],RN[c(3,3,1,1,3),12],col=rgb(0,0,1,0.5))
  
  if(Prog){
    PTMax <- max(T3, na.rm = T)
    PTMean <- mean(T3, na.rm = T)
    PTMin <- min(T3, na.rm = T)
    PTSD <- sd(T3, na.rm = T)
    
    PRMax <- max(R3, na.rm = T)
    PRMean <- mean(R3, na.rm = T)
    PRMin <- min(R3, na.rm = T)
    PRSD <- sd(R3, na.rm = T)
    
    polygon(c(PTMax,PTMax,PTMin,PTMin,PTMax),c(PRMin,PRMax,PRMax,PRMin,PRMin),col=rgb(1,0,0,0.2) )
    polygon(c(PTMean+PTSD,PTMean+PTSD,PTMean-PTSD,PTMean-PTSD,PTMean+PTSD),
            c(PRMean+PRSD,PRMean-PRSD,PRMean-PRSD,PRMean+PRSD,PRMean+PRSD),col=rgb(1,0,0,0.2) )
  }
}



RegStatus.Omformer <- function(Data){
  Aar <- unique(Data[,1])
  AntAar <- length(Aar)
  Resultat <- Aar
  for(m in 1:12){
    D2 <- Data[Data[,2]==m,3]
    if (length(D2)<AntAar){D2<-c(D2,NA)}
    
    Resultat <- cbind(Resultat,D2)
  }
  Resultat
}

RegStatus.Nedbor.Beregner <- function(D1){
  Resultat <- D1[,1:2]
  for(m in 2:12){
    Resultat <- cbind(Resultat,rowSums(D1[,2:(m+1)]))
  }
  Resultat
}

RegStatus.Temperatur.Beregner <- function(D1){
  Resultat <- D1[,1:2]
  for(m in 2:12){
    Resultat <- cbind(Resultat,rowMeans(D1[,2:(m+1)]))
  }
  Resultat
}

RegStatus.Nedbor.Prognose <- function(Obs,Akk,Aar,Mnd){
  A2 <- Akk[Akk[,1]==Aar,(Mnd+1)]
  if (Mnd == 11){ProgAkk <- A2+Obs[,13]}
  if (Mnd  < 11){ProgAkk <- rowSums(Obs[,(Mnd+1+1):13])+A2}
  ProgAkk
}

RegStatus.Temperatur.Prognose <- function(Obs,Akk,Aar,Mnd){
  A2 <- Akk[Akk[,1]==Aar,(Mnd+1)]
  if (Mnd == 11){ProgAkk <- A2*(11/12)+Obs[,13]/12}
#  if (Mnd  < 11){ProgAkk <- rowSums(Obs[,(Mnd+1+1):13])*(Mnd/12)+A2((12-Mnd)/12)} #Gammel kode med feil
  if (Mnd  < 11){ProgAkk <- rowMeans(Obs[,(Mnd+1+1):13])*((12-Mnd)/12)+A2*((Mnd)/12)}
  ProgAkk
}

RegStatus.NormBeregner <- function(Data,NormPeriode){
#  print("A")
  D2 <- Data[Data[,1]>=NormPeriode[1] & Data[,1]<=NormPeriode[2],2:13]
#  print("A")
  Snitt<-colMeans(D2)
#  print("A")
  Std <- apply(D2,2,sd)
#  print("A")
  Resultat <- rbind(Snitt+Std,Snitt,Snitt-Std)
#  print("A")
  Resultat
}

RegStatus.Plotter <- function(D2,D3,Normal,Reg=0, Aar=2022,Prog=T,TAM=T,RR=T){
  #print("A")
  MR <- c(0,max(c(D2[,13],Normal),na.rm=T))
  YTittel<-"Nedbør"
  if (TAM){MR <- range(D2[,2:13],na.rm=T); YTittel <- "Temperatur"}
  #print(Reg)
  if (Reg == 0){Regionnavn <- "GR0 - Norge"}
  #print("A")
  if (Reg == 1){Regionnavn <- "GR1 - Østlandet"}
  #print("A")
  if (Reg == 2){Regionnavn <- "GR2 - Sørlandet"}
  #print("A")
  if (Reg == 3){Regionnavn <- "GR3 - Vestlandet"}
  #print("A")
  if (Reg == 4){Regionnavn <- "GR4 - Midt-Norge"}
  #print("A")
  if (Reg == 5){Regionnavn <- "GR5 - Nord-Norge"}
  #print("A")
  
  
  plot(-1000000,-1000000,xlim=c(1,12),ylim=MR,xlab="",ylab=YTittel, xaxt = "n",main=Regionnavn)
  #print("A")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12),
       labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  #print("H")
  
  AntAar <- length(D2[,1])
  for( n in 1:AntAar){
    lines(c(1:12),c(D2[n,c(2:13)]),col="gray")
  }
  polygon(c(1:12,12:1,1),c(Normal[3,1:12],Normal[1,12:1],Normal[3,2]),col=rgb(0,0,1,0.5))
  lines(1:12,Normal[2,1:12],col="Black",lwd=2)
  
#  lines(1:12,D2[D2[,1]==2022,c(2:13)],col="darkred",lwd=2) #Gammel kode med feil
  lines(1:12,D2[D2[,1]==Aar,c(2:13)],col="darkred",lwd=2)
  
  if(Prog){
    PMax <- max(D3, na.rm = T)
    PMean <- mean(D3, na.rm = T)
    PMin <- min(D3, na.rm = T)
    PSD <- sd(D3, na.rm = T)
    lines(c(12,12),c(PMin,PMax),col=rgb(1,0,0,0.5),lwd=4)
    lines(c(12,12),c(PMean-PSD,PMean+PSD),col=rgb(1,0,0,0.5),lwd=8)
  }
  
}

RegionHenter <- function(){
  
  # Insert your own client ID here
  client_id = 'ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c'
  
  # Define andpoint and parameters
  endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")
  sources <- 'GR0,GR1,GR2,GR3,GR4,GR5'
  elements <- 'region_mean(air_temperature P1M 1991_2020),region_sum(precipitation_amount P1M 1991_2020)'
  referenceTime <- '1900-01-01/2032-11-01'
  # Build the URL to Frost
  url <- paste0(
    endpoint, "?",
    "sources=", sources,
    "&referencetime=", referenceTime,
    "&elements=", elements
  )
  #print(url)
  # Issue an HTTP GET request and extract JSON data
  xs <- try(fromJSON(URLencode(url),flatten=T))
  # Check if the request worked, print out any errors
  if (class(xs) != 'try-error') {
    #df <- unnest(xs$data)#Original
    df <- unnest(xs$data,cols = c(observations))
    print("Data retrieved from frost.met.no!")
  } else {
    print("Error: the data retrieval was not successful!")
  }
  # Convert the time value to something R understands
  df$referenceTime <- as.Date(df$referenceTime)
  # Preview the result
  head(df)
  df2 <- as.matrix(df)
  Verd <- as.numeric(gsub(",",".",df2[,4]))
  Reg <- as.numeric(gsub("GR","", df2[,1]))
  Aar <- as.numeric(substr(df2[,2],1,4))
  Mnd <- as.numeric(substr(df2[,2],6,7))
  Dag <- as.numeric(substr(df2[,2],9,10))
  Elem <- as.numeric(gsub("PT","",gsub("H","",df2[,6])))/6
  
  Resultat <- cbind(Reg,Elem,Aar,Mnd,Dag,Verd)
  #plot(1:12, Resultat[Resultat[,3]==1999 & Resultat[,2]==1 & Resultat[,1]==0,6])
  Resultat <- cbind(Resultat[Resultat[,2]==0,c(1,3,4,6)],Resultat[Resultat[,2]==1,c(6)])
  colnames(Resultat)<-c("Reg","Aar","Mnd","Temp","Nedbor")
  Resultat
}
