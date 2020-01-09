require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.R")

NyNormPlot2 <- function(StNr=99840,FAar=1976,TAar=2100,Navn=NA,Mnd=1,Trend=F){
  if (is.na(Navn)){Navn<-StNr}
  Maaneder <- c("Januar","Februar","Mars","April","Mai","Juni","Juli","August","September","Oktober","November","Desember")
  Overskrift <- paste(Navn,", ",Maaneder[Mnd],sep="")
  Mnd<-Mnd+1
  start <- paste(FAar,"-01-01",sep="")
  stopp <- paste(TAar,"-12-31",sep="")
  Data <- Data.Laster(StNr=StNr,start=start,stop=stopp,elements = "mean(air_temperature_anomaly P1M 1961_1990)",Daglig=F)
  Data<-cbind(as.numeric(format(Data[,3],format="%m")), as.numeric(format(Data[,3],format="%Y")),Data[,3:4])
  UnikeAar <- unique(Data[,2])
  NyData <- array(NA,c(length(UnikeAar),13))
  teller=0
  for(Aar in UnikeAar){
    teller <- teller+1
    NyData[teller,1]<-Aar
    D2 <- Data[Data[,2]==Aar,]
    NyData[teller,D2[,1]+1]<-D2[,4]
  }
  NormalEndring <- colMeans(NyData[NyData[,1]>1990,],na.rm = T)
  D2 <- NyData[,c(1,Mnd)]
  MNorm <- NormalEndring[Mnd]
  
  Underskrift <- paste("Ny normal (1991 - 2020) er ", round(MNorm,1), "C varmere enn gammel normal (1961 - 1990)")
  
  
  NNorm <- 0
  if (MNorm<NNorm){
    Underskrift <- paste("Ny normal (1991 - 2020) er ", abs(round(MNorm,1)), "C kaldere enn gammel normal (1961 - 1990)")
    NNorm<-MNorm;MNorm<-0}
  
  YGrense<-max(abs(D2[,2]),na.rm=T)
  plot(10000,10000,xlim = range(UnikeAar),ylim = c(YGrense*-1,YGrense),ylab="Temperaturavvik", xlab = " ",main=Overskrift, sub = Underskrift)
  lines(c(-1000,100000),c(0,0))
  lines(c(-1000,100000),c(MNorm,MNorm))
  for(n in 1:length(UnikeAar)){
    XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
    YVektor <- c(NNorm,D2[n,2],D2[n,2],NNorm,NNorm)
    farve="darkred"
    if(!is.na(D2[n,2]) & D2[n,2]<=NNorm){
      farve="darkblue"
      XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
      YVektor2 <- c(NNorm,MNorm,MNorm,NNorm,NNorm)
      polygon(XVektor,YVektor2,col="steelblue1")
    }
    if(!is.na(D2[n,2]) & D2[n,2]>NNorm & D2[n,2]<=MNorm){
      farve="pink"
      XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
      YVektor2 <- c(D2[n,2],MNorm,MNorm,D2[n,2],D2[n,2])
      polygon(XVektor,YVektor2,col="steelblue1")
    }
    if(!is.na(D2[n,2])){polygon(XVektor,YVektor,col=farve)}
    if(!is.na(D2[n,2]) & D2[n,2]>MNorm){
      farve="pink"
      XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
      YVektor2 <- c(NNorm,MNorm,MNorm,NNorm,NNorm)
      polygon(XVektor,YVektor2,col="pink")
    }
    
  }
  lines(c(1961-0.5,1990+0.5),c(0,0),col="darkgreen",lwd=5)
  lines(c(1991-0.5,2020+0.5),c(NormalEndring[Mnd],NormalEndring[Mnd]),col="green",lwd=5)
  
  if(Trend){
    Linje <- lm(D2[,2]~D2[,1])
#    print(Linje$coefficients[2])
    X <- c(0,10000)
    lines(X,Linje$coefficients[1]+Linje$coefficients[2]*X,lty=3,lwd=2)
  }
  
  #lines(D2[,1],D2[,2])
}

NyNormPlot <- function(StNr=98550,FAar=1961,TAar=2100,Overskrift="Vardø Radio",Mnd=1){
  Mnd<-Mnd+1
  start <- paste(FAar,"-01-01",sep="")
  stopp <- paste(TAar,"-12-31",sep="")
  Data <- Data.Laster(StNr=StNr,start=start,stop=stopp,elements = "mean(air_temperature_anomaly P1M 1961_1990)",Daglig=F)
  Data<-cbind(as.numeric(format(Data[,3],format="%m")), as.numeric(format(Data[,3],format="%Y")),Data[,3:4])
  UnikeAar <- unique(Data[,2])
  NyData <- array(NA,c(length(UnikeAar),13))
  teller=0
  for(Aar in UnikeAar){
    teller <- teller+1
    NyData[teller,1]<-Aar
    D2 <- Data[Data[,2]==Aar,]
    NyData[teller,D2[,1]+1]<-D2[,4]
  }
  NormalEndring <- colMeans(NyData[NyData[,1]>1990,],na.rm = T)
  D2 <- NyData[,c(1,Mnd)]
  MNorm <- NormalEndring[Mnd]
  YGrense<-max(abs(D2[,2]),na.rm=T)
  plot(10000,10000,xlim = range(UnikeAar),ylim = c(YGrense*-1,YGrense),ylab="Temperaturavvik", xlab = " ")
  lines(c(-1000,100000),c(0,0))
  lines(c(-1000,100000),c(MNorm,MNorm))
  for(n in 1:length(UnikeAar)){
    XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
    YVektor <- c(0,D2[n,2],D2[n,2],0,0)
    farve="darkred"
    if(D2[n,2]<=0){
      farve="darkblue"
      XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
      YVektor2 <- c(0,MNorm,MNorm,0,0)
      polygon(XVektor,YVektor2,col="steelblue1")
    }
    if(D2[n,2]>0 & D2[n,2]<=MNorm){
      farve="pink"
      XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
      YVektor2 <- c(D2[n,2],MNorm,MNorm,D2[n,2],D2[n,2])
      polygon(XVektor,YVektor2,col="steelblue1")
    }
    polygon(XVektor,YVektor,col=farve)
    if(D2[n,2]>MNorm){
      farve="pink"
      XVektor <- c(D2[n,1]-0.4,D2[n,1]-0.4,D2[n,1]+0.4,D2[n,1]+0.4,D2[n,1]-0.4)
      YVektor2 <- c(0,MNorm,MNorm,0,0)
      polygon(XVektor,YVektor2,col="pink")
    }
    
  }
  lines(c(1961,1990),c(0,0),col="darkgreen",lwd=3)
  lines(c(1991,2020),c(MNorm,MNorm),col="darkgreen",lwd=3)
  
  #lines(D2[,1],D2[,2])
}
