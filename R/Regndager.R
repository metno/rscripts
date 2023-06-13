require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.CSV.R")

RegnDager.Mnd <- function(StNr=18700, FAar=1991, TAar=2023, Mnd=5, RGrense=1){
  start <- paste(FAar,"-01-01",sep="")
  slutt <- paste(TAar+1,"-01-01",sep="")
  Dogn <- Dgn.Laster(StNr=StNr, start=start, slutt=slutt, elementer = "sum(precipitation_amount P1D)")
  D2 <- cbind(Dogn,Dogn[,c(4,4)])
  D2[D2[,5] < RGrense,5] <- 0
  D2[D2[,5] >= RGrense,5] <- 1
  D2[D2[,6] < RGrense,6] <- 0
  D2[D2[,6] >= RGrense,6] <- 1
  D2 <- cbind(D2,D2[,6],abs(D2[,c(5:6,6)]-1))
  LD <- length(D2[,1])
  for(n in 2:LD){
    D2[n,6] <- (D2[n-1,6]+D2[n,5])*D2[n,5]
    D2[n,9] <- (D2[n-1,9]+D2[n,8])*D2[n,8]
    D2[n-1,7]<-D2[n,9]*D2[n-1,6]
    D2[n-1,10]<-D2[n,6]*D2[n-1,9]
  }
  D2[LD,c(7,10)]<-D2[LD,c(6,9)]
  D2
  RD <- D2
  Overskrift <- paste("Sammenhengende dager i måned ", Mnd," på ", StNr, ",  med mindre enn ", RGrense, " mm", sep="")
  
  hist(RD[RD[,2]==Mnd & RD[,10]>0, 10],breaks = c(1:max(RD[RD[,2]==5 & RD[,10]>0, 10])), 
       main=Overskrift,ylab="Hyppighet",xlab="Dager")
  lines(D2[LD,c(10,10)]+0.5,c(-10000,10000),col="darkred",lwd=2,lty=3)
  print(D2[LD,])
  D2
}

RegnDager.Aar <- function(StNr=18700, FAar=1991, TAar=2023, RGrense=1){
  start <- paste(FAar,"-01-01",sep="")
  slutt <- paste(TAar+1,"-01-01",sep="")
  Dogn <- Dgn.Laster(StNr=StNr, start=start, slutt=slutt, elementer = "sum(precipitation_amount P1D)")
  D2 <- cbind(Dogn,Dogn[,c(4,4)])
  D2[D2[,5] < RGrense,5] <- 0
  D2[D2[,5] >= RGrense,5] <- 1
  D2[D2[,6] < RGrense,6] <- 0
  D2[D2[,6] >= RGrense,6] <- 1
  D2 <- cbind(D2,D2[,6],abs(D2[,c(5:6,6)]-1))
  LD <- length(D2[,1])
  for(n in 2:LD){
    D2[n,6] <- (D2[n-1,6]+D2[n,5])*D2[n,5]
    D2[n,9] <- (D2[n-1,9]+D2[n,8])*D2[n,8]
    D2[n-1,7]<-D2[n,9]*D2[n-1,6]
    D2[n-1,10]<-D2[n,6]*D2[n-1,9]
  }
  D2[LD,c(7,10)]<-D2[LD,c(6,9)]
  D2
  RD <- D2
  Overskrift <- paste("Sammenhengende dager på ", StNr, ",  med mindre enn ", RGrense, " mm", sep="")
  
  hist(RD[RD[,10]>0, 10],breaks = c(1:max(RD[RD[,10]>0, 10])), 
       main=Overskrift,ylab="Hyppighet",xlab="Dager")
  lines(D2[LD,c(10,10)]+0.5,c(-10000,10000),col="darkred",lwd=2,lty=3)
  print(D2[LD,])
  D2
}

