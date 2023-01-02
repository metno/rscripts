require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.CSV.R")

FrostIVekst <- function(StNr=18700, FAar=1991, TAar=2020){
  start <- paste(FAar,"-01-01",sep="")
  slutt <- paste(TAar,"-12-31",sep="")
  Dogn <- Dgn.Laster(StNr=StNr, start=start, slutt=slutt)
  DMin <- Dgn.Laster(StNr=StNr, start=start, slutt=slutt, elementer="min(air_temperature%20P1D)")
  LD <- length(Dogn[,1])
  Data <- cbind(DMin,
                Dogn[,4],
                c(NA,Dogn[1:(LD-1),4]),
                c(NA,NA,Dogn[1:(LD-2),4]),
                c(NA,NA,NA,Dogn[1:(LD-3),4]),
                c(NA,NA,NA,NA,Dogn[1:(LD-4),4]),
                c(NA,NA,NA,NA,NA,Dogn[1:(LD-5),4]),
                c(NA,NA,NA,NA,NA,NA,Dogn[1:(LD-6),4]),
                c(NA,NA,NA,NA,NA,NA,NA,Dogn[1:(LD-7),4]),
                c(NA,NA,NA,NA,NA,NA,NA,NA,Dogn[1:(LD-8),4]),
                c(NA,NA,NA,NA,NA,NA,NA,NA,NA,Dogn[1:(LD-9),4]))
  Data <- cbind(Data,
                Data[,5],
                rowMeans(Data[,5:6]),
                rowMeans(Data[,5:7]),
                rowMeans(Data[,5:8]),
                rowMeans(Data[,5:9]),
                rowMeans(Data[,5:10]),
                rowMeans(Data[,5:11]),
                rowMeans(Data[,5:12]),
                rowMeans(Data[,5:13]),
                rowMeans(Data[,5:14]))
  Data <- cbind(Data,Data[,5:24])
  for(n in 25:44){
    Data[Data[,n]<5,n]<-0
    Data[Data[,n]>=5,n]<-1}
  Data <- cbind(Data,
                Data[,35],
                rowMeans(Data[,35:36]),
                rowMeans(Data[,35:37]),
                rowMeans(Data[,35:38]),
                rowMeans(Data[,35:39]),
                rowMeans(Data[,35:40]),
                rowMeans(Data[,35:41]),
                rowMeans(Data[,35:42]),
                rowMeans(Data[,35:43]),
                rowMeans(Data[,35:44]))
  for(n in 45:54){
    Data[Data[,n]<1,n]<-0}
  colnames(Data) <- c("Year","Mnd","Dag","Min",   #Mintemperature
                      "Dg1","Dg2", "Dg3","Dg4", "Dg5","Dg6", "Dg7","Dg8", "Dg9","Dg10", #Grunnlag døgn
                      "M1","M2", "M3","M4", "M5","M6", "M7","M8", "M9","M10", #1-10 dg mean
                      "FDg1","FDg2", "FDg3","FDg4", "FDg5","FDg6", "FDg7","FDg8", "FDg9","FDg10", #Filter (0-1) dogn
                      "FM1","FM2", "FM3","FM4", "FM5","FM6", "FM7","FM8", "FM9","FM10",  #Filter 0-1 der middeltemperatur er over
                      "TFDg1","TFDg2", "TFDg3","TFDg4", "TFDg5","TFDg6", "TFDg7","TFDg8", "TFDg9","TFDg10") #Filter for at hver dager er over trigger
#  plot(c(1:180),Data[1:180,45]-0.9,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,45]-0.9,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,46]-0.8,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,47]-0.7,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,48]-0.6,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,49]-0.5,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,50]-0.4,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,51]-0.3,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,52]-0.2,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,53]-0.1,ylim=c(-1,1),pch=19)
#  points(c(1:180),Data[1:180,54]-0,ylim=c(-1,1),pch=19)

  MinFilter <- Data[,4]
  MinFilter[MinFilter<0]<-0
  MinFilter[MinFilter>0]<-1
  MinFilter <- MinFilter+1
  Farver <- c("blue","red")
  
  plot(c(1:180),Data[1:180,45]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]],main=paste(StNr,", Alle over",sep=""),xlab="Dager siden nyttår",ylab="")
  points(c(1:180),Data[1:180,45]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,46]-0.8,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,47]-0.7,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,48]-0.6,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,49]-0.5,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,50]-0.4,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,51]-0.3,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,52]-0.2,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,53]-0.1,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,54]-0,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  
  plot(c(1:180),Data[1:180,35]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]],main=paste(StNr,", Gjennomsnitt over",sep=""),xlab="Dager siden nyttår",ylab="")
  points(c(1:180),Data[1:180,35]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,36]-0.8,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,37]-0.7,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,38]-0.6,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,39]-0.5,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,40]-0.4,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,41]-0.3,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,42]-0.2,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,43]-0.1,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,44]-0,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  
  Data
}

FrostIVekstPlot<-function(Data,Aar,Sted="Oslo-Blindern"){
  MinFilter <- Data[,4]
  MinFilter[MinFilter<0]<-0
  MinFilter[MinFilter>0]<-1
  MinFilter <- MinFilter+1
  Farver <- c("blue","red")
  
  Data <- Data[Data[,1]==Aar,]
  plot(c(1:180),Data[1:180,45]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]],main=paste(Sted,", ",Aar,", Alle over",sep=""),xlab="Dager siden nyttår",ylab="")
  points(c(1:180),Data[1:180,45]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,46]-0.8,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,47]-0.7,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,48]-0.6,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,49]-0.5,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,50]-0.4,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,51]-0.3,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,52]-0.2,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,53]-0.1,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,54]-0,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  
  plot(c(1:180),Data[1:180,35]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]],main=paste(Sted,", ",Aar,", Gjennomsnitt over",sep=""),xlab="Dager siden nyttår",ylab="")
  points(c(1:180),Data[1:180,35]-0.9,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,36]-0.8,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,37]-0.7,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,38]-0.6,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,39]-0.5,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,40]-0.4,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,41]-0.3,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,42]-0.2,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,43]-0.1,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),Data[1:180,44]-0,ylim=c(-1,1),pch=19,col=Farver[MinFilter[1:180]])
  
}

FrostIVekstPlot2 <- function(Data,Aar,Sted="Oslo-Blindern",NGrense=NA,DG5linje=T){#Blindern NGrense=99
  Data2 <- Data[Data[,1]==Aar,]
  
  MinFilter <- Data2[,4]
  MinFilter[MinFilter<0]<-0
  MinFilter[MinFilter>0]<-1
  MinFilter <- MinFilter+1
  Farver <- c("blue","red")
  
  R1 <- Data2[1,]*0
  D2 <- R1
  for (n in 2:200){
    R2 <- Data2[n,]
    R1 <- R1+R2
    R1[R1>0]<-1
    D2 <- rbind(D2,R1)
    #print(dim(D2))
  }
  
  
  
#  plot(c(1:180),Data2[1:180,45]*10-9,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]],main=paste(Sted,", ",Aar,", Alle over",sep=""),xlab="Dager siden nyttår",ylab="")
#  points(c(1:180),Data2[1:180,46]*10-8,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,47]*10-7,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,48]*10-6,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,49]*10-5,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,50]*10-4,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,51]*10-3,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,52]*10-2,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,53]*10-1,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,54]*10,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  if(!is.na(NGrense)){lines(c(NGrense,NGrense),c(-1000,1000),lty=3)}
#  if(DG5linje){lines(c(-1000,1000),c(5,5),lty=3)}
#  lines(c(-1000,1000),c(0.5,0.5))
  
#  plot(c(1:180),Data2[1:180,35]*10-9,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]],main=paste(Sted,", ",Aar,", Gjennomsnitt over",sep=""),xlab="Dager siden nyttår",ylab="")
#  points(c(1:180),Data2[1:180,36]*10-8,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,37]*10-7,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,38]*10-6,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,39]*10-5,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,40]*10-4,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,41]*10-3,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,42]*10-2,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,43]*10-1,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  points(c(1:180),Data2[1:180,44]*10,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
#  if(!is.na(NGrense)){lines(c(NGrense,NGrense),c(-1000,1000),lty=3)}
#  if(DG5linje){lines(c(-1000,1000),c(5,5),lty=3)}
#  lines(c(-1000,1000),c(0.5,0.5))
  
  plot(c(1:180),D2[1:180,45]*10-9,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]],main=paste(Sted,", ",Aar,", Alle over",sep=""),xlab="Dager siden nyttår",ylab="")
  points(c(1:180),D2[1:180,46]*10-8,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,47]*10-7,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,48]*10-6,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,49]*10-5,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,50]*10-4,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,51]*10-3,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,52]*10-2,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,53]*10-1,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,54]*10,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  if(!is.na(NGrense)){lines(c(NGrense,NGrense),c(-1000,1000),lty=3)}
  if(DG5linje){lines(c(-1000,1000),c(5,5),lty=3)}
  lines(c(-1000,1000),c(0.5,0.5))
  
  plot(c(1:180),D2[1:180,35]*10-9,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]],main=paste(Sted,", ",Aar,", Gjennomsnitt over",sep=""),xlab="Dager siden nyttår",ylab="")
  points(c(1:180),D2[1:180,36]*10-8,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,37]*10-7,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,38]*10-6,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,39]*10-5,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,40]*10-4,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,41]*10-3,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,42]*10-2,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,43]*10-1,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  points(c(1:180),D2[1:180,44]*10,ylim=c(-10,10),pch=19,col=Farver[MinFilter[1:180]])
  if(!is.na(NGrense)){lines(c(NGrense,NGrense),c(-1000,1000),lty=3)}
  if(DG5linje){lines(c(-1000,1000),c(5,5),lty=3)}
  lines(c(-1000,1000),c(0.5,0.5))
  
}

FrostIVekstPlot3Tid <- function(Data=NA,FAar=1961,TAar=2021,Sted="Oslo-Blindern",NGrense=99,StNr=18700){#Blindern NGrense=99
  if(is.na(Data)){Data <- FrostIVekst(StNr=StNr,FAar=FAar-1,TAar=TAar)}

  Farver <- c("blue","red")
  Farver2 <- c("lightblue","pink")
  
  
  plot(1000000,1000000,ylim=c(FAar,TAar),xlim=c(0,180),
       main=paste(Sted,", 6 dager på rad over 5 C",sep=""),xlab="Dager siden nyttår",ylab="", xaxt = "n")
  axis(1, at = c(1,32,60,91,121,152,182,213,243,274,305,335),
       labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Des"))
  
  Farver <- c("blue","red")
  Farver2 <- c("lightblue","pink")
  
  AntallDager <- c()
  
  for(Aar in FAar:TAar){
    Data2 <- Data[Data[,1]==Aar,]
  
    MinFilter <- Data2[,4]
    MinFilter[MinFilter<0]<-0
    MinFilter[MinFilter>0]<-1
    MinFilter <- MinFilter+1
  
    R1 <- Data2[1,]*0
    D2 <- R1
    for (n in 2:200){
      R2 <- Data2[n,]
      R1 <- R1+R2
      R1[R1>0]<-1
      D2 <- rbind(D2,R1)
      #print(dim(D2))
    }
    points(c(1:180),array(Aar,180),pch=19,col=Farver2[MinFilter[1:180]])
    points(c(1:180),D2[1:180,50]*Aar,pch=19,col=Farver[MinFilter[1:180]])
    
    D2[1:180,50]*MinFilter[1:180] -> F1
    AntallDager <- rbind(AntallDager,
                         c(Aar,length(F1[F1==1])))
  }
  
  if(!is.na(NGrense)){lines(c(NGrense,NGrense),c(-10000,10000),lty=3)}

  AntallDager
}

FrostIVekstPlot4KIN <- function(Data=NA,FAar=1961,TAar=2021,Sted="Oslo-Blindern",NGrense=99,StNr=18700){
  Data <- FrostIVekstPlot3Tid(Data=Data,FAar=FAar,TAar=TAar,Sted=Sted,StNr=StNr)
#  Tittel <- paste("Dager med frost etter start av vekstsesong på ", Sted,sep="")
  Tittel <- paste(Sted, " (", StNr, ")", sep="")
  GN <- mean(Data[1:30,2])
  NN <- mean(Data[31:60,2])
#  plot(Data[,1],Data[,2],type="h",lwd=6,col="darkblue",xlab="",ylab="Dager",main=Tittel)
#  plot(Data[,1],Data[,2],type="h",lwd=6,col="#0c465f",xlab="",ylab="Dager",main=Tittel)
#  plot(Data[,1],Data[,2],type="h",lwd=6,col="#255c36",xlab="",ylab="Dager",main=Tittel)
  plot(Data[,1],Data[,2],type="h",lwd=6,col="#255c36",xlab="",ylab="", main=Tittel)
  lines(c(1961,1990),c(GN,GN),lty=2,lwd=2,col="#255c36")
  lines(c(1991,2020),c(NN,NN),lty=4,lwd=2,col="#255c36")
  print(c(GN,NN))
}
