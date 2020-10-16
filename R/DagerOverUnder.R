require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.R")

#Data <- Data.Laster(StNr=18700,start="2019-01-01",stop="2019-12-31",elements = "sum(precipitation_amount%20PT1H)",Daglig=F)


DagerOver <- function(StNr=18700,StAar=1990,SlAar=2019,Grenser=c(0.1,3,10,25,40)){
  FD <- paste(StAar,"-01-01",sep="")
  TD <- paste(SlAar,"-12-31",sep="")
  Data <- Data.Laster(start=FD, stop=TD, StNr=StNr)
  LG <- length(Grenser)
  Resultat <- StNr
  for(n in 1:LG){
    Resultat <- c(Resultat,round(length(Data[Data[,4]>Grenser[n],4])/length(Data[,4])*365,1))
  }
  Resultat
}

DagerOver.Multi <- function(StNr=c(18700,39040,44560,50540,69100,90450),StAar=1990,SlAar=2019,Grenser=c(0.1,3,10,25,40)){
  Resultat <- c()
  LS <- length(StNr)
  for (S in 1:LS){
    Resultat <- rbind(Resultat,
                      DagerOver(StNr[S],StAar=StAar,SlAar=SlAar,Grenser=Grenser))
  }
  colnames(Resultat)<-c("StNr",Grenser)
  rownames(Resultat) <- Resultat[,1]
  rownames(Resultat) <- c("Oslo","Kristiansand","Stavanger","Bergen","Trondheim","Tromsø")
  Farger <- colorRampPalette(c("lightblue","darkblue"))(length(Grenser))
  barplot(t(Resultat[,2:(length(Grenser)+1)]),
          beside = T,col=Farger,legend.text = paste("> ", Grenser,"mm",sep=""),
          main="Dager med nedbør",ylab="Dager/år")
  Resultat
}

DagerOver.Plot.Nebor <- function(StNr=18700,StAar=1900,SlAar=2100,Grense=10,elements = c("sum(precipitation_amount P1D)"),Overskrift=NA){
  FD <- paste(StAar,"-01-01",sep="")
  TD <- paste(SlAar,"-12-31",sep="")
  Data <- Data.Laster(start=FD, stop=TD, StNr=StNr,elements=elements)
  print("Data OK")
  Aar <- Data$time$year+1900
  Mnd <- Data$time$mon
  D2 <- cbind(Aar,Mnd,Data[,4])
  D2[D2[,3]<Grense,3]<-0
  D2[D2[,3]>=Grense,3]<-1
  UAar <- unique(Aar)
  Resultat <- c()
  for (aar in UAar){
    #print(aar)
    R2 <- aar
    for (m in 0:11){
      #print(m)
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])<25){R2 <- c(R2,NA)}
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])>=25){R2 <- c(R2,sum(D2[D2[,1]==aar & D2[,2]==m,3]))}
    }
    Resultat<-rbind(Resultat,R2)
  }
  colnames(Resultat)<-c(0:12)
  row.names(Resultat)<-Resultat[,1]
  Resultat <- Resultat[,c(2:13)]
  print("Analyse OK")
  farver <- colorRampPalette(c("white","darkblue"))(32)
  if(is.na(Overskrift)){Overskrift<-StNr}
  plot(-1000,-10000,xlim=c(0.9,12.1),ylim=c(range(UAar)),xlab="",ylab="", main=Overskrift)
  for (aar in UAar){
    for (m in 1:12){
      PX<-c(m-0.5, m+0.5, m+0.5, m-0.5, m-0.5)
      PY<-c(aar-0.5, aar-0.5, aar+0.5, aar+0.5, aar-0.5)
      DP <- Resultat[aar-UAar[1]+1,m]
      if (!is.na(DP)){
        polygon(PX,PY,col = farver[DP])
        if(length(UAar)<31){text(m,aar,DP)}
      }
    }
  }
  Resultat
}

#DagerOver.Plot.temperatur(StAar=1999,elements = "max(air_temperature P1D)",Grense=30)
#DagerOver.Plot.temperatur(StAar=1999,elements = "min(air_temperature P1D)",Grense=0.1)
DagerOver.Plot.temperatur <- function(StNr=18700,StAar=1900,SlAar=2100,Grense=10,elements = c("mean(air_temperature P1D)"),Overskrift=NA){
  FD <- paste(StAar,"-01-01",sep="")
  TD <- paste(SlAar,"-12-31",sep="")
  Data <- Data.Laster(start=FD, stop=TD, StNr=StNr,elements=elements)
  print("Data OK")
  Aar <- Data$time$year+1900
  Mnd <- Data$time$mon
  D2 <- cbind(Aar,Mnd,Data[,4])
  D2[D2[,3]<Grense,3]<-0
  D2[D2[,3]>=Grense,3]<-1
  UAar <- unique(Aar)
  Resultat <- c()
  for (aar in UAar){
    #print(aar)
    R2 <- aar
    for (m in 0:11){
      #print(m)
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])<25){R2 <- c(R2,NA)}
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])>=25){R2 <- c(R2,sum(D2[D2[,1]==aar & D2[,2]==m,3]))}
    }
    Resultat<-rbind(Resultat,R2)
  }
  colnames(Resultat)<-c(0:12)
  row.names(Resultat)<-Resultat[,1]
  Resultat <- Resultat[,c(2:13)]
  print("Analyse OK")
  farver <- colorRampPalette(c("white","darkred"))(32)
  if(is.na(Overskrift)){Overskrift<-StNr}
  plot(-1000,-10000,xlim=c(0.9,12.1),ylim=c(range(UAar)),xlab="",ylab="", main=Overskrift)
  for (aar in UAar){
    for (m in 1:12){
      PX<-c(m-0.5, m+0.5, m+0.5, m-0.5, m-0.5)
      PY<-c(aar-0.5, aar-0.5, aar+0.5, aar+0.5, aar-0.5)
      DP <- Resultat[aar-UAar[1]+1,m]
      if (!is.na(DP)){
        polygon(PX,PY,col = farver[DP])
        if(length(UAar)<31){text(m,aar,DP)}
      }
    }
  }
  Resultat
}

DagerUnder.Plot.temperatur <- function(StNr=18700,StAar=1900,SlAar=2100,Grense=0,elements = c("min(air_temperature P1D)"),Overskrift=NA){
  FD <- paste(StAar,"-01-01",sep="")
  TD <- paste(SlAar,"-12-31",sep="")
  Data <- Data.Laster(start=FD, stop=TD, StNr=StNr,elements=elements)
  print("Data OK")
  Aar <- Data$time$year+1900
  Mnd <- Data$time$mon
  D2 <- cbind(Aar,Mnd,Data[,4])
  D2[D2[,3]<Grense,3]<- -999
  D2[D2[,3]>=Grense,3]<-0
  D2[D2[,3]== -999,3]<- 1
  UAar <- unique(Aar)
  Resultat <- c()
  for (aar in UAar){
    #print(aar)
    R2 <- aar
    for (m in 0:11){
      #print(m)
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])<25){R2 <- c(R2,NA)}
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])>=25){R2 <- c(R2,sum(D2[D2[,1]==aar & D2[,2]==m,3]))}
    }
    Resultat<-rbind(Resultat,R2)
  }
  colnames(Resultat)<-c(0:12)
  row.names(Resultat)<-Resultat[,1]
  Resultat <- Resultat[,c(2:13)]
  print("Analyse OK")
  farver <- colorRampPalette(c("white","blue"))(32)
  if(is.na(Overskrift)){Overskrift<-StNr}
  plot(-1000,-10000,xlim=c(0.9,12.1),ylim=c(range(UAar)),xlab="",ylab="", main=Overskrift)
  for (aar in UAar){
    for (m in 1:12){
      PX<-c(m-0.5, m+0.5, m+0.5, m-0.5, m-0.5)
      PY<-c(aar-0.5, aar-0.5, aar+0.5, aar+0.5, aar-0.5)
      DP <- Resultat[aar-UAar[1]+1,m]
      if (!is.na(DP)){
        polygon(PX,PY,col = farver[DP])
        if(length(UAar)<31){text(m,aar,DP)}
      }
    }
  }
  Resultat
}

DagerOverUnderKombo.Plot <- function(StNr=18700,StAar=1961,SlAar=2100,Grense1=0,Grense2=0,element1 = c("min(air_temperature P1D)"),element2 = c("max(air_temperature P1D)"),Overskrift=NA){
  FD <- paste(StAar,"-01-01",sep="")
  TD <- paste(SlAar,"-12-31",sep="")
  Data1 <- Data.Laster(start=FD, stop=TD, StNr=StNr,elements=element1)
  Data2 <- Data.Laster(start=FD, stop=TD, StNr=StNr,elements=element2)
  print("Data OK")
  Aar <- Data1$time$year+1900
  Mnd <- Data1$time$mon
  D2 <- cbind(Aar,Mnd,Data1[,4],Data2[,4])
  D2[D2[,3]<Grense1,3]<- -999
  D2[D2[,3]>=Grense1,3]<-0
  D2[D2[,3]== -999,3]<- 1
  D2[D2[,4]>Grense2,4]<- 999
  D2[D2[,4]<=Grense2,4]<-0
  D2[D2[,4]== 999,4]<- 1
  D2 <- cbind(D2,D2[,3]*D2[,4])
  UAar <- unique(Aar)
  Resultat <- c()
  for (aar in UAar){
    #print(aar)
    R2 <- aar
    for (m in 0:11){
      #print(m)
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])<25){R2 <- c(R2,NA)}
      if(length(D2[D2[,1]==aar & D2[,2]==m,3])>=25){R2 <- c(R2,sum(D2[D2[,1]==aar & D2[,2]==m,5]))}
    }
    Resultat<-rbind(Resultat,R2)
  }
  colnames(Resultat)<-c(0:12)
  row.names(Resultat)<-Resultat[,1]
  Resultat <- Resultat[,c(2:13)]
  print("Analyse OK")
  farver <- colorRampPalette(c("white","purple"))(32)
  if(is.na(Overskrift)){Overskrift<-StNr}
  plot(-1000,-10000,xlim=c(0.9,12.1),ylim=c(range(UAar)),xlab="",ylab="", main=Overskrift)
  for (aar in UAar){
    for (m in 1:12){
      PX<-c(m-0.5, m+0.5, m+0.5, m-0.5, m-0.5)
      PY<-c(aar-0.5, aar-0.5, aar+0.5, aar+0.5, aar-0.5)
      DP <- Resultat[aar-UAar[1]+1,m]
      if (!is.na(DP)){
        polygon(PX,PY,col = farver[DP])
        if(length(UAar)<31){text(m,aar,DP)}
      }
    }
  }
  Resultat
}
