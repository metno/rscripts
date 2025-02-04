require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.CSV.R")

NullPass <- function(StNr=18700,FY=1961,TY=2024,FM="12",TM="02"){
  Start <- paste(FY-1,"-",FM,"-01",sep="")
  Slutt <- paste(TY,"-02-28",sep="")
  DMin <- Dgn.Laster(StNr=StNr, start = Start, slutt = Slutt, elementer="min(air_temperature%20P1D)")
  DMax <- Dgn.Laster(StNr=StNr, start = Start, slutt = Slutt, elementer="max(air_temperature%20P1D)")
  Resultat <- c()
  for(Y in FY:TY){
    D2 <- cbind(rbind(DMin[DMin[,1]==(Y-1) & DMin[,2]>=as.numeric(FM),],
                      DMin[DMin[,1]==Y & DMin[,2]<=as.numeric(TM),]),
                c(DMax[DMax[,1]==Y-1 & DMax[,2]>=as.numeric(FM),4],
                      DMin[DMax[,1]==Y & DMax[,2]<=as.numeric(TM),4]))
    Resultat <- rbind(Resultat,
                      c(Y,
                        length(D2[D2[,4]>0,4]),
                        length(D2[D2[,5]>0,5]),
                        length(D2[D2[,4]<0 & D2[,5]>0,4])))
  }
  
  YMax <- max(apply(Resultat[,2:4],2,max))
  colnames(Resultat)<-c("Aar","max>0","min>0","Nullpasering")
  plot(Resultat[,1],Resultat[,2],type="l",col="Blue",
       ylim = c(0,YMax),main=StNr,ylab="Dager",xlab="")
  lines(Resultat[,1],Resultat[,3],col="Red")
  lines(Resultat[,1],Resultat[,4],col="Darkgreen")
  Resultat
}

