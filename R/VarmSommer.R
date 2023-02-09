require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Datalaster.Frost.CSV.R")

Hetebolge <- function(StNr1=18700, FAar=1901, TAar=2023, RMar=T, TMin = 25, Dek=T, Periode = 5, Absolutt=T){
  start <- paste(FAar,"-01-01",sep="")
  slutt <- paste(TAar+1,"-01-01",sep="")
  DMax <- Dgn.Laster(StNr=StNr1, start=start, slutt=slutt, elementer="max(air_temperature%20P1D)")
  DMax2 <- DMax
  LD <- length(DMax[,4])
  print(LD)
  for (n in 2:Periode){
    #print(n)
    DMax <- cbind(DMax,c(array(NA,(n-1)),DMax[1:(LD-n+1),4]))
  }
  if (Absolutt){
    for(n in 1:Periode){
      DMax[DMax[,n+3]<TMin,n+3]<-0
      DMax[DMax[,n+3]>=TMin,n+3]<-TMin
    }
  }
  D2 <- cbind(DMax2,rowMeans(DMax[,4:(3+Periode)]))
  
  Aar <- unique(D2[,1])
  Resultat <- c()
  for(aar in Aar){
    D22 <- D2[D2[,1]==aar,]
    #print(aar)
    Resultat <- rbind(Resultat,
                      c(aar,length(D22[D22[,5] >= TMin & !is.na(D22[,5]),5])))
  }
  Resultat
}

Hetebolge.Sammenligner <- function(StNr1=18700, FAar=1901, TAar=2023, TMin.Abs = 25, Periode.Abs = 5, TMin.Mean = 28, Periode.Mean = 5){
  H.Abs <- Hetebolge(StNr1 = StNr1, FAar = FAar, TAar = TAar, Periode = Periode.Abs, TMin = TMin.Abs)
  H.Mean <- Hetebolge(StNr1 = StNr1, FAar = FAar, TAar = TAar, Periode = Periode.Mean, TMin = TMin.Mean, Absolutt = F)
  
  H.Test <- cbind(H.Abs,H.Mean[,2])
  rownames(H.Test) <- H.Test[,1]
  H.Test <- H.Test[,2:3]
  colnames(H.Test) <- c("Abs","Mean")
  Undert <- paste("Mean: ", TMin.Mean, "C, ", Periode.Mean, " Dager,   Abs: ", TMin.Abs, "C, ", Periode.Abs, " Dager",sep=""  )
  barplot(t(H.Test),beside=T,main="Sammenligning av to metoder for Hetebølgeindeks",ylab="Antall",legend.tex=colnames(H.Test),args.legend = list(x = "topleft"), sub=Undert)
  
}
  





  
