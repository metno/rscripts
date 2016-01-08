#Les denne funksjonen inn i R:
#
# require(devtools)
# source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/EndretVarighet.R")

require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/DataLaster.R")



EndretVarighet <- function(StNr=18700,element="TAM",grense=0,Maks=TRUE,FD=1961,TD=2015,Graf=T,Trend=T){
  Resultat <- c()
  for (aar in FD:TD){
    Data <- Dogn.Laster(StNr=StNr,elementer = element, FD=aar, TD=aar)
    OU ="under"
    if(Maks){
      Resultat <- rbind(Resultat,
                        c(aar,length(Data[Data[,5]<=grense & !is.na(Data[,5]),5])))
    }
    if(!Maks){
      Resultat <- rbind(Resultat,
                        c(aar,length(Data[Data[,5]>grense & !is.na(Data[,5]),5])))
      OU ="over"
    }
  }
  if(Trend){
    Endring <- lm(Resultat[,2]~Resultat[,1])
  }
  if(Graf){
    Overskrift <- paste("Antall dager på", StNr, "med", element, OU, grense,sep=" ")
    #Overskrift <- paste("Antall dager på Blindern med minst 1 cm snø",sep=" ")
    Overskrift <- paste("Antall dager på", StNr, "med", element, OU, grense,sep=" ")
    plot(Resultat[,1],Resultat[,2],pch=20,xlab="",ylab="Dager",main=Overskrift,col="blue")
    if(Trend){
      lines(c(0,10000),c(Endring$coefficients[1],Endring$coefficients[1]+Endring$coefficients[2]*10000))
      E2 <- round(Endring$coefficients[2]*10,1)
      #summary(fit)$coefficients[,4] 40*3,6
      f <- summary(Endring)$fstatistic
      p <- pf(f[1],f[2],f[3],lower.tail=F)
      undertittel <- paste(E2," dager/dekade, p: ",signif(p,2),", R^2 ", signif(as.numeric(summary(Endring)[8]),3),sep="")
      text(min(Resultat[,1]),min(Resultat[,2]),undertittel,pos=4)
    }
  }
  Resultat
}
