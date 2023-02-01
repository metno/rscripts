#Lastes rett i R ved
#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/Dobbeltplot.R")

#Bruk "rownames" for å sette navn på radene, f.eks. stasjonsvan og "colnames" for å sette navn på variablene

Dobbeltplot <- function(Datasett=NA, Overskrift="Testplot", Fyllfarve="gray",Y2min0=F){
  #Generere eksempeldata 
  if(is.na(Datasett[1])){
    #Kjør de neste linjene direkte i R for å se strukthuren på datasettet
    Y1 <- c(1:5)
    Y2 <- c(2,4,3,1,5)*10
    Datasett <- cbind(Y1,Y2)
    colnames(Datasett) <- c("Variabel 1","Variabel 2")
    rownames(Datasett) <- c("Tekst1","Tekst2","Tekst3","Tekst4","Tekst5")
  }
  #Start av plottefunksjon, først litt datahåndtering
  X <- c(1:length(Datasett[,1]))
  LX <- length(X)
  Y1 <- Datasett[,1]
  Y2 <- Datasett[,2]
  xgrenser <- range(X)
  Y2Grenser <- range(Y2)
  if(Y2min0){Y2Grenser[1]<-0}
  
  #Definerer rammer for plottet
  par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
  plot(NA,NA,
       ylim=c(0,max(Y1)),xlim=c(xgrenser[1]-0.5,xgrenser[2]+0.5),
       xlab="",ylab=colnames(Datasett)[1],main=Overskrift, xaxt = "n")
  axis(1, at = c(1:LX), labels = rownames(Datasett))
  
  #plotter histogrammet
  for (n in 1:LX){
    polygon(c(X[n]-0.4,X[n]-0.4,X[n]+0.4,X[n]+0.4,X[n]-0.4),c(0,Y1[n],Y1[n],0,0),col=Fyllfarve)
  }
  
  #Legger på strek og prikk (Y2)
  par(new = TRUE)
  plot(X, Y2, xlim=c(xgrenser[1]-0.5,xgrenser[2]+0.5), ylim = c(range(Y2)), pch = 16,              # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "", type="b")
  axis(side = 4, at = pretty(range(Y2)))      # Add second axis
  mtext(colnames(Datasett)[2], side = 4, line = 3)             # Add second axis label
}



#X <- c(1:5)
#Y1 <- c(1:5)
#Y2 <- c(2,4,3,1,5)*10
#ygrenser <- range(c(Y1))
#xgrenser <- range(X)
#par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
#plot(NA,NA,
#     ylim=c(0,ygrenser[2]),xlim=c(xgrenser[1]-0.5,xgrenser[2]+0.5),
#     xlab="",ylab="Y1",main="Testplot", xaxt = "n")
#axis(1, at = c(1:5),
#     labels = c("Tekst1","Tekst2","Tekst3","Tekst4","Tekst5"))
#LX <- length(X)
#for (n in 1:LX){
#  polygon(c(X[n]-0.4,X[n]-0.4,X[n]+0.4,X[n]+0.4,X[n]-0.4),c(0,Y1[n],Y1[n],0,0),col="red")
#}
#par(new = TRUE)
#plot(X, Y2, xlim=c(xgrenser[1]-0.5,xgrenser[2]+0.5), ylim = c(0,max(Y2)), pch = 16,              # Create second plot without axes
#     axes = FALSE, xlab = "", ylab = "", type="b")
#axis(side = 4, at = pretty(range(Y2)))      # Add second axis
#mtext("Y2", side = 4, line = 3)             # Add second axis label

