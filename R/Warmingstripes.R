library(readxl)
setwd("~/Rscript/ClimateStripes")

NorgeAar <- read_xlsx("NorgeAar.xlsx")

WarmingStripes <- function(Dataset,Fargedef){
  Dataset <- Dataset[!is.na(Dataset[,3]),]
  RD <- range(Dataset[,4],na.rm=T)
  StepD <- (RD[2]-RD[1])/16
  Klasser <- cbind(RD[1] + (0:15)*StepD, RD[1] + (1:16)*StepD)
  Dataset <- cbind(Dataset,NA)
  for(n in 1:16){
    Dataset[Dataset[,4]>=Klasser[n,1] & Dataset[,4]<=Klasser[n,2],5] <- 17-n
  }
  plot(-1000,-1000,ylim=c(0,5),xlim=range(as.numeric(Dataset[,3])),xlab="",ylab="", yaxt = "n",axes=FALSE)
  for (Aar in c(1901:2023)){
    polygon(x=c(Aar-0.5,Aar-0.5,Aar+0.5,Aar+0.5,Aar-0.5),y=c(-10,10,10,-10,-10),col=Fargedef[Dataset[Dataset[,3]==Aar,5],2],border = NA)
  }
}


Fargedef <- rbind(
  c( 8,"#67000d"), 
  c( 7,"#a50f15"), 
  c( 6,"#cb181d"), 
  c( 5,"#ef3b2c"), 
  c( 4,"#fb6a4a"), 
  c( 3,"#fc9272"), 
  c( 2,"#fcbba1"), 
  c( 1,"#fee0d2"), 
  c(-1,"#deebf7"), 
  c(-2,"#c6dbef"), 
  c(-3,"#9ecae1"), 
  c(-4,"#6baed6"), 
  c(-5,"#4292c6"), 
  c(-6,"#2171b5"), 
  c(-7,"#08519c"), 
  c(-8,"#08306b"))
