#Commands to use this directly from GIT:
#require(devtools)
#source_url(https://raw.githubusercontent.com/metno/rscripts/master/R/binleser.R")


require(devtools)
source_url(https://raw.githubusercontent.com/metno/rscripts/master/R/iobin.R")


Bin.Leser <- function(Para="TAM",Aar=1980,Mnd=01,Dag=01,dager=3,MaxMin="min",Test=FALSE){
  print(c(Aar,Mnd,Dag))
  #Test <- readbinfile(c(1550,1195),"T:/klimagrid/daily/tam/binary/1980/04/tm_1980_04_01.bil")
  if (Sys.getenv("OS") == "Windows_NT")(Klimadata <- "Z:/")
  if (isTRUE(all.equal(Para,"DUT"))){Klimadata <- "T:/"; StiOgFil <- paste(Klimadata,"hansoh/DimUteTemp/AarligDUT/DUT",dager,MaxMin,Aar,".bil",sep="")}
#  if (isTRUE(all.equal(Para,"DUT"))){ StiOgFil <- paste("T:/hansoh/DimUteTemp/AarligDUT/DUT",dager,MaxMin,Aar,".bil",sep="")}
  Para.Gruppe <- Para
  Para.Id <- Para

  Daily = "daily/"
  if (Test){Daily = "daily_test/"}
  
  ##Daglige grid
  if (!isTRUE(all.equal(Para,"DUT")) & !is.na(Dag)){
    if (isTRUE(all.equal(Para,"TAM"))){Para.Gruppe <- "tam"; Para.Id <- "tm" }
    if (isTRUE(all.equal(Para,"RR"))){Para.Gruppe <- "rr"; Para.Id <- "rr"}
    if (isTRUE(all.equal(Para,"SWE"))){Para.Gruppe <- "swe"; Para.Id <- "swe"}
    if (Mnd < 10){Mnd <- paste("0",Mnd,sep="")}
    if (Dag < 10){Dag <- paste("0",Dag,sep="")}
    Sti <- paste(Klimadata,Para.Gruppe,"/binary/",Aar,"/",Mnd,"/",sep="")
#    Sti <- paste(Klimadata,Daily,Para.Gruppe,"/binary/",Aar,"/",Mnd,"/",sep="")
    #Sti <- paste("T:/klimagrid/daily/",Para.Gruppe,"/binary/",Aar,"/",Mnd,"/",sep="")
    Fil <- paste(Para.Id,"_",Aar,"_",Mnd,"_",Dag,".bil",sep="")
    StiOgFil <- paste(Sti,Fil,sep="");
    #print(StiOgFil)
  }
  print(StiOgFil)
  ##Månedlige grid
  if (!isTRUE(all.equal(Para,"DUT")) & is.na(Dag)){
#    if (isTRUE(all.equal(Para,"RR"))){Para.Gruppe <- "rr"; Para.Id <- "rr"}
#    if (isTRUE(all.equal(Para,"SWE"))){Para.Gruppe <- "swe"; Para.Id <- "swe"}
    if (Mnd < 10){Mnd <- paste("0",Mnd,sep="")}
    Sti <- paste(Klimadata,Daily,Para.Gruppe,"/binary/",Aar,"/",sep="")
    Fil <- paste(Para.Id,"_",Aar,"_",Mnd,".bil",sep="")
    StiOgFil <- paste(Sti,Fil,sep="");#print(StiOgFil)
  }
  #Datasett <- readbinfile(c(1550,1195),StiOgFil)
  Datasett <- readbinfile(1550*1195,StiOgFil)
  Datasett
}
