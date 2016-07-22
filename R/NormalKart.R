#leses inn ved 
#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/NormalKart.R")

require(devtools)
source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/binleser.R")


#Element kan være "tam" eller "rr" (rr fungerer ikke ennå)
#MND er månedsnummer (1 - 12)
#Periode er 19611990 eller 19712000
# Utdrag er enten NA for hele Norge eller en vektor på formen c(Xmin, XMax, Ymin, Ymax). Stor-Oslo blir noe sånt som: c(250,400,100,300) 
# Eksempel du kan prøve er:
# Normal.Kart(MND=2,Utdrag = c(250,400,100,300))


Normal.Kart <- function(Element="tam",MND=1,Periode=19611990,Utdrag=NA){
  if(MND < 10){MND <- paste("0",MND,sep="")}
  Filnavn <- paste("Z:/",Element,"mth30yr/binary/", Periode, "/",Element,"mth30yr_",Periode,"_",MND,".bil",sep="")
  if(Element=="tam") {
    Data <- (readbinfile(1550*1195,Filnavn)/10)-273}
  if(Element=="rr") {
    Data <- (readbinfile(1550*1195,Filnavn))}
  ST2 <- array(Data, c(1195,1550))
  ST3 <- ST2[,1550:1]
  if (!is.na(Utdrag) & length((Utdrag)==4)){
    ST3 <- ST3[Utdrag[1]:Utdrag[2],Utdrag[3]:Utdrag[4]]
  }
  Nivaa <- c(-20:15);
  LN<-length(Nivaa);
  Farver <- rainbow(LN-1)[(LN-1):1]
  if(Element=="rr"){
    Nivaa <- c(0:20)*50;
    LN<-length(Nivaa)
    Farver <- rainbow(LN-1)
    }

  filled.contour(ST3,levels=Nivaa,axes=F,col = Farver,key.axes = axis(4, Nivaa))
#  filled.contour(ST3,levels=Nivaa,axes=F,col = rainbow(LN-1)[11:1],key.axes = axis(4, Nivaa,labels=c("1.Aug","15.Aug","1.Sep","15.Sep","1.Okt","15.Okt","1.Nov","15.Nov","1.Dec","15.Dec","Nyttår")),main="Første dag med snø",sub=undertittel)
}
