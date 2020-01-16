#Programmer og snutter for å lage døgnnormaler

Lag.Dogn.Normal <- function(S){
  #Funksjonen kjøres ved at du skriver Lag.Dogn.Normal(Normal)  Der Normal er en vektor med 12 månedverdier fra januar til desember
  S2 <- S[c(7:12,1:12,1:6)] #Lager en ny normalvektor fra Juli til Juli for 2 år
  md <- c(31,28,31,30,31,30,31,31,30,31,30,31) #Dager i månedene
  md2 <- cumsum(md)#Akkumulerte dager i måneden
  md3 <- c((md2[6:12]-365),md2,md2[1:7]+365)#Legger til 1/2 året før og etter
  dagnr <- array(NA,c(3,24))#definerer matrise
  dagnr[1,] <- md3[1:24]#dagen før månedens start
  dagnr[3,] <- md3[2:25]#Måneds slutt
  dagnr[2,] <- (dagnr[1,] + dagnr[3,])/2#Måneds midt
  for (n in 1:10){#Flere itterasjoner for optimalisering av resultatet, skru ned for høyere hastighet
  for (mnd in 1:12){#Kjører måned for måned, justerer "midtpunktet" for best mulig resultat
    dagnr <- plukk.dag(S2,dagnr,mnd)}}
  dogn.normal <- Norm.interp(dagnr,S2)#Lager opp døgnnormalen for de best "midtpunktene"

  siste.sjekk <- array(NA,c(12,7))#Definerer kontrollmatrise
  for (mnd in 1:12){#Sjekker avvik mot dataene
    siste.sjekk[mnd,1:6] <- c(mnd,dagnr[1,mnd+6]+1,dagnr[2,mnd+6],dagnr[3,mnd+6],S[mnd],mean(dogn.normal[(dagnr[1,mnd+6]+1):dagnr[3,mnd+6]]))
    siste.sjekk[mnd,7] <- siste.sjekk[mnd,6] - siste.sjekk[mnd,5]}
  print(siste.sjekk)#Skriver ut kontrollen på skjermen
  
  dogn.normal
}

plukk.dag <- function(S2,dagnr,mnd){#Rutine for å plukke best mulig dag
  if (mnd<7){mnd2 <- c(mnd+6,mnd+12+6);forskyvning <- 365}#gir riktig forsyvning for repitisjonen av måneden
  if (mnd>6){mnd2 <- c(mnd+6,mnd-12+6);forskyvning <- -365}#gir riktig forsyvning for repitisjonen av måneden
  avvik <- array(NA,c(31,3))#Definerer matrise for avvikene for hver kjøring
  dagImnd <- 0#Teller
  for (midt in (dagnr[1,mnd2[1]]+1):dagnr[3,mnd2[1]]){#Kjører med midtpunktet forskøvet
    dagnr[2,mnd2] <- c(midt,midt+forskyvning)#Flytter midten
    estimert <- Norm.interp(dagnr,S2)#Interpolerer
    dagImnd <- dagImnd+1#Teler dagen
    avvik[dagImnd,] <- c(dagImnd,dagnr[2,mnd2[1]],(mean(estimert[(dagnr[1,mnd2[1]]+1):dagnr[3,mnd2[1]]])-S2[mnd2[1]]))}#Teller opp avviket
  minsteavvik <- min(abs(avvik[,3]),na.rm=TRUE)#Minste avvik
  bestedag <- min(avvik[abs(avvik[,3])==minsteavvik,1],na.rm=TRUE);#Dag med minste avvik
  dagnr[2,mnd2] <- c(avvik[bestedag,2],avvik[bestedag,2]+forskyvning)#korrigerer dagnummeret i henhold til minste avvik
  dagnr <- plukk.dag.del(S2,dagnr,mnd)#Kjører en tilsvarende funksjon for deler av dagen, så deler av deler av dagen
  dagnr}

plukk.dag.del <- function(S2,dagnr,mnd){#Plukker beste 100 del av dagen
  if (mnd<7){mnd2 <- c(mnd+6,mnd+12+6);forskyvning <- 365}
  if (mnd>6){mnd2 <- c(mnd+6,mnd-12+6);forskyvning <- -365}
  avvik <- array(NA,c(201,3))
  dagImnd <- 0
  for (midt in seq((dagnr[2,mnd2[1]]-1),(dagnr[2,mnd2[1]]+1),by=0.01)){
    dagnr[2,mnd2] <- c(midt,midt+forskyvning)
    estimert <- Norm.interp(dagnr,S2)
    dagImnd <- dagImnd+1
    avvik[dagImnd,] <- c(dagImnd,dagnr[2,mnd2[1]],(mean(estimert[(dagnr[1,mnd2[1]]+1):dagnr[3,mnd2[1]]])-S2[mnd2[1]]))}
  minsteavvik <- min(abs(avvik[,3]),na.rm=TRUE)
  bestedag <- min(avvik[abs(avvik[,3])==minsteavvik,1],na.rm=TRUE);
  dagnr[2,mnd2] <- c(avvik[bestedag,2],avvik[bestedag,2]+forskyvning)
  dagnr <- plukk.dag.liten.del(S2,dagnr,mnd)
  dagnr}

plukk.dag.liten.del <- function(S2,dagnr,mnd){#plukker beste 10000 av dagen
  if (mnd<7){mnd2 <- c(mnd+6,mnd+12+6);forskyvning <- 365}
  if (mnd>6){mnd2 <- c(mnd+6,mnd-12+6);forskyvning <- -365}
  avvik <- array(NA,c(201,3))
  dagImnd <- 0
  for (midt in seq((dagnr[2,mnd2[1]]-0.01),(dagnr[2,mnd2[1]]+0.01),by=0.0001)){
    dagnr[2,mnd2] <- c(midt,midt+forskyvning)
    estimert <- Norm.interp(dagnr,S2)
    dagImnd <- dagImnd+1
    avvik[dagImnd,] <- c(dagImnd,dagnr[2,mnd2[1]],(mean(estimert[(dagnr[1,mnd2[1]]+1):dagnr[3,mnd2[1]]])-S2[mnd2[1]]))}
  minsteavvik <- min(abs(avvik[,3]),na.rm=TRUE)
  bestedag <- min(avvik[abs(avvik[,3])==minsteavvik,1],na.rm=TRUE);
#  print(c(minsteavvik,bestedag))
  dagnr[2,mnd2] <- c(avvik[bestedag,2],avvik[bestedag,2]+forskyvning)
  dagnr}

Norm.interp <- function(dagnr,S2){#Selve interpolasjonsrutinen
  dogn.normal <- spline(dagnr[2,],S2,n=(dagnr[3,24] - dagnr[1,1])+1,xmin=dagnr[1,1],xmax=dagnr[3,24])#Generer døgnnormal
  X <- dogn.normal$x#Dagene
  Y <- dogn.normal$y#Normalene
  dogn.norm.ut <- Y[X >= 1 & X <= 365]#Plukker ut relevante dager
  #plot(1:365,dogn.norm.ut)
  #points(dagnr[2,7:18],S2[7:18],col="red")
  dogn.norm.ut}

