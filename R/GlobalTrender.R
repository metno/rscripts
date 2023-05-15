# A small script that visualize the trends for various lengths based on data from NASA-GISS

Trender <- function(Lengder = 15){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,c(1,14)]
  plot(GData[,1],GData[,2],pch=".",ylab="Temperaturavvik",xlab="",main=paste("Globale temperaturtrender for perioder på ", Lengder, " år.", sep="" ))
  legend("topleft",lty=1,col=c("darkred","darkgreen","darkblue"),legend = c("Stigende","Nøytral","Synkende"))
  TAar <- 0
  Aar <- range(GData[,1])
  FAar <- Aar[1]
  Farger <- c("DarkBlue","DarkGreen","DarkRed")
  while (TAar < Aar[2]){
    TAar <- FAar+Lengder-1
    Utdrag <- GData[GData[,1]>=FAar & GData[,1]<=TAar, ]
    Trend <- lm(Utdrag[,2]~Utdrag[,1])
    Koeff <- Trend$coefficients
    Retn <- Koeff[2]/abs(Koeff[2])+2
    lines(c(FAar,TAar),Koeff[1]+Koeff[2]*c(FAar,TAar),col=Farger[Retn])
    FAar=FAar+2
  }
}

for(n in 2:140){Trender(Lengder=n)}
