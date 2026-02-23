library(jsonlite)

DUT <- function(Kommune="301",Winter=T){
  URL <- paste("https://ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c@frost.met.no/api/v1/designtemp?municipalityids=",Kommune,sep="")
  DUTdata <- fromJSON(URL)
  Datasett <-  DUTdata$municipalities$data$Summer
  Tittel <- paste("Design temperature summer for ", Kommune, sep="")
  plass = "bottomright"
  if(Winter){
    Datasett <-  DUTdata$municipalities$data$Winter
    Tittel <- paste("Design temperature winter for ", Kommune, sep="")
    plass = "topright"
  }
  farver <- rainbow(9)
  YRange <- c(min(Datasett[[1]][,3]),max(Datasett[[1]][,3]))
  plot(-1000,-1000,pch=16,ylim=YRange,xlim=c(1,5),
       ylab="Temperature",xlab = "Duration (days)", main=Tittel)
  legend(plass, legend=c("2","5","10","20","25","30","50","100","200"), col=farver, pch=16)
  n=0
  for(f in c(2,5,10,20,25,30,50,100,200)){
    n=n+1
    lines(Datasett[[1]][Datasett[[1]][,2]==f,1], Datasett[[1]][Datasett[[1]][,2]==f,3], pch=16, col=farver[n])
  }
  
  Datasett
}
