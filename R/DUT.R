library(jsonlite)

DUT <- function(Kommune="KS301",Winter=T){
  #fromJSON("https://frost-beta.met.no/api/v1/reports/get?type=dut&settings=%7B%22SourceID%22%3A%22KS301%22%7D")
  URL <- paste("https://frost-beta.met.no/api/v1/reports/get?type=dut&settings=%7B%22SourceID%22%3A%22",Kommune,"%22%7D",sep="")
  DUTdata <- fromJSON(URL)
  Datasett <-  DUTOslo$data$summer
  Tittel <- paste("Design temperature summer for ", Kommune, sep="")
  plass = "bottomright"
  if(Winter){
    Datasett <-  DUTOslo$data$winter
    Tittel <- paste("Design temperature winter for ", Kommune, sep="")
    plass = "topright"
  }
  plot(DUTtest[,4],DUTtest[,2],pch=16,col=farver[DUTtest[,1]],
       ylab="Temperature",xlab = "Return period (years)", main=Tittel)
  legend(plass, legend=c("1 day","2 day","3 day","4 day","5 day"), col=farver, pch=16)
  Datasett
}
