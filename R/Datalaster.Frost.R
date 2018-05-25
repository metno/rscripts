#Commands to use this directly from GIT:
#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/DataLaster.Frost.R")

#rm(list=ls())
library("httr")

Data.Laster <- function(start = "2017-01-01",
                        stop = "2017-01-31",
                        elements = c("sum(precipitation_amount P1D)"),
                        StNr = 18700,
                        Daglig=T){
  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  #sources    <- c("SN18700")
  #elements <- c("sum(precipitation_amount P1D)")  # Døgnnedbør RR
  #elements <- c("mean(air_temperature P1D)")     # Døgnetemperatur TAM
  #elements <- c("surface_snow_thickness P1D")     # Snødybde SA
  #elements = "mean(air_temperature_anomaly P1M 1961_1990)" #Maanedstemperaturavvik
  #start_date <- "2013-01-01T00:00"
  #stop_date  <- "2017-12-31T23:00"
  start_date <- paste(start,"T00:00",sep="")
  stop_date  <- paste(stop,"T23:00",sep="")
  sources <- paste("SN",StNr,sep="")

  url        <- paste("https://", client_id, "@frost.met.no/observations/v0.jsonld?",
                      "sources=", paste(sources,collapse=",") ,
                      "&referencetime=", start_date, "/", stop_date,
                      "&elements=", paste(elements,collapse=","),
                      sep = "", collapse= "")
  if(Daglig){url<-paste(url,"&timeresolutions=P1D",sep="")}


  system.time({
    xs <- scan(url, what="")
    print(object.size(xs))
    k  <- grep("referenceTime", xs)
    DataTable <- data.frame(Stnr=xs[k-2],param=xs[k+10], time=xs[k+2], value=as.numeric(gsub(",", "", xs[k+14])))
  })

  # Mer lesbart datoformat
  datetime <- strptime(x = as.character(DataTable$time),
                       format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
  DataTable$time <- datetime
  DataTable
}
