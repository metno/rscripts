rm(list=ls())
library("httr")

client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
sources    <- c("SN18700")
#elements <- c("sum(precipitation_amount P1D)"); tittel = "Daily precipitation at Blindern"; NullMin <- TRUE  # Døgnnedbør RR
elements <- c("surface_snow_thickness"); tittel = "Snow depth at Blindern"; NullMin <- TRUE  # snødybde
#elements <- c("mean(air_temperature P1D)"); tittel = "Mean temperature at Blindern"; NullMin <- FALSE     # Døgnetemperatur TAM
start_date <- "2011-01-01T00:00"
stop_date  <- "2017-12-31T23:00"

url        <- paste("https://", client_id, "@data.met.no/observations/v0.jsonld?",
                    "sources=", paste(sources,collapse=",") ,
                    "&referencetime=", start_date, "/", stop_date,
                    "&elements=", paste(elements,collapse=","),
                    sep = "", collapse= "")


system.time({
  xs <- scan(url, what="")
  print(object.size(xs))
  k  <- grep("referenceTime", xs)
  DataTable <- data.frame(Stnr=xs[k+52],param=xs[k+30], time=xs[k+2], value=as.numeric(gsub(",", "", xs[k+14])))
})

# Mer lesbart datoformat
datetime <- strptime(x = as.character(DataTable$time),
                     format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
DataTable$time <- datetime
if(NullMin){DataTable$value[DataTable$value<0] <- 0}




library(googleVis)
#t1 <-read.table("1.csv", header=TRUE,sep=",",stringsAsFactors = F)
#t1 <- cbind(as.Date(t1[,1]),t1[2])
t1 <- DataTable[,3:4]
#colnames(t1) <- c("Date","temp")
colnames(t1) <- c("Date","Temp")
#cl1 <- gvisCalendar(t1, datevar="Date", numvar="Temp")
#plot(cl1)

cl4 <- gvisCalendar(t1, datevar="Date", numvar="Temp",
                    options=list(
#                      title="Daily rainfall at Blindern",
                      title=tittel,
                      height=1000,width=1000,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                      fontSize: 32, color: '#1A8763', bold: true},
                      cellSize: 15,
                      colorAxis: {colors:['#888888','#000000']},
                      cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                      focusedCellColor: {stroke:'red'}}")
)
plot(cl4)

print(cl4,file="HOHTest2.html")


#cl3 <- gvisCalendar(t1, datevar="Date",
#                    numvar="Temp",
#                    options=list(calendar="{ cellSize: 10 }"))
#plot(cl3)
