OverUnderplot <- function(StNr=99840, start = "1961-01-01", stop = "2018-04-30", Overskrift=NA){

  Data <- Dogn.Laster(StNr=StNr,elements = "mean(air_temperature_anomaly P1M 1961_1990)", start = start, stop = stop,Daglig=F)
  LD <- length(Data[,1])
  if(is.na(Overskrift)){Overskrift <- StNr}
  plot(-1000,-10000,xlim=c(1,12),ylim=c(Aar<-as.numeric(format(Data[1,3],format="%Y")),Aar<-as.numeric(format(Data[LD,3],format="%Y"))),xlab="",ylab="", main=Overskrift)

  Data[,4] <- Data[,4]+0.0001
  Data[,4]<-Data[,4]/abs(Data[,4])/2+1.5
  farver=c("blue","red")
  LD <- length(Data[,1])
  for(n in 1:LD){
    Mnd<-as.numeric(format(Data[n,3],format="%m"))
    Aar<-as.numeric(format(Data[n,3],format="%Y"))
    Verd <- Data[n,4]
    PX<-c(Mnd-0.5, Mnd+0.5, Mnd+0.5, Mnd-0.5, Mnd-0.5)
    PY<-c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)

    polygon(PX,PY,col = farver[Verd])
  }
}

OverUnderplot2 <- function(StNr=99840, start = "2000-01-01", stop = "2100-04-30", Overskrift=NA){

  Data <- Dogn.Laster(StNr=StNr,elements = "mean(air_temperature_anomaly P1M 1961_1990)", start = start, stop = stop,Daglig=F)
  LD <- length(Data[,1])
  if(is.na(Overskrift)){Overskrift <- StNr}
  plot(-1000,-10000,xlim=c(1,12),ylim=c(Aar<-as.numeric(format(Data[1,3],format="%Y")),Aar<-as.numeric(format(Data[LD,3],format="%Y"))),xlab="",ylab="", main=Overskrift,lab=c(12,5,7),sub="Temperaturavvik fra normalen")


  Data<- cbind(Data, (Data[,4]+0.001)/abs(Data[,4]+0.001)/2+1.5)
  farver<-c("blue","red")
  LD <- length(Data[,1])
  for(n in 1:LD){
    Mnd<-as.numeric(format(Data[n,3],format="%m"))
    Aar<-as.numeric(format(Data[n,3],format="%Y"))
    FVerd <- Data[n,5]
    Verd <- Data[n,4]
    PX<-c(Mnd-0.5, Mnd+0.5, Mnd+0.5, Mnd-0.5, Mnd-0.5)
    PY<-c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)

    polygon(PX,PY,col = farver[FVerd])
    text(Mnd,Aar,Verd)
  }
}

OverUnderplot3 <- function(StNr=99840, start = "2000-01-01", stop = "2100-04-30", Overskrift=NA){

  Data <- Dogn.Laster(StNr=StNr,elements = "mean(air_temperature_anomaly P1M 1961_1990)", start = start, stop = stop,Daglig=F)
  LD <- length(Data[,1])
  if(is.na(Overskrift)){Overskrift <- StNr}
  plot(-1000,-10000,xlim=c(1,12),ylim=c(Aar<-as.numeric(format(Data[1,3],format="%Y")),Aar<-as.numeric(format(Data[LD,3],format="%Y"))),xlab="",ylab="", main=Overskrift, sub="Basert på avvik fra Normalen (1961 - 1990)", lab=c(12,5,7))


  Data<- cbind(Data, (Data[,4]+0.001)/abs(Data[,4]+0.001)/2+1.5)
  farver<-c("blue","red")
  LD <- length(Data[,1])
  teller <- 0
  for(n in 1:LD){
    Mnd<-as.numeric(format(Data[n,3],format="%m"))
    Aar<-as.numeric(format(Data[n,3],format="%Y"))
    FVerd <- Data[n,5]
    Verd <- Data[n,4]

    if(n == 1){teller <- 1; LVerd <- FVerd}
    if(n > 1){
      if (FVerd==LVerd){teller <- teller + 1}
      if (FVerd!=LVerd){teller <- 1}
      LVerd <- FVerd
    }


    PX<-c(Mnd-0.5, Mnd+0.5, Mnd+0.5, Mnd-0.5, Mnd-0.5)
    PY<-c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)

    polygon(PX,PY,col = farver[FVerd])
    text(Mnd,Aar,teller)
  }
}

OverUnderplot4 <- function(StNr=99840, start = "1800-01-01", stop = "2100-04-30", Overskrift=NA){
  
  Data <- Dogn.Laster(StNr=StNr,elements = "mean(air_temperature_anomaly P1M 1961_1990)", start = start, stop = stop,Daglig=F)
  LD <- length(Data[,1])
  if(is.na(Overskrift)){Overskrift <- StNr}
  plot(-1000,-10000,xlim=c(1,12),ylim=c(Aar<-as.numeric(format(Data[1,3],format="%Y")),Aar<-as.numeric(format(Data[LD,3],format="%Y"))),xlab="",ylab="", main=Overskrift, sub="Basert på avvik fra Normalen (1961 - 1990)", lab=c(12,5,7))
  
  
  DMax <- max(Data[,4],na.rm = T)
  DMin <- min(Data[,4],na.rm = T)
  DRange <- max(c(DMin,DMax))
  
  Data<- cbind(Data, (Data[,4]+0.001)/abs(Data[,4]+0.001)/2+1.5, ceiling(Data[,4]+DRange))
#  farver<-c("blue","red")
  farver <- colorRampPalette(c("darkblue","blue","white","red","darkred"))(2*DRange)
  LD <- length(Data[,1])
  teller <- 0
  for(n in 1:LD){
    Mnd<-as.numeric(format(Data[n,3],format="%m"))
    Aar<-as.numeric(format(Data[n,3],format="%Y"))
    FVerd <- Data[n,5]
    Verd <- Data[n,4]
    
    if(n == 1){teller <- 1; LVerd <- FVerd}
    if(n > 1){
      if (FVerd==LVerd){teller <- teller + 1}
      if (FVerd!=LVerd){teller <- 1}
      LVerd <- FVerd
    }
    
    
    PX<-c(Mnd-0.5, Mnd+0.5, Mnd+0.5, Mnd-0.5, Mnd-0.5)
    PY<-c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)
    
#    polygon(PX,PY,col = farver[FVerd])
    polygon(PX,PY,col = farver[Data[n,6]])
    text(Mnd,Aar,teller)
  }
}

OverUnderplot5 <- function(StNr=99840, start = "1800-01-01", stop = "2100-04-30", Overskrift=NA){
  
  Data <- Dogn.Laster(StNr=StNr,elements = "mean(air_temperature_anomaly P1M 1961_1990)", start = start, stop = stop,Daglig=F)
  LD <- length(Data[,1])
  if(is.na(Overskrift)){Overskrift <- StNr}
  plot(-1000,-10000,xlim=c(1,12),ylim=c(Aar<-as.numeric(format(Data[1,3],format="%Y")),Aar<-as.numeric(format(Data[LD,3],format="%Y"))),xlab="",ylab="", main=Overskrift, sub="Basert på avvik fra Normalen (1961 - 1990)", lab=c(12,5,7))
  
  
  DMax <- max(Data[,4],na.rm = T)
  DMin <- min(Data[,4],na.rm = T)
  DRange <- max(c(DMin,DMax))
  
  Data<- cbind(Data, (Data[,4]+0.001)/abs(Data[,4]+0.001)/2+1.5, ceiling(Data[,4]+DRange))
  #  farver<-c("blue","red")
  farver <- colorRampPalette(c("darkblue","blue","white","red","darkred"))(2*DRange)
  LD <- length(Data[,1])
  teller <- 0
  for(n in 1:LD){
    Mnd<-as.numeric(format(Data[n,3],format="%m"))
    Aar<-as.numeric(format(Data[n,3],format="%Y"))
    FVerd <- Data[n,5]
    Verd <- Data[n,4]
    
    if(n == 1){teller <- 1; LVerd <- FVerd}
    if(n > 1){
      if (FVerd==LVerd){teller <- teller + 1}
      if (FVerd!=LVerd){teller <- 1}
      LVerd <- FVerd
    }
    
    
    PX<-c(Mnd-0.5, Mnd+0.5, Mnd+0.5, Mnd-0.5, Mnd-0.5)
    PY<-c(Aar-0.5, Aar-0.5, Aar+0.5, Aar+0.5, Aar-0.5)
    
    #    polygon(PX,PY,col = farver[FVerd])
    polygon(PX,PY,col = farver[Data[n,6]])
    text(Mnd,Aar,paste(teller, " / ",Data[n,4],sep=""))
  }
}








Dogn.Laster <- function(start = "2017-01-01",
                        stop = "2017-01-31",
                        elements = c("sum(precipitation_amount P1D)"),
                        StNr = 18700,
                        Daglig=T){
  client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"
  #sources    <- c("SN18700")
  #elements <- c("sum(precipitation_amount P1D)")  # Døgnnedbør RR
  #elements <- c("mean(air_temperature P1D)")     # Døgnetemperatur TAM
  #elements <- c("surface_snow_thickness P1D")     # Snødybde SA
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
#print(url)

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
