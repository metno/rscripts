# --~-- windrose.R -~--
# Plot Directional Vector Histogram (Rose Diagrams) using MET Norway's wind
# data
#
# History:
# 2014.11.26 Cristian Lussana. original code. (cristian.lussana@met.no)
#------------------------------------------------------------------------------
# MAIN ========================================================================
source("./windrose_lib.R")
# [] Read command line arguments
arguments <- commandArgs()
arguments
date.b.string<-arguments[3]
date.e.string<-arguments[4]
if (length(arguments)!=4) {
  print("Error in command line arguments:")
  print("R --vanilla yyyy.mm.dd.hh yyyy.mm.dd.hh")
  quit(status=1)
}
# date.x.string<-"YYYY.MM.DD.HH"
#                 1234567890123
start.string<-paste(date.b.string,sep="")
end.string<-paste(date.e.string,sep="")
start.string.day<-paste(substr(start.string,1,10),".01",sep="")
end.string.day  <-paste(substr(end.string,1,10),".24",sep="")
#------------------------------------------------------------------------------
# [] create a sequence of days
start <- strptime(start.string.day,"%Y.%m.%d.%H","UTC")
end <- strptime(end.string.day,"%Y.%m.%d.%H","UTC")
#
timeseq<-as.POSIXlt(seq(as.POSIXlt(start),as.POSIXlt(end),by="1 hour"),"UTC")
nhour<-length(timeseq)
nhour.day<-  length(which(timeseq$hour>6 & timeseq$hour<=18))
nhour.night<-nhour-nhour.day
#
start.day <- strptime(start.string.day,"%Y.%m.%d.%H","UTC")
end.day <- strptime(end.string.day,"%Y.%m.%d.%H","UTC")
dayseq<-as.POSIXlt(seq(as.POSIXlt(start.day),as.POSIXlt(end.day),by="1 day"),"UTC")
nday<-length(dayseq)
monthseq<-as.POSIXlt(seq(as.POSIXlt(start.day),as.POSIXlt(end.day),by="1 month"),"UTC")
nmonth<-length(monthseq)
monthseq<-as.POSIXlt(seq(as.POSIXlt(start.day),as.POSIXlt(end.day),by="1 month"),"UTC")
nmonth<-length(monthseq)
yearseq<-unique(dayseq$year+1900)
nyear<-length(yearseq)
print(yearseq)
#------------------------------------------------------------------------------
# [] Read Station Information 
myurl<-paste("http://klapp.oslo.dnmi.no/metnopub/production/metno?re=27&ct=text/plain",
             "&del=semicolon&tab=T_ELEM_OBS&p=FF",
             "&geo=cnr&geo=lat&geo=utm&geo=AMSL&geo=name&nod=NA",sep="")
print(myurl)
o.cont<-1
while (o.cont<=10) {
  stataux<-NULL
  try(stataux <-read.table(myurl, header = TRUE,  sep = ";",
                         stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1",
                         encoding = "UTF-8", quote = "",na.string=-999))
  if (length(stataux)<10) {
    print("exit with error in command:")
        print(myurl)
        o.cont<-o.cont+1
        Sys.sleep(5)
#      q(status=1)
  } else {
    break
  }
}
if (o.cont>10) {
  print("Fatal Error in command:")
  print(myurl)
  print(stataux)
  q(status=1)
}
print(names(stataux))
#> print(names(stataux))
# [1] "STNR"       "ELEM_CODE"  "FROMDATE"   "TODATE"     "TABLE_NAME"
# [6] "COUNTYID"   "COUNTY"     "LAT"        "LON"        "UTM_NORTH" 
#[11] "UTM_EAST"   "UTM_ZONE"   "NAME"
# countyid
#  1 Østfold
#  2 Akershus
#  3 Oslo
#  4 Hedmark
#  5 Oppland
#  6 Buskerud
#  7 Vestfold
#  8 Telemark
#  9 Aust-Agder
# 10 Vest-Agder
# 11 Rogaland
# 12 Hordaland
# 14 Sogn Og Fjordane
# 15 Møre Og Romsdal
# 17 Nord-Trøndelag
# 18 Nordland
# 19 Troms
# 20 Finnmark
# 21 Svalbard
# 22 Jan Mayen
# 23 Flyttbare Plattformer
# 24 Norskehavet
# 25 Barentshavet
# 26 Nordsjøen
# 28 skip
# -1 utland_data
indx<-which( !is.na(as.numeric(stataux$UTM_EAST)) & 
             !is.na(as.numeric(stataux$UTM_NORTH)) & 
             !is.na(as.numeric(stataux$AMSL)) &
             as.numeric(stataux$COUNTYID)<=20 )
stations<-data.frame(matrix(nrow=length(indx),ncol=8))
# [1] "STNR"       "ELEM_CODE"  "FROMDATE"   "TODATE"     "TABLE_NAME"
# [6] "COUNTYID"   "COUNTY"     "LAT"        "LON"        "UTM_NORTH" 
#[11] "UTM_EAST"   "UTM_ZONE"   "NAME"
names(stations)<-c("Stnr","z","x","y","name","countyid","fromdate","todate")
stations$Stnr<-as.numeric(stataux$STNR[indx])
stations$z<-stataux$AMSL[indx]
stations$x<-stataux$UTM_EAST[indx]
stations$y<-stataux$UTM_NORTH[indx]
stations$name<-stataux$NAME[indx]
stations$countyid<-stataux$COUNTYID[indx]
stations$fromdate<-stataux$FROMDATE[indx]
stations$todate<-stataux$TODATE[indx]
stations$todate[which(stations$todate=="NA")]<-"current"
# main loop: for each station...
L.y.tot<-length(stations$Stnr)
for (s in 1:L.y.tot) {
  if (exists("OBS")) rm(OBS)
  OBS<-data.frame(matrix(nrow=8760*length(yearseq),ncol=8))
  names(OBS)<-c("Stnr","Year","Month","Day","Hour","FF","DD","datetime")
  ini<-1
  end<-0
  for (yyyy in yearseq) {
    print(paste("yyyy=",yyyy))
    if (yyyy == yearseq[1]) {
      date.b<-paste(dayseq$mday[1],".",(dayseq$mon[1]+1),".",yyyy,sep="")
    } else {
      date.b<-paste("1.1.",yyyy,sep="")
    }
    if (yyyy == yearseq[length(yearseq)]) {
      date.e<-paste(dayseq$mday[nday],".",(dayseq$mon[nday]+1),".",yyyy,sep="")
    } else {
      date.e<-paste("31.12.",yyyy,sep="")
    }
    # get data from KDVH
    ulric<-paste("http://klapp.oslo.dnmi.no/metnopub/production/",
                 "metno?re=17&p=FF&p=DD&fd=",date.b,"&td=",date.e,
                 "&s=",stations$Stnr[s],
                 "&nob=0.0&ddel=dot&del=semicolon&nmt=0",
                 "&ct=text/plain&split=1&nod=-999",sep="")
    print(ulric)
    o<-NULL
    try(o <- read.table(ulric, header = TRUE,  sep = ";", #nrows = nrows,
            stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1",
              encoding = "UTF-8", 
                quote = "",na.strings="-999"))
#    if (length(which(!is.na(o$FF)))==0 | length(which(!is.na(o$DD)))==0) next
    if (is.null(o$FF) | is.null(o$DD)) next
    indx<-which(!is.na(o$FF) & !is.na(o$DD))
    L.y.fromKDVH<-length(indx)
    if (L.y.fromKDVH==0) next
    end<-L.y.fromKDVH+ini-1
    OBS$Stnr[ini:end]<-as.numeric(o$Stnr[indx])
    OBS$Year[ini:end]<-o$Year[indx]
    OBS$Month[ini:end]<-o$Month[indx]
    OBS$Day[ini:end]<-o$Day[indx]
    OBS$Hour[ini:end]<-as.integer(o$Time.UTC.[indx])
    OBS$FF[ini:end]<-as.numeric(o$FF[indx])
    OBS$DD[ini:end]<-as.numeric(o$DD[indx])
#    rm(indx)
#    OBS$datetime<-as.POSIXlt(strptime(paste(OBS$Year,
#                                        formatC(OBS$Month,width=2,flag="0"),
#                                        formatC(OBS$Day,width=2,flag="0"),
#                                        formatC(OBS$Hour,width=2,flag="0"),sep=""),"%Y%m%d%H"),"UTC")
#    print(paste("OBS",yyyy,"\n"))
#    print(paste(OBS$Stnr,OBS$Year,OBS$Month,OBS$Day,OBS$Hour,OBS$FF,OBS$DD,OBS$datetime,"\n"))
    ini<-end+1
  }
  if (end==0) next
  yyyy.tot<-vector(mode="numeric",length=end)
  mm.tot<-vector(mode="numeric",length=end)
  dd.tot<-vector(mode="numeric",length=end)
  hh.tot<-vector(mode="numeric",length=end)
  speed.tot<-vector(mode="numeric",length=end)
  dir.tot<-vector(mode="numeric",length=end)
  speed.tot<-OBS$FF[1:end]
  dir.tot<-OBS$DD[1:end]
  yyyy.tot<-OBS$Year[1:end]
  mm.tot<-OBS$Month[1:end]
  dd.tot<-OBS$Day[1:end]
  hh.tot<-OBS$Hour[1:end]
  idx.day<-which(hh.tot>6 & hh.tot<=18)
  speed.day<-speed.tot[idx.day]
  dir.day<-dir.tot[idx.day]
  speed.night<-speed.tot[-idx.day]
  dir.night<-dir.tot[-idx.day]
  print(paste(speed.tot,dir.tot,"\n"))
  if (exists("OBS")) rm(OBS)
  # output on png file
  png(file=paste("windrose_",formatC(stations$Stnr[s],width=7,flag="0",format="d"),".png",sep=""),width=800,height=800)
  scala <- c(0.5,2,4,6,8,10)
  title1 <- paste(stations$name[s]," (Stnr=",stations$Stnr[s],")",sep="")
  title2 <- paste("from ",date.b.string," to ",date.e.string,sep="")
  print("station")
  print(stations$todate[s])
  print(is.na(stations$todate[s]))
  operative <- paste("operative from ",stations$fromdate[s]," to ",stations$todate[s],sep="")
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),widths=c(1,1), heights=c(1.5,1))
  print("graph 1")
  res <- rose2(time.description = "day&night",dir=dir.tot , mag=speed.tot, nplumes = 16 ,
               shrink=1, shrink.top=0.8, shrink.bottom=0.8,
               fg="black", bg="linen", border="black", lwd.border=1,
               mag.bins=scala, main1 = title1, main2=title2,
               mag.col=c("springgreen","blue","cyan", "yellow","tomato"),
               fg.lows="black", bg.lows="azure", rscale=NULL, rings=TRUE, ring.labels=TRUE,
               lwd.ring=1, lty.ring=2, col.ring="black", lwd.axis=1, cex.lab=1.6, cex.dir=1.5,
               n.tot=nhour, operative=operative)
  print("graph 2")
  res <- rose2(time.description = "day",dir=dir.day , mag=speed.day, nplumes = 16 ,
               shrink=1, shrink.top=0.8, shrink.bottom=0.8,
               fg="black", bg="linen", border="black", lwd.border=1,
               mag.bins=scala, #main1 = title1, main2=title2,
               mag.col=c("springgreen","blue","cyan", "yellow","tomato"),
               fg.lows="black", bg.lows="azure", rscale=NULL, rings=TRUE, ring.labels=TRUE,
               lwd.ring=1, lty.ring=2, col.ring="black", lwd.axis=1, cex.lab=1, cex.dir=1 ,
               legend=F,n.tot=nhour.day)
  print("graph 3")
  res <- rose2(time.description = "night",dir=dir.night , mag=speed.night, nplumes = 16 ,
               shrink=1, shrink.top=0.8, shrink.bottom=0.8,
               fg="black", bg="linen", border="black", lwd.border=1,
               mag.bins=scala, #main1 = title1, main2=title2,
               mag.col=c("springgreen","blue","cyan", "yellow","tomato"),
               fg.lows="black", bg.lows="azure", rscale=NULL, rings=TRUE, ring.labels=TRUE,
               lwd.ring=1, lty.ring=2, col.ring="black", lwd.axis=1, cex.lab=1, cex.dir=1,
               legend=F,n.tot=nhour.night)
  dev.off()
}
q()
