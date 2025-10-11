
GStatus.poly.Norm <- function(Y=2025,M=1,YRef=2024){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  D2 <- GData[,1:2]
  for (n in 2:12){
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
  }
  plot(1000,1000,type="l",xlim=c(1,12),ylim=c(-1,1.8),xlab="Month",ylab="Temperature deviation",main="NASA - GISS", sub="Normal from the dataset")
#  NY <- length(D2[,1])
  NY <- length(D2[,1])
  for (n in 1:NY){
    #    lines(c(1:12),D2[n,2:13]/100,col="gray")
    lines(c(1:11,12.05),D2[n,2:13],col="gray")
  }
  
  
  MaxPre  <- c()
  Max0130 <- c()
  Max3160 <- c()
  Max6190 <- c()
  Max9120 <- c()
  Max2150 <- c()
  MaxNaa  <- c()
  
  MinPre  <- c()
  Min0130 <- c()
  Min3160 <- c()
  Min6190 <- c()
  Min9120 <- c()
  Min2150 <- c()
  MinNaa  <- c()
  
  MeanPre  <- c()
  Mean0130 <- c()
  Mean3160 <- c()
  Mean6190 <- c()
  Mean9120 <- c()
  Mean2150 <- c()
  MeanNaa  <- c()
  
  SDPre  <- c()
  SD0130 <- c()
  SD3160 <- c()
  SD6190 <- c()
  SD9120 <- c()
  SD2150 <- c()
  SDNaa  <- c()
  
  for(n in 2:13){
    MaxPre <- c(MaxPre,max(D2[D2[,1] <= 1900,n],na.rm=T))
    MinPre <- c(MinPre,min(D2[D2[,1] <= 1900,n],na.rm=T))
    SDPre <- c(SDPre,sd(D2[D2[,1] <= 1900,n],na.rm=T))
    MeanPre <- c(MeanPre,mean(D2[D2[,1] <= 1900,n],na.rm=T))
    
    Max0130 <- c(Max0130,max(D2[D2[,1]>1900 & D2[,1]<=1930,n],na.rm=T))
    Min0130 <- c(Min0130,min(D2[D2[,1]>1900 & D2[,1]<=1930,n],na.rm=T))
    SD0130 <- c(SD0130,sd(D2[D2[,1]>1900 & D2[,1]<=1930,n],na.rm=T))
    Mean0130 <- c(Mean0130,mean(D2[D2[,1]>1900 & D2[,1]<=1930,n],na.rm=T))
    
    Max3160 <- c(Max3160,max(D2[D2[,1]>1930 & D2[,1]<=1960,n],na.rm=T))
    Min3160 <- c(Min3160,min(D2[D2[,1]>1930 & D2[,1]<=1960,n],na.rm=T))
    SD3160 <- c(SD3160,sd(D2[D2[,1]>1930 & D2[,1]<=1960,n],na.rm=T))
    Mean3160 <- c(Mean3160,mean(D2[D2[,1]>1930 & D2[,1]<=1960,n],na.rm=T))
    
    Max6190 <- c(Max6190,max(D2[D2[,1]>1960 & D2[,1]<=1990,n],na.rm=T))
    Min6190 <- c(Min6190,min(D2[D2[,1]>1960 & D2[,1]<=1990,n],na.rm=T))
    SD6190 <- c(SD6190,sd(D2[D2[,1]>1960 & D2[,1]<=1990,n],na.rm=T))
    Mean6190 <- c(Mean6190,mean(D2[D2[,1]>1960 & D2[,1]<=1990,n],na.rm=T))
    
    Max9120 <- c(Max9120,max(D2[D2[,1]>1990 & D2[,1]<=2020,n],na.rm=T))
    Min9120 <- c(Min9120,min(D2[D2[,1]>1990 & D2[,1]<=2020,n],na.rm=T))
    SD9120 <- c(SD9120,sd(D2[D2[,1]>1990 & D2[,1]<=2020,n],na.rm=T))
    Mean9120 <- c(Mean9120,mean(D2[D2[,1]>1990 & D2[,1]<=2020,n],na.rm=T))
    
#    Max2150 <- c(Max9120,max(D2[D2[,1]>2020 & D2[,1]<=2050,n],na.rm=T))
#    Min2150 <- c(Min9120,min(D2[D2[,1]>2020 & D2[,1]<=2050,n],na.rm=T))
#    SD2150 <- c(SD9120,sd(D2[D2[,1]>2020 & D2[,1]<=2050,n],na.rm=T))
#    Mean2150 <- c(Mean9120,mean(D2[D2[,1]>2020 & D2[,1]<=2050,n],na.rm=T))
        
    MaxNaa <- c(MaxNaa,max(D2[D2[,1] > 2020,n],na.rm=T))
    MinNaa <- c(MinNaa,min(D2[D2[,1] > 2020,n],na.rm=T))
    SDNaa <- c(SDNaa,sd(D2[D2[,1] > 2020,n],na.rm=T))
    MeanNaa <- c(MeanNaa,mean(D2[D2[,1] > 2020,n],na.rm=T))
  }
  
  Farver <- colorRampPalette(c("blue","red"))(6)
  Farver <- colorRampPalette(c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)), alpha = TRUE)(5)
  
  XPol <- c(1:12,12:1,1)
  
  #polygon(x=XPol, y=c(MeanPre+SDPre,MeanPre[12:1]-SDPre[12:1],MeanPre[1]+SDPre[1]), col=Farver[1], border = NA)
  #polygon(x=XPol, y=c(Mean0130+SD0130,Mean0130[12:1]-SD0130[12:1],Mean0130[1]+SD0130[1]), col=Farver[2], border = NA)
  #polygon(x=XPol, y=c(Mean3160+SD3160,Mean3160[12:1]-SD3160[12:1],Mean3160[1]+SD3160[1]), col=Farver[3], border = NA)
  #polygon(x=XPol, y=c(Mean6190+SD6190,Mean6190[12:1]-SD6190[12:1],Mean6190[1]+SD6190[1]), col=Farver[4], border = NA)
  #polygon(x=XPol, y=c(Mean9120+SD9120,Mean9120[12:1]-SD9120[12:1],Mean9120[1]+SD9120[1]), col=Farver[5], border = NA)
  #polygon(x=XPol, y=c(MeanNaa+SDNaa,MeanNaa[12:1]-SDNaa[12:1],MeanNaa[1]+SDNaa[1]), col=Farver[6], border = NA)
  
  print(length(XPol))
  print(length(c(MaxNaa,MinNaa[12:1],MaxNaa[1])))
  print(Max9120)
  print(MaxNaa)
  
  polygon(x=XPol, y=c(MaxPre,MinPre[12:1],MaxPre[1]), col=Farver[1],border = NA)
  polygon(x=XPol, y=c(Max0130,Min0130[12:1],Max0130[1]), col=Farver[2],border = NA)
  polygon(x=XPol, y=c(Max3160,Min3160[12:1],Max3160[1]), col=Farver[3],border = NA)
  polygon(x=XPol, y=c(Max6190,Min6190[12:1],Max6190[1]), col=Farver[4],border = NA)
  polygon(x=XPol, y=c(Max9120,Min9120[12:1],Max9120[1]), col=Farver[5],border = NA)
#  polygon(x=XPol, y=c(Max2150,Min2150[12:1],Max2150[1]), col=Farver[5],border = NA)
#  polygon(x=XPol, y=c(MaxNaa,MinNaa[12:1],MaxNaa[1]), col=Farver[6],border = NA)

  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    #lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #print(D3)
  D3Max <- c()
  D3Min <- c()
  D3X <- M:12
  
  for (m in D3X+1){
    D3Max <- c(D3Max,max(D3[,m],na.rm = T))
    D3Min <- c(D3Min,min(D3[,m],na.rm = T))
  }
  LD3X <- length(D3X)
  polygon(c(D3X,D3X[LD3X:1],M),c(D3Max,D3Min[LD3X:1],D3Max[1]),col=rgb(1,0.65,0,0.75),border=NA)
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  
  polygon(c(0.8,1.8,1.8,0.8,0.8),c(-0.9,-0.9,-1.1,-1.1,-0.9), col=Farver[1],border = NA)
  text(1.3,-1,"    -1901")
  polygon(c(2,3,3,2,2),c(-0.9,-0.9,-1.1,-1.1,-0.9), col=Farver[2],border = NA)
  text(2.5,-1,"1901-1930")
  polygon(c(3.2,4.2,4.2,3.2,3.2),c(-0.9,-0.9,-1.1,-1.1,-0.9), col=Farver[3],border = NA)
  text(3.7,-1,"1931-1960")
  polygon(c(4.4,5.4,5.4,4.4,4.4),c(-0.9,-0.9,-1.1,-1.1,-0.9), col=Farver[4],border = NA)
  text(4.9,-1,"1961-1990")
  polygon(c(5.6,6.6,6.6,5.6,5.6),c(-0.9,-0.9,-1.1,-1.1,-0.9), col=Farver[5],border = NA)
  text(6.1,-1,"1991-2020")
  lines(c(6.8, 7.2),c(-1,-1),lwd=2,col="red")
  text(7.65,-1,"Current year")
  lines(c(10.2, 10.6),c(-1,-1),lwd=2,col="blue")
  text(10.85,-1,YRef)
  lines(c(8.2, 8.6),c(-1,-1),lwd=2,col="orange")
  text(9.3,-1,"Possible outcomes" )
}


GStatus.poly <- function(Y=2020,M=9,YRef=2016){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  D2 <- GData[,1:2]
  for (n in 2:12){
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
  }
  plot(1000,1000,type="l",xlim=c(1,12),ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS", sub="Normal fra datasettet")
  NY <- length(D2[,1])
  
  MaxPre  <- c()
  Max0125 <- c()
  Max2650 <- c()
  Max5175 <- c()
  Max7600 <- c()
  MaxNaa  <- c()
  
  MinPre  <- c()
  Min0125 <- c()
  Min2650 <- c()
  Min5175 <- c()
  Min7600 <- c()
  MinNaa  <- c()
  
  MeanPre  <- c()
  Mean0125 <- c()
  Mean2650 <- c()
  Mean5175 <- c()
  Mean7600 <- c()
  MeanNaa  <- c()
  
  SDPre  <- c()
  SD0125 <- c()
  SD2650 <- c()
  SD5175 <- c()
  SD7600 <- c()
  SDNaa  <- c()
  
  for(n in 2:13){
    MaxPre <- c(MaxPre,max(D2[D2[,1] <= 1900,n],na.rm=T))
    MinPre <- c(MinPre,min(D2[D2[,1] <= 1900,n],na.rm=T))
    SDPre <- c(SDPre,sd(D2[D2[,1] <= 1900,n],na.rm=T))
    MeanPre <- c(MeanPre,mean(D2[D2[,1] <= 1900,n],na.rm=T))
    
    Max0125 <- c(Max0125,max(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    Min0125 <- c(Min0125,min(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    SD0125 <- c(SD0125,sd(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    Mean0125 <- c(Mean0125,mean(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    
    Max2650 <- c(Max2650,max(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    Min2650 <- c(Min2650,min(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    SD2650 <- c(SD2650,sd(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    Mean2650 <- c(Mean2650,mean(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    
    Max5175 <- c(Max5175,max(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    Min5175 <- c(Min5175,min(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    SD5175 <- c(SD5175,sd(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    Mean5175 <- c(Mean5175,mean(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    
    Max7600 <- c(Max7600,max(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    Min7600 <- c(Min7600,min(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    SD7600 <- c(SD7600,sd(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    Mean7600 <- c(Mean7600,mean(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    
    MaxNaa <- c(MaxNaa,max(D2[D2[,1] > 2000,n],na.rm=T))
    MinNaa <- c(MinNaa,min(D2[D2[,1] > 2000,n],na.rm=T))
    SDNaa <- c(SDNaa,sd(D2[D2[,1] > 2000,n],na.rm=T))
    MeanNaa <- c(MeanNaa,mean(D2[D2[,1] > 2000,n],na.rm=T))
  }
  
  Farver <- colorRampPalette(c("blue","red"))(6)
  Farver <- colorRampPalette(c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), alpha = TRUE)(6)
  
  XPol <- c(1:12,12:1,1)
  
  polygon(x=XPol, y=c(MeanPre+SDPre,MeanPre[12:1]-SDPre[12:1],MeanPre[1]+SDPre[1]), col=Farver[1], border = NA)
  polygon(x=XPol, y=c(Mean0125+SD0125,Mean0125[12:1]-SD0125[12:1],Mean0125[1]+SD0125[1]), col=Farver[2], border = NA)
  polygon(x=XPol, y=c(Mean2650+SD2650,Mean2650[12:1]-SD2650[12:1],Mean2650[1]+SD2650[1]), col=Farver[3], border = NA)
  polygon(x=XPol, y=c(Mean5175+SD5175,Mean5175[12:1]-SD5175[12:1],Mean5175[1]+SD5175[1]), col=Farver[4], border = NA)
  polygon(x=XPol, y=c(Mean7600+SD7600,Mean7600[12:1]-SD7600[12:1],Mean7600[1]+SD7600[1]), col=Farver[5], border = NA)
  polygon(x=XPol, y=c(MeanNaa+SDNaa,MeanNaa[12:1]-SDNaa[12:1],MeanNaa[1]+SDNaa[1]), col=Farver[6], border = NA)
  #polygon(x=XPol, y=c(Max0125,Min0125[12:1],Max0125[1]), col=Farver[2])
  #polygon(x=XPol, y=c(Max2650,Min2650[12:1],Max2650[1]), col=Farver[3])
  #polygon(x=XPol, y=c(Max5175,Min5175[12:1],Max5175[1]), col=Farver[4])
  #polygon(x=XPol, y=c(Max7600,Min7600[12:1],Max7600[1]), col=Farver[5])
  #polygon(x=XPol, y=c(MaxNaa,MinNaa[12:1],MaxNaa[1]), col=Farver[6])
  
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  
}

GStatus.poly4 <- function(Y=2020,M=9,YRef=2016){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  D2 <- GData[,1:2]
  for (n in 2:12){
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
  }
  plot(1000,1000,type="l",xlim=c(1,12),ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS", sub="Normal fra datasettet")
  NY <- length(D2[,1])
  
  MaxPre  <- c()
  Max0125 <- c()
  Max2650 <- c()
  Max5175 <- c()
  Max7600 <- c()
  MaxNaa  <- c()
  
  MinPre  <- c()
  Min0125 <- c()
  Min2650 <- c()
  Min5175 <- c()
  Min7600 <- c()
  MinNaa  <- c()
  
  MeanPre  <- c()
  Mean0125 <- c()
  Mean2650 <- c()
  Mean5175 <- c()
  Mean7600 <- c()
  MeanNaa  <- c()
  
  SDPre  <- c()
  SD0125 <- c()
  SD2650 <- c()
  SD5175 <- c()
  SD7600 <- c()
  SDNaa  <- c()
  
  for(n in 2:13){
    MaxPre <- c(MaxPre,max(D2[D2[,1] <= 1900,n],na.rm=T))
    MinPre <- c(MinPre,min(D2[D2[,1] <= 1900,n],na.rm=T))
    SDPre <- c(SDPre,sd(D2[D2[,1] <= 1900,n],na.rm=T))
    MeanPre <- c(MeanPre,mean(D2[D2[,1] <= 1900,n],na.rm=T))
    
    Max0125 <- c(Max0125,max(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    Min0125 <- c(Min0125,min(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    SD0125 <- c(SD0125,sd(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    Mean0125 <- c(Mean0125,mean(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    
    Max2650 <- c(Max2650,max(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    Min2650 <- c(Min2650,min(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    SD2650 <- c(SD2650,sd(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    Mean2650 <- c(Mean2650,mean(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    
    Max5175 <- c(Max5175,max(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    Min5175 <- c(Min5175,min(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    SD5175 <- c(SD5175,sd(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    Mean5175 <- c(Mean5175,mean(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    
    Max7600 <- c(Max7600,max(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    Min7600 <- c(Min7600,min(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    SD7600 <- c(SD7600,sd(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    Mean7600 <- c(Mean7600,mean(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    
    MaxNaa <- c(MaxNaa,max(D2[D2[,1] > 2000,n],na.rm=T))
    MinNaa <- c(MinNaa,min(D2[D2[,1] > 2000,n],na.rm=T))
    SDNaa <- c(SDNaa,sd(D2[D2[,1] > 2000,n],na.rm=T))
    MeanNaa <- c(MeanNaa,mean(D2[D2[,1] > 2000,n],na.rm=T))
  }
  
  Farver <- colorRampPalette(c("blue","red"))(6)
  
  XPol <- c(1:12,12:1,1)
  
  polygon(x=XPol, y=c(MaxPre,MinPre[12:1],MaxPre[1]), col=Farver[1])
  polygon(x=XPol, y=c(Max0125,Min0125[12:1],Max0125[1]), col=Farver[2])
  polygon(x=XPol, y=c(Max2650,Min2650[12:1],Max2650[1]), col=Farver[3])
  polygon(x=XPol, y=c(Max5175,Min5175[12:1],Max5175[1]), col=Farver[4])
  polygon(x=XPol, y=c(Max7600,Min7600[12:1],Max7600[1]), col=Farver[5])
  polygon(x=XPol, y=c(MaxNaa,MinNaa[12:1],MaxNaa[1]), col=Farver[6])
  
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  
}


GStatus.poly2 <- function(Y=2020,M=9,YRef=2016){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  D2 <- GData[,1:2]
  for (n in 2:12){
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
  }
  plot(1000,1000,type="l",xlim=c(1,12),ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS", sub="Normal fra datasettet")
  NY <- length(D2[,1])
  
  MaxPre  <- c()
  Max0125 <- c()
  Max2650 <- c()
  Max5175 <- c()
  Max7600 <- c()
  MaxNaa  <- c()
  
  MinPre  <- c()
  Min0125 <- c()
  Min2650 <- c()
  Min5175 <- c()
  Min7600 <- c()
  MinNaa  <- c()
  
  MeanPre  <- c()
  Mean0125 <- c()
  Mean2650 <- c()
  Mean5175 <- c()
  Mean7600 <- c()
  MeanNaa  <- c()
  
  SDPre  <- c()
  SD0125 <- c()
  SD2650 <- c()
  SD5175 <- c()
  SD7600 <- c()
  SDNaa  <- c()
    
  for(n in 2:13){
    MaxPre <- c(MaxPre,max(D2[D2[,1] <= 1900,n],na.rm=T))
    MinPre <- c(MinPre,min(D2[D2[,1] <= 1900,n],na.rm=T))
    SDPre <- c(SDPre,sd(D2[D2[,1] <= 1900,n],na.rm=T))
    MeanPre <- c(MeanPre,mean(D2[D2[,1] <= 1900,n],na.rm=T))
    
    Max0125 <- c(Max0125,max(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    Min0125 <- c(Min0125,min(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    SD0125 <- c(SD0125,sd(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    Mean0125 <- c(Mean0125,mean(D2[D2[,1]>1900 & D2[,1]<=1925,n],na.rm=T))
    
    Max2650 <- c(Max2650,max(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    Min2650 <- c(Min2650,min(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    SD2650 <- c(SD2650,sd(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    Mean2650 <- c(Mean2650,mean(D2[D2[,1]>1925 & D2[,1]<=1950,n],na.rm=T))
    
    Max5175 <- c(Max5175,max(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    Min5175 <- c(Min5175,min(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    SD5175 <- c(SD5175,sd(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    Mean5175 <- c(Mean5175,mean(D2[D2[,1]>1950 & D2[,1]<=1975,n],na.rm=T))
    
    Max7600 <- c(Max7600,max(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    Min7600 <- c(Min7600,min(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    SD7600 <- c(SD7600,sd(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    Mean7600 <- c(Mean7600,mean(D2[D2[,1]>1975 & D2[,1]<=2000,n],na.rm=T))
    
    MaxNaa <- c(MaxNaa,max(D2[D2[,1] > 2000,n],na.rm=T))
    MinNaa <- c(MinNaa,min(D2[D2[,1] > 2000,n],na.rm=T))
    SDNaa <- c(SDNaa,sd(D2[D2[,1] > 2000,n],na.rm=T))
    MeanNaa <- c(MeanNaa,mean(D2[D2[,1] > 2000,n],na.rm=T))
  }
  
  Farver <- colorRampPalette(c("blue","red"))(6)
  
  XPol <- c(1:12,12:1,1)
  
  polygon(x=XPol, y=c(MaxPre,MinPre,MaxPre[1]), col=Farver[1])
  polygon(x=XPol, y=c(Max0125,Min0125,Max0125[1]), col=Farver[2])
  polygon(x=XPol, y=c(Max2650,Min2650,Max2650[1]), col=Farver[3])
  polygon(x=XPol, y=c(Max5175,Min5175,Max5175[1]), col=Farver[4])
  polygon(x=XPol, y=c(Max7600,Min7600,Max7600[1]), col=Farver[5])
  polygon(x=XPol, y=c(MaxNaa,MinNaa,MaxNaa[1]), col=Farver[6])
  
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  
}



GStatus.poly3 <- function(Y=2020,M=9,YRef=2016){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  D2 <- GData[,1:2]
  for (n in 2:12){
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
  }
  plot(1000,1000,type="l",xlim=c(1,12),ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS", sub="Normal fra datasettet")
  NY <- length(D2[,1])
  
  D4 <- cbind(ceiling(D2[,1]/10),D2)
  D4[1,1] <- D4[2,1]
  Dekader <- unique(D4[,1])
  Farver <- rainbow(length(Dekader))
  DMax <- c()
  DMin <- c()
  Farve<-0
  for (dek in Dekader){
    Farve<-Farve+1
    Storst <- dek*10
    Minst <- dek*10
    for(n in 3:14){
      Storst<-c(Storst,max(D4[D4[,1]==dek,n],na.rm=T))
      Minst<-c(Minst,min(D4[D4[,1]==dek,n],na.rm=T))
    }
    DMax <- rbind(DMax,Storst)
    DMin <- rbind(DMin,Minst)
    print(round(Storst,2))
    XPol <- c(1:12,12:1,1)
    YPol <- c(Storst[2:13],Minst[13:2],Storst[2])
    polygon(x=XPol,y=YPol,col=Farver[Farve],border = NA,densit=c(50),angle=Farve*10)
  }
  
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  
}



GStatus <- function(Y=2018,M=3,YRef=2016){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  print(GData)
  D2 <- GData[,1:2]
  for (n in 2:12){
    #print(n)
    #print(dim(D2))
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
    #print(dim(D2))
  }
  #  plot(1:12,D2[D2[,1]==Y,2:13]/100,type="l",ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS")
  plot(1:12,D2[D2[,1]==Y,2:13],type="l",ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS", sub="Normal fra datasettet")
  NY <- length(D2[,1])
  for (n in 1:NY){
    #    lines(c(1:12),D2[n,2:13]/100,col="gray")
    lines(c(1:12),D2[n,2:13],col="gray")
  }
  
  #  lines(1:12,D2[D2[,1]==(Y-1),2:13]/100,col="blue",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  D3
}

GStatus.rainbow <- function(Y=2016,M=3,YRef=2016){
  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  #  GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,1:13]
  print(GData)
  D2 <- GData[,1:2]
  for (n in 2:12){
    #print(n)
    #print(dim(D2))
    D2 <- cbind(D2,
                rowMeans(GData[,2:(n+1)]))
    #print(dim(D2))
  }
  #  plot(1:12,D2[D2[,1]==Y,2:13]/100,type="l",ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS")
  plot(1:12,D2[D2[,1]==Y,2:13],type="l",ylim=c(-1,1.5),xlab="M?ned",ylab="Temperaturavvik",main="NASA - GISS", sub="Normal fra datasettet")
  NY <- length(D2[,1])
  Farver=rainbow(NY)
  for (n in 1:NY){
    #    lines(c(1:12),D2[n,2:13]/100,col="gray")
    lines(c(1:12),D2[n,2:13],col=Farver[n])
  }
  
  #  lines(1:12,D2[D2[,1]==(Y-1),2:13]/100,col="blue",lwd=2)
  #lines(1:12,D2[D2[,1]==(Y-1),2:13],col="blue",lwd=2)
  D3 <- D2
  NY <- length(D3[,1])
  for (n in 1:NY){
    D3[n,c(M:12)+1]<-D3[n,c(M:12)+1]-(D3[n,M+1]-D3[NY,M+1])
    #    lines(c(M:12),D3[n,c(M:12)+1]/100,col="orange")
    lines(c(M:12),D3[n,c(M:12)+1],col="orange")
  }
  if(is.na(YRef)){YRef<-Y-1}
  #  lines(1:12,D2[D2[,1]==Y,2:13]/100,col="red",lwd=2)
  for (n in 1:NY){
    #    lines(c(1:12),D2[n,2:13]/100,col="gray")
    lines(c(1:12),D2[n,2:13],col=Farver[n])
  }
  
  lines(1:12,D2[D2[,1]==Y,2:13],col="red",lwd=2)
  lines(1:12,D2[D2[,1]==YRef,2:13],col="blue",lwd=2)
  D3
}

