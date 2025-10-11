GData <- read.csv("http://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",na.strings = c("***","****"),colClasses = "numeric", skip=1)[,c(1,14)]
#GData <- rbind(GData,c(2025,NA))
LG <- length(GData[,1])
GData <- cbind(GData,c(NA,GData[1:(LG-1),2]))
GData <- cbind(GData,GData[,2]-GData[,3])
MG <- mean(GData[,4],na.rm=T)
SG <- sd(GData[,4],na.rm=T)
GData <- cbind(GData,c(NA,GData[1:(LG-1),2])+SG+MG)
GData <- cbind(GData,c(NA,GData[1:(LG-1),2])-SG+MG)
GData <- cbind(GData,c(NA,GData[1:(LG-1),2])+SG+SG+MG)
GData <- cbind(GData,c(NA,GData[1:(LG-1),2])-SG-SG+MG)
GData <- cbind(GData,c(NA,GData[1:(LG-1),2])+max(GData[,4],na.rm=T))
GData <- cbind(GData,c(NA,GData[1:(LG-1),2])+min(GData[,4],na.rm=T))

GData2 <- GData[,1:2]
GData2 <- cbind(GData2,
               c(NA,GData2[1:(LG-1),2]),
               c(NA,NA,GData2[1:(LG-2),2]),
               c(NA,NA,NA,GData2[1:(LG-3),2]),
               c(NA,NA,NA,NA,GData2[1:(LG-4),2]),
               c(NA,NA,NA,NA,NA,GData2[1:(LG-5),2]))
GData2 <- cbind(GData2,GData2[,2]-GData2[,3])
GData2 <- cbind(GData2,GData2[,2]-GData2[,4])
GData2 <- cbind(GData2,GData2[,2]-GData2[,5])
GData2 <- cbind(GData2,GData2[,2]-GData2[,6])
GData2 <- cbind(GData2,GData2[,2]-GData2[,7])

GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+max(GData2[,8],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+max(GData2[,9],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+max(GData2[,10],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+max(GData2[,11],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+max(GData2[,12],na.rm=T))

GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+min(GData2[,8],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+min(GData2[,9],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+min(GData2[,10],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+min(GData2[,11],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+min(GData2[,12],na.rm=T))

GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+2*sd(GData2[,8],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+2*sd(GData2[,9],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+2*sd(GData2[,10],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+2*sd(GData2[,11],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+2*sd(GData2[,12],na.rm=T))

GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-2*sd(GData2[,8],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-2*sd(GData2[,9],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-2*sd(GData2[,10],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-2*sd(GData2[,11],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-2*sd(GData2[,12],na.rm=T))

GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+sd(GData2[,8],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+sd(GData2[,9],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+sd(GData2[,10],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+sd(GData2[,11],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])+sd(GData2[,12],na.rm=T))

GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-sd(GData2[,8],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-sd(GData2[,9],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-sd(GData2[,10],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-sd(GData2[,11],na.rm=T))
GData2 <- cbind(GData2,c(NA,GData2[1:(LG-1),2])-sd(GData2[,12],na.rm=T))



plot(GData[,1],GData[,2],type="l",ylim=c(-0.7,2.0), xlim=c(1880,2030),ylab="Temp.avvik", xlab="")
plot(GData[,1],GData[,2],type="l",ylim=c(0.5,2.0), xlim=c(2015,2030),ylab="Temp.avvik", xlab="")

lines(c(-1000,10000),c(-0.5,-0.5),lty=3)
lines(c(-1000,10000),c(0,0),lty=3)
lines(c(-1000,10000),c(0.5,0.5),lty=3)
lines(c(-1000,10000),c(1,1),lty=3)
lines(c(-1000,10000),c(1.5,1.5),lty=3)
lines(c(-1000,10000),c(2,2),lty=3)



#polygon(c(GData[LG-1,1],GData[LG,1],GData[LG,1],GData[LG-1,1]),c(GData[LG-1,2],GData[LG,9],GData[LG,10],GData[LG-1,2]),col=rgb(0,0,1,0.5),border = NA)
polygon(c(GData2[LG-1,1]+c(0,1,2,3,4,5,5,4,3,2,1,0)),c(GData2[LG-1,2],GData2[LG,c(13:17,22:18)],GData2[LG-1,2]),col=rgb(0,0,1,0.3),border = NA)
polygon(c(GData2[LG-1,1]+c(0,1,2,3,4,5,5,4,3,2,1,0)),c(GData2[LG-1,2],GData2[LG,c(13:17,22:18)+10],GData2[LG-1,2]),col=rgb(0,0,1,0.3),border = NA)
polygon(c(GData2[LG-1,1]+c(0,1,2,3,4,5,5,4,3,2,1,0)),c(GData2[LG-1,2],GData2[LG,c(13:17,22:18)+20],GData2[LG-1,2]),col=rgb(0,0,1,0.3),border = NA)
lines(c(GData2[LG-1,1],GData2[LG-1,1]+5),GData2[LG-1,c(2,2)],lty=2)

#lines(GData[,1],GData[,5],lty=3)
#lines(GData[,1],GData[,6],lty=3)
#lines(GData[,1],GData[,7],lty=3)
#lines(GData[,1],GData[,8],lty=3)
#lines(GData[,1],GData[,9],lty=3,col="red")
#lines(GData[,1],GData[,10],lty=3,col="blue")
#polygon(c(GData[,1],GData[LG:1,1]),c(GData[1,2],GData[2:LG,9],GData[LG:2,10],GData[1,2]),col=rgb(0,0,1,0.5))
#polygon(c(GData[,1],GData[LG:1,1]),c(GData[1,2],GData[2:LG,9],GData[LG:2,10],GData[1,2]),col=rgb(1,0,0,0.5),border = NA)
#polygon(c(GData[,1],GData[LG:1,1]),c(GData[1,2],GData[2:LG,5],GData[LG:2,6],GData[1,2]),col=rgb(0,0,1,0.5),border = NA)

lines(GData[,1],GData[,2],lwd=2)

