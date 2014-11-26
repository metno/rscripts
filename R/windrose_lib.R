# windrose_lib.R
#
# History
# 26.11.2014 Cristian Lussana. cristianl@met.no
#            original code from:
# David Finlayson (with help from Joerg Maeder and Ben Bolker)
# david_p_finlayson@hotmail.com
# Version 1.0
# November 23, 2001
# then modified by Maria Ranci and Cristian Lussana
#==============================================================================

#! Helper function to draw the pie slices, i.e., individual plume segments 
# corresponding to each wind magnitude at a given direction.
pie <- function(idx, x0, y0, r, direction, angle, magnitude, col,
                shrink.top,shrink.bottom,
                border,lwd.border,
                mag.labels,cex.lab,
                label=FALSE, ...)
# --- Parameters: -----
# idx is the index used to select colors and stuff x0,y0 are the coordinates
# of the origin where plume emanates from r is the starting radial length for
# the segment being drawn direction is the angle where the plume segment is
# centered angle is the spread, i.e., sector angle, for the plume segment
# magnitude is the radial length corresponding to the current air speed, 
# i.e., current segmen col is the color for the current magnitude segment
# label determines whether or not segment labels are drawn
#------------------------------------------------------------------------------
{
# 0 degrees equals North
# adjust coordinate from compass to polar
direction<-360-direction+90
# calculate theta start and stop
start.top<-(direction+(0.5)*angle*shrink.top)*pi/180
stop.top<-(direction-(0.5)*angle*shrink.top)*pi/180
start.bottom <- (direction + 0.5 * angle * shrink.bottom) * pi / 180
stop.bottom <- (direction - 0.5 * angle * shrink.bottom) * pi / 180
# Get four points of polygon (slice)
x1 <- r*cos(start.bottom) + x0
y1 <- r*sin(start.bottom) + y0
x2 <- r*cos(stop.bottom) + x0
y2 <- r*sin(stop.bottom) + y0
x3 <- (magnitude + r)*cos(stop.top) + x0
y3 <- (magnitude + r)*sin(stop.top) + y0
x4 <- (magnitude + r)*cos(start.top) + x0
y4 <- (magnitude + r)*sin(start.top) + y0
x <- c(x1, x2, x3, x4)
y <- c(y1, y2, y3, y4)
#
polygon(x, y, col=col, lwd=lwd.border, border=border, ...)

if (label)
  text(x2, y2 + 0.5*(y3-y2), mag.labels[idx], pos=2, offset=0.6,
       cex=cex.lab)

}

#-------------------------------------------------------------------------------
#! Directional Vector Histogram (Rose Diagrams).
rose2 <- function(time.description=NULL,
                  dir,
                  mag=NULL,
                  nplumes=16,
                  shrink=0.6,
                  shrink.top=0.55,
                  shrink.bottom=0.7, 
                  fg="black",
                  bg="linen",
                  border="black",
                  lwd.border=1.9,
                  mag.bins=c(0.5,1,2,4,8,16,32,100),
                  main1=NULL,
                  main2=NULL,
                  operative=NULL,
                  mag.col=c("springgreen","cyan","blue","magenta","yellow","tomato","tan"),
                  fg.lows="black",
                  bg.lows="azure",
                  rscale=NULL,
                  rings=TRUE,
                  ring.labels=TRUE,
                  lwd.ring=1,
                  lty.ring=2,
                  col.ring="black",
                  lwd.axis=2, 
                  cex.lab=1.5,
                  cex.dir=3,
                  n.tot=NULL,
                  legend=T)
#------------------------------------------------------------------------------
# original code from:
# David Finlayson (with help from Joerg Maeder and Ben Bolker)
# david_p_finlayson@hotmail.com
# Version 1.0
# November 23, 2001
#
# Use for plotting directional data such as wind direction or the
# angles of imbricated pebbles in rivers and streams. This is basically
# an extension of the hist(x) function though I did not implement all of
# hist. I have placed limits on the range of bins so that they always
# fall within 0 and 360 (i.e. directions of the compass). The standard
# color and line adjustment commands work as well but you will need to add
# annotation (i.e.. main, xlab, ylab) separately (see par).
#
# bins: Approximate number of bins see hist() function for details
# rscale: Ring Scale, the approximate number of rings for scaling see pretty()
# NULL value will call pretty() with default number of rings
# labels: (T/F) draw labels for the top 10% largest petals and the
#          cardinal dirctions?
# rings: (T/F) draw scale rings?
#
# example:
# dir <- runif(30) * 360
# mag <- runif(30) * 50
# rose2(dir,mag)
#
#  IMPORTANT NOTE:  The wind rose shows wind direction `dir' in terms of the
#   direction that wind is COMING FROM.  In other words, a wind direction
#   `dir' of NORTH (0 degrees) means that the wind is coming out of the north.
#
#  shrink.top=0.7, shrink.bottom=1 or 0.9 is OK too -- a little thicker
#          telescope effect
#
#  UPDATE:  Now deals with NA values properly for dir.  Also we change
#           the midpoints of direction to be centered on the four
#           primary directions: N,S,E,W  --NK 27-10-04
#
#  UPDATE:  Default number of plumes is now 16, so that if wind direction is
#            in any of primary directions N-S-E-W or 45 degree directions
#            NW, NE, SE, SW, then the plumes point exactly 
#            in these directions.   NK 27-10-04
#                         
#
# rose -- from R-help archives
#
# rose2:  updated version of David Finlayson's `rose' by Neil Klepeis, 
#        adding space between plumes and segmented
#        plumes representing different magnitudes of wind speed.
#
# TODO:   Add percent labels to rings
#         Add plume segments for wind speed
#         Add space between plumes as a fraction of total sector angle
#          --> Do overall shrink and then extra shrink for top and bottom
#             of magnitude segments, to give telescoping effect.
#         Center segment is for calm winds (zero)
#          --> OK,  add a center circle corresponding to low winds, i.e., under
#             1 m/s, thereby assigning it equally to all directions
#              (really turbulence eddies), report the percentage of low winds.
#             Low winds cannot really be assigned reliably to any direction, 
#             user determines magnitude bins and what constitutes low winds,i.e.
#             the lowest bin.
#
# Degrees for direction are interpreted as follows:
#  [Note:  These are directions that wind is COMING FROM]
#
# North = 0 degrees
# East = 90
# South = 180
# West = 270
#
# We convert to polar coordinates prior to plotting.....
# ----------------------------------------------------------------------
{
  # Plot only if valid data are available
  if (max(dir,na.rm=T)!=-Inf & max(mag,na.rm=T)!=-Inf){
    # Check the wind speed (mag) data, if given, assign to one value, otherwise
    if (!is.null(mag)) {
      plotmag<-T
      if (length(mag.bins)!=length(mag.col)+1)
        stop("`mag.bins' must be the same length as `mag.col' plus 1.")
      if (min(mag,na.rm=T)<0){
        print("WARNING: If specified, `mag' must contain non-negative magnitude data.")
        print(paste(min(mag,na.rm=T)))
      }
    } else {
      plotmag <- FALSE
      mag <- rep(0, length=length(dir))
      mag.col <- rep(mag.col[length(mag.col)], length=length(mag.col))
      mag.bins <- c(0,1)
    }  
# count number of speed measurement "fuori scala"
    fuori_scala_max<-NULL
    fuori_scala_min<-NULL
    sp<-mag>max(scala)
    st<-mag<min(scala)
    if(any(ll<-which(sp))) fuori_scala_max<-mag[ll]
    if(any(ll<-which(st))) fuori_scala_min<-mag[ll]
# Remove Missing Data
    good <- complete.cases(mag, dir)
    mag <- mag[good]
    dir <- dir[good]
    if (is.null(n.tot)) {
      good.perc <- round ( NROW(dir) * 100 / NROW(good) , digits = 2 )
    } else {
      good.perc <- round ( NROW(dir) * 100 / n.tot , digits = 2 )
    }
# Ensure that `direction' is angular data (0-360)
    if (max(dir) > 360 || min(dir) < 0){
      print(paste("WARNING, max direction ",max(dir),
                  " and min direction ",min(dir),"\n"))
    }
# Create factors of wind speed and direction
    width<-360/nplumes
    dir[dir>(360-width/2)]<-dir[dir>(360-width/2)]-360
    mids<-seq(0,360-width,by=width)
    breaks<-seq(-width/2,360-width/2, length=nplumes+1)
    width<-shrink*width
    n<-length(mag)
    lows<-mag<mag.bins[1]
    nlows<-sum(lows)
    plows<-100*nlows/n
    mag<-mag[!lows]
    dir<-dir[!lows]
    dir<-cut(dir, breaks=breaks) 
    mag.labels<-paste(mag.bins[-length(mag.bins)],"-",mag.bins[-1],sep="")
    mag<-cut(mag, breaks=mag.bins, labels=mag.labels, include.lowest=TRUE) 
    nmags <- length(levels(mag))
# Get list of percentages for each wind direction and speed
    tab <- as.data.frame(table(mag=mag, dir=dir))
    tab$Perc <- 100 * tab$Freq / n
    totals <- aggregate(tab$Perc, by=list(tab$dir), FUN=sum)$x
    perc <- split(tab$Perc, tab$dir)
    perc.lows <- plows / nplumes
    cumul <- lapply(perc, FUN=function(x) c(0,cumsum(x)[-length(x)]))
#    cat("Direction Midpoints (",nplumes,") :",mids,"\n")
#    cat("Direction Breaks (",nplumes,") :", breaks,"\n")
#    cat("Magnitude Bins (",nmags,") :",mag.labels,"\n")
#    cat(plows,"% of Magnitudes Are Below",mag.bins[1],"\n")
#    cat("Low Magnitudes Distributed Among",nplumes,"Directions:",
#         perc.lows,"% each direction \n")
# Initialize Plot Area
    oldpar <- par()
#   xpd If ‘FALSE’, all plotting is clipped to the plot region
#   las=1, style of axis labels always horizontal
    par(pty="s", las=1, xpd=FALSE, bg=bg, fg=fg)
#
    if (!is.null(rscale)){
      rscale <- pretty(rscale)
    } else {
      rscale <- pretty(c(totals+perc.lows, tab$Perc))
    }
#
    limits <- c(-max(rscale), max(rscale)) * 1.05
#
    plot(0,0, ylim=limits, xlim=limits, axes=FALSE,
         xlab="", ylab="", type="n")
#
    abline(h=0, lwd=lwd.axis)
    abline(v=0, lwd=lwd.axis)
# Title
    par(xpd=FALSE)
    if (!is.null(main1)) mtext(main1,cex=1.7,font=4,side=3,adj=0.5,line=2.5) 
    if (!is.null(main2)) mtext(main2,cex=2,font=3,side=3,adj=-4.5,line=0.4) 
    par<-oldpar
# Plot Rings
    if (rings == TRUE) {
      symbols(rep(0,length(rscale)-1), rep(0,length(rscale)-1),
              circles=rscale[-1], inches=FALSE, add=TRUE, lwd=lwd.ring,
              lty=lty.ring, fg=col.ring)
      text(rscale[-1]*cos(pi/6), rscale[-1]*sin(pi/6),
           labels=paste(rscale[-1],"%",sep=""), pos=4, offset=0.6,
  	       cex=cex.lab-0.2) 	   
    }	   
# Plot ring of low magnitudes
    if (plotmag)
      symbols(0,0, circles=perc.lows, inches=FALSE, add=TRUE,
              bg=bg.lows, fg=fg.lows)
# Plot the slices, with a base percentage from low magnitudes spread equally in each direction
    for (i in 1:nplumes)
      for (j in 1:nmags) 
        if (perc[[i]][j] > 0)
          pie(j, 0, 0, perc.lows + cumul[[i]][j], mids[i],
              width, perc[[i]][j], mag.col[j],
              shrink.top,shrink.bottom,
              border,lwd.border,mag.labels,cex.lab)
# write season name 
    mtext(time.description,side=3,line=-1.4,cex=1.8,adj=-0.4)
# Plot Direction Labels
    mtext("S",side=1,line=0.5,cex=cex.dir,adj=0.5,font = 3 )
    mtext("W",side=2,line=0.3,cex=cex.dir,adj=1  ,font = 3 )
    mtext("N",side=3,line=0.1,cex=cex.dir,adj=0.5,font = 3 )
    mtext("E",side=4,line=0.3,cex=cex.dir,adj=0  ,font = 3 )
# Plot Legends for Magnitudes
    par(xpd=NA)
    coord <- par()$usr
    if (plotmag & legend) {
# piramidina solo se legend="si"
      leg.perc <- (1/nmags)*abs(coord[3]-coord[4])/2
      leg.cumul <- c(0, cumsum(rep(leg.perc, nmags)))
      for (j in 1:nmags) 
        pie(j,coord[1],-0.15*abs(coord[3]-coord[4])/2,
            leg.cumul[j], 180, 15, leg.perc, mag.col[j], 
            shrink.top, shrink.bottom,border,lwd.border,
            mag.labels,cex.lab,label=TRUE)
    } # end if(plotmag)	   
    text(coord[2]-0.33*abs(coord[1]-coord[2])/2,coord[3], 
         paste(round(plows),"% below ", mag.bins[1] , " m/s", sep=""), 
         pos=3,cex=cex.lab+0.1)
    text(coord[2]-0.33*abs(coord[1]-coord[2])/2,coord[3], 
         paste(length(fuori_scala_max)," above ", max(scala) , " m/s", sep=""),
         pos=1, cex=cex.lab+0.1)
    mtext(paste(good.perc," % valid data", sep = "" ),side=3,
          at=coord[2],line=0.3,cex=cex.lab+0.1)
    if (!is.null(operative)) mtext(operative,cex=1.5,font=3,side=3,at=coord[2],line=-1) 
 ### Reset the par
    par <- oldpar
 ### Return invisible list of percents for each speed/direction
    invisible(tab)
    print("Success!")
  }else{ # In case of no-data
    print(paste(time.description ,": no valid data available\n"))
# Initialize Plot Area
    oldpar <- par()
    par(pty="s", las=1, xpd=FALSE, bg=bg, fg=fg)
# if (!is.null(rscale)){
#  rscale <- pretty(rscale)
# } else {
# rscale <- pretty(c(totals+perc.lows, tab$Perc))
# }
# limits <- c(-max(rscale), max(rscale)) * 1.04
    limits <- c(-1, 1)
    plot(0,0, ylim=limits, xlim=limits, axes=FALSE,
         xlab="", ylab="", type="n")
    abline(h=0, lwd=lwd.axis)
    abline(v=0, lwd=lwd.axis)
# Title
    if (!is.null(main1)) mtext(main1,cex=2,font=4,side=3,adj=0,line=2.8) 
    if (!is.null(main2)) mtext(main2,cex=1,font=4,side=3,at=c(1,1)) 
# Reset the par
    par <- oldpar
  }
}
