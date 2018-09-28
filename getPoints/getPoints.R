#!/usr/bin/env Rscript
#..............................................................................
#Copyright and license
# Copyright (C) 2018 MET Norway. The software is licensed under GPL version 3 
# or (at your option) any later version.
# https://www.gnu.org/licenses/gpl-3.0.en.html
# 
# History:
# 12.05.2018 - Cristian Lussana. Original code.
# 28.09.2018 - Cristian Lussana. Modified for NGCD.
# -----------------------------------------------------------------------------
rm(list=ls())
#
# -----------------------------------------------------------------------------
# Libraries
suppressPackageStartupMessages(library("argparser"))
suppressPackageStartupMessages(library("sp"))
suppressPackageStartupMessages(library("raster"))
suppressPackageStartupMessages(library("rgdal"))
suppressPackageStartupMessages(library("ncdf4"))
options(scipen = 999)

#
# -----------------------------------------------------------------------------
# Functions

`nc4.getTime` <-
function(file.name,varid="time") {
    # open netCDF file
    defs<-nc_open(file.name)
    tcors <- ncvar_get(defs, varid=varid)
    tunits <- ncatt_get(defs, varid,"units")$value
    tt <- nc4t2str(nct=tcors,nct.unit=tunits,format="%Y%m%d%H%M")
    # close file
    nc_close(defs)
    # return the result
    tt    
}

`nc4.getDim` <-
function(file.name,varid="ensemble_member") {
    # open netCDF file
    defs<-nc_open(file.name)
    vals<-ncvar_get(defs, varid=varid)
    # close file
    nc_close(defs)
    # return the result
    vals    
}


`str2Rdate` <-
function(ts,format="%Y-%m-%d %H:%M:%S") {
# Author: Christoph Frei
# ===========================================================================
# converts a string into an R (POSIXt,POSIXct) date object
# date objects can be used with arithmetic operations +/-
# ts is a character or a vector of characters with date information
# in the format specified in format
# Output is a date object

     # the lengthy bunch of testing is necessary because strptime needs
     # explicit specifications for month and day, otherwise it returns NA.
     # this extension allows inputs of the format "%Y-%m" in which case the
     # the first day of the month is taken as a reference.

     #Êcheck if year/month/day is specified
     ysp <- length(c(grep("%Y",format,fixed=TRUE),
                     grep("%y",format,fixed=TRUE)))
     msp <- length(c(grep("%m",format,fixed=TRUE),
                     grep("%b",format,fixed=TRUE),
                     grep("%B",format,fixed=TRUE)))
     jsp <- length(c(grep("%j",format,fixed=TRUE)))
     dsp <- length(c(grep("%d",format,fixed=TRUE)))
     if (ysp > 1) { stop("ERROR: Multiple specification of year in 
                         date format.") }
     if (ysp == 0) { stop("ERROR: No year specification in 
                         date format.") }
     if (msp > 1) { stop("ERROR: Multiple specification of month in 
                         date format.") }
     if (dsp > 1) { stop("ERROR: Multiple specification of day in 
                         date format.") }

     # append month or day if not specified
     tss <- ts
     formati <- format
     if (jsp == 0) {
     if (msp == 0) { 
        tss <- paste(tss,"01",sep="")
        formati <- paste(formati,"%m",sep="")
     }
     if (dsp == 0) { 
        tss <- paste(tss,"01",sep="") 
        formati <- paste(formati,"%d",sep="")
     }
     }

     # this is necessary because strptime() returns NA otherwise
     as.POSIXct(strptime(tss,format=formati),tz="GMT")
}

`Rdate2str` <-
function(date,format="%Y-%m-%d %H:%M:%S") {
#Author: Christoph Frei
# ===========================================================================
# converts a R (POSIXt,POSIXct) date object into a string
# date is one or a vector of R date-time objects
# format specifies the desired character format analogous to str2Rdate
# Output is one or a vector of characters
     format(date,format=format,tz="GMT")
}


`nc4t2str` <- 
function (nct, nct.unit, format = "%Y%m%d%H%M") {
#Author: Christoph Frei
    fmt.nc <- "%Y-%m-%d %H:%M:00"
    rd.expl <- str2Rdate("190001010000", format = "%Y%m%d%H%M")
    ss.expl <- Rdate2str(rd.expl, format = fmt.nc)
    ll <- nchar(ss.expl)
    reference <- nct.unit
    t.unit <- switch(substr(reference, 1, 4), 
                     minu = "T",
                     hour = "H", 
                     days = "D",
                     mont = "M",
                     year = "Y",
                     seco = "S")
    c.start <- switch(t.unit, T = 15, H = 13, D = 12, M = 14, Y = 13, S=15)
    ref.date.str <- substr(reference, c.start, stop = c.start + ll - 1)
    if (nchar(ref.date.str)<nchar(ss.expl)) {
      ll1 <- nchar(ref.date.str)
      aux<- substr(ss.expl, (ll1+1), stop = ll)
      ref.date.str<-paste(ref.date.str,":00",sep="")
    }
#           ref.date <- str2Rdate(ref.date.str, format = fmt.nc)
    ref.date <- str2Rdate(ref.date.str)
    if (!(t.unit %in% c("Y", "M", "T", "H", "D", "S"))) {
        stop("Time conversion not available")
    }
    if (t.unit %in% c("T", "H", "D", "S")) {
        t.scal <- switch(t.unit, T = 60, H = 3600, D = 86400, S=1)
        secs.elapsed <- nct * t.scal
        r.tims <- ref.date + secs.elapsed
    }
    if (t.unit == "Y") {
        yy.ref <- as.numeric(Rdate2str(ref.date, format = "%Y"))
        rr.ref <- Rdate2str(ref.date, format = "%m%d%H%M")
        yy.ch <- sprintf("%03d", nct + yy.ref)
        dd.ch <- sapply(yy.ch, FUN = function(x) paste(x, rr.ref, 
            sep = ""))
        r.tims <- str2Rdate(dd.ch, format = "%Y%m%d%H%M")
    }
    if (t.unit == "M") {
        yy.ref <- as.numeric(Rdate2str(ref.date, format = "%Y"))
        mm.ref <- as.numeric(Rdate2str(ref.date, format = "%m"))
        rr.ref <- Rdate2str(ref.date, format = "%d%H%M")
        mm <- nct + (yy.ref * 12 + mm.ref)
        hhy <- trunc((mm - 1)/12)
        hhm <- mm - hhy * 12
        dd.ch <- sapply(1:length(hhy), 
                 FUN = function(ii) paste(sprintf("%04d", 
                     hhy[ii]), sprintf("%02d", hhm[ii]), rr.ref, sep = ""))
        r.tims <- str2Rdate(dd.ch, format = "%Y%m%d%H%M")
    }
    Rdate2str(r.tims, format = format)
}


# read netCDF file format
nc4in<-function(nc.file,
                nc.varname,
                topdown=F,
                out.dim=NULL,
                proj4=NULL,
                nc.proj4=NULL,
                selection=NULL,
                verbose=F) {
#-------------------------------------------------------------------------------
# Read data from ncdf file.
# Parameters:
# + nc.file. character. netCDF file name
# + nc.varname. character. variable name in the netCDF file
# + out.dim. list. variable dimension parameters 
#   out.dim$
#    ndim. integer. number of dimensions
#    tpos. integer. position of the dimension identifying "time"
#    epos. integer. position of the dimension identifying "ensemble member"
#    zpos. integer. position of the dimension identifying "vertical level"
#    names. character vector (1...ndim). variable-specific dimension names
#           names[1]=x must be the Easting coordinate
#           names[2]=y must be the Northing coordinate
#           ...
# + selection. list. select gridded field to read
#   selection$
#    t. character. timestamp to read from the netCDF file (YYYYMMDDHH00)
#    z. numeric. hight level to read from the netCDF file
#    e. numeric. ensemble member to read from the netCDF file
# + proj4 string can be obtained either: 
#     proj4. character. proj4 string set by the user (override nc.proj4)
#   OR
#     nc.proj4. list. read proj4 string from file
#     nc.proj4$
#      var. variable which contains proj4 attribute
#      att. attribute of var which contains proj4 string
#
# example:
# file<-"/lustre/storeB/project/metproduction/products/meps/meps_extracted_2_5km_20161230T06Z.nc"
# nc.varname<-"precipitation_amount_acc"
# ndim<-4
# tpos<-4
# epos<-3
# names<-vector(length=4,mode="character")
# names<-c("x","y","ensemble_member","time") # same names as the nc dimensions
#   # morevoer, these must be all the dimensions of the nc.varname in the nc file
#   # (apart from dimensions that have just 1-field, i.e. they are not really 
#   #  useful dimensions)
#===============================================================================
  # open/read/close netcdf file
  if (!file.exists(nc.file)) return(NULL)
  nc <- nc_open(nc.file)
  nc.varnames<-attr(nc$var,"names")
  nc.dimnames<-attr(nc$dim,"names")
  if (is.null(out.dim)) {
    ndim<-2
    names<-vector(length=ndim,mode="character")
    names<-c(nc.dimnames[1],nc.dimnames[2])
    out.dim<-list(ndim=ndim,names=names)
    rm(ndim,names)
  }
  out.dimids4nc<-match(out.dim$names,nc.dimnames)
  if (any(is.na(out.dimids4nc))) return(NULL)
  if (!(nc.varname%in%nc.varnames)) return(NULL)
  # proj4 string is needed either (1) set by the user or (2) read from nc
  if (is.null(proj4)) {
    aux<-ncatt_get(nc,nc.proj4$var,nc.proj4$att)
    if (!aux$hasatt) return(NULL)
    proj4<-aux$value
  }
  idx<-which(nc.varnames==nc.varname)
  var<-nc$var[[idx]]
  var.size  <- var$varsize
  var.ndims <- var$ndims
  var.dimids<- var$dimids + 1
  out.dimids4var<-match(out.dimids4nc,var.dimids)
  if (any(is.na(out.dimids4var))) return(NULL)
  # define raster
  text.xdim<-paste("nc$dim$",out.dim$names[1],"$vals",sep="")
  text.ydim<-paste("nc$dim$",out.dim$names[2],"$vals",sep="")
  dx<-abs(eval(parse(text=paste(text.xdim,"[1]",sep="")))-
          eval(parse(text=paste(text.xdim,"[2]",sep=""))))
  ex.xmin<-min(eval(parse(text=text.xdim)))-dx/2
  ex.xmax<-max(eval(parse(text=text.xdim)))+dx/2
  dy<-abs(eval(parse(text=paste(text.ydim,"[1]",sep="")))-
          eval(parse(text=paste(text.ydim,"[2]",sep=""))))
  ex.ymin<-min(eval(parse(text=text.ydim)))-dy/2
  ex.ymax<-max(eval(parse(text=text.ydim)))+dy/2
  nx<-eval(parse(text=paste("nc$dim$",out.dim$names[1],"$len",sep="")))
  ny<-eval(parse(text=paste("nc$dim$",out.dim$names[2],"$len",sep="")))
  if (nx>1 & ny>1) {
    is.raster<-T
    r <-raster(ncol=nx, nrow=ny,
               xmn=ex.xmin, xmx=ex.xmax,
               ymn=ex.ymin, ymx=ex.ymax,
               crs=proj4)
    r[]<-NA
  } else {
    is.raster<-F
    s<-NULL
  }
  # time coordinate
  if (!is.null(out.dim$tpos)) {
    tcors  <- ncvar_get(nc, varid=out.dim$names[out.dim$tpos])
    tunits <- ncatt_get(nc, out.dim$names[out.dim$tpos],"units")$value
    tall <- nc4t2str(nct=tcors,nct.unit=tunits,format="%Y%m%d%H%M")
    ntall<-length(tall)
    tsel<-1:ntall
    if (!is.null(selection)) {
      if (!is.null(selection$t)) {
        if (any(selection$t %in% tall)) {
          tsel<-which(tall %in% selection$t)
        }
      }
    }
    ntsel<-length(tsel)
  } else {
    ntsel<-1
  }
  # ensemble member
  esel<-NULL
  nesel<-0
  if (!is.null(out.dim$epos)) {
    ecors<-ncvar_get(nc, varid=out.dim$names[out.dim$epos])
    eall<-ecors
    neall<-length(ecors)
    esel<-1:neall
    if (!is.null(selection)) {
      if (!is.null(selection$e)) {
        if (any(selection$e %in% eall)) {
          esel<-which(eall %in% selection$e)
          nesel<-length(esel)
        }
      }
    }
  }
  # elevation
  zsel<-NULL
  nzsel<-0
  if (!is.null(out.dim$zpos)) {
    zcors<-ncvar_get(nc, varid=out.dim$names[out.dim$zpos])
    zall<-zcors
    nzall<-length(zcors)
    zsel<-1:nzall
    if (!is.null(selection)) {
      if (!is.null(selection$z)) {
        if (any(selection$z %in% zall)) {
          zsel<-which(zall %in% selection$z)
          nzsel<-length(zsel)
        }
      }
    }
  }
  # labels
  jtot<-ntsel
  if (nzsel>0) jtot<-jtot*nzsel
  if (nesel>0) jtot<-jtot*nesel
  j<-0
  tlab<-vector(mode="character",length=jtot)
  if (nesel>0) {
    elab<-vector(mode="character",length=jtot)
  } else {
    elab<-NULL
  }
  zlab<-vector(mode="character",length=jtot)
  if (nesel>0) {
    zlab<-vector(mode="character",length=jtot)
  } else {
    zlab<-NULL
  }
  labels<-list(t=tlab,e=elab,z=zlab)
  data.vec<-array(data=NA,dim=c(nx*ny,jtot))
  if (out.dim$ndim==2) {
    j<-1
    start <- rep(1,var.ndims) # begin with start=(1,1,1,...,1)
    count <- var.size # begin w/count=(nx,ny,nz,...,nt), reads entire var
    data <- ncvar_get( nc, var, start=start, count=count )
    if (length(dim(data))!=2) return(NULL)
    if (topdown) for (i in 1:nx) data[i,1:ny]<-data[i,ny:1]
    if (is.raster) {
      r[]<-t(data)
      data.vec[,j]<-getValues(r)
      if (exists("s")) s<-stack(s,r)
      if (j==1) {
        s<-r
      } else {
        s<-stack(s,r)
      }
    }
    labels$t[j]<-NA
  } else {
    for (t in tsel) {
      # == cycle for data with 5 dimension (x,y,z,e,t)
      if ((nzsel>0) & (nesel>0)) {
        for (e in esel) {
          for (z in zsel) {
            j<-j+1
            start <- rep(1,var.ndims) # begin with start=(1,1,1,...,1)
            start[out.dimids4var[out.dim$tpos]] <- t # change to start=(1,1,1,...,t) 
                                                     # to read timestep t
            start[out.dimids4var[out.dim$epos]] <- e # change to start=(1,1,1,...,t) 
                                                     # to read timestep t, ens e
            start[out.dimids4var[out.dim$zpos]] <- z # change to start=(1,1,1,...,t) 
                                                     # to read timestep t, ens e, lev z
            count <- var.size # begin w/count=(nx,ny,nz,...,nt), reads entire var
            count[out.dimids4var[out.dim$tpos]] <- 1 # change to count=(nx,ny,nz,...,1)
                                                     # to read 1 tstep
            count[out.dimids4var[out.dim$epos]] <- 1 # change to count=(nx,ny,nz,...,1)
                                                     # to read 1 tstep, 1ens
            count[out.dimids4var[out.dim$zpos]] <- 1 # change to count=(nx,ny,nz,...,1)
                                                     # to read 1 tstep, 1ens, 1z
            data <- ncvar_get( nc, var, start=start, count=count )
            if (length(dim(data))!=2) return(NULL)
            if (topdown) for (i in 1:nx) data[i,1:ny]<-data[i,ny:1]
            if (is.raster) {
              r[]<-t(data)
              data.vec[,j]<-getValues(r)
              if (j==1) {
                s<-r
              } else {
                s<-stack(s,r)
              }
            }
            labels$t[j]<-tall[t]
            labels$e[j]<-eall[e]
            labels$z[j]<-zall[z]
            if (verbose) print(paste("Data for variable",nc.varname,
                                     "at timestep",labels$t[j],
                                     "for ensemble_member",labels$e[j],
                                     "at elevation",labels$z[j]))
          }   # end for z
        } # end for e
      # == cycle for data with 4 dimension (x,y,z,t)
      } else if (nzsel>0) {
        for (z in zsel) {
          j<-j+1
          start <- rep(1,var.ndims) # begin with start=(1,1,1,...,1)
          start[out.dimids4var[out.dim$tpos]] <- t # change to start=(1,1,1,...,t) 
                                                   # to read timestep t
          start[out.dimids4var[out.dim$zpos]] <- z # change to start=(1,1,1,...,t) 
                                                   # to read timestep t, ens e, lev z
          count <- var.size # begin w/count=(nx,ny,nz,...,nt), reads entire var
          count[out.dimids4var[out.dim$tpos]] <- 1 # change to count=(nx,ny,nz,...,1)
                                                   # to read 1 tstep
          count[out.dimids4var[out.dim$zpos]] <- 1 # change to count=(nx,ny,nz,...,1) 
                                                   # to read 1 tstep, 1ens, 1z
          data <- ncvar_get( nc, var, start=start, count=count )
          if (length(dim(data))!=2) return(NULL)
          if (topdown) for (i in 1:nx) data[i,1:ny]<-data[i,ny:1]
          if (is.raster) {
            r[]<-t(data)
            data.vec[,j]<-getValues(r)
            if (j==1) {
              s<-r
            } else {
              s<-stack(s,r)
            }
          }
          labels$t[j]<-tall[t]
          labels$z[j]<-zall[z]
          if (verbose) print(paste("Data for variable",nc.varname,
                                   "at timestep",labels$t[j],
                                   "at elevation",labels$z[j]))
        } # end for z
      # == cycle for data with 4 dimension (x,y,e,t)
      } else if (nesel>0) {
        for (e in esel) {
          j<-j+1
          start <- rep(1,var.ndims) # begin with start=(1,1,1,...,1)
          start[out.dimids4var[out.dim$tpos]] <- t # change to start=(1,1,1,...,t) 
                                                   # to read timestep t
          start[out.dimids4var[out.dim$epos]] <- e # change to start=(1,1,1,...,t) 
                                                 # to read timestep t, ens e, lev z
          count <- var.size # begin w/count=(nx,ny,nz,...,nt), reads entire var
          count[out.dimids4var[out.dim$tpos]] <- 1 # change to count=(nx,ny,nz,...,1)
                                                   # to read 1 tstep
          count[out.dimids4var[out.dim$epos]] <- 1 # change to count=(nx,ny,nz,...,1) 
                                                   # to read 1 tstep, 1ens, 1z
          data <- ncvar_get( nc, var, start=start, count=count )
          if (is.raster) {
            if (length(dim(data))!=2) return(NULL)
            if (topdown) for (i in 1:nx) data[i,1:ny]<-data[i,ny:1]
            r[]<-t(data)
            data.vec[,j]<-getValues(r)
            if (j==1) {
              s<-r
            } else {
              s<-stack(s,r)
            }
          } else {
            if (topdown) data[1:length(data)]<-data[length(data):1]
            data.vec[,j]<-data
          }
          labels$t[j]<-tall[t]
          labels$e[j]<-eall[e]
          if (verbose) print(paste("Data for variable",nc.varname,
                                   "at timestep",labels$t[j],
                                   "for ensemble_member",labels$e[j]))
        } # end for e
      # == cycle for data with 3 dimension (x,y,t)
      } else if (out.dim$ndim>2) {
        j<-j+1
        start <- rep(1,var.ndims) # begin with start=(1,1,1,...,1)
        start[out.dimids4var[out.dim$tpos]] <- t # change to start=(1,1,1,...,t) 
                                                 # to read timestep t
        count <- var.size # begin w/count=(nx,ny,nz,...,nt), reads entire var
        count[out.dimids4var[out.dim$tpos]] <- 1 # change to count=(nx,ny,nz,...,1) 
                                                 # to read 1 tstep
        data <- ncvar_get( nc, var, start=start, count=count )
        if (length(dim(data))!=2) return(NULL)
        if (topdown) for (i in 1:nx) data[i,1:ny]<-data[i,ny:1]
        if (is.raster) {
          r[]<-t(data)
          data.vec[,j]<-getValues(r)
          if (exists("s")) s<-stack(s,r)
          if (j==1) {
            s<-r
          } else {
            s<-stack(s,r)
          }
        }
        labels$t[j]<-tall[t]
        if (verbose) print(paste("Data for variable",nc.varname,
                                 "at timestep",labels$t[j]))
      } else {
        print("stranger things are going on right now!")
      } 
    } # end for "time"
  }
  nc_close(nc)
  return(list(data=data.vec,stack=s,labels=labels))
}

#+ Create a time sequence having daily/hourly timestep
createTimeSeq<-function(start_date="2015.01.01.01",
                        stop_date="2015.12.31.23",
                        format="%Y.%m.%d.%H",
                        time_step=1,
                        unit="hours",
                        season=NULL,
                        hourOFday.sel=NULL,
                        dayOFmonth.sel=NULL,
                        N.prev=NULL,
                        N.succ=NULL,
                        RdateOnlyOut=F,
                        verbose=F) {
#==============================================================================
#  This file is free software: you may copy, redistribute and/or modify it  
#  under the terms of the GNU General Public License as published by the  
#  Free Software Foundation, either version 2 of the License, or (at your  
#  option) any later version.  
#  
#  This file is distributed in the hope that it will be useful, but  
#  WITHOUT ANY WARRANTY; without even the implied warranty of  
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
#  General Public License for more details.  
#  
#  You should have received a copy of the GNU General Public License  
#  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
#==============================================================================
  # set parameters
  if (!is.null(stop_date))
    if  (is.na(stop_date)) stop_date<-NULL
  if (!is.null(season))
    if  (any(is.na(season))) season<-NULL
  if (!is.null(hourOFday.sel))
    if  (any(is.na(hourOFday.sel))) hourOFday.sel<-NULL
  if (!is.null(dayOFmonth.sel))
    if  (any(is.na(dayOFmonth.sel))) dayOFmonth.sel<-NULL
  if (!is.null(N.prev))
    if  (is.na(N.prev)) N.prev<-NULL
  if (!is.null(N.succ))
    if  (is.na(N.succ)) N.succ<-NULL
  #
  mon.s<-0:11
  if (!is.null(season)) {
    mon.s<-integer(0)
    if (any(season=="MAM")) mon.s<-c(mon.s,2,3,4)
    if (any(season=="JJA")) mon.s<-c(mon.s,5,6,7)
    if (any(season=="SON")) mon.s<-c(mon.s,8,9,10)
    if (any(season=="DJF")) mon.s<-c(mon.s,11,0,1)
  }
  hour.s<-0:23
  if (!is.null(hourOFday.sel)) hour.s<-hourOFday.sel
  day.s<-1:31
  if (!is.null(dayOFmonth.sel)) day.s<-dayOFmonth.sel
  # elaboration
  Rstart_date<-as.POSIXlt(str2Rdate(start_date,format=format))
  bystr<-paste(time_step," ",unit,sep="")
  if (is.null(stop_date)) {
    Rstop_date<-Rstart_date+as.difftime(N.succ, unit=unit)
    Rstart_date<-Rstart_date-as.difftime(N.prev, unit=unit)
  } else {
    Rstop_date<-as.POSIXlt(str2Rdate(stop_date,format=format))
  }
  tseq<-as.POSIXlt(seq(Rstart_date,Rstop_date,by=bystr),"UTC")
  # 
  ix<-which( (tseq$mon %in% mon.s)  & 
             (tseq$mday %in% day.s) & 
             (tseq$hour %in% hour.s) )
  if (length(ix)==0) return(integer(0))
  if (RdateOnlyOut) return(tseq[ix])
  yyyymm.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                  formatC(tseq$mon[ix]+1,width=2,flag="0"),sep="")
  yyyymmddhh.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                      formatC(tseq$mon[ix]+1,width=2,flag="0"),
                      formatC(tseq$mday[ix],width=2,flag="0"),
                      formatC(tseq$hour[ix],width=2,flag="0"),sep="")
  yyyymmdd.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                    formatC(tseq$mon[ix]+1,width=2,flag="0"),
                    formatC(tseq$mday[ix],width=2,flag="0"),sep="")
  nt<-length(yyyymmddhh.v)
  return(list(n=nt,
              yyyymm=yyyymm.v,
              yyyymmdd=yyyymmdd.v,
              yyyymmddhh=yyyymmddhh.v,
              yyyy=formatC(tseq$year[ix]+1900,width=4,flag="0"),
              mm=formatC(tseq$mon[ix]+1,width=2,flag="0"),
              dd=formatC(tseq$mday[ix],width=2,flag="0"),
              hh=formatC(tseq$hour[ix],width=2,flag="0")))
}

# -----------------------------------------------------------------------------
# Constants
# CRS strings
proj4.wgs84<-"+proj=longlat +datum=WGS84"
proj4.ETRS_LAEA<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
proj4.utm33<-"+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#==============================================================================
# MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN - MAIN -
#==============================================================================
t0<-Sys.time()
# Read command line arguments and/or set parameters to default
# create parser object
p <- arg_parser("getP")
# specify our desired options 
# by default ArgumentParser will add an help option 
p <- add_argument(p, "ffout",
                  help="full output file name",
                  default="./out.txt",
                  type="character")
# options 
p <- add_argument(p, "--verbose",help="debug mode",flag=T,short="-v")
p <- add_argument(p, "--debug",help="debug mode",flag=T,short="-d")
p <- add_argument(p, "--append",help="append output to a file",flag=T,short="-a")
# date formats
p <- add_argument(p, "--date.format.in",
                  help="format of the date/time",
                  type="character",
                  default="%Y.%m.%d.%H")
p <- add_argument(p, "--date.format.out",
                  help="format of the date/time",
                  type="character",
                  default="%Y.%m.%d.%H")
p <- add_argument(p, "--date.offset.out",
                  help="offset (hours) to add to the netcdf time to get the desired output time (\"_\" for negative)",
                  type="character",
                  default=NA)
# configuration file
p <- add_argument(p, "--config.file",
                  help="configuration file",
                  type="character",
                  default=NULL)
# sequence of points/timesteps to read from the gridded fields 
p <- add_argument(p, "--proj4xy",
                  help="proj4 string for x/y sequences",
                  default=proj4.wgs84,
                  type="character")
p <- add_argument(p, "--x.seq",
                  help="sequence of x-coordinates",
                  type="numeric",
                  nargs=Inf)
p <- add_argument(p, "--y.seq",
                  help="sequence of y-coordinates",
                  type="numeric",
                  nargs=Inf)
p <- add_argument(p, "--dr.seq",
                  help="sequence of radial distances to search for buddies",
                  type="numeric",
                  nargs=Inf)
p <- add_argument(p, "--z.seq",
                  help="sequence of z-coordinates",
                  type="numeric",
                  nargs=Inf)
#p <- add_argument(p, "--t.seq",
#                  help="sequence of timesteps (format YYYYMMDDHH00)",
#                  type="character",
#                  nargs=Inf)
# temp 
p <- add_argument(p,"--var.file",
                  help="air temperature netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwt")
p <- add_argument(p, "--proj4var",
                  help="proj4 string for the air temperature file",
                  type="character",
                  short="-rrwtp")
p <- add_argument(p, "--var.varname",
                  help="air temperature variable name in the netCDF file",
                  type="character",
                  default=NULL,
                  short="-rrwtv")
p <- add_argument(p, "--var.topdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the file upside down",
                  type="logical",
                  default=FALSE,
                  short="-rrwtt")
p <- add_argument(p, "--var.ndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwtn")
p <- add_argument(p, "--var.tpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwtti")
p <- add_argument(p, "--var.timeformat",
                  help="time format",
                  type="numeric",
                  default="%Y%m%d%H")
p <- add_argument(p, "--var.epos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwtei")
p <- add_argument(p, "--var.dimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-rrwtna",
                  nargs=Inf)
p <- add_argument(p, "--var.proj4_var",
                  help="variable that include the specification of the proj4 string",
                  type="character",
                  default="projection_lambert",
                  short="-rrwtp4v")
p <- add_argument(p, "--var.proj4_att",
                  help="attribute with the specification of the proj4 string",
                  type="character",
                  default="proj4",
                  short="-rrwtp4a")
p <- add_argument(p, "--var.e",
                  help="ensemble member to read in the air temperature netCDF file",
                  type="numeric",
                  default=NA,
                  short="-rrwtee")
# dem for temperature adjustments
p <- add_argument(p,"--var.demfile",
                  help="dem file associated to the first-guess or background file",
                  type="character",
                  default=NULL,
                  short="-rrwdf")
p <- add_argument(p, "--var.demt",
                  help="timestamp to read in the netCDF file (YYYYMMDDHH00)",
                  type="character",
                  default=NA,
                  short="-rrwdt")
p <- add_argument(p, "--var.demvarname",
                  help="variable name in the netCDF file (dem associated to the first-guess)",
                  type="character",
                  default="none",
                  short="-rrwdv")
p <- add_argument(p, "--var.demtopdown",
                  help="logical, netCDF topdown parameter. If TRUE then turn the field upside down",
                  type="logical",
                  default=FALSE,
                  short="-rrwdtd")
p <- add_argument(p, "--var.demndim",
                  help="number of dimensions in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwdnd")
p <- add_argument(p, "--var.demepos",
                  help="position of the dimension ''ensemble'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwdei")
p <- add_argument(p, "--var.demtpos",
                  help="position of the dimension ''time'' in the netCDF file",
                  type="numeric",
                  default=3,
                  short="-rrwdti")
p <- add_argument(p, "--var.deme",
                  help="ensemble member to read in the netCDF file",
                  type="numeric",
                  default=NA,
                  short="-rrwdee")
p <- add_argument(p, "--var.demdimnames",
                  help="dimension names in the netCDF file",
                  type="character",
                  default=NA,
                  short="-rrwdna",
                  nargs=Inf)
#
p <- add_argument(p,"--start.date",
                  help="first day of the time interval (yyyy.mm.dd)",
                  type="character",
                  default=NULL,
                  short="-bd")
p <- add_argument(p,"--end.date",
                  help="last day of the time interval (yyyy.mm.dd)",
                  type="character",
                  default=NULL,
                  short="-ld")
p <- add_argument(p,"--season",
                  help="season of the year (MAM,JJA,SON,DJF)",
                  type="character",
                  default=NULL,
                  short="-soy")
#
argv <- parse_args(p)
#
#-----------------------------------------------------------------------------
# read configuration file
if (!is.na(argv$config.file)) {
  if (file.exists(argv$config.file)) {
    source(argv$config.file)
    argv_tmp<-append(argv,conf)
    names_argv_tmp<-names(argv_tmp)
    argv_def<-list()
    names_argv_def<-integer(0)
    k<-0
    for (i in 1:length(argv_tmp)) {
      if (names_argv_tmp[i] %in% names_argv_def) next
      k<-k+1
      j<-which(names_argv_tmp==names_argv_tmp[i])
      argv_def[[k]]<-argv_tmp[[j[length(j)]]]
      names_argv_def<-c(names_argv_def,names_argv_tmp[i])
    }
    names(argv_def)<-names_argv_def
    rm(argv_tmp,names_argv_tmp,names_argv_def)
    rm(argv)
    argv<-argv_def
    rm(argv_def)
  } else {
    print("WARNING: config file not found")
    print(argv$config.file)
  }
}
#
#------------------------------------------------------------------------------
# checks on command-line arguments
boom<-function(text) {print(text);quit(status=1)}
if (any(!is.na(argv$z.seq))) {
  if (length(argv$z.seq)!=length(argv$x.seq)) {
    argv$z.seq<-rep(argv$z.seq[1],length(argv$x.seq))
  }
}
if (any(!is.na(argv$dr.seq))) {
  if (length(argv$dr.seq)!=length(argv$x.seq)) {
    argv$dr.seq<-rep(argv$dr.seq[1],length(argv$x.seq))
  }
}
nseq<-length(argv$x.seq)
if (!is.na(argv$date.offset.out)) 
  argv$date.offset.out<-as.numeric(gsub("_","-",argv$date.offset.out))
#
#------------------------------------------------------------------------------
# date-time 
if (is.na(argv$season)) argv$season<-NULL
if (!is.na(argv$start.date) & !is.na(argv$end.date)) {
  tseq<-createTimeSeq(start_date=argv$start.date,
                      stop_date=argv$end.date,
                      format="%Y.%m.%d",
                      time_step=1,
                      unit="days",
                      season=argv$season,
                      hourOFday.sel=NULL,
                      dayOFmonth.sel=NULL,
                      N.prev=NULL,
                      N.succ=NULL,
                      RdateOnlyOut=F,
                      verbose=F)
}
#
#------------------------------------------------------------------------------
# transform CRS, if needed 
if (argv$proj4var!=argv$proj4xy) {
  coord<-SpatialPoints(cbind(argv$x.seq,argv$y.seq),
                       proj4string=CRS(argv$proj4xy))
  coord.new<-spTransform(coord,CRS(argv$proj4var))
  xy.var<-coordinates(coord.new)
  rm(coord,coord.new)
} else {
  xy.var<-coordinates(cbind(argv$x.seq,argv$y.seq))
}
#
#------------------------------------------------------------------------------
# read elevation from gridded field
if (!is.null(argv$var.demtpos)) {
  var.demtpos<-argv$var.demtpos
  ti<-nc4.getTime(argv$var.demfile)
  if (is.na(argv$var.demt)) argv$var.demt<-ti[1]
  if (!(argv$var.demt %in% ti)) {
    print("ERROR timestamp requested is not in the file:")
    print(argv$var.demt)
    print(ti)
    quit(status=1)
  }
} else {
  var.demtpos<-argv$var.demtpos
}
if (!is.null(argv$var.demepos)) {
  var.demepos<-argv$var.demepos
  if (is.na(argv$var.demepos)) var.demepos<-NULL
} else {
  var.demepos<-NULL
}
var.deme<-argv$var.deme
if (is.na(argv$var.deme)) var.deme<-NULL
raux<-try(nc4in(nc.file=argv$var.demfile,
                nc.varname=argv$var.demvarname,
                topdown=argv$var.demtopdown,
                out.dim=list(ndim=argv$var.demndim,
                             tpos=var.demtpos,
                             epos=var.demepos,
                             names=argv$var.demdimnames),
                proj4=argv$proj4var,
                nc.proj4=list(var=argv$var.proj4_var,
                              att=argv$var.proj4_att),
                selection=list(t=argv$var.demt,e=var.deme)))
if (is.null(raux)) {
  print("ERROR while reading file:")
  print(argv$var.demfile)
  quit(status=1)
}
rvardem<-raux$stack
dvardem<-getValues(rvardem)
xy.grid<-xyFromCell(rvardem,1:ncell(rvardem))
#
#------------------------------------------------------------------------------
# Given a preset radius around a point,find the grid point with the 
# closest elevation
ix.grid<-vector(mode="numeric",length=nseq)
for (i in 1:nseq) {
  if (argv$z.seq[i]<xres(rvardem)) argv$z.seq[i]<-xres(rvardem)
  if (argv$z.seq[i]<yres(rvardem)) argv$z.seq[i]<-yres(rvardem)
  distz<-abs(dvardem-argv$z.seq[i])
  disth<-sqrt((xy.grid[,1]-xy.var[i,1])**2 + 
              (xy.grid[,2]-xy.var[i,2])**2)
  ix0<-which.min(distz[which(disth<=argv$dr.seq[i])]) 
  ix.grid[i]<-which(disth<=argv$dr.seq[i])[ix0]
}
if (argv$debug) {
  png(file="rvardem.png",width=800,height=800)
  image(rvardem,   
        main="digital elevation model (cyan=your point;black=closest elevation)")
  points(xy.var,pch=19,col="cyan")
  points(cbind(xy.grid[ix.grid,1],xy.grid[ix.grid,2]),pch=19,col="black")
  dev.off()
  for (i in 1:nseq) {
    png(file=paste0("deb0_point",formatC(i,width=3,flag="0"),".png"),
        width=800,height=800)
    image(rvardem,xlim=c(xy.var[i,1]-argv$dr.seq[i],xy.var[i,1]+argv$dr.seq[i]),
                  ylim=c(xy.var[i,2]-argv$dr.seq[i],xy.var[i,2]+argv$dr.seq[i]),
        main="digital elevation model (cyan=your point;black=closest elevation)")
    points(cbind(xy.var[i,1],xy.var[i,2]),pch=19,col="cyan")
    points(cbind(xy.grid[ix.grid[i],1],xy.grid[ix.grid[i],2]),pch=19,col="black")
    dev.off()
    distz<-abs(dvardem-argv$z.seq[i])
    disth<-sqrt((xy.grid[,1]-xy.var[i,1])**2 + 
                (xy.grid[,2]-xy.var[i,2])**2)
    ix<-which(disth<=argv$dr.seq[i]) 
    png(file=paste0("deb1_point",formatC(i,width=3,flag="0"),".png"),
        width=800,height=800)
    plot(xy.grid[ix,1],dvardem[ix],xlab="X",ylab="elevation",pch=19,col="gray",
    main="elev vs X-coord (cyan=your point;black=closest elevation;gray=others)")
    points(xy.grid[ix.grid[i],1],dvardem[ix.grid[i]],pch=19,col="black",cex=2)
    points(xy.var[i,1],argv$z.seq[i],pch=19,col="cyan",cex=2)
    abline(h=argv$z.seq[i],lty=2,lwd=2)
    dev.off()
    png(file=paste0("deb2_point",formatC(i,width=3,flag="0"),".png"),
        width=800,height=800)
    plot(xy.grid[ix,2],dvardem[ix],xlab="Y",ylab="elevation",pch=19,col="gray",
    main="elev vs Y-coord (cyan=your point;black=closest elevation;gray=others)")
    points(xy.grid[ix.grid[i],2],dvardem[ix.grid[i]],pch=19,col="black",cex=2)
    points(xy.var[i,2],argv$z.seq[i],pch=19,col="cyan",cex=2)
    abline(h=argv$z.seq[i],lty=2,lwd=2)
    dev.off()
  }
}
#
#------------------------------------------------------------------------------
# read variable from gridded field & write the output
if (!argv$append) { 
  cat(file=argv$ffout,
      "pid;date;x;y;x_transf;y_transf;z;val;\n",
      append=F)
}
var.epos<-argv$var.epos
if (is.na(argv$var.epos)) var.epos<-NULL
var.e<-argv$var.e
if (is.na(argv$var.e)) var.e<-NULL
ti<-nc4.getTime(argv$var.file)
tiRdate<-str2Rdate(ti,format=argv$var.timeformat)
if (!exists("tseq")) {
  tseq<-list(n=length(ti))
  tseqRdate<-tiRdate
} else {
  tseq<-list(n=length(ti))
  tseqRdate<-str2Rdate(tseq$yyyymmddhh,format="%Y%m%d%H")
}
for (i in 1:tseq$n) {
  ixt<-which(tiRdate==tseqRdate[i])
  if (length(ixt)!=1) {
    print("ERROR timestamp requested is not in the file:")
    next
  }
  raux<-try(nc4in(nc.file=argv$var.file,
                  nc.varname=argv$var.varname,
                  topdown=argv$var.topdown,
                  out.dim=list(ndim=argv$var.ndim,
                               tpos=argv$var.tpos,
                               epos=var.epos,
                               names=argv$var.dimnames),
                  proj4=argv$proj4var,
                  nc.proj4=list(var=argv$var.proj4_var,
                                att=argv$var.proj4_att),
                  selection=list(t=ti[ixt],e=var.e)))
  rvar<-raux$stack
  rm(raux)
  dvar<-getValues(rvar)
  toutRdate<-tseqRdate[i]
  if (!is.na(argv$date.offset.out)) toutRdate<-toutRdate+as.difftime(argv$date.offset.out,unit="hours")
  tout<-Rdate2str(toutRdate,format=argv$date.format.out)
  cat(file=argv$ffout,
      paste0(1:length(ix.grid),";",
             tout,";",
             argv$x.seq,";",
             argv$y.seq,";",
             round(xy.var[,1],6),";",
             round(xy.var[,2],6),";",
             round(dvardem[ix.grid],0),";",
             round(dvar[ix.grid],3),";\n"),
      append=T)
}
#
#------------------------------------------------------------------------------
print("Normal exit")
quit(status=0)
