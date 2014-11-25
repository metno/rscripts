# --~- SpInt_PREC_plot_analysis_map.R  -~--
# Create maps using the seNorge v2.0 precipitation analysis field
#
# History:
# 01.09.2014 - Cristian Lussana
# -----------------------------------------------------------------------------
# LIBRARIES
library(raster)
library(rgdal)
library(ncdf)
# PARAMETERS
# paths
main.path<-"/disk1/R/projects/SpInt"
path2commonlib<-paste(main.path,"/lib",sep="")
path2commonetc<-paste(main.path,"/etc",sep="")
# Read Geographical Information
filenamedem<-paste(path2commonetc,"/fenno_dem_u33.asc",sep="")
filenamemask<-paste(path2commonetc,"/maskgrid.bil",sep="")
norgeshp <- readOGR(paste(path2commonetc,"/norge.shp",sep=""),"norge")
# Graphics
xlim.sw<--75000
xlim.ne<-1120000
ylim.sw<-6450000
ylim.ne<-8000000
# South Norway only
#xlim.sw<--75000
#xlim.ne<-500000
#ylim.sw<-6450000
#ylim.ne<-7100000
# Colors
tcol<-c("mediumorchid","plum","paleturquoise3","paleturquoise","palegreen",
        "green3","forestgreen","yellow2","darkorange","red")
tcol_ext<-c("gray","orangered4")
# Breaks
# day
bcol.daily<-c(0.05,0.5,3,7,10,15,20,30,40,50,60)
# month
bcol.monthly<-c(0.05,0.5,3,10,50,100,200,300,500,750,1500)
# annual
bcol.annual<-c(0.05,0.5,250,500,750,1000,1250,1500,2000,3000,4000)
#-------------------------------------------------------------------
# External Functions
source(paste(path2commonlib,"/SpInt_plots.R",sep=""))
#-------------------------------------------------------------------
# MAIN ========================================================================
# [] Read command line arguments
arguments <- commandArgs()
# input netcdf file
file.nc<-arguments[3]
# output png file
file.png<-arguments[4]
# main title (just in case)
main<-arguments[5]
if (length(arguments)!=5) {
  print("Error in command line arguments:")
  print("R --vanilla file.nc file.png main_title < SpInt_PREC_plot_analysis_map.R")
  quit(status=1)
}
# [] Grid -> same as the DEM grid 
# CRS Coordinate Reference System
projstr<-"+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
proj_format_str<-"PROJ.4"
stackGeoGrid<-raster(filenamedem)
mask<-raster(filenamemask)
projection(stackGeoGrid)<-projstr
projection(mask)<-projstr
nx<-ncol(stackGeoGrid)
ny<-nrow(stackGeoGrid)
xmn<-xmin(stackGeoGrid)
xmx<-xmax(stackGeoGrid)
ymn<-ymin(stackGeoGrid)
ymx<-ymax(stackGeoGrid)
# Extract orography on unmasked gridpoints only
# orog has the same dimensions of stackGeoGrid but it is masked (NAs) outside Norway
orog<-mask(stackGeoGrid,mask,maskvalue=0)
# Define raster variable "xx"
xx <-raster(ncol=nx, nrow=ny, 
            xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx,
            crs=projstr)
xx[]<-NA
# open/read/close netcdf file
nc <- open.ncdf(file.nc)
data <- get.var.ncdf( nc )
close.ncdf(nc)
# put data on raster variable (t=transpose)
xx[]<-t(data)
# rainspatplot is in "SpInt_plots.R"
ee<-rainspatplot(x=NA,
                 y=NA,
                 yvar=NA,
                 xvar=xx,
                 xvar.orog=orog,
                 brk=bcol.daily,
                 col=tcol,
                 colext=tcol_ext,
                 legcex=2,
                 mtxt=main,
                 namefileout=file.png,
                 cx=rep(1,(length(tcol)+2)),
                 bnd=norgeshp,
                 xl=c(xlim.sw,xlim.ne),yl=c(ylim.sw,ylim.ne))
#
quit(status=0)
