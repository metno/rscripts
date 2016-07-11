######################################################################
#
#  iobin.R
#
#  funksjoner for å skrive og lese binære filer

#  writebinfile		skriver en array til en binær fil.
#  readbinfile		leser en binær fil og returnerer verdiene som array.
#  ascii2bil		leser en ArcGis ascii fil og skriver verdiene til
#			en binær fil.
#  cutgrid		leser en binær fil, klipper ut spesifisert rektangel
#			og skriver the nye griddet til en binær fil.
#  ReadMaskedGrid	leser et eller flere grid, maskerer de og returnerer
#			en array med de maskerte griddene
#  GridFileName		returnerer filnavn for binære gridfiler
#  NormGridFileName	returnerer filnavn for binære normalen gridfiler
#
#  Eli Alfnes, met.no, 2005-08-22
#  rev. 2006-01-03, 2006-01-19
#
######################################################################

#require(devtools)
#source_url("https://raw.githubusercontent.com/metno/rscripts/master/R/iobin.R")


#--------------------------------------------------------------------------
# writebinfile writes a vector or a 2D-array to a binary file (filename)
# the 2D-array is first transfered to a vector (wvector).
#
# When converting from array to vector R takes column by column adding the
# values to the vector. Thus, the first column in the array must represent 
# the first(upper) line in the grid.
#--------------------------------------------------------------------------
# warray	array or vector to be written
# filename	name of the binary file
# na		NaN-value for the binary file
# size		number of bytes (1, 2 or 4 (eq 8, 16 and 32 bits, respectively)
writebinfile <- function(warray, filename, na=65535, size=2) {

   if (size > 4)  {
      stop("writebinfile() is only implemented for integers of size=1, 2 and 4, aborting... ")
   }
   else if (filename == "") {
      stop("filename of output file is missing, aborting... ")
   }
   else {
   # seting nodata and infinite values to na (defaault = 10000)
     warray[is.na(warray)] <- na 
     warray[!is.finite(warray)] <- na

     if (is.array(warray) || is.vector(warray)) 
        wvector <- as.integer(warray,dim=length(warray))
     else
        stop("the first function argument is neighter an array nor a vector")

   # writing vector to binary file
     fpout <- file(filename,"wb")
     writeBin(wvector, fpout, size=size) 
     print(paste(length(wvector), "values written to the", (size*8), "bits binary file", filename))
     close(fpout)

   }
}


#--------------------------------------------------------------------------
# readbinfile reads a binary file (filename) and transfers the data to a
# 2D-array (rarray) or a vector if length(arraydim) = 1)
#--------------------------------------------------------------------------
#
# arraydim	c(numOfCol,numOfRow)
# filename	name of the binary file
# na		NaN-value in the binary file
# size		number of bytes (1, 2 or 4 (eq 8, 16 and 32 bits, respectively)
# signed	logical TRUE/FALSE, set to TRUE if a signed binary file
# return	rarray
readbinfile <- function(arraydim, filename, na=65535, size=2, signed=FALSE) {

   if (size > 4)  {
      stop("readbinfile() is only implemented for integers of size=1, 2 and 4, aborting... ")
   }
   else {
   # defining and initializing the vector
     nrow <- arraydim[1]
     if (length(arraydim)==2) 
        ncol <- arraydim[2]
     else ncol <- 1
     
     fpin <- file(filename,"rb")
     rvector <- readBin(fpin, integer(), nrow*ncol, size=size, signed=signed) 
     if (length(rvector) < nrow*ncol)
        stop(paste("Input file",filename,"contains too few records (",
             length(rvector),"of",nrow*ncol,")."))
     close(fpin)
     rvector[rvector==na] <- NA

     return(array(rvector,dim=arraydim))
   }
}


#--------------------------------------------------------------------------
# ascii2bil reads an ArcGis formatted ascii file (arc_srcfile) and writes the
# data to a binary file.
# The ArcGis ascii format constist of 6 lines of header information (which
# is currently ignorded in this function) followed by space separated 
# numeric values.
#--------------------------------------------------------------------------
# arc_srcfile	file name of the ArcGis ascii file
# trgfile	file name of the target binary file
# srcnan	NaN-value in the ascii file
# trgnan	NaN-value in the binary file
# size		number of bytes (1, 2 or 4 (eq 8, 16 and 32 bits, respectively)
ascii2bil <- function(arc_srcfile,trgfile,srcnan=-9999,trgnan=65535, size=2) {

   asciilines <- readLines(arc_srcfile)
   gridlines <- asciilines[7:length(asciilines)]
   gridvalues <- as.numeric(unlist(strsplit(gridlines,split=" ")))
   gridvalues[gridvalues==srcnan] <- NA
   writebinfile(gridvalues,trgfile,na=trgnan,size=size)
}


#--------------------------------------------------------------------------
# cutgrid cuts a rectangle (srow:erow,scol:ecol) out of a grid read from
# srcfile and save the new grid to trgfile.
#--------------------------------------------------------------------------
# srcfile	file name of the source binary grid file
# trgfile	file name of the target binary grid file, the cutted grid
# srcdim	dimension of the source grid, c(numOfCol,numOfRow)
# srow		first row to cut out
# erow		last row to cut out
# scol		first column to cut out
# ecol		last column to cut out
# size		number of bytes (1, 2 or 4 (eq 8, 16 and 32 bits, respectively)
# signed	logical TRUE/FALSE, set to TRUE if srcfile is signed
cutgrid <- function(srcfile,trgfile,srcdim,srow,erow,scol,ecol,size=2,signed=FALSE) {

   srcgrid <- readbinfile(srcdim,srcfile,na=NA,size=size,signed=signed)
   trggrid <- srcgrid[srow:erow,scol:ecol]
   writebinfile(trggrid,trgfile,na=NA,size=size)
}

#
# Function	ReadMaskedGrid
#		Read binary grids and mask out selected pixels
#
# Input		param     parameter name (abbreviation)
#		sdate     start date
#		edate     end date
#		maskgrid  grid mask (cells containing na will be masked out)
#		          if maskgrid is not given, a default mask grid is used
#		griddim   number of cells in the grid
#		na        NaN-value for the binary file
#		size      number of bytes (1, 2 or 4 (eq 8, 16 and 32 bits, 
#		          respectively))
#		signed	  logical TRUE/FALSE, set to TRUE if a signed binary file
#		src_dir	  main directory of the grids
#                         the files are assumed stored in a structure following
#		          src_dir/param/binary/year/month/param_year_month_day.bil
#
# Return	array (or vector, if only one grid) of masked grids
#
# Author	Eli Alfnes, met.no, 2006-01-19
#
ReadMaskedGrid <- function(param,sdate,edate,maskgrid,griddim=1550*1195,
                na=65535,size=2,signed=FALSE,src_dir="/klimadata/klimagrid/daily",
		src_sub_dir="binary")
{
   require(date)

 # functions for reading av writing binary files
#   source("/klimadata/applikasjon/gridding/src/iobin.R")

 # Declaration of local variables and arrays
   numOfDay <- edate-sdate+1

 # get maskgrid
   if (missing("maskgrid"))
   {
      maskfile <- "/klimadata/applikasjon/gridding/grd/maskgrid.bil"
      maskgrid <- readbinfile(griddim,maskfile,na=0,size=1)
   }

   grid_val <- array(NA,dim=c(numOfDay,sum(!is.na(maskgrid))))

 # Loop through the binary files and generate the time series
   for (adate in sdate:edate) {
      day_idx <- adate-sdate+1

      fname <- GridFileName(src_dir,param,fdate=as.date(adate))
      if (file.access(fname,4)==0)  # file readable
      {
         grid_val[day_idx,] <-
             readbinfile(griddim,fname,na=na,size=size,
             signed=signed)[!is.na(maskgrid)]
      }
   }

   if (numOfDay==1)
     return(as.integer(grid_val,dim=length(grid_val)))
   else
     return(grid_val)
}


#
# Function	GridFileName
#
# Input         dir	main directory where the binary grids are stored
#		param	parameter name (abbreviation)
#		fdate   date of the file (for daily files)
#		year    date of the file (for annual, monthly and seasonal files)
#		month   date of the file (for monthly and seasonal files)
#
# Return	filename string for binary grid files
#
# Author	Eli Alfnes, met.no, 20051111
#
GridFileName <- function(dir,param,fdate=NULL,year=NULL,month=NULL)
{
   require(date)
   fileprefix <- param
   if (param=='tam') { fileprefix <- 'tm'}

   if (is.date(fdate) && !is.na(fdate))
   {
     dd <- date.mdy(fdate)
     fname <- sprintf("%s/%s/binary/%04i/%02i/%s_%04i_%02i_%02i.bil",
                dir,param,dd$year,dd$month,fileprefix,dd$year,dd$month,dd$day)
   }
   else if (length(year)>0)
   {
     if (length(month)>0)
        fname <- sprintf("%s/%s/binary/%04i/%s_%04i_%02i.bil",
                dir,param,year,fileprefix,year,month)
     else
        fname <- sprintf("%s/%s/binary/%s_%04i.bil", dir,param,fileprefix,year)
   }
   else fname <- ""   #feil i input parametre, "" gir -1 ved file.access(fname)
   return(fname)
}

#
# Function	NormGridFileName
#		As GridFileName except that the year part of the
#		filename contains start and end year of the normal
#		period. (ex. 19712000)
#
# Input         dir	main directory where the binary grids are stored
#		param	parameter name (abbreviation)
#		normyear startyear + endyear concatenated
#		month   month number (1-12)
#		day	day of month
#
# Return	filename string for binary normal grid files
#
# Author	Eli Alfnes, met.no, 20060103
#
NormGridFileName <- function(dir,param,normyear=NULL,month=NULL,day=NULL)
{
   fileprefix <- param
   if (param=='tam') { fileprefix <- 'tm'}  #shouldn't happend here

   if (length(normyear)>0)
   {
     if (length(month)>0)
     {
   	if (length(day)>0)	#day normal
           fname <- sprintf("%s/%s/binary/%s/%02i/%s_%s_%02i_%02i.bil",
                dir,param,normyear,month,fileprefix,normyear,month,day)
        else			#month normal
           fname <- sprintf("%s/%s/binary/%s/%s_%s_%02i.bil",
                dir,param,normyear,fileprefix,normyear,month)
     }
     else
        fname <- sprintf("%s/%s/binary/%s_%s.bil", 
                dir,param,fileprefix,normyear)
   }
   else fname <- ""   #feil i input parametre, "" gir -1 ved file.access(fname)
   return(fname)
}
