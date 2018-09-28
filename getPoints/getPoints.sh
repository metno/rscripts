#!/bin/bash
#$ -N getp 
#$ -S /bin/bash
#$ -l h_rt=72:00:00
#$ -pe mpi 1
#$ -q ded-parallelx.q
#$ -l h_vmem=5G
#$ -o /lustre/storeB/users/cristianl/log/out_$JOB_NAME.$JOB_ID.$HOSTNAME
#$ -e /lustre/storeB/users/cristianl/log/err_$JOB_NAME.$JOB_ID.$HOSTNAME
# (*) change the -o & -e options to your user directory
#==============================================================================
# load modules
  source /etc/profile.d/modules.sh
  module load R/R-3.3.1-met
  module load hdf5/1.8.17
  module load netcdf/4.4.1
  module load fyba/master
  module load gridpp/0.2.9
  module load gdal/2.1.1
# Constants
  export R_LIBS=/home/cristianl/Rpackages-xenial # <== (*) set your R_LIBS
  SECONDS=0
#----------------------------
# Read command line arguments
#----------------------------
  flag_s=0
  flag_e=0
  flag_v=0
  flag_t=0
  flag_o=0
  while getopts "s:e:v:t:o:" Option 
  do
    case $Option in
    s ) flag_s=1
    DATESTART=$OPTARG
    ;;
    e ) flag_e=1
    DATEEND=$OPTARG
    ;;
    v ) flag_v=1
    ngcd_var=$OPTARG
    ;;
    t ) flag_t=1
    ngcd_type=$OPTARG
    ;;
    o ) flag_o=1
    ffout=$OPTARG
    ;;
    * ) echo " not recognized Option ";;
    esac
  done
#----------------------------
# Set constants
#----------------------------
  ngcd_ver=18.09
  ngcd_config_file=/home/cristianl/projects/getPoints/ngcd_getpoints.cfg # <== (*) change to point to your config file
  ngcd_repo=/lustre/storeB/project/metkl/senorge2/thredds/Nordic_Gridded_Climate_Dataset_NGCD/version_$ngcd_ver/$ngcd_var/type$ngcd_type
  script=/home/cristianl/projects/getPoints/getPoints.R # <== (*) change to point to your R-script
#----------------------------
# Specify points
#----------------------------
# example: suppose you need point1=(x1,y1,z1) point2=(x2,y2,z2) ... coordinate reference system is "proj4"
# then you specify xseq=x1,x2,... yseq=y1,y2,... zseq=z1,z2,...
# dr.seq is a vector with the radius to search for the ngd-point with the closest elevation 
  xseq=61819
  yseq=6651310
  zseq=1119
  drseq=10000
  proj4="+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#------------------------------------------------------------------------------
# date/time manipulations
  YYYYbeg=${DATESTART:0:4}
  MMbeg=${DATESTART:5:2}
  DDbeg=${DATESTART:8:2}
  HHbeg=06
  YYYYend=${DATEEND:0:4}
  MMend=${DATEEND:5:2}
  DDend=${DATEEND:8:2}
  HHend=08
  SECbeg=`date +%s -d "$YYYYbeg-$MMbeg-$DDbeg $HHbeg:00:00"`
  SECend=`date +%s -d "$YYYYend-$MMend-$DDend $HHend:00:00"`
#------------------------------------------------------------------------------ 
# Checks
#------------------------------------------------------------------------------
# check if: start OR end date is missing OR if the start date is after end
  if [ "$flag_s" -eq 0 ] || [ "$flag_e" -eq 0 ] || [ "$SECbeg" -gt "$SECend" ]
  then
    echo "error in dates"
    exit 1
  fi
  if [ "$flag_v" -eq 0 ] || [ "$flag_t" -eq 0 ]
  then
    echo "error in command line arguments"
    exit 1
  fi
  if [ "$flag_o" -eq 0 ]
  then
    echo "error in command line arguments: specify output file"
    exit 1
  fi
#------------------------------------------------------------------------------ 
  SECcur=$SECbeg
  out="/lustre/storeB/users/cristianl/archive"
  case="/lustre/storeB/project/metkl/senorge2/case/case_20180801"
  geoinfo="/lustre/storeB/users/cristianl/geoinfo"
  so=$HOME/share/ngcd/
  echo `date +%Y-%m-%d" "%H:%M`
  first=1
  while (( "$SECcur" <= "$SECend" )) 
  do
    DATEcur=`date --date="1970-01-01 $SECcur sec UTC" +%Y.%m.%d.%H`
    year=${DATEcur:0:4}
    month=${DATEcur:5:2}
    day=${DATEcur:8:2}
    hour=${DATEcur:11:2}
    date=$year$month$day
    ffin=$ngcd_repo/$year/$month/NGCD_$ngcd_var\_type$ngcd_type\_$date.nc
    #
    if [ "$first" -eq 1 ]; then append=""; else append="--append"; fi
    if [ "$first" -eq 1 ]; then first=0; fi
    echo "$script $ffout $append --x.seq $xseq --y.seq $yseq --dr.seq $drseq --z.seq $zseq --proj4xy \"$proj4\" --var.file $ffin --var.varname $ngcd_var --config.file $ngcd_config_file"
    $script $ffout $append --x.seq $xseq --y.seq $yseq --dr.seq $drseq --z.seq $zseq --proj4xy "$proj4" --var.file $ffin --var.varname $ngcd_var --config.file $ngcd_config_file
    # next timestep
    SECcur=$(( SECcur+3600*24 ))
  done
  echo `date +%Y-%m-%d" "%H:%M`
#--------------------------
# Clean and Exit 
#--------------------------
  duration=$SECONDS
  echo "the end: $(($duration / 60)) minutes and $(($duration % 60)) seconds elapsed."
  exit 0

