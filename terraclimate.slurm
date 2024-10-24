#!/bin/bash -l
#SBATCH --job-name=terraclim
#SBATCH --account=lp_climateplants
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=36
##SBATCH --partition=bigmem
#SBATCH --mem-per-cpu=4000M
#SBATCH --time=10:00:00
#SBATCH --cluster=genius
##SBATCH --mail-type=FAIL,BEGIN,END
##SBATCH --mail-user=xiaqu.zhou@kuleuven.be


#==========================================
#        PARAMETERS
#==========================================
#latitude bounding box -90.0 to 90.0 (North is positive, South is negative)
LAT_MIN="30.0"
LAT_MAX="75.0"

#longitude bounding box: -180.0 to 180.0 (East is positive, West is negative)
LON_MIN="-9.0"
LON_MAX="35.0"

#variable choices: tmax,tmin,ws,vpd,vap,swe,srad,soil,q,ppt,pet,def,aet,PDSI
VARIABLES=("tmax" "tmin")   

#time range choices - 1958-01 to 2017-01
TIME_START="2000-01-01T00%3A00%3A00Z"  
TIME_END="2020-12-31T00%3A00%3A00Z"    
#==========================================
#       DOWNLOAD REQUESTS
#==========================================
ncssPath="http://thredds.northwestknowledge.net:8080/thredds/ncss"
outputDir="/lustre1/scratch/348/vsc34871/output/"  # Directory where files will be saved

for variable in "${VARIABLES[@]}"
do
	filename="agg_terraclimate_${variable}_1958_CurrentYear_GLOBE.nc"
	queryString="$ncssPath/${filename}?"
	queryString="$queryString&var=${variable}"
	queryString="$queryString&south=${LAT_MIN}&north=${LAT_MAX}&west=${LON_MIN}&east=${LON_MAX}&horizStride=1"
	queryString="$queryString&time_start=${TIME_START}&time_end=${TIME_END}&timeStride=1"
	queryString="$queryString&disableProjSubset=on&addLatLon=true&accept=netcdf"

	newfilename="${outputDir}/terraclimate_${variable}.nc"  # Save in the output directory
	wget -nc -c -nd "$queryString" -O "$newfilename"
done

