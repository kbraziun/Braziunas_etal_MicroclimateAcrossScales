#!/bin/bash
# bash script to run iland for a list of scenarios
# script should be run from the main directory, which is the
# directory containing the readme
# script takes 2 arguments: climate year and output resolution
# usage: bash scripts/step04a_run_iland_local_temperature_buffering.sh [year] [res]

#####
# catch errors
#####

set -e # terminate if non-zero exit
set -u # terminate if variable unset
set -o pipefail # terminate if pipe command fails

#####
# arguments
#####

# input arguments
let year=$1-1 # use previous year to set min year
res=$2

# variables
path="iland_exe/iLand_executable_Qt6.5/" # set this path to location of iland exe
simulation_years="1"
xml="temp_buffering_microclimate.xml"

#####
# run iland for given year
#####

echo "running year $1"

# run iland with custom year
${path}ilandc.exe iland/${xml} $simulation_years \
model.climate.filter="year>$year"

# move buffer outputs to folder with consistent naming
mkdir iland/output/temp_buffering_microclimate/year_$1
mv iland/output/temp_buffering_microclimate/raw/* iland/output/temp_buffering_microclimate/year_$1/

# run R script to process outputs
Rscript.exe iland/scripts/process_microclimate_grids_${res}.R $1 "iland/output/temp_buffering_microclimate/"

# delete raw outputs
rm -r iland/output/temp_buffering_microclimate/year_$1/
