#!/bin/bash
# bash script to run iland for a list of scenarios
# script should be run from the main directory, which is the
# directory containing the readme
# script takes 1 argument: number of reps
# usage: bash scripts/step04c_run_iland_local_sensitivity_analysis.sh [reps]

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
reps=$1
path="iland_exe/iLand_executable_Qt6.5/" # set this path to location of iland exe
simulation_years="30"
start_rep=1

# set filename of csv that will be used to customize climate and environment in each project file
# this script is hard-coded to specific columns in a specific order
csv_name="iland/sensitivity_analysis.csv"

#####
# loop through entire csv and run iland for each site
#####

sed 1d $csv_name | while IFS=, read -r barkbeetle decomposition establishment output
do 
    for rep in $(seq $start_rep $reps)
    do
        echo "running barkbeetle $barkbeetle decomposition $decomposition establishment $establishment rep $rep"

        # run iland with timevents
        ${path}ilandc.exe iland/sensitivity_analysis_microclimate.xml $simulation_years \
        system.database.out=sensitivity_analysis_microclimate/output_microclimate_${rep}_${output}.sqlite \
        model.world.timeEventsFile=timeevents_${rep}.txt \
        model.climate.microclimate.barkbeetle=${barkbeetle} \
        model.climate.microclimate.decomposition=${decomposition} \
        model.climate.microclimate.establishment=${establishment} 
        
        # pre-process outputs for analyses
        Rscript.exe scripts/step05b_iland_preprocess_outputs_sensitivity_analysis.R $output $rep
  
    done
done
