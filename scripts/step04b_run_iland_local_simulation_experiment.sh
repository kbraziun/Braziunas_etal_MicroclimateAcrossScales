#!/bin/bash
# bash script to run iland for a list of scenarios
# script should be run from the main directory, which is the
# directory containing the readme
# script takes 2 arguments: climate scenario and number of reps
# usage: bash scripts/step04b_run_iland_local_simulation_experiment.sh [climate] [reps]

#####
# catch errors
#####

set -e # terminate if non-zero exit
set -u # terminate if variable unset
set -o pipefail # terminate if pipe command fails

#####
# arguments
#####

# currently this is just set up for the sensitivity analysis, but will update for full run

# input arguments
climate=$1
reps=$2
path="iland_exe/iLand_executable_Qt6.5/" # set this path to location of iland exe
simulation_years="1000"
start_rep=1

# set filename of csv that will be used to customize climate and environment in each project file
# this script is hard-coded to specific columns in a specific order
table_name="iland/main_file.txt"
xml="simulation_experiment_$climate.xml"

echo $xml

#####
# loop through entire csv and run iland for each site
#####

# sequence through reps
for rep in $(seq $start_rep $reps)
do
    # read table with random sampling list
    sed 1d $table_name | while IFS=" " read -r table_rep sample_list
    do
        # only use sampling list for given rep
        if [ $table_rep = $rep ]
        then
            echo "running $climate rep $rep table_rep $table_rep sampling list $sample_list"

            mkdir iland/output/simulation_experiment_${climate}/rep_${rep}

            # if microclimate create folders for years 1 and 11, hard coded for buffers at year 1 and 11
            if [ $climate = "microclimate" ]
            then
                mkdir iland/output/simulation_experiment_${climate}/year_1
                mkdir iland/output/simulation_experiment_${climate}/year_11

            fi

            # run iland with timevents
            ${path}ilandc.exe iland/${xml} $simulation_years \
            system.database.out=simulation_experiment_${climate}/rep_${rep}/output_${climate}.sqlite \
            model.world.timeEventsFile=timeevents_${rep}.txt \
            model.climate.randomSamplingList=$sample_list 

            # move output grids to rep folder
            mv iland/output/simulation_experiment_${climate}/*.asc iland/output/simulation_experiment_${climate}/rep_${rep}

            # run buffering processing R script for microclimate, hard coded for buffers at year 1 and 11
            if [ $climate = "microclimate" ]
            then
                Rscript.exe iland/scripts/process_microclimate_grids_1ha.R 1 "iland/output/simulation_experiment_${climate}/"
                Rscript.exe iland/scripts/process_microclimate_grids_1ha.R 11 "iland/output/simulation_experiment_${climate}/"

                # move monthly summaries to rep folder
                mv iland/output/simulation_experiment_${climate}/*.csv iland/output/simulation_experiment_${climate}/rep_${rep}

                # delete raw outputs
                rm -r iland/output/simulation_experiment_${climate}/year_1/
                rm -r iland/output/simulation_experiment_${climate}/year_11/
            fi

            # pre-process outputs for analyses
            Rscript.exe scripts/step05a_iland_preprocess_outputs_simulation_experiment.R $climate $rep

        fi
    done
done
