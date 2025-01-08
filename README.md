# readme for Braziunas et al. Microclimate across scales

## purpose

This readme gives an overview of directory structure, files, and steps for recreating
outputs and analyses associated with the following manuscript (full dataset is included in data deposit, code only is available on GitHub https://github.com/kbraziun/Braziunas_etal_MicroclimateAcrossScales): 

**Manuscript citation:** Braziunas, K.H., W. Rammer, P. De Frenne, J. Díaz-Calafat, P.-O. Hedwall, C. Senf, D. Thom, F. Zellweger, and R. Seidl. Submitted. Microclimate temperature effects propagate across scales in forest ecosystems. Landscape Ecology.

**Dataset citation:** To be added in GitHub version. Data is not uploaded to GitHub due to size constraints.

## platforms

- Operating systems and software used for development and implementation
  - OS: Windows 10
  - iLand version: 1.1.3 SVN-Revision 1535 - build date: Nov 20 2023
  - R version: 4.3.2

## directory overview

Directory structure and files:

- `analysis/`: Results from data analysis.
- `data/`: Raw data used in this study. Data deposit only includes iLand species parameter, Berchtesgaden forest type, and independent comparison microclimate datasets. Other raw data is not included in this data deposit because it is publicly available, previously published, or unpublished but provided specifically for this study. Note that all cleaned and lightly processed data used in this study is provided in the `processed_data/` folder.
- `iland/`: All inputs, project files, initialization data, javascript files, and additional R scripts used to replicate the iLand simulation experiments and generate outputs for this study. Note that if new simulations are run, results may be slightly different due to stochastic processes included in iLand.
- `iland_exe/`: iLand executable used in this study.
- `processed_data/`: Any data altered from its raw format. Includes data derived from field and temperature measurements in forest microclimate plots, along with associated macroclimate data (`microclimate_data_prep/`); processed iLand model outputs for the summer mean temperature offset comparison, simulation experiment, and sensitivity analysis; and a stand ID raster used to match up iLand outputs with spatial data (`iland_simulation_prep/`).
- `scripts/`: R and bash scripts for reproducing results. See below for more detail.

## scripts

Scripts are named in order (`step01_`, etc.). Some scripts rely on external or intermediate inputs not included in this deposit.

Script descriptions:

- `step01_prep_microclimate_macroclimate_plot_data.R`: Prepares field data on microclimate temperature, forest inventory, and macroclimate temperature (in situ or weather station data) from previous studies for fitting an empirical equation to predict subcanopy temperatures from previously identified predictor variables. Includes screening of microclimate and macroclimate temperature data for snow days, outliers, and erroneous time periods; preparation of topographic, forest, and other predictor variables; and consolidation to monthly temporal resolution. Outputs in `processed_data/microclimate_data_prep/`. Most raw data is not included in data deposit.
- `step02_empirical_microclimate_equation.R`: Fits empirical model to predict maximum and minimum microclimate temperature buffering from selected predictor variables. Includes predictor selection, evaluation of model assumptions, fit of mixed effects multiple linear regression models, and evaluations of model goodness of fit. Final models are implemented in the microclimate module in iLand to predict minimum and maximum daily microclimate temperature at 10 m spatial resolution.
- `step03_setup_simulation_experiments.R`: Prepares files for iLand simulations, including identifying an average historical climate year for evaluating historical temperature buffering capacity in Berchtesgaden forests based on the newly fit empirical models, creating wind disturbance scenarios, and creating a raster for matching up iLand outputs with spatial data. Outputs in `iland/` and `processed data/iland_simulation_prep/` folders.
- `step04a_run_iland_local_temperature_buffering.sh`: Bash script to run iLand simulation for a selected historical climate year, using the `iland/temp_buffering_microclimate.xml` iLand project file. Must be run from home directory. Also automatically runs R script to pre-process outputs. Can choose between creating microclimate outputs at 1ha or 10m spatial resolution. Outputs will be saved in `iland/output/temp_buffering_microclimate/`.

Run this script in bash for year 1988 with 10m resolution output:

```
bash scripts/step04a_run_iland_local_temperature_buffering.sh 1988 "10m"
```
- `step04b_run_iland_local_simulation_experiment.sh`: Bash script to rerun iLand simulation experiment replicates with model processes driven by either macroclimate (`iland/simulation_experiment_macroclimate.xml` project file) or microclimate (`iland/simulation_experiment_microclimate.xml` project file) temperature for 1000 simulation years. Inputs for up to 10 simulation replicates are in `iland/main_file.txt`. Must be run from home directory. Also automatically runs R scripts to pre-process outputs. Outputs will be saved in `processed_data/simulation_experiment/`.

Run this script in bash for full simulation experiment:

```
bash scripts/step04b_run_iland_local_simulation_experiment.sh "macroclimate" 10
bash scripts/step04b_run_iland_local_simulation_experiment.sh "microclimate" 10
```
- `step04c_run_iland_local_sensitivity_analysis.sh`: Bash script to rerun iLand sensitivity analysis replicates using the `iland/sensitivity_analysis_microclimate.xml` project file for 30 simulation years. Inputs for turning different processes "on" or "off" and in `iland/sensitivity_analysis.csv`. Must be run from home directory. Also automatically runs R script to pre-process outputs. Outputs will be saved in `processed_data/sensitivity_analysis/`.

Run this script in bash for full sensitivity analysis:

```
bash scripts/step04c_run_iland_local_sensitivity_analysis.sh 10
```
- `step05a_iland_preprocess_outputs_simulation_experiment.R`: Pre-processes iLand outputs from simulation experiment runs, automatically called by earlier bash script `step04b_run_iland_local_simulation_experiment.sh`. Pre-processing includes reading in and subsetting iLand outputs in preparation for local, meso, and landscape scale analyses. Local scale: subsets outputs for selected variables in dense forested stands. Meso scale: Identifies disturbance patches and subsets outputs for pre- and post-disturbance selected variables. Also subsets outputs for selected tree species regeneration density along their thermal regeneration niche. Landscape scale: subsets outputs for selected variables across the entire landscape. Outputs are in `processed_data/simulation_experiment/`.
- `step05b_iland_preprocess_outputs_sensitivity_analysis.R`: Pre-processes iLand outputs from sensitivity analysis runs, automatically called by earlier bash script `step04c_run_iland_local_sensitivity_analysis.sh`. Pre-processing includes reading in and subsetting selected landscape-scale iLand outputs in preparation for sensitivity analysis. Outputs are in `processed_data/sensitivity_analysis/`.
- `step06_analysis.R`: Reruns all analyses for study, using previously generated pre-processed iLand outputs in `processed_data/` folders. Includes evaluation of historical temperature buffering by forest type and season, as well as comparison with independent dataset; local, meso, and landscape scale analyses; and sensitivity analysis. Outputs in corresponding folders in `analysis/`.
