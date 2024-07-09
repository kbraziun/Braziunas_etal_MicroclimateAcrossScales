/**
 * microclimate outputs
 */

function onYearEnd()
{
	// microclimate buffering capacity for year 1 and 11
	if (Globals.year==1 || Globals.year==11) {

		// Outputs for further analyses
		// Monthly temperature buffering
		Globals.microclimateGrid('MaxTBuffer',1).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_01.asc');
		Globals.microclimateGrid('MaxTBuffer',2).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_02.asc');
		Globals.microclimateGrid('MaxTBuffer',3).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_03.asc');
		Globals.microclimateGrid('MaxTBuffer',4).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_04.asc');
		Globals.microclimateGrid('MaxTBuffer',5).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_05.asc');
		Globals.microclimateGrid('MaxTBuffer',6).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_06.asc');
		Globals.microclimateGrid('MaxTBuffer',7).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_07.asc');
		Globals.microclimateGrid('MaxTBuffer',8).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_08.asc');
		Globals.microclimateGrid('MaxTBuffer',9).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_09.asc');
		Globals.microclimateGrid('MaxTBuffer',10).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_10.asc');
		Globals.microclimateGrid('MaxTBuffer',11).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_11.asc');
		Globals.microclimateGrid('MaxTBuffer',12).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MaxTBuffer_12.asc');

		Globals.microclimateGrid('MinTBuffer',1).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_01.asc');
		Globals.microclimateGrid('MinTBuffer',2).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_02.asc');
		Globals.microclimateGrid('MinTBuffer',3).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_03.asc');
		Globals.microclimateGrid('MinTBuffer',4).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_04.asc');
		Globals.microclimateGrid('MinTBuffer',5).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_05.asc');
		Globals.microclimateGrid('MinTBuffer',6).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_06.asc');
		Globals.microclimateGrid('MinTBuffer',7).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_07.asc');
		Globals.microclimateGrid('MinTBuffer',8).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_08.asc');
		Globals.microclimateGrid('MinTBuffer',9).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_09.asc');
		Globals.microclimateGrid('MinTBuffer',10).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_10.asc');
		Globals.microclimateGrid('MinTBuffer',11).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_11.asc');
		Globals.microclimateGrid('MinTBuffer',12).save('output/simulation_experiment_microclimate/year_' + Globals.year + '/MinTBuffer_12.asc');

	}

	// cumulative disturbance events at end of year 10
	if (Globals.year==10) {
		BarkBeetle.grid('nEvents').save('output/simulation_experiment_microclimate/beetle_events_' + Globals.year + '.asc');	
		Wind.grid('nEvents').save('output/simulation_experiment_microclimate/wind_events_' + Globals.year + '.asc');
	}

	// other detailed spatial outputs for first 30 years
	if (Globals.year < 31) {
		BarkBeetle.gridToFile('generations','output/simulation_experiment_microclimate/beetle_generations_' + Globals.year + '.asc');	
	}

}


