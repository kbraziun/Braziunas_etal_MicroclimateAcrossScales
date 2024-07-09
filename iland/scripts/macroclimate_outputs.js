/**
 * macroclimate outputs
 */

function onYearEnd()
{
	// cumulative disturbance events at end of year 10
	if (Globals.year==10) {
		BarkBeetle.grid('nEvents').save('output/simulation_experiment_macroclimate/beetle_events_' + Globals.year + '.asc');	
		Wind.grid('nEvents').save('output/simulation_experiment_macroclimate/wind_events_' + Globals.year + '.asc');
	}

	// other detailed spatial outputs for first 30 years
	if (Globals.year < 31) {
		BarkBeetle.gridToFile('generations','output/simulation_experiment_macroclimate/beetle_generations_' + Globals.year + '.asc');	
	}

}


