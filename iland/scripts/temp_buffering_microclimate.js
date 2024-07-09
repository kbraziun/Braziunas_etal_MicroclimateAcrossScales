/**
 * microclimate outputs
 */

function onYearEnd()
{

	// Monthly temperature buffering
	Globals.microclimateGrid('MaxTBuffer',1).save('output/temp_buffering_microclimate/raw/MaxTBuffer_01.asc');
	Globals.microclimateGrid('MaxTBuffer',2).save('output/temp_buffering_microclimate/raw/MaxTBuffer_02.asc');
	Globals.microclimateGrid('MaxTBuffer',3).save('output/temp_buffering_microclimate/raw/MaxTBuffer_03.asc');
	Globals.microclimateGrid('MaxTBuffer',4).save('output/temp_buffering_microclimate/raw/MaxTBuffer_04.asc');
	Globals.microclimateGrid('MaxTBuffer',5).save('output/temp_buffering_microclimate/raw/MaxTBuffer_05.asc');
	Globals.microclimateGrid('MaxTBuffer',6).save('output/temp_buffering_microclimate/raw/MaxTBuffer_06.asc');
	Globals.microclimateGrid('MaxTBuffer',7).save('output/temp_buffering_microclimate/raw/MaxTBuffer_07.asc');
	Globals.microclimateGrid('MaxTBuffer',8).save('output/temp_buffering_microclimate/raw/MaxTBuffer_08.asc');
	Globals.microclimateGrid('MaxTBuffer',9).save('output/temp_buffering_microclimate/raw/MaxTBuffer_09.asc');
	Globals.microclimateGrid('MaxTBuffer',10).save('output/temp_buffering_microclimate/raw/MaxTBuffer_10.asc');
	Globals.microclimateGrid('MaxTBuffer',11).save('output/temp_buffering_microclimate/raw/MaxTBuffer_11.asc');
	Globals.microclimateGrid('MaxTBuffer',12).save('output/temp_buffering_microclimate/raw/MaxTBuffer_12.asc');

    Globals.microclimateGrid('MinTBuffer',1).save('output/temp_buffering_microclimate/raw/MinTBuffer_01.asc');
    Globals.microclimateGrid('MinTBuffer',2).save('output/temp_buffering_microclimate/raw/MinTBuffer_02.asc');
    Globals.microclimateGrid('MinTBuffer',3).save('output/temp_buffering_microclimate/raw/MinTBuffer_03.asc');
    Globals.microclimateGrid('MinTBuffer',4).save('output/temp_buffering_microclimate/raw/MinTBuffer_04.asc');
    Globals.microclimateGrid('MinTBuffer',5).save('output/temp_buffering_microclimate/raw/MinTBuffer_05.asc');
    Globals.microclimateGrid('MinTBuffer',6).save('output/temp_buffering_microclimate/raw/MinTBuffer_06.asc');
    Globals.microclimateGrid('MinTBuffer',7).save('output/temp_buffering_microclimate/raw/MinTBuffer_07.asc');
    Globals.microclimateGrid('MinTBuffer',8).save('output/temp_buffering_microclimate/raw/MinTBuffer_08.asc');
    Globals.microclimateGrid('MinTBuffer',9).save('output/temp_buffering_microclimate/raw/MinTBuffer_09.asc');
    Globals.microclimateGrid('MinTBuffer',10).save('output/temp_buffering_microclimate/raw/MinTBuffer_10.asc');
    Globals.microclimateGrid('MinTBuffer',11).save('output/temp_buffering_microclimate/raw/MinTBuffer_11.asc');
    Globals.microclimateGrid('MinTBuffer',12).save('output/temp_buffering_microclimate/raw/MinTBuffer_12.asc');

}


