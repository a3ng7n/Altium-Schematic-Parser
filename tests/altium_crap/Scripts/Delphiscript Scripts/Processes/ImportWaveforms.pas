{..............................................................................}
{ Summary ImportWaveforms - Demonstrate the use of the ImportWaveforms         }
{                           process to import data from a csv file             }
{ Copyright (c) 2009 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure ImportWaveformsFromFile_Real;
Begin
    ResetParameters;
    AddStringParameter('DocumentFilename', 'C:\ImportedWaveforms.sdf'); //filename of sdf file to import data into
    AddStringParameter('FileName', 'C:\Transient Analysis.csv');        //file containing data to be imported in same format as data exported
    AddStringParameter('ChartName', 'ImportedRealData');  //name of chart to import data into
    AddStringParameter('ListSeparator', ',');             //the separator char used in the data file
    AddStringParameter('ChartType', 'XY-Scatter');        //chart type: 'Table' or 'XY-Scatter'
    AddStringParameter('DataType', 'Real');               //waveform data type: real, complex or table
    AddStringParameter('PlotWaves0','input');             //comma delimited list of wave names to add to Plot 0
    AddStringParameter('PlotWaves1','output');            //comma delimited list of wave names to add to Plot 1
    AddStringParameter('OverwriteWaves', 'True');         //set to true to silently overwrite existing waves of same name
    AddStringParameter('XScaleMode', 'Linear');           //X-scale mode: Log10, Log2 or Linear
    AddStringParameter('XUnits', 's');                    //X-Axis unit string
    AddStringParameter('XAxisLabel', 'Time');             //X-Axis label string
    //the following two parameters are useful to reduce memory consumed importing large data files
    AddStringParameter('OptimiseWaves', 'True');          //performs optimisation of imported waves to reduce memory usage (e.g. drops redundant points on straight line).
    AddStringParameter('ImportWaves', 'input,output');    //comma delimited list of waves to import.  If blank, or not given all waveforms will be imported.
    RunProcess('SimView:ImportWaveforms');
End;
{..............................................................................}

{..............................................................................}
Procedure ImportWaveformsFromFile_Complex;
Begin
    ResetParameters;
    AddStringParameter('DocumentFilename', 'C:\ImportedWaveforms.sdf'); //filename of sdf file to import data into
    AddStringParameter('FileName', 'C:\AC Analysis.csv');               //file containing data to be imported in same format as data exported
    AddStringParameter('ChartName', 'ImportedComplexData'); //name of chart to import data into
    AddStringParameter('ListSeparator', ',');               //the separator char used in the data file
    AddStringParameter('ChartType', 'XY-Scatter');          //chart type: 'Table' or 'XY-Scatter'
    AddStringParameter('DataType', 'Complex');              //waveform data type: real or complex
    AddStringParameter('PlotWaves0','input,output');        //comma delimited list of wave names to add to Plot 0
    AddStringParameter('OverwriteWaves', 'True');           //set to true to silently overwrite existing waves of same name
    AddStringParameter('XScaleMode', 'Log10');              //X-scale mode: Log10, Log2 or Linear
    AddStringParameter('XUnits', 'Hz');                     //X-Axis unit string
    AddStringParameter('XAxisLabel', 'Frequency');          //X-Axis label string
    RunProcess('SimView:ImportWaveforms');
End;
{..............................................................................}

{..............................................................................}
Procedure ImportWaveformsFromFile_Table;
Begin
    ResetParameters;
    AddStringParameter('DocumentFilename', 'C:\ImportedWaveforms.sdf'); //filename of sdf file to import data into
    AddStringParameter('FileName', 'C:\Operating Point.CSV');           //file containing data to be imported in same format as data exported
    AddStringParameter('ChartName', 'ImportedTableData'); //name of chart to import data into
    AddStringParameter('ListSeparator', ',');             //the separator char used in the data file
    AddStringParameter('ChartType', 'Table');             //chart type: 'Table' or 'XY-Scatter'
    AddStringParameter('DataType', 'Real');               //waveform data type: real or complex
    AddStringParameter('PlotWaves0','input');             //comma delimited list of wave names to add to Plot 0
    AddStringParameter('PlotWaves1','output');            //comma delimited list of wave names to add to Plot 1
    AddStringParameter('OverwriteWaves', 'True');         //set to true to silently overwrite existing waves of same name
    RunProcess('SimView:ImportWaveforms');
End;
{..............................................................................}

