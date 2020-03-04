{..............................................................................}
{ Summary  Generate a PDF from a OutJob File with                              }
{          Assembly Drawings and Schematic Prints                              }
{ Copyright (c) 2006 by Altium Limited                                         }  
{..............................................................................}

{..............................................................................}
Procedure PublishToPDF_AssemblyDrawingsAndSchematicPrints;
Begin
    ResetParameters;
    AddStringParameter ('Action'               ,'PublishToPDF');
    AddStringParameter ('ObjectKind'           ,'OutputSingle');
    AddStringParameter ('SelectedName1'        ,'Assembly Drawings');
    AddStringParameter ('SelectedName2'        ,'Schematic Prints');
    AddStringParameter ('DisableDialog'        ,'True');
    AddStringParameter ('OutputFilePath' ,'c:\AssemblyAndSchematic.pdf');

    AddStringParameter ('OpenOutput'           ,'True');
    AddIntegerParameter('ZoomLevel'            ,50);
    AddStringParameter ('FitSCHPrintSizeToDoc' ,'True');
    AddStringParameter ('FitPCBPrintSizeToDoc' ,'True');
    AddStringParameter ('GenerateNetsInfo'     ,'True');
    AddStringParameter ('MarkPins'             ,'True');
    AddStringParameter ('MarkNetLabels'        ,'True');
    AddStringParameter ('MarkPorts'            ,'True');
    RunProcess('WorkspaceManager:Print');
End;
{..............................................................................}

{..............................................................................}
                                                                                       
