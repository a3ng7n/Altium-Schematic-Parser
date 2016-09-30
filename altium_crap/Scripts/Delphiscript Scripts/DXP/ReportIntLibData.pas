{..............................................................................}
{ Summary Demo the use of Integrated Library Manager and Model Type Manager    }
{         interfaces to extract data associated with each interface            }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function BooleanToString (Value : LongBool) : String;
Begin
    Result := 'True';

    If Value = True Then Result := 'True'
                    Else Result := 'False';
End;
{..............................................................................}

{..............................................................................}
Procedure ReportIntegratedLibraryManager;
Var
    I,J,K          : Integer;

    IntMan         : IIntegratedLibraryManager;
    FileName       : TDynamicString;
    ReportDocument : IServerDocument;

    doc            : IDocument;

    Part           : IPart;
    Imp            : IComponentImplementation;

    Param          : IParameter;
    CompLoc        : WideString;
    DataLoc        : WideString;
    TopLevelLoc    : WideString;
    MyDummy        : WideString;
    a              : WideString;

    WS             : IWorkspace;
    Prj            : IProject;
    IntLibReport   : TStringList;
    FilePath       : WideString;
Begin
    WS  := GetWorkspace;
    If WS = Nil Then Exit;

    Prj := WS.DM_FocusedProject;
    If Prj = Nil Then Exit;

    // Compile the project to fetch the connectivity
    // information for the design.
    Prj.DM_Compile;


    // Get current schematic document.
    Doc := WS.DM_FocusedDocument;
    If Doc.DM_DocumentKind <> 'SCH' Then
    Begin
        ShowWarning('This is not a schematic document');
        Exit;
    End;

    If Doc.DM_PartCount = 0 Then
    Begin
        ShowWarning('This schematic document has no schematic components(Parts)' + #13 +
                     'Thus a Integrated Library report will not be generated');

        Exit;
    End;

    IntMan := IntegratedLibraryManager;
    If IntMan = Nil Then Exit;

    IntLibReport := TStringList.Create;

    //Using the integrated library manager ...
    IntLibReport.Add('Integrated Library Interface information:');
    IntLibReport.Add('=========================================');
    IntLibReport.Add('');

    For i := 0 to Doc.DM_PartCount -1 do
    Begin
        Part := Doc.DM_Parts(i);
        MyDummy := Part.DM_LogicalDesignator;
        // CompLoc will be a full path to the sch library. This could be an internal vfs path if the component
        // is in an integrated library. The Dummy variable will be the int lib in that case. Dummy will be the top level library
        // if you like.
        CompLoc := IntMan.GetComponentLocation(Part.DM_SourceLibraryName, Part.DM_LibraryReference, TopLevelLoc);
        IntLibReport.Add(' Designator:         ' + Part.DM_LogicalDesignator);
        IntLibReport.Add(' Lib Reference:      ' + Part.DM_LibraryReference);
        IntLibReport.Add(' Component Location: ' + CompLoc);
        IntLibReport.Add(' Top Level Location: ' + TopLevelLoc);

        For j := 0 to Part.DM_ImplementationCount - 1 do
        Begin
            // Implementations for the part
            Imp := Part.DM_Implementations(j);
            If Imp <> Nil Then
            Begin
                a := Imp.DM_ModelName;
                IntLibReport.Add('      ' + 'Model Name : ' + a);
                IntLibReport.Add('      ' + 'Description: ' + Imp.DM_Description);
                IntLibReport.Add('      ' + 'Type : '       + Imp.DM_ModelType);
                IntLibReport.Add('');

                // parameter count
                If Imp.DM_ParameterCount > 0 Then
                Begin
                   For k := 0 to Imp.DM_ParameterCount - 1 do
                   Begin
                       Param := Imp.DM_Parameters(k);
                       IntLibReport.Add('        ' + 'Parameter Name: '        + Param.DM_Name);
                       IntLibReport.Add('        ' + 'Parameter Object Kind: ' + Param.DM_ObjectKindString);
                       IntLibReport.Add('        ' + 'Parameter Value: '       + Param.DM_Value);
                       IntLibReport.Add('');
                   End;
                End
                Else
                    IntLibReport.Add('No parameters for this implementation');


                // datafile count
                For k := 0 to Imp.DM_DatafileCount - 1 do
                Begin
                    If Imp.DM_IntegratedModel Then
                        DataLoc := IntegratedLibraryManager.GetComponentDatafileLocation(k,
                                                                                         Imp.DM_ModelName,
                                                                                         Imp.DM_ModelType,
                                                                                         Part.DM_LibraryReference,
                                                                                         CompLoc,
                                                                                         TopLevelLoc)
                    Else
                        DataLoc := IntegratedLibraryManager.FindDatafileInStandardLibs(Imp.DM_DatafileEntity(k),
                                                                                       Imp.DM_DatafileKind(k),
                                                                                       Imp.DM_DatafileLocation(k),
                                                                                       True,
                                                                                       TopLevelLoc);

                    IntLibReport.Add('        Data File Location: ' + DataLoc);
                    IntLibReport.Add('        Top Level Location: ' + TopLevelLoc);
                End;
            End;
            IntLibReport.Add('        +++++++++++++++++++++++++++++++++++');
        End;
        IntLibReport.Add(' ******************************************');
    End;
    IntLibReport.Add('');


    FilePath := ExtractFilePath(Doc.DM_FullPath);
    FileName := FilePath + '\IntLibrary_Report.Txt';
    IntLibReport.SaveToFile(FileName);

    Prj.DM_AddSourceDocument(FileName);
    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
Procedure ReportModelTypeManager;
Var
    I,J               : Integer;
    ReportDocument    : IServerDocument;

    ModelTypeMan      : IModelTypeManager;
    ModelType         : IModelType;
    ModelDatafileType : IModelDatafileType;

    WS                : IWorkspace;
    Prj               : IProject;
    Doc               : IDocument;
    ModelTypeReport   : TStringList;
    FilePath          : WideString;
    FileName          : WideString;
Begin
    WS  := GetWorkspace;
    If WS = Nil Then Exit;

    Prj := WS.DM_FocusedProject;
    If Prj = Nil Then Exit;

    // Compile the project to fetch the connectivity
    // information for the design.
    Prj.DM_Compile;

    // Get current schematic document.
    Doc := WS.DM_FocusedDocument;
    If Doc = Nil Then Exit;


    //Gets the acccess to the interfaces of the ModelTypeManager in DXP.
    ModelTypeMan := ModelTypeManager;
    If ModelTypeMan = Nil Then Exit;

    ModelTypeReport := TStringList.Create;
    ModelTypeReport.Add('Model Types information:   ');
    ModelTypeReport.Add('======================');
    ModelTypeReport.Add('');

    For i := 0 To ModelTypeMan.ModelTypeCount -1 do
    Begin
        ModelType := ModelTypeMan.ModelTypes[i];
        ModelTypeReport.Add(' Model Type Name: '         + ModelType.Name);
        ModelTypeReport.Add(' Model Type Description: '  + ModelType.Description);
        ModelTypeReport.Add(' Port Descriptor: '         + ModelType.PortDescriptor);
        ModelTypeReport.Add('');
    End;
    ModelTypeReport.Add('');

    ModelTypeReport.Add('Model Datafile Types:   ');
    ModelTypeReport.Add('=====================');
    For j := 0 To ModelTypeMan.ModelDatafileTypeCount - 1 do
    Begin
        ModelDatafileType := ModelTypeMan.ModelDatafileTypes[j];
        ModelTypeReport.Add(' Model Datafile Kind: '        + ModelDatafileType.FileKind);
        ModelTypeReport.Add(' Model Datafile Ext Filter: '  + ModelDatafileType.ExtensionFilter);
        ModelTypeReport.Add(' Model Datafile Description: ' + ModelDatafileType.Description);
        ModelTypeReport.Add(' Model Datafile Entity Type: ' + ModelDatafileType.EntityType);
        ModelTypeReport.Add('');
    End;

    FilePath := ExtractFilePath(Doc.DM_FullPath);
    FileName := FilePath + '\ModelType_Report.Txt';;
    ModelTypeReport.SaveToFile(FileName);
    Prj.DM_AddSourceDocument(FileName);

    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
