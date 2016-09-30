{..............................................................................}
{ Summary Obtain Simulation models and relevant parameters for each component  }
{         and generate a report.                                               }
{                                                                              }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function BooleanToStr(AValue : Boolean) : String;
Begin
    Result := 'False';

    If AValue = True Then Result := 'True';
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateModelsReport (ModelsList : TStringList);
Var
    S              : TString;
    ReportDocument : IServerDocument;
Begin
    ModelsList.Insert(0,'Schematic Components and their Simulation models Report');
    ModelsList.Insert(1,'_______________________________________________________');
    ModelsList.Insert(2,'');
    ModelsList.Insert(3,'');

    ModelsList.Insert(4,'Schematic Sheet FileName: ');
    ModelsList.Insert(5, SchServer.GetCurrentSchDocument.OwnerDocument.DocumentName);

    S := 'C:\ComponentSimModels.Txt';
    ModelsList.SaveToFile(S);

    ReportDocument := Client.OpenDocument('Text', S);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
(*
TLibIdentifierKind = (eLibIdentifierKind_Any,
                      eLibIdentifierKind_NameNoType,
                      eLibIdentifierKind_NameWithType,
                      eLibIdentifierKind_FullPath);
*)
{..............................................................................}

{..............................................................................}
Function GetParameterValueByName (ASchContainer : ISch_BasicContainer; AName : TDynamicString) : String;
Var
    Param    : ISch_Parameter;
    Iterator : ISch_Iterator;
Begin
    Result := '';
    Iterator := ASchContainer.SchIterator_Create;
    Iterator.SetState_IterationDepth(eIterateFirstLevel);
    Iterator.AddFilter_ObjectSet(MkSet(eParameter));
    Param := Iterator.FirstSchObject;
    Try
        While Param <> Nil Do
        Begin
            If StringsEqual(Param.Name, AName) Then
            Begin
                Result := Param.Text;
                Break;
            End;
            Param := Iterator.NextSchObject;
        End;
    Finally
        ASchContainer.SchIterator_Destroy (Iterator);
    End;
End;
{..............................................................................}

{..............................................................................}
Function RequiresModelFile (ASimImplementation : ISch_Implementation) : Boolean;
Var
    Template : String;
Begin
    Result := False;
    If ASimImplementation = Nil Then Exit;
    Template := GetParameterValueByName(ASimImplementation, 'Netlist');

    If (Pos('@MODEL', Uppercase(Template)) <= 0) And
       (Pos('&MODEL', Uppercase(Template)) <= 0)  Then Exit;

    Result := True;
End;
{..............................................................................}

{..............................................................................}
Procedure ExtractSimModelsFromComponents;
Var
    CurrentSheet       : ISch_Document;
    Iterator           : ISch_Iterator;
    PIterator          : ISch_Iterator;
    Component          : ISch_Component;
    Parameter          : ISch_Parameter;
    j                  : Integer;

    ImplIterator       : ISch_Iterator;
    SchImplementation  : ISch_Implementation;
    Modelslist         : TStringList;
    ModelDataFile      : ISch_ModelDataFileLink;

    IntLibManager      : IntegratedLibraryManager;
    ModelLibraryPath   : String;
    AModelName         : String;
    SpicePrefix        : String;
    ModelType          : String;

    Path, ComponentPath, ModelPath : String;

    SourceLibraryPath  : String;
    LibraryPath        : String;

    SourceDatafilePath : String;
    Datafilepath       : String;

Begin
    If SchServer = Nil Then Exit;
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;

    If IntegratedLibraryManager = Nil Then Exit;
    // temporarily install simulation ready component libraries
    IntegratedLibraryManager.InstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Math Function.IntLib');
    IntegratedLibraryManager.InstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Pspice Functions.IntLib');
    IntegratedLibraryManager.InstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Sources.IntLib');
    IntegratedLibraryManager.InstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Special Function.IntLib');
    IntegratedLibraryManager.InstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Transmission Line.IntLib');

    // iterate the sheet for components
    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Modelslist := TStringList.Create;
    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            ModelsList.Add('_______________________________________________________');
            ModelsList.Add('_______________________________________________________');
            ModelsList.Add('Component Designator: '            + Component.Designator.Text);
            ModelsList.Add(' Component''s Library Reference: ' + Component.LibReference);
            ModelsList.Add(' Component''s Library Path: '      + Component.LibraryPath);

            ImplIterator := Component.SchIterator_Create;
            ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));
            Try
                SchImplementation := ImplIterator.FirstSchObject;
                While SchImplementation <> Nil Do
                Begin
                    If SchImplementation.ModelType = 'SIM' Then
                    Begin
                        // Look for components' parameters
                        ModelsList.Add('');
                        ModelsList.Add(' Parameters for this component: ' + Component.Designator.Text);
                        Try
                            PIterator := Component.SchIterator_Create;
                            PIterator.AddFilter_ObjectSet(MkSet(eParameter));
                            Parameter := PIterator.FirstSchObject;
                            While Parameter <> Nil Do
                            Begin
                                ModelsList.Add(' Parameter.Name: ' + Parameter.Name);
                                ModelsList.Add(' Parameter.Text: ' + Parameter.Text);
                                ModelsList.Add('');
                                Parameter := PIterator.NextSchObject;
                            End;
                        Finally
                            Component.SchIterator_Destroy(PIterator);
                        End;

                        ModelsList.Add('');
                        ModelsList.Add(' Implementation Model details:');

                        ModelsList.Add('   ModelName: '         + SchImplementation.ModelName +
                                       ' ModelType: '           + SchImplementation.ModelType +
                                       ' Description: '         + SchImplementation.Description);

                        ModelsList.Add('   This Current Implementation:'         + BooleanToStr(SchImplementation.IsCurrent));
                        ModelsList.Add('   Component is in Integrated Library: ' + BooleanToStr(SchImplementation.UseComponentLibrary));
                        ModelsList.Add('   DatafileLinkCount: '                  + IntToStr    (SchImplementation.DatafileLinkCount));
                        ModelsList.Add('   DatalinksLocked: '                    + BooleanToStr(SchImplementation.DatalinksLocked));
                        ModelsList.Add('   DatabaseModel: '                      + BooleanToStr(SchImplementation.DatabaseModel));
                        ModelsList.Add('   IntegratedModel: '                    + BooleanToStr(SchImplementation.IntegratedModel));


                        // Pins of component and Ports of simulation model mapping information
                        ModelsList.Add('');
                        ModelsList.Add(' Map Component(Pins) and Simulation Model(Ports): ' + SchImplementation.MapAsString);

                        // reset path strings to blank
                        ComponentPath      := '';
                        ModelPath          := '';
                        SourceLibraryPath  := '';
                        LibraryPath        := '';
                        SourceDatafilePath := '';
                        Datafilepath       := '';
                        Path               := '';


                        If Not RequiresModelFile(SchImplementation) Then
                        Begin
                            ModelsList.Add(' No external model file required for this Implementation');
                        End
                        Else
                        Begin
                            // using the RT_IntegratedLibrary unit to obtain information for the component, its library path,
                            // the model and its model library path. 3 cases.

                            If SchImplementation.UseComponentLibrary Then
                            Begin
                                ComponentPath  := IntegratedLibraryManager.FindComponentLibraryPath(Component.LibIdentifierKind, Component.LibraryIdentifier, Component.DesignItemID);
                                ModelPath      := IntegratedLibraryManager.FindModelLibraryPath    (Component.LibIdentifierKind, Component.LibraryIdentifier, Component.DesignItemID, SchImplementation.ModelName, SchImplementation.ModelType);

                                ModelsList.Add(' The Component is in Integrated Library');
                                ModelsList.Add('  Component path: ' + ComponentPath);
                                ModelsList.Add('  Model path: '     + ModelPath);
                            End
                            Else
                            Begin
                                If SchImplementation.DatafileLinkCount = 0 Then
                                Begin
                                    ModelsList.Add(' Model Location = Any : DataFileLinks.Count = 0');
                                    SpicePrefix := GetParameterValueByName(SchImplementation, 'Spice Prefix');
                                    If StringsEqual(SpicePrefix, 'X')
                                      Then ModelType := 'CKT'
                                      Else ModelType := 'MDL';
                                    SourceLibraryPath := IntegratedLibraryManager.FindDatafileEntitySourceLibraryPath(0{eLibIdentifierKind_Any}, '', SchImplementation.ModelName, ModelType);
                                    LibraryPath       := IntegratedLibraryManager.FindDatafileEntityLibraryPath      (0{eLibIdentifierKind_Any}, '', SchImplementation.ModelName, ModelType);

                                    ModelsList.Add(' SourceLibraryPath: ' +  SourceLibraryPath);
                                    ModelsList.Add(' LibraryPath: '       +  LibraryPath      );
                                End;

                                If SchImplementation.DatafileLinkCount > 0 Then
                                Begin
                                    // Assumption: use the first data file link for the simulation model since we
                                    // normally only use one sim model per component.
                                    ModelDataFile := SchImplementation.DatafileLink[0];

                                    SourceLibraryPath := IntegratedLibraryManager.FindDatafileEntitySourceLibraryPath(ModelDataFile.LibIdentifierKind, ModelDataFile.LibraryIdentifier, ModelDataFile.EntityName, ModelDataFile.FileKind);
                                    LibraryPath       := IntegratedLibraryManager.FindDatafileEntityLibraryPath      (ModelDataFile.LibIdentifierKind, ModelDataFile.LibraryIdentifier, ModelDataFile.EntityName, ModelDataFile.FileKind);

                                    SourceDatafilePath := IntegratedLibrarymanager.FindDatafileEntitySourceDatafilePath(ModelDataFile.LibIdentifierKind, ModelDataFile.LibraryIdentifier, ModelDataFile.EntityName, ModelDataFile.FileKind, True);
                                    Datafilepath       := IntegratedLibrarymanager.FindDatafileEntityDatafilePath      (ModelDataFile.LibIdentifierKind, ModelDataFile.LibraryIdentifier, ModeldataFile.EntityName, ModelDataFile.FileKind, True);

                                    ModelsList.Add(' Model : DatafilelinkCount > 0');
                                    ModelsList.Add('  Source Library path: '  + SourceLibraryPath);
                                    ModelsList.Add('  Library path: '         + LibraryPath);
                                    ModelsList.Add('  Source datafile path: ' + SourceDatafilePath);
                                    ModelsList.Add('  Datafile path: '        + DataFilePath);
                                End;
                            End;
                        End;
                    End;
                    SchImplementation := ImplIterator.NextSchObject;
                End;
            Finally
                Component.SchIterator_Destroy(ImplIterator);
            End;

            ModelsList.Add('');
            ModelsList.Add('');
            Component := Iterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(Iterator);
    End;

    GenerateModelsReport(ModelsList);
    Modelslist.Free;

    IntegratedLibraryManager.UnInstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Math Function.IntLib');
    IntegratedLibraryManager.UnInstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Pspice Functions.IntLib');
    IntegratedLibraryManager.UnInstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Sources.IntLib');
    IntegratedLibraryManager.UnInstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Special Function.IntLib');
    IntegratedLibraryManager.UnInstallLibrary('C:\Program Files\Altium Designer Summer 08\Library\Simulation\Simulation Transmission Line.IntLib');
End;
{..............................................................................}

{..............................................................................}

//Notes, a simulation ready component has simulation specific parameters
// model files should be in the same design project as the simulation is in.

//Simulation Math Function.IntLib
//Simulation Pspice Functions.IntLib
//Simulation Sources.IntLib
//Simulation Special Function.IntLib
//Simulation Transmission Line.IntLib
