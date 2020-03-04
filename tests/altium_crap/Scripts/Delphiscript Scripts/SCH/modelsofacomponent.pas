{..............................................................................}
{ Summary Obtain models for each component and generate a report.              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure GenerateModelsReport (ModelsList : TStringList);
Var
    S              : TString;
    ReportDocument : IServerDocument;
Begin
    ModelsList.Insert(0,'Schematic Components and their models Report...');
    ModelsList.Insert(1,'===============================================');
    ModelsList.Insert(2,'');
    ModelsList.Insert(3,'');

    S := 'C:\ComponentModels.Txt';
    ModelsList.SaveToFile(S);

    ReportDocument := Client.OpenDocument('Text', S);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
Procedure ExtractModelsFromComponents;
Var
    CurrentSheet       : ISch_Document;
    Iterator           : ISch_Iterator;
    Component          : ISch_Component;
    j                  : Integer;
    ImplIterator       : ISch_Iterator;
    SchImplementation  : ISch_Implementation;
    Modelslist         : TStringList;
    ModelDataFile      : ISch_ModelDatafileLink;
Begin
    If SchServer = Nil Then Exit;
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;

    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Modelslist := TStringList.Create;

    Try
        Component := Iterator.FirstSchObject;

        While Component <> Nil Do
        Begin
            ModelsList.Add('Designator: '         + Component.Designator.Text);
            ModelsList.Add(' Library Reference: ' + Component.LibReference);
            ModelsList.Add(' Library Path: '      + Component.LibraryPath);

            ImplIterator := Component.SchIterator_Create;
            ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));

            Try
                SchImplementation := ImplIterator.FirstSchObject;
                While SchImplementation <> Nil Do
                Begin
                    ModelsList.Add(' Implementation Model details:');
                    ModelsList.Add('   ModelName: '         + SchImplementation.ModelName +
                                   ' ModelType: '           + SchImplementation.ModelType +
                                   ' Description: '         + SchImplementation.Description);
                    ModelsList.Add('   Map: ' + SchImplementation.MapAsString);

                    For j := 0 To SchImplementation.DatafileLinkCount - 1 Do
                    Begin
                        ModelDataFile := SchImplementation.DatafileLink[j];
                        If ModelDataFile <> Nil Then
                        Begin
                            ModelsList.Add(' Implemenation Data File Link Details:');
                            ModelsList.Add('   Data File Location: ' + ModelDataFile.Location +
                                           ', Entity Name: '         + ModelDataFile.EntityName +
                                           ', FileKind: '            + ModelDataFile.FileKind);
                            ModelsList.Add('');
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
End;
{..............................................................................}

{..............................................................................}
