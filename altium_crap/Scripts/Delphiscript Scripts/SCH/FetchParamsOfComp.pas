{..............................................................................}
{ Summary Demo how to fetch parameters of a component.                         }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure FetchParameters;
Var
    CurrentSch : ISch_Sheet;
    Iterator   : ISch_Iterator;
    PIterator  : ISch_Iterator;
    AComponent : ISch_Component;
    AnIndex    : Integer;
    i          : integer;
    S          : TDynamicString;

    ReportList : TStringList;
    Parameter  : ISch_Parameter;
    Document   : IServerDocument;
Begin
    // CHeck if schematic server exists or not.
    If SchServer = Nil Then Exit;

    // Obtain the current schematic document interface.
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;


    // Look for components only
    Iterator := CurrentSch.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    S := '';
    ReportList := TStringList.Create;
    Try
        AComponent := Iterator.FirstSchObject;
        While AComponent <> Nil Do
        Begin
            ReportList.Add(AComponent.Designator.Name + ' ' + AComponent.Designator.Text);
            ReportList.Add(' Parameters');

            Try
                PIterator := AComponent.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                Parameter := PIterator.FirstSchObject;
                While Parameter <> Nil Do
                Begin
                    ReportList.Add('  ' + Parameter.Name + ' ' + Parameter.Text);
                    Parameter := PIterator.NextSchObject;
                End;
            Finally
                AComponent.SchIterator_Destroy(PIterator);
            End;

            ReportList.Add('');
            AComponent := Iterator.NextSchObject;
        End;
    Finally
        CurrentSch.SchIterator_Destroy(Iterator);
    End;

    ReportList.SaveToFile('C:\ParamReport.Txt');
    ReportList.Free;

    // Display the report containing parameters for each component found.
    Document := Client.OpenDocument('Text','C:\ParamReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
End.
