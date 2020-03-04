{..............................................................................}
{ Summary Demo how to fetch pins of components.                                }
{                                                                              }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function OrientationToStr(ARotate : TRotationBy90) : String;
Begin
    Result := '';

    Case ARotate Of
        eRotate0   : Result := '0  ';
        eRotate90  : Result := '90 ';
        eRotate180 : Result := '180';
        eRotate270 : Result := '270';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchPinsOfComponents;
Var
    CurrentSch : ISch_Sheet;
    Iterator   : ISch_Iterator;
    PIterator  : ISch_Iterator;
    AComponent : ISch_Component;
    AnIndex    : Integer;

    ReportList : TStringList;
    Pin        : ISch_Pin;
    Document   : IServerDocument;
Begin
    // Check if schematic server exists or not.
    If SchServer = Nil Then Exit;

    // Obtain the current schematic document interface.
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    // Look for components only
    Iterator := CurrentSch.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    ReportList := TStringList.Create;
    Try
        AComponent := Iterator.FirstSchObject;
        While AComponent <> Nil Do
        Begin
            ReportList.Add(AComponent.Designator.Name + ' ' + AComponent.Designator.Text);
            ReportList.Add(' Pins');

            Try
                PIterator := AComponent.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(ePin));

                Pin := PIterator.FirstSchObject;
                While Pin <> Nil Do
                Begin
                    ReportList.Add('  Name: ' + Pin.Name + ' Designator: ' + Pin.Designator + ' Orientation: ' +  OrientationToStr(Pin.Designator));
                    Pin := PIterator.NextSchObject;
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

    ReportList.SaveToFile('C:\PinReport.Txt');
    ReportList.Free;

    // Display the report for all components found and their associated pins.
    Document := Client.OpenDocument('Text','C:\PinReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
End.
