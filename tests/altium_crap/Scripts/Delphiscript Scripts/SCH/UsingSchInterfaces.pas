{..............................................................................}
{ Summary Demonstrate the use of several Schematic Object Interfaces           }
{         to fetch details of schematic components on a current schematic.     }
{                                                                              }
{  Key Schematic Object Interfaces;                                            }
{         ISch_Implementation                                                  }
{         ISch_Component                                                       }
{         ISch_Iterator                                                        }
{                                                                              }
{ Copyright (c) 2007 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function BooleanToStr (AValue : Boolean) : TString;
Begin
    Result := '';
    If AValue = True Then Result := 'True'
    Else                  Result := 'False';
End;
{..............................................................................}

{..............................................................................}
Function RotationToStr (AValue : TRotationBy90) : TString;
Begin
    Result := '';
    Case AValue Of
        eRotate0   : Result := '0 Deg'  ;
        eRotate90  : Result := '90 Deg' ;
        eRotate180 : Result := '180 Deg';
        eRotate270 : Result := '270 Deg';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure GetComponentInformation;
Var
    CurrentSheet      : ISch_Document;
    Iterator          : ISch_Iterator;
    Component         : ISch_Component;
    ImplIterator      : ISch_Iterator;
    SchImplementation : ISch_Implementation;
    S                 : TDynamicString;
    ReportDocument    : IServerDocument;
    FootprintString   : TDynamicString;
    ComponentsList    : TStringList;
Begin
    If SchServer = Nil Then
    Begin
        ShowError('Please run the script on a schematic document.');
        Exit;
    End;

    CurrentSheet := SchServer.GetCurrentSchDocument;
    If (CurrentSheet = Nil) or (CurrentSheet.ObjectID = eSchLib) Then
    Begin
        ShowError('Please run the script on a schematic document.');
        Exit;
    End;

    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Componentslist := TStringList.Create;
    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            FootprintString := '';

            ImplIterator := Component.SchIterator_Create;
            ImplIterator.AddFilter_ObjectSet(MkSet(eImplementation));
            SchImplementation := ImplIterator.FirstSchObject;
            While SchImplementation <> Nil Do
            Begin
                If StringsEqual(SchImplementation.ModelType, 'PCBLIB') And SchImplementation.IsCurrent Then
                    FootprintString := SchImplementation.ModelName;
                SchImplementation := ImplIterator.NextSchObject;
            End;
            Component.SchIterator_Destroy(ImplIterator);

            Componentslist.Add(
                'Designator: '       + Component.Designator.Text                + ', ' +
                'Footprint: '        + FootprintString                          + ', ' +
                'Comment:'           + Component.Comment.Text                   + ', ' +
                'UniqueID: '         + Component.UniqueId                       + ', ' +
                'Orientation: '      + RotationToStr(Component.Orientation)     + ', ' +
                'IsMirrored: '       + BooleanToStr(Component.IsMirrored)       + ', ' +
                'AreaColor: '        + IntToStr(Component.AreaColor)            + ', ' +
                'PinColor: '         + IntToStr(Component.PinColor)             + ', ' +
                'ShowHiddenFields: ' + BooleanToStr(Component.ShowHiddenFIelds) + ', ' +
                'LibraryPath: '      + Component.LibraryPath                    + ', ' +
                'Description: '      + Component.ComponentDescription);

            Component := Iterator.NextSchObject;
            Componentslist.Add('');
        End;
    Finally
        CurrentSheet.SchIterator_Destroy (Iterator);
    End;

    ComponentsList.Add('Report created on '+ DateToStr(Date) + ' ' + TimeToStr(Time));
    ComponentsList.Insert(0,'Schematic Components Report...');
    ComponentsList.Insert(1,'==============================');
    ComponentsList.Insert(2,'');
    ComponentsList.Insert(3,'');

    S := 'C:\Report.Txt';
    ComponentsList.SaveToFile(S);
    ComponentsList.Free;

    // Display report in Altium Designer with a list of components and their PCB Footprints.
    ReportDocument := Client.OpenDocument('Text', S);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
End.
