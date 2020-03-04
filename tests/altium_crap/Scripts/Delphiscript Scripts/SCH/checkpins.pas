{..............................................................................}
{ Summary Checks for valid pins of symbols in a library.                       }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
  ReportInfo : TStringList;

{..............................................................................}

{..............................................................................}
Procedure GenerateReport(Dummy : Integer = 0);
Var
  Document : IServerDocument;
Begin
    ReportInfo.Insert(0,'Schematic Library Pin Names Check');
    ReportInfo.Insert(1,'---------------------------------');
    ReportInfo.SaveToFile('c:\SchPinsReport.txt');

    Document := Client.OpenDocument('Text','c:\SchPinsReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
Procedure CheckSchLibraryFilePinNames;
Var
  DocType       : WideString;
  SchComponent  : ISch_Component;
  SchLib        : ISch_Lib;
  SchDoc        : ISCh_Doc;
  SchIterator   : ISch_Iterator;
  Pin           : ISch_Pin;
  LibName       : TDynamicString;
  PinName       : TDynamicString;
  PinDesignator : TDynamicString;
  PinIterator   : ISch_Iterator;
  LPinName      : Integer;
  LDesignator   : Integer;
Begin
    If SchServer = Nil Then Exit;

    // Obtain the Client interface so can get the Kind property.
    DocType := UpperCase(Client.CurrentView.OwnerDocument.Kind);
    If DocType <> 'SCHLIB' Then
    Begin
        ShowWarning('This is not a Library document!');
        Exit;
    End;

    SchLib := SchServer.GetCurrentSchDocument;
    If SchLib = Nil Then Exit;

    // Create a TStringList object to store Pin data
    ReportInfo := TStringList.Create;
    ReportInfo.Clear;

    LibName := SchLib.DocumentName;
    LibName := ExtractFileName(LibName);
    ReportInfo.Add(LibName);

    // Create an iterator to look for components only
    SchIterator := SchLib.SchLibIterator_Create;
    SchIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Try
        SchComponent := SchIterator.FirstSchObject;
        While SchComponent <> Nil Do
        Begin
            // Look for Pins associated with this component.
            PinIterator := SchComponent.SchIterator_Create;
            PinIterator.AddFilter_ObjectSet(MkSet(ePin));
            ReportInfo.Add(SchComponent.LibReference);
            Try
                Pin := PinIterator.FirstSchObject;
                While Pin <> Nil Do
                Begin
                    ReportInfo.Add(' The Pin Designator: ' + Pin.Designator);

                    PinName       := Pin.Name;
                    PinDesignator := Pin.Designator;
                    LPinName      := Length(Pin.Name);
                    LDesignator   := Length(Pin.Designator);

                         If PinName       = ''                 Then ReportInfo.Add('   Pin Name is a blank')
                    Else If PinDesignator = ''                 Then ReportInfo.Add('   Pin Designator is a blank')
                    Else If (PinName[1] = #32)                 Then ReportInfo.Add('    ' + Pin.Name + ',' + 'The first character of the Pin Name is a blank')
                    Else If (PinDesignator[1] = #32)           Then ReportInfo.Add('    ' + Pin.Designator + ',' + 'The first character of the Pin Designator is a blank')
                    Else If (PinName[LPinName] = #32)          Then ReportInfo.Add('    ' + Pin.Name + ',' + 'The last character of the Pin Name is a blank')
                    Else If (PinDesignator[LDesignator] = #32) Then ReportInfo.Add('    ' + Pin.Designator + ',' + 'The last character of the Pin Designator is a blank');

                    Pin := PinIterator.NextSchObject;
                End;
            Finally
                SchComponent.SchIterator_Destroy(PinIterator);
            End;

            ReportInfo.Add('');
            SchComponent := SchIterator.NextSchObject;
        End;
    Finally
        SchLib.SchIterator_Destroy(SchIterator);
    End;

    GenerateReport;
    ReportInfo.Free;
End;
{..............................................................................}

{..............................................................................}

