{..............................................................................}
{ Summary Iterate a Schematic Library and report elements of Symbols.           }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure GenerateReport(Report : TStringList);
Var
    Document : IServerDocument;
Begin
    Report.Insert(0,'Schematic Library Symbol Report');
    Report.Insert(1,'-------------------------------');
    Report.SaveToFile('c:\SymbolReport.txt');

    Document := Client.OpenDocument('Text','c:\SymbolReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
Function ObjectIdToString(AnObjectId : TObjectId) : WideString;
Begin
    Result := 'Unknown';
    Case AnObjectId Of
       eDesignator          : Result := 'Designator';
       eRectangle           : Result := 'Rectangle';
       eLine                : Result := 'Line';
       eArc                 : Result := 'Arc';
       eEllipticalArc       : Result := 'EllipticalArc';
       eRoundRectangle      : Result := 'RoundRectangle';
       eImage               : Result := 'Image';
       ePie                 : Result := 'Pie';
       eEllipse             : Result := 'Ellipse';
       ePolygon             : Result := 'Polygon';
       ePolyline            : Result := 'Polyline';
       eWire                : Result := 'Wire';
       eBezier              : Result := 'Bezier';
       eLabel               : Result := 'Annotation / Label';
       eParameter           : Result := 'Parameter';
       eParameterSet        : Result := 'ParameterSet';
       eParameterList       : Result := 'ParameterList';
       eSymbol              : Result := 'Symbol';
       ePin                 : Result := 'Pin';
       eMapDefiner          : Result := 'Map Definer';
       eImplementationMap   : Result := 'Implementation Map';
       eImplementation      : Result := 'Implementation';
       eImplementationsList : Result := 'Implemenations List';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateReportOfSymbols;
Var
  DocType       : WideString;
  SchComponent  : ISch_Component;
  SchLib        : ISch_Lib;
  SchDoc        : ISCh_Doc;
  SchIterator   : ISch_Iterator;

  AnObject      : ISch_GraphicalObject;
  LibName       : TDynamicString;
  Iterator      : ISch_Iterator;
  PartCount     : Integer;
  ReportInfo    : TStringList;

Begin
    If SchServer = Nil Then Exit;

    // Obtain the Client interface so can get the Kind property.
    DocType := UpperCase(Client.CurrentView.OwnerDocument.Kind);
    If DocType <> 'SCHLIB' Then
    Begin
        ShowWarning('This is not a Library document!');
        Exit;
    End;

    // Obtain the schematic library interface
    SchLib := SchServer.GetCurrentSchDocument;
    If SchLib = Nil Then Exit;

    // Create a TStringList object to store data
    ReportInfo := TStringList.Create;
    ReportInfo.Clear;

    // Obtain schematic library filename
    LibName := SchLib.DocumentName;
    LibName := ExtractFileName(LibName);
    ReportInfo.Add(LibName);

    // Create an iterator to look for symbols within the library
    SchIterator := SchLib.SchLibIterator_Create;
    SchIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    PartCount := 0;

    Try
        SchComponent := SchIterator.FirstSchObject;
        While SchComponent <> Nil Do
        Begin
            // Look for child objects associated with this symbol.
            Iterator := SchComponent.SchIterator_Create;
            ReportInfo.Add('The Symbol Name : ' + SchComponent.LibReference);
            Try
                AnObject := Iterator.FirstSchObject;
                While AnObject <> Nil Do
                Begin
                    // this counts the number of items in a symbol
                    Inc(PartCount);

                    // puts item name in the reportinfo TStringList container
                    ReportInfo.Add(' The symbol has : ' + ObjectIdToString(AnObject.ObjectId));

                    // look for the next item of a symbol
                    AnObject := Iterator.NextSchObject;
                End;

                //no more items found for this symbol, add item count for this symbol
                ReportInfo.Add(' This symbol has item count: ' + IntToStr(PartCount));
                PartCount := 0;
            Finally
                SchComponent.SchIterator_Destroy(Iterator);
            End;

            ReportInfo.Add('');
            SchComponent := SchIterator.NextSchObject;
        End;
    Finally
        SchLib.SchIterator_Destroy(SchIterator);
    End;

    GenerateReport(ReportInfo);
    ReportInfo.Free;
End;
{..............................................................................}

{..............................................................................}
