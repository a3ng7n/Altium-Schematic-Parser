{..............................................................................}
{ Summary Iterates Regions from the current PCB document.                     }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure AddParentRelationShipsToReport(Region : IPCB_Region; Report : TStringList);
Begin
    If Region.InPolygon Then
        Report.Add(' Region InPolygon : ' + Region.Polygon.Descriptor);

    If Region.InComponent Then
        Report.Add(' Region InComponent : ' + Region.Component.Descriptor);

    If Region.InNet Then
        Report.Add(' Region InNet : ' + Region.Net.Descriptor);
End;
{..............................................................................}

{..............................................................................}
Procedure AddContourToReport(Contour : IPCB_Contour; Report : TStringList);
Var
    I : Integer;
Begin
    For I := 1 To Contour.Count Do
        Report.Add('  [' + IntToStr(I) + '] : ' + '(' + CoordUnitToString(Contour.X[I], eMetric) + ',' + CoordUnitToString(Contour.Y[I], eMetric) + ')');
End;
{..............................................................................}

{..............................................................................}
Procedure IterateRegions;
Var
    Board         : IPCB_Board;
    Region        : IPCB_Region;
    BoardIterator : IPCB_BoardIterator;
    Rpt           : TStringList;

    FileName      : TPCBString;
    Document      : IServerDocument;
    Count         : Integer;
    I             : Integer;
    J             : Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Retrieve the iterator
    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(eRegionObject));
    BoardIterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    Count := 0;
    Rpt   := TStringList.Create;

    // Search for Regions and for each region found
    // get its attributes and put them in a TStringList
    // to be saved as a text file...
    Region := BoardIterator.FirstPCBObject;
    While (Region <> Nil) Do
    Begin
        Inc(Count);
        Rpt.Add('Region No : ' + IntToStr(Count));
        Rpt.Add('===============');
        Rpt.Add(' Region Name : ' + Region.Name);
        Rpt.Add(' Region Layer : ' + Layer2String(Region.Layer));

        AddParentRelationShipsToReport(Region, Rpt);

        Rpt.Add(#10 + ' MainContour');
        AddContourToReport(Region.MainContour, Rpt);

        For I := 0 To Region.HoleCount - 1 Do
        Begin
            Rpt.Add(#10 + ' Hole[' + IntToStr(I) + ']');
            AddContourToReport(Region.Holes[I], Rpt);
        End;

        Rpt.Add('');
        Region := BoardIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(BoardIterator);

    // Display the Regions report
    FileName := ChangeFileExt(Board.FileName,'.reg');
    Rpt.SaveToFile(Filename);
    Rpt.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);
 End;
{..............................................................................}

{..............................................................................}
