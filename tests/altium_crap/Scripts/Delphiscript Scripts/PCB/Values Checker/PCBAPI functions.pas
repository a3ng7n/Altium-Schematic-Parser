{..............................................................................}
{ Summary This unit is used by the ValuesCheckerUnit unit.                     }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CheckArcRadii(ABoard : IPCB_Board; AList : TStringList);
Var
    Arc      : IPCB_Arc;
    Iterator : IPCB_BoardIterator;
Begin
    Iterator        := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eArcObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for pads with zero diameter
    Arc := Iterator.FirstPCBObject;
    While (Arc <> Nil) Do
    Begin
        If Arc.Radius = 0 Then
            AList.Add(' An arc with zero radius found at ' +
                      IntToStr(CoordToMils(Arc.XCenter)) + 'mils' + ', ' +
                      IntToStr(CoordToMils(Arc.YCenter)) + 'mils');

        Arc := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);
End;
{..............................................................................}

{..............................................................................}
Procedure CheckPadDiameters(ABoard : IPCB_Board; AValue : Integer; AList : TStringList);
Var
    Pad      : IPCB_Pad;
    Iterator : IPCB_BoardIterator;
Begin
    Iterator := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for pads with specified holesize diameter
    Pad := Iterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        // Assumption, only check TopX and TopY of Pad for 0 value
        If Pad.HoleSize = AValue Then
             AList.Add(' A pad with ' + IntToStr(AValue) + 'mils diameter found at ' +
                       IntToStr(CoordToMils(Pad.X)) + 'mils'  + ', ' +
                       IntToStr(CoordToMils(Pad.Y)) + 'mils');

        Pad := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);
End;
{..............................................................................}

{..............................................................................}
Procedure CheckViaDiameters(ABoard : IPCB_Board; AValue : Integer; AList : TStringList);
Var
    Via      : IPCB_Via;
    Iterator : IPCB_BoardIterator;
Begin
    Iterator        := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for vias with specified holesize diameter
    Via := Iterator.FirstPCBObject;
    While (Via <> Nil) Do
    Begin
        If Via.HoleSize = AValue Then
             AList.Add(' A via with ' + IntToStr(AValue) + 'mils diameter found at ' +
                       IntToStr(CoordToMils(Via.X)) + 'mils' + ', ' +
                       IntToStr(CoordToMils(Via.Y)) + 'mils');

        Via := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);
End;
{..............................................................................}

{..............................................................................}
Procedure CheckNegativeValues(ABoard : IPCB_Board; AList : TStringList);
Var
    Track     : IPCB_Track;
    Pad       : IPCB_Pad;
    Via       : IPCB_Via;
    Arc       : IPCB_Arc;

    Iterator  : IPCB_BoardIterator;
Begin
    // Search for tracks with negative locations
    Iterator        := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eTrackObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);
    Track := Iterator.FirstPCBObject;
    While (Track <> Nil) Do
    Begin
        If (Track.x1 < 0) or
           (Track.y1 < 0) or
           (Track.x2 < 0) or
           (Track.y2 < 0) Then
                AList.Add(' A track found with negative boundaries at ' +
                          IntToStr(CoordToMils(Track.X1)) + 'mils' + ', ' +
                          IntToStr(CoordToMils(Track.Y1)) + 'mils' + ', ' +
                          IntToStr(CoordToMils(Track.X2)) + 'mils' + ', ' +
                          IntToStr(CoordToMils(Track.Y2)) + 'mils');

        Track := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);


    // Search for pads with negative locations
    Iterator        := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);
    Pad := Iterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        If (Pad.x < 0) or
           (Pad.y < 0) Then
                AList.Add(' A pad found with negative boundaries at ' +
                          IntToStr(CoordToMils(Pad.X)) + 'mils' + ', ' +
                          IntToStr(CoordToMils(Pad.Y)) + 'mils');

        Pad := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);

    // Search for vias with negative locations
    Iterator        := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eViaObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);
    Via := Iterator.FirstPCBObject;
    While (Via <> Nil) Do
    Begin
        If (Via.x < 0) or
           (Via.y < 0) Then
                AList.Add(' A via found with negative boundaries at ' +
                          IntToStr(CoordToMils(Via.X)) + 'mils' + ', ' +
                          IntToStr(CoordToMils(Via.Y)) + 'mils' + ', ');

        Via := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);

    // Search for arcs with negative locations
    Iterator        := ABoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eArcObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);
    Arc := Iterator.FirstPCBObject;
    While (Arc <> Nil) Do
    Begin
        If (Arc.XCenter < 0) or
           (Arc.YCenter < 0) Then
                AList.Add(' An arc found with negative boundaries at ' +
                          IntToStr(CoordToMils(Arc.XCenter))  + 'mils' + ', ' +
                          IntToStr(CoordToMils(Arc.YCenter))  + 'mils' + ', ');

        Arc := Iterator.NextPCBObject;
    End;
    ABoard.BoardIterator_Destroy(Iterator);
End;
{..............................................................................}

{..............................................................................}
Procedure OutputErrorReport(ABoard : IPCB_Board; AList : TStringList);
Var
   S              : TDynamicString;
   ReportDocument : IServerDocument;
Begin
    If AList.Count > 0 Then
    Begin
        AList.Insert(0,'Object Bad Property Values for this PCB,');
        AList.Insert(1, ExtractFileName(ABoard.FileName));
        AList.Insert(2,'________________________________________');
        AList.Insert(3,'');

        // Generate a text file with bad values of PCB objects found
        // using PCB board file name but with _BadValues added and
        // file extension changed to RPT.
        S := ChangeFileExt(ABoard.FileName,'.RPT');
        AList.SaveToFile(S);

        // Open the text file in DXP.
        ReportDocument := Client.OpenDocument('Text', S);
        If ReportDocument <> Nil Then
            Client.ShowDocument(ReportDocument);
    End;
End;
{..............................................................................}

{..............................................................................}
