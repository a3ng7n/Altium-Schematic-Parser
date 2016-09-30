{..............................................................................}
{ Summary Iterates Polygons from the current PCB document.                     }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function PolygonPourOverToStr(PPO : TPolygonPourOver) : TPCBString;
Begin
    Result := '';
    Case PPO of
        ePolygonPourOver_None            : Result := 'Don''t Pour Over Same Net Objects';
        ePolygonPourOver_SameNet         : Result := 'Pour Over All Same Net Objects';
        ePolygonPourOver_SameNetPolygons : Result := 'Pour Over Same Net Polygons Only';
    End;
End;
{..............................................................................}

{..............................................................................}
Function PolyHatchStyleToStr(PHS : TPolyHatchStyle) : TPCBString;
Begin
    Result := '';
    Case PHS of
        ePolyHatch90 : Result := '90 Degree hatch';
        ePolyHatch45 : Result := '45 Degree hatch';
        ePolyVHatch  : Result := 'Vertical hatch';
        ePolyHHatch  : Result := 'Horizontal hatch';
        ePolyNoHatch : Result := 'Outlines Only';
        ePolySolid   : Result := 'Solid Region Polygon';
    End;
End;
{..............................................................................}

{..............................................................................}
Function BoolToString(B : Boolean) : TPCBString;
Begin
    Result := 'False';
    If B Then Result := 'True';
End;
{..............................................................................}

{..............................................................................}
Procedure IteratePolygons;
Var
    Board         : IPCB_Board;
    Polygon       : IPCB_Polygon;
    BoardIterator : IPCB_BoardIterator;
    PolygonRpt    : TStringList;

    FileName      : TPCBString;
    Document      : IServerDocument;
    PolyNo        : Integer;
    I             : Integer;
    J             : Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Retrieve the iterator
    BoardIterator        := Board.BoardIterator_Create;
    BoardIterator.AddFilter_ObjectSet(MkSet(ePolyObject));
    BoardIterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    BoardIterator.AddFilter_Method(eProcessAll);

    PolyNo     := 0;
    PolygonRpt := TStringList.Create;

    // Search for Polygons and for each polygon found
    // get its attributes and put them in a TStringList
    // to be saved as a text file...
    Polygon := BoardIterator.FirstPCBObject;
    While (Polygon <> Nil) Do
    Begin
        Inc(PolyNo);
        PolygonRpt.Add('Polygon No : '                  + IntToStr(PolyNo));
        PolygonRpt.Add('===============');
        PolygonRpt.Add(' Polygon Detail : '             + Polygon.Detail);
        PolygonRpt.Add(' Polygon Layer : '              + Layer2String(Polygon.Layer));
        PolygonRpt.Add(' Polygon Hatch Style : '        + PolyHatchStyleToStr(Polygon.PolyHatchStyle));

        //Check if polygon is a solid region
        If Polygon.PolyHatchStyle = ePolySolid Then
        Begin
            PolygonRpt.Add(' Polygon Min Primitive Size : '     + FloatToStr(Polygon.MinTrack));
            PolygonRpt.Add(' Polygon Remove Islands By Area : ' + BoolToString(Polygon.RemoveIslandsByArea));
            PolygonRpt.Add(' Polygon Island Area Threshold : '  + FloatToStr(Polygon.IslandAreaThreshold));
            PolygonRpt.Add(' Polygon Remove Narrow Necks : '    + BoolToString(Polygon.RemoveNarrowNecks));
            PolygonRpt.Add(' Polygon Neck Width Threshold : '   + FloatToStr(Polygon.NeckWidthThreshold));
            PolygonRpt.Add(' Polygon Arc Approximation : '      + FloatToStr(Polygon.ArcApproximation));
        End
        Else
        Begin
            PolygonRpt.Add(' Polygon Pour Over Same Net : ' + PolygonPourOverToStr(Polygon.PourOver));
            PolygonRpt.Add(' Polygon Remove Dead Copper : ' + BoolToString(Polygon.RemoveDead));
            PolygonRpt.Add(' Polygon Grid : '               + FloatToStr(Polygon.Grid));
            PolygonRpt.Add(' Polygon Track Width : '        + FloatToStr(Polygon.TrackSize));

            If Polygon.UseOctagons Then
                PolygonRpt.Add(' Polygon Surround Pads With : Octagons')
            Else
                PolygonRpt.Add(' Polygon Surround Pads With : Arcs');
        End;


        // Check if the polygon has a Net property. If the polygon is not
        // connected to a net on the PCB, don't extract its net name.
        If Polygon.Net <> Nil Then
            PolygonRpt.Add(' Polygon Net : ' + Polygon.Net.Name)
        Else
            PolygonRpt.Add(' Polygon Net : No Net');

        PolygonRpt.Add('');
        PolygonRpt.Add(' Segments of polygon' + IntToStr(PolyNo) + ' :');
        // Segments of a polygon
        For I := 0 To Polygon.PointCount - 1 Do
        Begin
            PolygonRpt.Add(' Polygon Vertex ' + IntToStr(I + 1) + ' at X,Y : '
                         + IntToStr(Polygon.Segments[I].vx) + ','
                         + IntToStr(Polygon.Segments[I].vy));

            // if a segment is an arc, then process attributes
            // associated with this arc
            If Polygon.Segments[I].Kind = ePolySegmentArc Then
            Begin
                PolygonRpt.Add('   Polygon Arc Center ' + IntToStr(I + 1) + ' at X,Y : '
                             + FloatToStr(Polygon.Segments[I].cx) + ',' +
                             + FloatToStr(Polygon.Segments[I].cy));
                PolygonRpt.Add('   Polygon Segment ' + IntToStr(I + 1) + ' Arc 1  : '
                             + FloatToStr(Polygon.Segments[I].Angle1));
                PolygonRpt.Add('   Polygon Segment ' + IntToStr(I + 1) + ' Arc 2  : '
                             + FloatToStr(Polygon.Segments[I].Angle2));
                PolygonRpt.Add('   Polygon Segment ' + IntToStr(I + 1) + ' Radius : '
                             + FloatToStr(Polygon.Segments[I].Radius));
            End;
        End;

        PolygonRpt.Add('');
        Polygon := BoardIterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(BoardIterator);

    // Display the Polygons report
    FileName := ChangeFileExt(Board.FileName,'.pol');
    PolygonRpt.SaveToFile(Filename);
    PolygonRpt.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);
 End;
{..............................................................................}

{..............................................................................}
