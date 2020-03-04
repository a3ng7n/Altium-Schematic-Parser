{..............................................................................}
{ Summary Queries the current PCB document for board settings information      }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function UnitToString(U : TUnit) : TPCBString;
Begin
    Result := '';
    Case U of
       eImperial : Result := 'Imperial (mil)';
       eMetric   : Result := 'Metric (mm)';
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
Procedure Query_OutlinePerimeter(Var BR : TCoordRect; Var OtherUnit : TUnit; Var Edge_Str : TPCBString; Var Area_Str : TPCBString);
Var
    PCB_Board : IPCB_Board;
//    BR        : TCoordRect;
//    OtherUnit : TUnit;
    I,J       : Integer;
    Del_X     : Real;
    Del_Y     : Real;
    Del_Arc   : Real;
    Perimeter : Real;
    Arc_Angle : TAngle;
//    Edge_Str  : TPCBString;
//    Area_Str  : TPCBString;
Begin
    PCB_Board       := PCBServer.GetCurrentPCBBoard;
    If PCB_Board     = Nil Then Exit;

    PCB_Board.BoardOutline.Invalidate;
    PCB_Board.BoardOutline.Rebuild;
    PCB_Board.BoardOutline.Validate;
    BR := PCB_Board.BoardOutline.BoundingRectangle;

    // Set OtherUnit to "opposite" of PCB_Board.DisplayUnit
    If PCB_Board.DisplayUnit = eImperial Then
        OtherUnit := eMetric
    Else
        OtherUnit := eImperial;

    // Determine length of perimeter.
    // Note that the first vertex of a polygon is the same as the last vertex.
    Perimeter := 0;
    For I := 0 To PCB_Board.BoardOutline.PointCount - 1 Do
    Begin
       If I = PCB_Board.BoardOutline.PointCount - 1 Then
           J := 0
       Else
           J := I + 1;
{
We want to calculate (PCB_Board.BoardOutline.Segments[I+1].vx - PCB_Board.BoardOutline.Segments[I].vx) for all
values of I from 0 to PCB_Board.BoardOutline.PointCount - 2; when I = PCB_Board.BoardOutline.PointCount - 1,
PCB_Board.BoardOutline.Segments[PCB_Board.BoardOutline.PointCount].vx is dodgy, hence the use of the J variable
in addition to the I variable.
}
        If PCB_Board.BoardOutline.Segments[I].Kind = ePolySegmentLine Then
        Begin
            Del_X := CoordToMils(PCB_Board.BoardOutline.Segments[J].vx
                               - PCB_Board.BoardOutline.Segments[I].vx);

            Del_Y := CoordToMils(PCB_Board.BoardOutline.Segments[J].vy
                               - PCB_Board.BoardOutline.Segments[I].vy);

            Perimeter := Perimeter + Sqrt((Del_X * Del_X) + (Del_Y * Del_Y));
        End
        Else
        Begin
            Arc_Angle := PCB_Board.BoardOutline.Segments[I].Angle2
                       - PCB_Board.BoardOutline.Segments[I].Angle1;

            If Arc_Angle < 0 Then Arc_Angle := Arc_Angle + 360;

            Del_Arc   := CoordToMils(PCB_Board.BoardOutline.Segments[I].Radius)
                       * Degrees2Radians(Arc_Angle);

            Perimeter := Perimeter + Del_Arc;
        End;
    End;

    // Construct perimeter-reporting string - this is reported in units of both inches and cm.
    // If the current value of the Measurement Unit is Imperial, the former is listed first,
    // then the latter within brackets; otherwise (i.e. the current value of the Measurement Unit
    // is Metric), the latter is listed first, then the former within brackets. (1 inch = 2.54 cm)
    If PCB_Board.DisplayUnit = eImperial Then
    Begin
        Edge_Str := FloatToStr(Perimeter * 0.001)   + ' inch' + '  ('
                  + FloatToStr(Perimeter * 0.00254) + ' cm)';
    End
    Else
    Begin
        Edge_Str := FloatToStr(Perimeter * 0.00254) + ' cm' + '  ('
                  + FloatToStr(Perimeter * 0.001)   + ' inch)';
    End;

    // Construct area-reporting string  - this is reported in units of both inch^2 and cm^2.
    // If the current value of the Measurement Unit is Imperial, the former is listed first,
    // then the latter within brackets; otherwise (i.e. the current value of the Measurement
    // Unit is Metric), the latter is listed first, then the former within brackets.
    // The AreaSize property is returned in units of 1 x 10E-14 inch^2, which is subsequently
    // scaled as required. (1 inch = 2.54 cm, so 1 inch^2 = (2.54)^2 cm^2 = 6.4516 cm^2.)
    If PCB_Board.DisplayUnit = eImperial Then
    Begin
        Area_Str := FloatToStr(PCB_Board.BoardOutline.AreaSize / (10000000 * 10000000))
                  + ' inch^2' + '  ('
                  + FloatToStr(PCB_Board.BoardOutline.AreaSize * 6.4516 / (10000000 * 10000000))
                  + ' cm^2)';
    End
    Else
    Begin
        Area_Str := FloatToStr(PCB_Board.BoardOutline.AreaSize * 6.4516 / (10000000 * 10000000))
                  + ' cm^2' + '  ('
                  + FloatToStr(PCB_Board.BoardOutline.AreaSize / (10000000 * 10000000))
                  + ' inch^2)';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure Query_Board;
Var
    Board         : IPCB_Board;
    LibraryExists : TPCBString;
    AShowSheet    : TPCBString;
    ALockSheet    : TPCBString;

    BR        : TCoordRect;
    OtherUnit : TUnit;
    Edge_Str  : TPCBString;
    Area_Str  : TPCBString;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    LibraryExists := BoolToString(Board.IsLibrary);
    AShowSheet    := BoolToString(Board.PCBSheet.ShowSheet);
    ALockSheet    := BoolToString(Board.PCBSheet.LockSheet);

    //Note, the Xorigin and YOrigin attributes will always be 0
    // Unless the origin marker is present and set on the PCB document.

    Query_OutlinePerimeter(BR, OtherUnit, Edge_Str, Area_Str);

    ShowMessage(
        'Board Handle  = '   + IntToStr    (Board.I_ObjectAddress)      + #13 +
        'Window Handle = '   + IntToStr    (Board.PCBWindow)            + #13 +
        'Board Filename ='   +              Board.FileName              + #13 +
        'Origin X = '        + IntToStr    (Board.XOrigin)              + #13 +
        'Origin Y = '        + IntToStr    (Board.YOrigin)              + #13 +
        'Board Units = '     + UnitToString(Board.DisplayUnit)          + #13 +
        'Current layer = '   + Layer2String(Board.CurrentLayer)         + #13 +
        'Sheet.X = '         + IntToStr    (Board.PCBSheet.SheetX)      + #13 +
        'Sheet.Y = '         + IntToStr    (Board.PCBSheet.SheetY)      + #13 +
        'Sheet.Height = '    + IntToStr    (Board.PCBSheet.SheetHeight) + #13 +
        'Sheet.Width = '     + IntToStr    (Board.PCBSheet.SheetWidth)  + #13 +
        'Sheet is shown = '  +              AShowSheet                  + #13 +
        'Sheet is locked = ' +              ALockSheet                  + #13 +  #13 +

        'Board Outline Width  : '
        + CoordUnitToString(BR.Right - BR.Left, Board.DisplayUnit) +  ' ('
        + CoordUnitToString(BR.Right - BR.Left, OtherUnit) + ')' + #13

        + 'Board Outline Height : '
        + CoordUnitToString(BR.Top - BR.Bottom, Board.DisplayUnit) +  ' ('
        + CoordUnitToString(BR.Top - BR.Bottom, OtherUnit) + ')' + #13 + #13

        + 'Board Outline Perimeter : '   + Edge_str + #13 + #13
        + 'Area within Board Outline : ' + Area_Str);
End;
{..............................................................................}

{..............................................................................}























































































































































