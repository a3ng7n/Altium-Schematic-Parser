{..............................................................................}
{ Summary An Altium Designer script that does the following                    }
{                                                                              }
{ 1/ LookForFiducials                                                          }
{ 2/ Check Board Outline Dimensions                                            }
{ 3/ Generate an output (PickPlace and BOM)                                    }
{                                                                              }
{ Copyright (c) 2007 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    Board : IPCB_Board;

Procedure GenerateAnOutput(Quiet : Boolean; Which : Integer);
Begin
    Case Which Of
       // Generate Pick n place
       1: Client.SendMessage('WorkspaceManager:GenerateReport', 'ObjectKind=Assembly|Index=2' , 255, Client.CurrentView);

       // Generate a simple BOM
       2: Client.SendMessage('WorkspaceManager:GenerateReport', 'ObjectKind=Report|KInd=SimpleBOM   |Target=Document' , 255, Client.CurrentView);

       // generate a BOM part type file
       3: Client.SendMessage('WorkspaceManager:GenerateReport', 'ObjectKind=Report|Kind=BOM_PartType|Target=Document' , 255, Client.CurrentView);
    End;
End;
{..............................................................................}

{..............................................................................}
(*
Fiducial marks are copper features used for the optical alignment of a PCB by an
automatic assembly machine.

a fiducial mark is created as a component in the library.
using a single layer pad and given the designator name FID
*)
Procedure LookForFiducials(Quiet : Boolean);
Var
    Pad       : IPCB_Pad;
    Iterator  : IPCB_BoardIterator;
    Report    : WideString;

    Filename  : TPCBString;
    RDocument : IServerDocument;
    OutFile   : TextFile;
Begin
    Report := '';

    // retrieve the iterator
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search and count pads
    Pad := Iterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        If UpperCase(Pad.Name) = 'FID' Then
            Report := Report + Pad.Name + ' X: ' + IntToStr(CoordToMils(Pad.X)) + ' , Y: ' + IntToStr(CoordToMils(Pad.Y)) + #13;

        Pad := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    If Quiet Then
    Begin
        FileName := ChangeFileExt(Board.FileName, '') + '_Fiducials.txt';
        Try
            AssignFile(OutFile, Filename);
            Rewrite(OutFile);
            Write(OutFile, Report);
        Finally
            CloseFile(OutFile);
        End;

        RDocument  := Client.OpenDocument('Text',FileName);
        If RDocument <> Nil Then
              Client.ShowDocument(RDocument);
    End
    Else
        ShowMessage(Report);
End;
{..............................................................................}

{..............................................................................}
Procedure CheckTheBoardOutlineDimensions(Quiet : Boolean);
Var
    BR        : TCoordRect;
    OtherUnit : TUnit;
    I,J       : Integer;
    Del_X     : Real;
    Del_Y     : Real;
    Del_Arc   : Real;
    Perimeter : Real;
    Arc_Angle : TAngle;
    Edge_Str  : TPCBString;

    Filename  : TPCBString;
    RDocument : IServerDocument;
    OutFile   : TextFile;
Begin
    Board.BoardOutline.Invalidate;
    Board.BoardOutline.Rebuild;
    Board.BoardOutline.Validate;
    BR := Board.BoardOutline.BoundingRectangle;

    // Set OtherUnit to "opposite" of PCB_Board.DisplayUnit
    If Board.DisplayUnit = eImperial Then
        OtherUnit := eMetric
    Else
        OtherUnit := eImperial;

    // Determine length of perimeter.
    // Note that the first vertex of a polygon is the same as the last vertex.
    Perimeter := 0;
    For I := 0 To Board.BoardOutline.PointCount - 1 Do
    Begin
       If I = Board.BoardOutline.PointCount - 1 Then
           J := 0
       Else
           J := I + 1;
        If Board.BoardOutline.Segments[I].Kind = ePolySegmentLine Then
        Begin
            Del_X := CoordToMils(Board.BoardOutline.Segments[J].vx
                               - Board.BoardOutline.Segments[I].vx);

            Del_Y := CoordToMils(Board.BoardOutline.Segments[J].vy
                               - Board.BoardOutline.Segments[I].vy);

            Perimeter := Perimeter + Sqrt((Del_X * Del_X) + (Del_Y * Del_Y));
        End
        Else
        Begin
            Arc_Angle := Board.BoardOutline.Segments[I].Angle2
                       - Board.BoardOutline.Segments[I].Angle1;

            If Arc_Angle < 0 Then Arc_Angle := Arc_Angle + 360;

            Del_Arc   := CoordToMils(Board.BoardOutline.Segments[I].Radius)
                       * Degrees2Radians(Arc_Angle);

            Perimeter := Perimeter + Del_Arc;
        End;
    End;

    // Construct perimeter-reporting string - this is reported in units of both inches and cm.
    // If the current value of the Measurement Unit is Imperial, the former is listed first,
    // then the latter within brackets; otherwise (i.e. the current value of the Measurement Unit
    // is Metric), the latter is listed first, then the former within brackets. (1 inch = 2.54 cm)
    If Board.DisplayUnit = eImperial Then
    Begin
        Edge_Str := FloatToStr(Perimeter * 0.001)   + ' inch' + '  ('
                  + FloatToStr(Perimeter * 0.00254) + ' cm)';
    End
    Else
    Begin
        Edge_Str := FloatToStr(Perimeter * 0.00254) + ' cm' + '  ('
                  + FloatToStr(Perimeter * 0.001)   + ' inch)';
    End;

    If Quiet Then
    Begin
        FileName := ChangeFileExt(Board.FileName, '') + '_BoardOutline.txt';
        Try
            AssignFile(OutFile, Filename);
            Rewrite(OutFile);

            Writeln(Outfile, 'Board Outline Width  : ' + CoordUnitToString(BR.Right - BR.Left, Board.DisplayUnit) +  ' ('
            + CoordUnitToString(BR.Right - BR.Left, OtherUnit) + ')');

            Writeln(Outfile,'Board Outline Height : ' + CoordUnitToString(BR.Top - BR.Bottom, Board.DisplayUnit) +  ' ('
            + CoordUnitToString(BR.Top - BR.Bottom, OtherUnit) + ')');

            Writeln(Outfile,'Board Outline Perimeter : '   + Edge_str);

        Finally
            CloseFile(OutFile);
        End;

        RDocument  := Client.OpenDocument('Text',FileName);
        If RDocument <> Nil Then
              Client.ShowDocument(RDocument);
    End
    Else
        ShowMessage(
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
Procedure RunABatchProgram;
Var
    GenerateReportQuietly : Boolean;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    GenerateReportQuietly := True;
    GenerateAnOutput              (GenerateReportQuietly, 1);    
    LookForFiducials              (GenerateReportQuietly);
    CheckTheBoardOutlineDimensions(GenerateReportQuietly);
End;
{..............................................................................}

{..............................................................................}

