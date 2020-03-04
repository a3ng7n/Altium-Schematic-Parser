Var
    FDoFlipH  : Boolean;
    FDoFlipV  : Boolean;
    FNegative : Boolean;

{......................................................................................................................}
Procedure SetState_DoFlipH(Value : Boolean);
Begin
    FDoFlipH := Value;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure SetState_Negative(Value : Boolean);
Begin
    FNegative := Value;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure SetState_DoFlipV(Value : Boolean);
Begin
    FDoFlipV := Value;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure FlipCoords(aPicture : TPicture; InX, InY : TCoord; Var OutX, OutY : TCoord);
Begin
    If FDoFlipH Then
       OutX := aPicture.Width - InX - 1
    Else
       OutX := InX;

    If FDoFlipV Then
       OutY := InY
    Else
       OutY := aPicture.Height - InY - 1;
End;
{......................................................................................................................}

{......................................................................................................................}
Function CreateContourFromPixel(X, Y, BaseX, BaseY, PixelToCoordScale : TCoord; Color : TColor) : IPCB_Contour;
Var
   GrayProportion  : Double;
   GrayWidth       : TCoord;
   Left            : TCoord;
   Right           : TCoord;
   Bottom          : TCoord;
   Top             : TCoord;
   RedProportion   : Integer;
   GreenProportion : Integer;
   BlueProportion  : Integer;
Begin
     RedProportion   := (Color And $0000FF);
     GreenProportion := (Color And $00FF00) Shr 8;
     BlueProportion  := (Color And $FF0000) Shr 16;

     GrayProportion := (RedProportion + GreenProportion + BlueProportion) / (3 * 255);

     If FNegative Then
        GrayProportion := 1 - GrayProportion;

     GrayWidth := Round((1 - sqrt(GrayProportion)) * PixelToCoordScale / 2);

     Left   := X       * PixelToCoordScale + BaseX + GrayWidth;
     Right  := (X + 1) * PixelToCoordScale + BaseX - GrayWidth;
     Bottom := Y       * PixelToCoordScale + BaseY + GrayWidth;
     Top    := (Y + 1) * PixelToCoordScale + BaseY - GrayWidth;

     If GrayProportion = 0.0 Then Exit;

     Result := PCBServer.PCBContourFactory;

     Result.AddPoint(Left , Bottom);
     Result.AddPoint(Right, Bottom);
     Result.AddPoint(Right, Top   );
     Result.AddPoint(Left , Top   );
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure AddPixelToUnion(X, Y, BaseX, BaseY, PixelToCoordScale : TCoord; Color : TColor; Var Union : IPCB_Contour);
Var
   PixelContour : IPCB_Contour;
Begin
   PixelContour := CreateContourFromPixel(X, Y, BaseX, BaseY, PixelToCoordScale, Color);

   If PixelContour <> Nil Then
      PCBServer.PCBContourUtilities.ClipSetContour(eSetOperation_Union, Union, PixelContour, Union);
End;
{......................................................................................................................}

{......................................................................................................................}
Function ConstructGeometricPolygonFromPicture_NoUnion(aImage : TImage; ProgressBar : TProgressBar; StatusBar : TStatusBar) : IPCB_GeometricPolygon;
Var
   X, Y               : Integer;
   tX, tY             : Integer;
   PixelContour       : IPCB_Contour;
Begin
    If ProgressBar <> Nil Then
        ProgressBar.Max := aImage.Picture.Height;

    If StatusBar <> Nil Then
    Begin
        StatusBar.SimpleText := 'Converting Pixels To Contours...';
        StatusBar.Update;
    End;

    Result := PcbServer.PCBGeometricPolygonFactory;

    For Y := 0 To aImage.Picture.Height - 1 Do
    Begin
        If ProgressBar <> Nil Then
        Begin
            ProgressBar.Position := Y;
            ProgressBar.Update;
        End;

        For X :=  0 To aImage.Picture.Width - 1 Do
        Begin
            FlipCoords(aImage.Picture, X, Y, tX, tY);
            PixelContour := CreateContourFromPixel(X, Y, FBaseX, FBaseY, FPixelSize, aImage.Canvas.Pixels[X, Y]);

            If PixelContour <> Nil Then
               Result.AddContour(PixelContour);
        End;
     End;
End;
{......................................................................................................................}

{......................................................................................................................}
Function ConstructGeometricPolygonFromPicture_Union(aImage : TImage; ProgressBar : TProgressBar; StatusBar : TStatusBar) : IPCB_GeometricPolygon;
Var
   X, Y               : Integer;
   tX, tY             : Integer;
Begin
    If ProgressBar <> Nil Then
        ProgressBar.Max := aImage.Picture.Height;

    If StatusBar <> Nil Then
    Begin
        StatusBar.SimpleText := 'Converting Pixels To Contours...';
        StatusBar.Update;
    End;

    Result := PcbServer.PCBGeometricPolygonFactory;

    For Y := 0 To aImage.Picture.Height Do
    Begin
        If ProgressBar <> Nil Then
        Begin
            ProgressBar.Position := Y;
            ProgressBar.Update;
        End;

        For X :=  0 To aImage.Picture.Width Do
        Begin
            FlipCoords(aImage.Picture, X, Y, tX, tY);
            AddPixelToUnion(tX, tY, FBaseX, FBaseY, FPixelSize, aImage.Canvas.Pixels[X, Y], Result);
        End;
     End;
End;
{......................................................................................................................}

{......................................................................................................................}
Function ConstructGeometricPolygonFromPicture_UnionOptimized1(aImage : TImage; ProgressBar : TProgressBar; StatusBar : TStatusBar) : IPCB_GeometricPolygon;
Var
   X, Y               : Integer;
   tX, tY             : Integer;
   UnionGeometricPolygon    : IPCB_GeometricPolygon;
   RowUnionGeometricPolygon : IPCB_GeometricPolygon;
Begin
    If ProgressBar <> Nil Then
        ProgressBar.Max := aImage.Picture.Height;

    If StatusBar <> Nil Then
    Begin
        StatusBar1.SimpleText := ' Converting Pixels To Contours...';
        StatusBar1.Update;
    End;

    UnionGeometricPolygon := PcbServer.PCBGeometricPolygonFactory;

    For Y := 0 To aImage.Picture.Height Do
    Begin
        If ProgressBar <> Nil Then
        Begin
            ProgressBar1.Position := Y;
            ProgressBar1.Update;
        End;

        RowUnionGeometricPolygon := PcbServer.PCBGeometricPolygonFactory;

        For X :=  0 To aImage.Picture.Width Do
        Begin
            FlipCoords(aImage.Picture, X, Y, tX, tY);
            AddPixelToUnion(tX, tY, FBaseX, FBaseY, FPixelSize, aImage.Canvas.Pixels[X, Y], RowUnionGeometricPolygon);
        End;

        PCBServer.PCBContourUtilities.ClipSetSet(eSetOperation_Union, UnionGeometricPolygon, RowUnionGeometricPolygon, UnionGeometricPolygon);
     End;

     Result := UnionGeometricPolygon;
End;
{......................................................................................................................}

{......................................................................................................................}
Function ConstructGeometricPolygonFromPicture_UnionOptimized2(aImage : TImage; ProgressBar : TProgressBar; StatusBar : TStatusBar) : IPCB_GeometricPolygon;
Var
   I, J, K        : Integer;
   X, Y           : Integer;
   tX, tY         : Integer;
   TempGeometricPolygon : IPCB_GeometricPolygon;
   PixelContour   : IPCB_Contour;
Begin
    If ProgressBar <> Nil Then
        ProgressBar.Max := aImage.Picture.Height * 2;

    Result := PcbServer.PCBGeometricPolygonFactory;

    For K := 0 To 3 Do
    Begin
        TempGeometricPolygon := PcbServer.PCBGeometricPolygonFactory;

        If StatusBar <> Nil Then
        Begin
            StatusBar.SimpleText  := ' Converting Pixels To Contours...';
            StatusBar.Update;
        End;

        For J := 0 To aImage.Picture.Height Div 2 Do
        Begin
            If ProgressBar <> Nil Then
            Begin
                ProgressBar.Position := ProgressBar.Position + 1;
                ProgressBar.Update;
            End;

            For I := 0 To aImage.Picture.Width Div 2 Do
            Begin
                X := I * 2 + K Mod 2;
                Y := J * 2 + K Div 2;

                FlipCoords(aImage.Picture, X, Y, tX, tY);

                If (X < aImage.Picture.Width) And (Y < aImage.Picture.Height) Then
                Begin
                  PixelContour := CreateContourFromPixel(tX, tY, FBaseX, FBaseY, FPixelSize, aImage.Canvas.Pixels[X, Y]);
                  TempGeometricPolygon.AddContour(PixelContour);
                End;
            End;

        End;

        If StatusBar <> Nil Then
        Begin
            StatusBar.SimpleText := ' Creating Union...';
            StatusBar.Update;
        End;

        PCBServer.PCBContourUtilities.ClipSetSet(eSetOperation_Union, TempGeometricPolygon, Result, Result);
    End;
End;
{......................................................................................................................}
