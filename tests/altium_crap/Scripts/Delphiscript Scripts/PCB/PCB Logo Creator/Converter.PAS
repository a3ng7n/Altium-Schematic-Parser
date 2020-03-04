{..............................................................................}
{ Summary																							 }
{   Converts a monochrome image as a PCB Logo into a series of thin				 }
{   PCB tracks that can be placed on a PCB document as a logo.						 }
{																										 }	
{ Copyright (c) 2008 by Altium Limited                                         }
{																					                }
{ Version 1.5                                        									 }
{		                                        											 }
{ Changes For Version 1.5                                        					 }
{    - Fix off by one errors accessing Canvas.Pixels                           }
{    - Make more tolerant of non-monochrome images, now tracks are created at  }
{      the boundary of white and non-white pixels                              }
{    - Use user customized layer names                                         }
{..............................................................................}

Var
   gvBoard : IPCB_Board;

{......................................................................................................................}
Procedure RunConverterScript;
Begin
    ConverterForm.ShowModal;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure PlaceATrack(ABoard : IPCB_Board; X1,Y1,X2,Y2 : TCoord; ALayer : TLayer, AWidth : Float);
Var
    PCBTrack    : IPCB_Track;
    Sheet       : IPCB_Sheet;
    OffSet      : TCoord;
Begin
    // obtain the coordinates of the sheet so can place logo within the board
    Sheet  := ABoard.PCBSheet;
    OffSet := MilsToCoord(100);

    // place a new track on the blank PCB
    PCBTrack       := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    PCBTrack.Width := MilsToCoord(1) * AWidth;

    PCBTrack.X1    := Sheet.SheetX + MilsToCoord(X1) + Offset;
    PCBTrack.Y1    := Sheet.SheetY + MilsToCoord(Y1) + Offset;
    PCBTrack.X2    := Sheet.SheetX + MilsToCoord(X2) + Offset;
    PCBTrack.Y2    := Sheet.SheetY + MilsToCoord(Y2) + Offset;
    PCBTrack.Layer := ALayer;

    ABoard.AddPCBObject(PCBTrack);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure ScalingFactorChange(Dummy : TObject);
Begin
    ConverterForm.lImageSize.Caption := FloatToStr((ConverterForm.Image1.Picture.Width + 1)  * ConverterForm.eScalingFactor.Text) + ' x ' +
                          FloatToStr((ConverterForm.Image1.Picture.Height + 1) * ConverterForm.eScalingFactor.Text) + ' mils';
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.eScalingFactorChange(Sender: TObject);
Begin
     ScalingFactorChange(Nil);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.loadbuttonClick(Sender: TObject);
Var
   I, J : Integer;
Begin
    If OpenPictureDialog1.Execute then
    Begin
        XPProgressBar1.Position := 0;
        XStatusBar1.SimpleText  := '  Loading...';
        XStatusBar1.Update;

        // loading a monochrome bitmap only
        Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);

        // Check if image is monochrome, otherwise prompt a warning
        If Image1.Picture.Bitmap.PixelFormat <> pf1bit Then
        Begin
            For J := 0 to Image1.Picture.Height - 1 Do
                For I := 0 to Image1.Picture.Height - 1 Do
            Begin
                If Image1.Canvas.Pixels[I,J] <> clWhite Then
                    Image1.Canvas.Pixels[I,J] := clBlack;
            End;
        End;

        ScalingFactorChange(Nil);

        convertbutton.Enabled  := True;
        LoadButton.Enabled     := False;
        XStatusBar1.SimpleText := '  Ready...';
        XStatusBar1.Update;
    End;
End;
{......................................................................................................................}

{......................................................................................................................}
procedure TConverterForm.ConverterFormCreate(Sender: TObject);
begin
    // Create a standalone blank PCB document and add the new logo to it
    // from the PCBLogoContainer d.s.
    CreateNewDocumentFromDocumentKind('PCB');

    // GetCurrentPCBBoard returns a IPCB_Board type.
    gvBoard := PCBServer.GetCurrentPCBBoard;

    If gvBoard = Nil Then
    Begin
        ShowWarning('A PCB document is not created properly.');
        ShowModal := mrError;
    End
    Else
        SetupComboBoxFromLayer(ComboBoxLayers, gvBoard);
end;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.convertbuttonClick(Sender: TObject);
Var
    x, y, x1, FlipY, FlipX : Integer;
    PixelColor             : TColor;
    Start                  : Boolean;
    //PCBBoard               : IPCB_Board;
    PCBLayer               : TLayer;
    TrackWidth             : Integer;
Begin
    Screen.Cursor      := crHourGlass;
    XPProgressBar1.Max := Image1.Picture.Height;
    PCBLayer   := GetLayerFromComboBox(ComboBoxLayers, gvBoard);
    TrackWidth := StrToFloat(eScalingFactor.Text);

    // ensure the layer selected is displayed in the PCB workspace
    gvBoard.LayerIsDisplayed[PCBLayer] := True;

    For Y := 0 to Image1.Picture.Height - 1 Do
    Begin
        XPProgressBar1.Position := Y;
        XPProgressBar1.Update;

        XStatusBar1.SimpleText  := ' Converting...';
        XStatusBar1.Update;

        If (cbMirrorY.Checked) Then
            FlipY := Y
        Else
            FlipY := Abs(Y - Image1.Picture.Height - 1);

        FlipY := FlipY * StrToFloat(eScalingFactor.Text);

        // Denotes the start of a line on a row of an image
        Start := False;

        For X := 0 To Image1.Picture.Width Do
        Begin
            If (cbNegative.Checked) Then
                PixelColor := clBlack
            Else
                PixelColor := clWhite;

            If X < Image1.Picture.Width Then
               PixelColor := Image1.Canvas.Pixels[x,y];

            If cbMirrorX.Checked Then
                FlipX := abs(X - Image1.Picture.Width)
            Else
                FlipX  := X;

            FlipX := FlipX * StrToFloat(eScalingFactor.Text);

            If (cbNegative.Checked) Then
            Begin
                Case PixelColor Of
                     clWhite :
                        If Not (Start) Then
                        Begin
                             x1    := FlipX;
                             Start := True;
                        End;

                     Else
                        Begin
                            If (Start) Then
                                PlaceATrack(gvBoard, X1,FlipY,FlipX,FlipY, PCBLayer, TrackWidth);

                            Start := False;
                        End;
                 End;
            End
            Else
            Begin
                Case PixelColor Of
                    clWhite:
                        Begin
                            If (Start) Then
                                PlaceATrack(gvBoard, X1,FlipY,FlipX,FlipY, PCBLayer, TrackWidth);
                            Start := False;
                        End;

                    Else
                        If Not (Start) Then
                        Begin
                            x1    := FlipX;
                            Start := True;
                        End;

                 End;
            End;
        End;
     End;

    Screen.Cursor          := crArrow;
    XStatusBar1.SimpleText := ' Done...';
    XStatusBar1.Update;

    // toggle buttons
    ConvertButton.Enabled := False;
    LoadButton.Enabled    := True;

    // clear out progress bar
    XPProgressBar1.Position := 0;
    XPProgressBar1.Update;

    //clear out image
    Image1.Picture.Bitmap := nil;

    Client.SendMessage('PCB:Zoom', 'Action=All' , 255, Client.CurrentView);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.exitbuttonClick(Sender: TObject);
Begin
    Close;
End;
{......................................................................................................................}

