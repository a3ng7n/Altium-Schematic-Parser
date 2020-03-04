{..............................................................................}
{ Converts bitmap image into PCB regions, addaped from 'Scan Pixel Algorithm'  }
{ Version 1.3 script by  Paul D. Fincato (fincato@infinet.com)                 }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
   FPixelSize     : TCoord;
   FBaseX         : TCoord;
   FBaseY         : TCoord;
   CanChangeScale : Boolean;
   Board          : IPCB_Board;
{......................................................................................................................}

{......................................................................................................................}
Procedure SetState_ControlsEnable(Enabled : Boolean);
Begin
    ConverterForm.XEdit.Enabled                      := Enabled;
    ConverterForm.YEdit.Enabled                      := Enabled;
    ConverterForm.ComboBoxLayers.Enabled             := Enabled;
    ConverterForm.CreateComponentRadioButton.Enabled := Enabled;
    ConverterForm.CreateFreeRadioButton.Enabled      := Enabled;

    DoUnionRadioButton.Enabled                       := Enabled;
    DontUnionRadioButton.Enabled                     := Enabled;

    ConverterForm.NegativeCheckBox.Enabled           := Enabled;
    ConverterForm.FlipHorCheckBox.Enabled            := Enabled;
    ConverterForm.FlipVertCheckBox.Enabled           := Enabled;

    ConverterForm.PixelSizeEdit.Enabled              := Enabled;
    ConverterForm.HeightEdit.Enabled                 := Enabled;
    ConverterForm.WidthEdit.Enabled                  := Enabled;

    ConverterForm.ConvertButton.Enabled              := Enabled;
    ConverterForm.SelectButton.Enabled               := Enabled;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure RunConverterScript;
Begin
    If PCBServer = Nil Then
        Client.StartServer('PCB');

    If PCBServer.GetCurrentPCBBoard = Nil Then
        CreateNewDocumentFromDocumentKind('PCB');

    Board := PCBServer.GetCurrentPCBBoard;

    SetupComboBoxFromLayer(ConverterForm.ComboBoxLayers, Board);

    // Set the default layer to Top layer.
    ConverterForm.ComboBoxLayers.ItemIndex := 0;

    CanChangeScale := False;
    SetState_ControlsEnable(False);
    ConverterForm.ShowModal;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure SetState_EditControls(Edit : TEdit);
Begin
     If Edit = Nil Then
     Begin
          SetState_EditControls(PixelSizeEdit);
          SetState_EditControls(WidthEdit);
          SetState_EditControls(HeightEdit);
          SetState_EditControls(XEdit);
          SetState_EditControls(YEdit);
     End;

     If Edit = PixelSizeEdit Then
        PixelSizeEdit.Text := CoordUnitToString(FPixelSize, eImperial)
     Else If Edit = WidthEdit Then
     Begin
          If Image1.Picture <> Nil Then
             WidthEdit.Text := CoordUnitToString(Abs(Image1.Picture.Width * FPixelSize), eImperial);
     End
     Else If Edit = HeightEdit Then
     Begin
          If Image1.Picture <> Nil Then
             HeightEdit.Text := CoordUnitToString(Abs(Image1.Picture.Height * FPixelSize), eImperial);
     End
     Else If Edit = XEdit Then
          XEdit.Text := CoordUnitToString(FBaseX, eImperial)
     Else If Edit = YEdit Then
          YEdit.Text := CoordUnitToString(FBaseY, eImperial);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.XEditChange(Sender : TObject);
Var
   DummyUnit : TUnit;
Begin
     StringToCoordUnit(XEdit.Text, FBaseX, DummyUnit);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.YEditChange(Sender : TObject);
Var
   DummyUnit : TUnit;
Begin
     StringToCoordUnit(YEdit.Text, FBaseY, DummyUnit);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.WidthEditChange(Sender : TObject);
Var
   DummyUnit : TUnit;
   NewWidth  : TCoord;
Begin
     If Not CanChangeScale Then Exit;

    CanChangeScale := False;
    StringToCoordUnit(WidthEdit.Text, NewWidth, DummyUnit);

    If Image1.Picture.Width <> Nil Then
       FPixelSize := NewWidth / Image1.Picture.Width;

    SetState_EditControls(HeightEdit);
    SetState_EditControls(PixelSizeEdit);

    CanChangeScale := True;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.HeightEditChange(Sender : TObject);
Var
   DummyUnit : TUnit;
   NewHeight : TCoord;
Begin
    If Not CanChangeScale Then Exit;

    CanChangeScale := False;
    StringToCoordUnit(HeightEdit.Text, NewHeight, DummyUnit);

    If Image1.Picture.Height <> Nil Then
       FPixelSize := NewHeight / Image1.Picture.Height;

    SetState_EditControls(WidthEdit);
    SetState_EditControls(PixelSizeEdit);

    CanChangeScale := True;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.PixelSizeEditChange(Sender : TObject);
Var
   DummyUnit : TUnit;
Begin
    If Not CanChangeScale Then Exit;

    CanChangeScale := False;
    StringToCoordUnit(PixelSizeEdit.Text, FPixelSize, DummyUnit);

    SetState_EditControls(WidthEdit);
    SetState_EditControls(HeightEdit);

    CanChangeScale := True;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.LoadButtonClick(Sender: TObject);
Begin
    If OpenPictureDialog1.Execute then
    Begin
        XPProgressBar1.Position := 0;
        XStatusBar1.SimpleText  := '  Loading...';
        XStatusBar1.Update;

        Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);

        SetState_ControlsEnable(True);
        ConvertButton.Enabled := True;
        LoadButton.Enabled    := True;

        XStatusBar1.SimpleText := '  Ready...';
        XStatusBar1.Update;

        ImageSizeLabel.Caption := IntToStr(Image1.Picture.Width) + 'x' + IntToStr(Image1.Picture.Height);

        CanChangeScale := True;
        FPixelSize := MilsToCoord(1);
        FBaseX     := MilsToCoord(1000);
        FBaseY     := MilsToCoord(1000);

        SetState_EditControls(Nil);
    End;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure CreateFreeRegionsFromImage(AImage : TImage; Board : IPCB_Board; Layer : TLayer);
Begin
     If Board = Nil Then Exit;

     PCBServer.PreProcess;
     If DoUnionRadioButton.Checked Then
         CreateRegionsFromPicture_Union(AImage, Layer, Board, XPProgressBar1, XStatusBar1)
     Else
         CreateRegionsFromPicture_NoUnion(AImage, Layer, Board, XPProgressBar1, XStatusBar1);
     PCBServer.PostProcess;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure CreateComponentFromImage(AImage : TImage; Board : IPCB_Board; Layer : TLayer);
Var
   I         : Integer;
   Component : IPCB_Component;
Begin
     If Board = Nil Then Exit;

     PCBServer.PreProcess;
     Component := PCBServer.PCBObjectFactory(eComponentObject, eNoDimension, eCreate_Default);
     Component.X := FBaseX;
     Component.Y := FBaseY;
     Component.NameOn := False;

     If DoUnionRadioButton.Checked Then
         CreateRegionsFromPicture_Union(AImage, Layer, Component, XPProgressBar1, XStatusBar1)
     Else
         CreateRegionsFromPicture_NoUnion(AImage, Layer, Component, XPProgressBar1, XStatusBar1);

     Board.AddPCBObject(Component);
     PCBServer.PostProcess;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.ConvertButtonClick(Sender: TObject);
Var
    Layer             : TLayer;
    OldCursor         : TCursor;
    OldDoOnlineDRC    : Boolean;
Begin
    OldCursor     := Screen.Cursor;
    Screen.Cursor := crHourGlass;

    OldDoOnlineDRC := PCBServer.SystemOptions.DoOnlineDRC;
    PCBServer.SystemOptions.DoOnlineDRC := False;

    XStatusBar1.SimpleText := 'Registering Regions...';
    XStatusBar1.Update;

    SetState_Negative(NegativeCheckBox.Checked);
    SetState_DoFlipH(FlipHorCheckBox.Checked);
    SetState_DoFlipV(FlipVertCheckBox.Checked);

    Layer := String2Layer(ComboBoxLayers.Items[ComboBoxLayers.ItemIndex]);
    Board.LayerIsDisplayed[Layer] := True;

    If CreateComponentRadioButton.Checked Then
        CreateComponentFromImage(Image1, Board, Layer)
    Else
        CreateFreeRegionsFromImage(Image1, Board, Layer);

    Screen.Cursor := OldCursor;
    PCBServer.SystemOptions.DoOnlineDRC := OldDoOnlineDRC;

    XStatusBar1.SimpleText := ' Done...';
    XStatusBar1.Update;

    XPProgressBar1.Position := 0;
    XPProgressBar1.Update;

    Board.GraphicallyInvalidate;
    Board.GraphicalView_ZoomOnRect(FBaseX, FBaseY,  FBaseX + FPixelSize * Image1.Picture.Width, FBaseY + FPixelSize * Image1.Picture.Height);

    Close;
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.ExitButtonClick(Sender: TObject);
Begin
    Close;
End;
{......................................................................................................................}

{......................................................................................................................}

{......................................................................................................................}
Procedure TConverterForm.SelectButtonClick(Sender: TObject);
Var
   X, Y : TCoord;
begin
     Self.Hide;
     If Board.ChooseLocation(x, y, 'Choose Insertion Location') Then
     Begin
          FBaseX := X;
          FBaseY := Y;

          SetState_EditControls(XEdit);
          SetState_EditControls(YEdit);
     End;
     Self.Show;
End;
{......................................................................................................................}


