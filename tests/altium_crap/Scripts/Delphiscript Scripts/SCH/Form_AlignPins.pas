Procedure TForm1.RadioGroup_AlignHorizontallyClick(Sender: TObject);
Begin
    If Sender = RadioGroup_AlignVertically Then
        RadioGroup_AlignHorizontally.ItemIndex := -1
    Else
    If Sender = RadioGroup_AlignHorizontally Then
        RadioGroup_AlignVertically.ItemIndex := -1;
End;
{..............................................................................}

{..............................................................................}
Procedure GetAveXYMinXY(Const AList : TList;
                        Var   AveX  : TCoord;
                        Var   AveY  : TCoord;
                        Var   MinX  : TCoord;
                        Var   MinY  : TCOord);
Var
    Pin : ISch_Pin;
    i   : Integer;
Begin
    AveX := 0;
    AveY := 0;
    MinX := 10000000;
    MinY := 10000000;
    For i := 0 To AList.Count - 1 Do
    Begin
        Pin  := AList.Items[i];
        AveX := AveX + Pin.Location.x;
        AveY := AveY + Pin.Location.y;
        If Pin.Location.x < MinX Then MinX := Pin.Location.x;
        If Pin.Location.y < MinY Then MinY := Pin.Location.y;
    End;
    AveX := AveX Div AList.Count;
    AveY := AveY Div AList.Count;
End;
{..............................................................................}

{..............................................................................}
Procedure Allign_VerticallyLeft(Const AList : TList);
Var
    Pin  : ISch_Pin;
    AveX : TCoord;
    AveY : TCoord;
    MinX : TCoord;
    MinY : TCoord;
    x,y  : TCoord;
    i    : Integer;
    Loc  : TLocation;
Begin
    GetAveXYMinXY(AList,AveX,AveY,MinX,MinY);
    AveX := MILsToCoord(Trunc(CoordToMILs(AveX)));
    AveY := MILsToCoord(Trunc(CoordToMILs(AveY)));
    x    := AveX;
    y    := MinY;
    For i := 0 To AList.Count - 1 Do
    Begin
        Pin  := AList.Items[i];
        Pin.Orientation := eRotate180;
        Loc := Point(x,y);
        Pin.Location := Loc;
        Dec(y,MILsToCoord(100));
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure Allign_VerticallyRight(Const AList : TList);
Var
    Pin  : ISch_Pin;
    AveX : TCoord;
    AveY : TCoord;
    MinX : TCoord;
    MinY : TCoord;
    x,y  : TCoord;
    i    : Integer;
    Loc  : TLocation;
Begin
    GetAveXYMinXY(AList,AveX,AveY,MinX,MinY);
    AveX := MILsToCoord(Trunc(CoordToMILs(AveX)));
    AveY := MILsToCoord(Trunc(CoordToMILs(AveY)));
    x    := AveX;
    y    := MinY;
    For i := 0 To AList.Count - 1 Do
    Begin
        Pin  := AList.Items[i];
        Pin.Orientation := eRotate0;
        Loc := Point(x,y);
        Pin.Location := Loc;
        Dec(y,MILsToCoord(100));
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure Allign_HorizontallyUp(Const AList : TList);
Var
    Pin  : ISch_Pin;
    AveX : TCoord;
    AveY : TCoord;
    MinX : TCoord;
    MinY : TCoord;
    x,y  : TCoord;
    i    : Integer;
    Loc  : TLocation;
Begin
    GetAveXYMinXY(AList,AveX,AveY,MinX,MinY);
    AveX := MILsToCoord(Trunc(CoordToMILs(AveX)));
    AveY := MILsToCoord(Trunc(CoordToMILs(AveY)));
    x    := MinX;
    y    := AveY;
    For i := 0 To AList.Count - 1 Do
    Begin
        Pin  := AList.Items[i];
        Pin.Orientation := eRotate90;
        Loc := Point(x,y);
        Pin.Location := Loc;
        Inc(x,MILsToCoord(100));
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure Allign_HorizontallyDown(Const AList : TList);
Var
    Pin  : ISch_Pin;
    AveX : TCoord;
    AveY : TCoord;
    MinX : TCoord;
    MinY : TCoord;
    x,y  : TCoord;
    i    : Integer;
    Loc  : TLocation;
Begin
    GetAveXYMinXY(AList,AveX,AveY,MinX,MinY);
    AveX := MILsToCoord(Trunc(CoordToMILs(AveX)));
    AveY := MILsToCoord(Trunc(CoordToMILs(AveY)));
    x    := MinX;
    y    := AveY;
    For i := 0 To AList.Count - 1 Do
    Begin
        Pin  := AList.Items[i];
        Pin.Orientation := eRotate270;
        Loc := Point(x,y);
        Pin.Location := Loc;
        Inc(x,MILsToCoord(100));
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm1.Button_OKClick(Sender: TObject);
Var
    DocType      : WideString;
    SchComponent : ISch_Component;
    SchLib       : ISch_Lib;
    SchIterator  : ISch_Iterator;
    PinIterator  : ISch_Iterator;
    Pin          : ISch_Pin;
    SelectedPins : TList;
Begin
    Client.StartServer('SCH');

    // Obtain the Client interface so can get the Kind property.
    DocType := UpperCase(Client.CurrentView.OwnerDocument.Kind);
    If DocType <> 'SCHLIB' Then Exit;
    If SchServer = Nil     Then Exit;

    SchLib := SchServer.GetCurrentSchDocument;
    If SchLib = Nil Then Exit;

    SelectedPins := TList.Create;
    Try
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
                Try
                    Pin := PinIterator.FirstSchObject;
                    While Pin <> Nil Do
                    Begin
                        If Pin.Selection Then SelectedPins.Add(Pin);
                        Pin := PinIterator.NextSchObject;
                    End;
                Finally
                    SchComponent.SchIterator_Destroy(PinIterator);
                End;
                SchComponent := SchIterator.NextSchObject;
            End;
        Finally
            SchLib.SchIterator_Destroy(SchIterator);
        End;
        If SelectedPins.Count = 0 Then Exit;

        If RadioGroup_AlignVertically.ItemIndex <> -1 Then
        Begin
            Case RadioGroup_AlignVertically.ItemIndex Of
                0 : Allign_VerticallyRight(SelectedPins);
                1 : Allign_VerticallyLeft (SelectedPins);
            End;
        End
        Else
        If RadioGroup_AlignHorizontally.ItemIndex <> -1 Then
        Begin
            Case RadioGroup_AlignHorizontally.ItemIndex Of
                0 : Allign_HorizontallyUp  (SelectedPins);
                1 : Allign_HorizontallyDown(SelectedPins);
            End;
        End;
    Finally
        SelectedPins.Free;
    End;
    SchLib.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
Procedure TForm1.Form1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
    If Key = 27 {VK_ESCAPE} Then
    Begin
        Key := 0;
        Close;
    End;
End;

