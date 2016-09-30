{..............................................................................}
{ Summary PCB Hole Size Editor version 1.2                                     }
{                                                                              }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
  HoleSizeEditorForm     : THoleSizeEditorForm;

  Board                  : IPCB_Board;

  // when iterating thru the board, these lists have original hole sizes.
  // these pad and via lists are ordered and never modified.
  // values always a TCOORD.
  PadHoleSizeList        : TStringList;
  ViaHoleSizeList        : TStringList;

  // these dynamic lists are used when holesizes being changed.
  // these lists do not accept duplicates. values always a TCOORD.
  PadDynamicHoleSizeList : TStringList;
  ViaDynamicHoleSizeList : TStringList;

  // dynamicholesizelist (that do not accept duplicates) updates the listbox on
  // the form. dynamicholesize list has TCOORD values and converted on the fly to Real units
  // when updating the ListBox on the form.
  DynamicHoleSizeList    : TStringList;

  // Current Unit determined by MIl or MM setting (DisplayUnit).
  CurrentUnit            : TUnit;
  PadCount               : Integer;
  ViaCount               : Integer;

{..............................................................................}

{..............................................................................}
Procedure SortList(StringList : TStringList);
Begin
    QuickSort(StringList, 0, StringList.Count - 1)
End;
{..............................................................................}

{..............................................................................}
Function DetermineUnit(Dummy : Integer = 0) : TUnit;
Begin
    If HoleSizeEditorForm.rgUnit.ItemIndex = 0 Then
        Result := eImperial
    Else
        Result := eMetric;

    //PCBAPI mixes up its DisplayUnit ! Temp workaround
    If Result = eImperial Then Result  := eMetric
                           Else Result := eImperial;
End;
{..............................................................................}

{..............................................................................}
Function UpdateHoleSizeListWithUniqueHoleSizes(AHoleSizeList : TStringList; WhichObject : TObjectId) : Integer;
Var
    Iterator    : IPCB_BoardIterator;
    Item        : IPCB_Primitive;
    AString     : TString;
    Found       : Boolean;
    I           : Integer;
    NoOfEntries : Integer;
    S           : TString;
    C           : TCoord;
    HoleSize    : TCoord;
Begin
    Result := 0;
    Iterator := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(WhichObject));
    Iterator.AddFilter_LayerSet (AllLayers);
    Iterator.AddFilter_Method   (eProcessAll);

    Try
        Item := Iterator.FirstPCBObject;
        While Item <> Nil Do
        Begin
            // insert coord values in the original holesizelist
            // duplicates not permitted.
            AHolesizeList.Add(IntToStr(Item.holesize));

            Inc(Result);
            Item := Iterator.NextPCBObject;
        End;

        SortList(AHoleSizeList);

    Finally
        PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(Iterator);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure RefreshGroupDynamicList(Dummy : Integer = 0);
Begin
    DynamicHoleSizeList.Clear;
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateGroupDynamicList(DynamicList : TStringList);
Var
    I : Integer;
Begin
    If DynamicList.Count = 0 Then Exit;

    For I := 0 to DynamicList.Count - 1 Do
        DynamicHoleSizeList.Add(DynamicList.Strings[I]);

    SortList(DynamicHoleSizeList);
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateListBoxFromGroupDynamicList(Dummy : Integer = 0);
Var
    ANewString : TString;
    ACoord     : TCoord;
    I          : Integer;
Begin
    ListBox1.Clear;
    If DynamicHoleSizeList.Count = 0 Then Exit;

    For I := 0 To DynamicHoleSizeList.Count - 1 Do
    Begin
        ACoord  := StrToInt(DynamicHoleSizeList.Strings[I]);

        //Convert Coord string to Real Unit value depending on Unit.
        ANewString := RealUnitToString(ACoord,DetermineUnit);
        ListBox1.Items.Insert(I,ANewString);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateObjectsWithNewHoleSizesOnBoard(WhichObject : TObjectId; OldSize, NewSize : TString);
Var
    Iterator  : IPCB_BoardIterator;
    Primitive : IPCB_Primitive;
    HoleSize  : TCoord;
Begin
    Iterator := PCBServer.GetCurrentPCBBoard.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(WhichObject));
    Iterator.AddFilter_LayerSet (AllLayers);
    Iterator.AddFilter_Method   (eProcessAll);

    Primitive := Iterator.FirstPCBObject;
    While Primitive <> Nil Do
    Begin
        If Primitive.HoleSize = OldSize Then
        Begin
            PCBServer.SendMessageToRobots(Primitive.I_ObjectAddress,c_Broadcast,PCBM_BeginModify ,c_NoEventData);
            Primitive.HoleSize := NewSize;
            PCBServer.SendMessageToRobots(Primitive.I_ObjectAddress,c_Broadcast,PCBM_EndModify ,c_NoEventData);
        End;

        Primitive := Iterator.NextPCBObject;
    End;
    PCBServer.GetCurrentPCBBoard.BoardIterator_Destroy(Iterator);
End;
{..............................................................................}

{..............................................................................}
Procedure UpdateBoard(Dummy : Integer = 0);
Var
    OldHoleSize : TCoord;
    NewHoleSize : TCoord;
    I           : Integer;
Begin
    // Check if HoleSizeLists are not empty.
    If ListBox1.Items.Count = 0 Then Exit;

    BeginHourGlass;
    Try
        PCBServer.PreProcess;

        // If Pad option is enabled, then pads on the board will be updated whether
        // their holesizes have changed or not.
        If HoleSizeEditorForm.cbPads.GetChecked Then
        Begin
            For I := 0 To PadHoleSizeList.Count - 1 Do
            Begin
                OldHoleSize := StrToInt(PadHoleSizeList.Strings[i]);
                NewHoleSize := StrToInt(PadDynamicHoleSizeList.Strings[i]);
                If OldHoleSize <> NewHoleSize Then
                    UpdateObjectsWithNewHoleSizesOnBoard(ePadObject,OldHoleSize,NewHoleSize);
            End;
        End;

        // If Via option is enabled, then vias on the board will be updated whether
        // their holesizes have changed or not.
        If HoleSizeEditorForm.cbVias.GetChecked Then
        Begin
            For I := 0 To ViaHoleSizeList.Count - 1 Do
            Begin
                OldHoleSize := StrToInt(ViaHoleSizeList.Strings[i]);
                NewHoleSize := StrToInt(ViaDynamicHoleSizeList.Strings[i]);
                If OldHoleSize <> NewHoleSize Then
                    UpdateObjectsWithNewHoleSizesOnBoard(eViaObject,OldHoleSize,NewHoleSize);
            End;
        End;
    Finally
        PCBServer.PostProcess;
    End;
    EndHourGlass;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessObjectDynamicList(OldC,NewC : TCoord; OriginalList, Dynamiclist : TStringList);
Var
    I,J              : Integer;
    Holder           : Integer;
Begin
    For J := 0 to OriginalList.Count - 1 Do
    Begin
        // Check if old size matches item in ObjectDynamicLisst
        If OldC = StrToInt(OriginalList.Strings[J]) Then
        Begin
           // Update ObjectDynamicHoleSizeList by replacing the original value with the new value.
           // but this ObjectDynamicHoleSizeList is not sorted because it is compared with the OriginalHoleSizeList
           // when updating the PCB board... Only Coord values not Real Units in lists!
           I := 0;
           Holder := DynamicList.Count;
           While I < Holder Do
           Begin
               If OldC = StrToInt(DynamicList.Strings[I]) Then
               Begin
                   DynamicList.Delete(I);
                   DynamicList.Insert(I,IntToStr(NewC));
               End;
               Inc(I);
           End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure RunEditHoleSizeForm(Dummy : Integer = 0);
Var
    OriginalHoleSize : TPCBString;
    NewHoleSize      : TPCBString;
    OldC, NewC       : TCoord;
Begin
    If ListBox1.Count = 0 Then Exit;
    If ListBox1.ItemIndex < 0 Then
    Begin
        ShowWarning('Please select a holesize first!');
        Exit;
    End;

    OriginalHoleSize                    := ListBox1.Items[ListBox1.ItemIndex];
    EditHoleSizeForm.eEditHoleSize.Text := OriginalHoleSize;
    EditHoleSizeForm.ShowModal;

    If EditHoleSizeForm.bOk.ModalResult = mrOk Then
    Begin
        NewHoleSize := EditHoleSizeForm.eEditHoleSize.Text;
        If NewHoleSize = '' Then Exit;

        StringToCoordUnit(OriginalHoleSize,OldC,DetermineUnit);
        StringToCoordUnit(NewHoleSize,NewC,DetermineUnit);

        //cannot edit 0 mil at this stage...
        //check if OriginalHoleSize string is not 0mil/0mm and NewC is 0
        // which means NewHoleSize value is illegal.
        If DetermineUnit = eImperial Then
             If Not(UpperCase(OriginalHoleSize) = '0MIL') and (NewC = 0) Then Exit
        Else
             If Not(UpperCase(OriginalHoleSize) = '0MM')  and (NewC = 0) Then Exit;


        Try
            RefreshGroupDynamicList;

            If cbPads.GetChecked Then
            Begin
                ProcessObjectDynamicList(OldC,NewC,PadHoleSizeList,PadDynamicHoleSizeList);
                UpdateGroupDynamicList(PadDynamicHoleSizeList);
            End;

            If cbVias.GetChecked Then
            Begin
                ProcessObjectDynamicList(OldC,NewC,ViaHoleSizeList,ViaDynamicHoleSizeList);
                UpdateGroupDynamicList(ViaDynamicHoleSizeList);
            End;
        Finally
            UpdateListBoxFromGroupDynamicList;
        End;
    End;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.bEditClick(Sender: TObject);
Begin
    RunEditHoleSizeForm;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.ListBox1DblClick(Sender: TObject);
Begin
    RunEditHoleSizeForm;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.bOKClick(Sender: TObject);
Begin
    UpdateBoard;
    Close;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.bCancelClick(Sender: TObject);
Begin
    Close;
End;
{.........................................................................................}

{.........................................................................................}
Procedure UpdateListBoxFromPadViaOption(Dummy : Integer = 0);
Var
    I : Integer;
Begin
    RefreshGroupDynamicList;

    If Not(cbPads.GetChecked) and Not(cbVias.GetChecked) then
        HoleSizeEditorForm.lHoleSizeCount.Caption := 'Pads : 0 ' + '   Vias : 0';

    If (cbPads.GetChecked) and (cbVias.GetChecked) Then
    Begin
        UpdateGroupDynamicList(PadDynamicHolesizeList);
        UpdateGroupDynamicList(ViaDynamicHolesizeList);
        HoleSizeEditorForm.lHoleSizeCount.Caption := 'Pads : ' + IntToStr(PadCount) + '   Vias : ' + IntToStr(ViaCount);
    End;

    If (cbPads.GetChecked) and Not(cbVias.GetChecked) Then
    Begin
        UpdateGroupDynamicList(PadDynamicHolesizeList);
        HoleSizeEditorForm.lHoleSizeCount.Caption := 'Pads : ' + IntToStr(PadCount) + '   Vias : 0';
    End;

    If Not(cbPads.GetChecked) and (cbVias.GetChecked) Then
    Begin
        UpdateGroupDynamicList(ViaDynamicHolesizeList);
        HoleSizeEditorForm.lHoleSizeCount.Caption := 'Pads : 0 ' + '   Vias : ' + IntToStr(ViaCount);
    End;

    UpdateListBoxFromGroupDynamicList;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.cbPadsClick(Sender: TObject);
Begin
    UpdateListBoxFromPadViaOption;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.cbViasClick(Sender: TObject);
Begin
    UpdateListBoxFromPadViaOption;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.rgUnitClick(Sender: TObject);
Var
    I          : Integer;
    NewUnit    : TUnit;
    OldUnit    : TUnit;
    AString    : TString;
    ANewString : TString;
    C          : TCoord;
    Holder     : Integer;
Begin
    // TUnit = (eMetric,eImperial)
    If rgUnit.ItemIndex = 0 Then
    Begin
        OldUnit := eImperial;
        NewUnit := eMetric;
    End
    Else
    Begin
        OldUnit := eMetric;
        NewUnit := eImperial;
    End;

    //Use DynamicHoleSizeList and NewUnit to update the ListBox...
    UpdateListBoxFromGroupDynamicList(NewUnit);
End;
{.........................................................................................}

{.........................................................................................}
Procedure SetupLists(Dummy : Integer = 0);
Begin
    // Setup dynamic holesize list for updating the ListBox
    DynamicHoleSizeList            := TStringList.Create;
    DynamicHoleSizeList.Sorted     := True;
    DynamicHoleSizelist.Duplicates := dupIgnore;


    // Setup pad and via holesizes.
    PadHoleSizeList := TStringList.Create;
    ViaHoleSizeList := TStringList.Create;

    PadDynamicHoleSizeList := TStringList.Create;
    ViaDynamicHoleSizeList := TStringList.Create;

    PadHoleSizeList.Sorted := True;
    ViaHoleSizeList.Sorted := True;

    PadDynamicHoleSizeList.Sorted := False;
    ViaDynamicHoleSizeList.Sorted := False;

    PadHolesizeList.Duplicates := dupIgnore;
    ViaHolesizeList.Duplicates := dupIgnore;


    // dynamic list changing, and duplicates permitted
    // consider this scenario
    // since hole1 and hole2 originally different
    // user changes hole2 and it matches hole1 size.
    // but when borad is updated, need to update hole1 and hole2!
    PadDynamicHoleSizeList.Duplicates  := dupAccept;
    ViaDynamicHoleSizeList.Duplicates  := dupAccept;
End;
{.........................................................................................}

{.........................................................................................}
Procedure FreeLists(Dummy : Integer = 0);
Begin
    PadHoleSizeList.Free;
    ViaHoleSizeList.Free;
    PadDynamicHoleSizeList.Free;
    ViaDynamicHoleSizeList.Free;

    DynamicHoleSizeList.Free;
End;
{.........................................................................................}

{.........................................................................................}
Procedure RunHoleSizeEditor;
Var
    I : Integer;
    S,S1 : TString;

Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        ShowWarning('This document is not a PCB document!' + #13 +
                    'therefore HoleSize Editor script will not be executed.');
        Exit;
    End;

    BeginHourGlass;
    SetupLists;

    // TUnit = (eMetric,eImperial)
    CurrentUnit := PCBServer.GetCurrentPCBBoard.DisplayUnit;

    // currently DisplayUnit is the otherway around!!! Bug in PCB API
    If CurrentUnit = eImperial Then CurrentUnit := eMetric
                               Else CurrentUnit := eImperial;


    If CurrentUnit = eImperial Then
        HoleSizeEditorForm.rgUnit.ItemIndex := 0
    Else
        HoleSizeEditorForm.rgUnit.ItemIndex := 1;

    // Collect pad and via holesizes from the board and store unique holesizes
    // in Pad and Via holesize lists.
    // Ther are two sets of holesize lists for Pads and Vias...
    // Original Lists and Dynamic Lists. Dynamic lists store edited holesizes.
    PadCount := UpdateHoleSizeListWithUniqueHoleSizes(PadHoleSizeList,ePadObject);
    ViaCount := UpdateHoleSizeListWithUniqueHoleSizes(ViaHoleSizeList,eViaObject);

    // DynamicLists always have Coord Values. The Listbox has Real Units
    //   depending on the Form's Mil or MM radiobutton status.

    // Transfer original object holesizes to dynamic object holesize lists.
    PadDynamicHoleSizeList.Text := PadHoleSizeList.Text;
    ViaDynamicHoleSizeList.Text := ViaHoleSizeList.Text;

    RefreshGroupDynamicList;
    UpdateGroupDynamicList(PadDynamicHoleSizeList);
    UpdateGroupDynamicList(ViaDynamicHoleSizeList);
    UpdateListBoxFromGroupDynamicList;
    EndHourGlass;

    HoleSizeEditorForm.lHoleSizeCount.Caption := 'Pads : ' + IntToStr(PadCount) + '   Vias : ' + IntToStr(ViaCount);
    HoleSizeEditorForm.ShowModal;

    FreeLists;
End;
{.........................................................................................}

{.........................................................................................}
Procedure THoleSizeEditorForm.ListBox1Click(Sender: TObject);
Var
    UnitString       : TPCBString;
    PadHoleSizeExist : Boolean;
    ViaHoleSizeExist : Boolean;
Begin
    //update holesize count depending on pad and via options
    UnitString := ListBox1.Items[ListBox1.ItemIndex];
    
    // check if unitstring exists in
(*
    If cbPads.Checked Then
        PadCount :=

    If cbVias.Checked Then
        ViaCount :=
*)
    HoleSizeEditorForm.lHoleSizeCount.Caption := 'Pads : ' + IntToStr(PadCount) + '   Vias : ' + IntToStr(ViaCount);
End;
{.........................................................................................}

{.........................................................................................}
End.
