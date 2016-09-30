{
Update Constraint File script Version 1.0

In brief, you have the ability to change the swap group Ids for the pins of a
FPGA component on the PCB document without invoking the FPGA Pin Swap Manager.
These swap group ids are stored as records in the constraint file
part of a configuration in the FPGA project.

The sequence is as follows;
1) select pads
2) run script (which i would hook to a shortcut key)
3) pops a dialog where you type in a swap group number
4) updates/inserts SWAPID type records in this specified constraint file

if there are multiple configurations for this project...
you have the ability to choose which constraint file that targets the FPGA device.

Major limitation at the moment - for multiple records with same TargetIDs
the first SWAPIDs found for same TargetIDs are replaced only.


A warning dialog is invoked and scirpt is closed if the following conditions are met:
No PCB document
No selected pads on the pcb
No configurations
No Constraint files

Copyright Altium Ltd (c) 2005.
}
{..............................................................................}

{..............................................................................}
Var
    SwapIDValue : String;
    SL          : TStringList;

{..............................................................................}

{..............................................................................}
Procedure UpdateConstraintFilesWithNewGroupSwapIDs;
Var
    ConstraintFileCount : Integer;
    Workspace           : IWorkspace;
    ConfigCount         : Integer;
    Config              : IConfiguration;
    FirstTargetDeviceName : String;
    First               : Boolean;

    Board               : IPCB_Board;
    Pad                 : IPCB_Pad;
    Iterator            : IPCB_BoardIterator;
    I,J                 : Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then
    Begin
        Showmessage('This is not a PCB document!');
        Exit;
    End;

    // Retrieve the configurations and their constraint files.
    Workspace := GetWorkSpace;
    If Workspace = Nil Then Exit;
    Workspace.DM_FocusedProject.DM_Compile;

    ConfigCount := Workspace.DM_FocusedProject.DM_ConfigurationCount;
    If ConfigCount = 0 Then
    Begin
        ShowMessage('No configuration file found!');
        Exit;
    End;

    For I := 0 to ConfigCount - 1 Do
    Begin
        Config := Workspace.DM_FocusedProject.DM_Configurations(I);
        ConstraintFileCount := Config.DM_ConstraintsFileCount;
        If ConstraintFileCount = 0 Then
        Begin
            ShowMessage('No constraint files found!');
            Exit;
        End;

        First := True;
        For J := 0 to ConstraintFileCount - 1 Do
        Begin
            If First Then
            Begin
                First := False;
                FirstTargetDeviceName := Config.DM_GetTargetDeviceName;
            End;

            fUpdate.cbConstraintFiles.items.add(ExtractFileName(Config.DM_ConstraintsFilePath(j)));
        End;
    End;
    fUpdate.cbConstraintFiles.itemindex   := 0;
    fUpdate.labelTargetDeviceName.Caption := 'Target Device Name: ' + FirstTargetDeviceName;

    // Retrieve the board iterator
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Look for selected pads and update the List box
    I := 0;
    Pad := Iterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        If Pad.Selected then
        Begin
            fUpdate.clbPads.Items.Add[Pad.Name];
            fUpdate.clbPads.Checked[i] := True;
            Inc(I);
        End;
        Pad := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    // if pad count zero then exit. whats the point!
    If fUpdate.clbPads.items.count  > 0 Then
    Begin
        SwapIDValue := IntToStr(fUpdate.editSwapID.Text);
        fUpdate.ShowModal;
    End
    Else
        Showmessage('No pads were selected!');
End;
{..............................................................................}

{..............................................................................}
procedure TfUpdate.cbConstraintFilesClick(Sender: TObject);
Var
    Workspace           : IWorkspace;
    I,J                 : Integer;
    ConstraintFileCount : Integer;
    ConfigCount         : Integer;
    Config              : IConfiguration;

    ConstraintFileName  : String;
Begin
    ConstraintFilename := cbConstraintFiles.Items[cbConstraintFiles.ItemIndex];

    Workspace := GetWorkSpace;
    ConfigCount := Workspace.DM_FocusedProject.DM_ConfigurationCount;
    For I := 0 to ConfigCount - 1 Do
    Begin
        Config := Workspace.DM_FocusedProject.DM_Configurations(I);
        ConstraintFileCount := Config.DM_ConstraintsFileCount;

        For J := 0 to ConstraintFileCount - 1 Do
        Begin
            If ConstraintFileName = ExtractFileName(Config.DM_ConstraintsFilePath(j)) Then
            Begin
                fUpdate.labelTargetDeviceName.Caption := 'Target Device Name: ' + Config.DM_GetTargetDeviceName;
                Break;
            End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Function  UpdateConstraintRecords(AList: TCheckListBox, AFileName : String) : TStringList;
Var
    I,J            : Integer;
    ARecord        : String;
    PinID          : String;
    ANewRecord     : String;
    TempList       : TStringlist;
Begin
    // populate temporary list with checked entries only.
    TempList := TStringList.Create;
    For j := 0 To  aList.Count - 1 Do
        If alist.Checked[j] Then
            TempList.Add(AList.Items[j]);

    SwapIDValue := fUpdate.EditSwapID.Text;

    Try
        SL := TStringList.Create;
        SL.LoadFromFile(AFileName);
        SL.SaveToFile(AFileName + 'BackUp'); // back up the original file!

       // update existing TargetKind=Pin records
        For I := 0 to SL.Count - 1 Do
        Begin
            ARecord := UpperCase(SL.Strings[i]);
            If CheckNameAndValue(Trim(ARecord), 'RECORD','CONSTRAINT') Then
            Begin
                If CheckNameAndValue(Trim(ARecord), 'TARGETKIND','PIN') Then
                Begin
                    //go thru the entries of the ListBoxPads.Items
                    J := 0;
                    While J < TempList.Count Do
                    Begin
                        PinId := TempList.Strings[j];
                        If CheckNameAndValue(Trim(ARecord),'TARGETID',PinID) Then
                        Begin
                            ANewRecord := UpdateOrInsertNameValue(Trim(ARecord),'SWAPID',SwapIDValue);
                            SL.Delete(I);
                            SL.Insert(I,ANewRecord);

                            // since the SwapID record has been updated,
                            // remove the pad item from the list.
                            TempList.Delete(J);
                            Break;
                        End
                        Else
                            Inc(J);
                    End;
                End;
            End;
        End;

        // insert new TargetKind=Pin records
        // based on the pad entries in TempPadList
        For j := 0 To TempList.Count - 1 Do
        Begin
            PinId   := TempList.Strings[j];
            ANewRecord := 'Record=Constraint | TargetKind=Pin | TargetId=' + PinID + ' | SWAPID=' + SwapIDValue;
            SL.Add(ANewRecord);
        End;

    // display the new contents in the memo
    fPreviewer.MemoConstraintFile.Text := SL.Text;
    fPreviewer.ShowModal;
    If fPreviewer.ModalResult = mrOk Then
        SL.SaveToFile(AFileName);

    Finally
        TempList.Free;
        SL.Free;
    End;
End;
{..............................................................................}

{..............................................................................}
procedure TfUpdate.bOKClick(Sender: TObject);
Var
    Workspace           : IWorkspace;
    ConstraintFileName  : String;
    ConfigCount         : Integer;
    Config              : IConfiguration;
    ConstraintFileCount : Integer;
    I,J                 : Integer;
Begin
    Workspace := GetWorkSpace;
    ConfigCount := Workspace.DM_FocusedProject.DM_ConfigurationCount;
    For I := 0 to ConfigCount - 1 Do
    Begin
        Config := Workspace.DM_FocusedProject.DM_Configurations(I);
        ConstraintFileCount := Config.DM_ConstraintsFileCount;

        //check if combolist item same as the constraint filename.
        For J := 0 to ConstraintFileCount - 1 Do
            If ExtractFileName(Config.DM_ConstraintsFilePath(j)) =
            cbConstraintFiles.items(cbConstraintFiles.itemindex) Then
                 ConstraintFilename := Config.DM_ConstraintsFilePath(j);
    End;

    UpdateConstraintRecords(clbPads,ConstraintFileName);

    Close;
End;
{..............................................................................}

{..............................................................................}
Procedure TfUpdate.Button2Click(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}
procedure TfUpdate.bEnableAllClick(Sender: TObject);
var
    I : Integer;
begin
    For I := 0 to clbPads.Items.Count - 1 Do
        clbPads.Checked[i] := True;
end;
{..............................................................................}

{..............................................................................}
procedure TfUpdate.bDisableAllClick(Sender: TObject);
var
    I : Integer;
begin
    For I := 0 to clbPads.Items.Count - 1 Do
        clbPads.Checked[i] := False;
end;
{..............................................................................}

{..............................................................................}
procedure TfUpdate.UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
begin
    SwapIDValue := IntToStr(fUpdate.editSwapID.Text);
end;
{..............................................................................}

{..............................................................................}
{ Typical Constraint file with SWAPIDs
Record=Constraint | TargetKind=Pin | TargetId=10
Record=Constraint | TargetKind=Pin | TargetId=11
Record=Constraint | TargetKind=Pin | TargetId=15 | SWAPID=1
Record=Constraint | TargetKind=Pin | TargetId=16 | SWAPID=1
Record=Constraint | TargetKind=Pin | TargetId=17 | SWAPID=1
Record=Constraint | TargetKind=Pin | TargetId=18 | SWAPID=1
}

