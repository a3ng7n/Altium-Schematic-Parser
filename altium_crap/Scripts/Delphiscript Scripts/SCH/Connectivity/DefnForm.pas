Var
    DocumentScope     : Integer;
    ParameterColor    : TColor;
    ParameterOffset   : Integer;
    ParameterName     : String;
    ParameterNameShow : Boolean;
    AssignParamToPin  : Boolean;
    FormParamMaker    : TFormParamMaker;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.FormParamMakerCreate(Sender: TObject);
Var
    J           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    FocussedDoc : IDocument;
    CurrentSch  : ISch_Document;
Begin
    // Check if schematic server exists or not.
    If SchServer = Nil Then Exit;

    // Obtain the current schematic document interface.
    CurrentSch := SchServer.GetCurrentSchDocument;
    If CurrentSch = Nil Then Exit;

    // do a compile so the logical documents get expanded into physical documents.
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;
    Project.DM_Compile;

    // Populate the CheckListBoxDocuments control with physical document filenames
    For J := 0 to Project.DM_PhysicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_PhysicalDocuments(J);
        CheckListBoxDocuments.Items.Add(Doc.DM_FileName);
    End;

     ButtonClearAll.Enabled        := False;
     ButtonSelectAll.Enabled       := False;
     CheckListBoxDocuments.Enabled := False;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.RadioGroupScopeClick(Sender: TObject);
Begin
     If RadioGroupScope.ItemIndex <> 0 Then
     Begin
         // focussed document only
         ButtonClearAll.Enabled        := True;
         ButtonSelectAll.Enabled       := True;
         CheckListBoxDocuments.Enabled := True
     End
     Else
     Begin
         // selected or all documents
         ButtonClearAll.Enabled        := False;
         ButtonSelectAll.Enabled       := False;
         CheckListBoxDocuments.Enabled := False;
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.XPBitBtnColorClick(Sender: TObject);
Begin
    If ColorDialog1.Execute Then
         ParameterColor := ColorDialog1.Color
    Else
         ParameterColor := clBlue;

    ColorShape.Brush.Color := ParameterColor;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.XPGenerateClick(Sender: TObject);
Var
    I            : Integer;
    DocumentList : TStringList;
Begin
    Try
        DocumentList      := TStringList.Create;
        DocumentScope     := RadioGroupScope.ItemIndex;
        ParameterOffset   := StrToInt(EditBoxOffset.Text);
        ParameterName     := EditParameterName.Text;
        ParameterNameShow := False;
        AssignParamToPin  := False;


        If CheckBoxParameterNameVisibility.State = cbChecked Then
            ParameterNameShow := True;

        If CheckBoxAssignParamToPin.State = cbChecked Then
            AssignParamToPin := True;

        If DocumentScope = 1 Then
        Begin
            For I := 0 to CheckListBoxDocuments.Items.Count -1 Do
            Begin
                If CheckListBoxDocuments.State[i] = cbChecked Then
                   DocumentList.Add(CheckListBoxDocuments.Items[i]);
            End;
        End;

        FetchComponentNetInfo(DocumentScope, DocumentList, ParameterColor, ParameterOffset, ParameterName, ParameterNameShow, AssignParamToPin);
        DocumentList.Free;
    Finally
        Close;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.ButtonClearAllClick(Sender: TObject);
Var
    I : Integer;
Begin
    // clear all toggled items
      For i := 0 To CheckListBoxDocuments.Items.Count - 1 Do
          CheckListBoxDocuments.State[i] := cbUnChecked;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.ButtonSelectAllClick(Sender: TObject);
Var
    I : Integer;
Begin
    // select all items
      For i := 0 To CheckListBoxDocuments.Items.Count - 1 Do
          CheckListBoxDocuments.State[i] := cbChecked;
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.UpDownParameterOffsetClick(Sender: TObject; Button: TUDBtnType);
Begin
    ParameterOffset := StrToInt(EditBoxOffset.Text);
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.CheckBoxAssignParamToPinClick(Sender: TObject);
Begin
    //
End;
{..............................................................................}

{..............................................................................}
Procedure TFormParamMaker.XPCancelClick(Sender: TObject);
Begin
    Close;
End;
{..............................................................................}

{..............................................................................}


