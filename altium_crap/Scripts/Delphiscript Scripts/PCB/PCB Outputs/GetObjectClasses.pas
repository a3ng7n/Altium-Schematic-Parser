{......................................................................................................}
{ }
{ }
{......................................................................................................}

{......................................................................................................}
Var
    Board : IPCB_Board;
{......................................................................................................}

{......................................................................................................}
Function MbrKindToStr(AMbrKind : TClassMemberKind) : String;
Begin
    Result := '';
    Case AMbrKind Of
        0 : Result := 'Net';
        1 : Result := 'Component';
        2 : Result := 'FromTo';
        3 : Result := 'Pad';
        4 : Result := 'Layer';
//        5 : Result := 'DesignChannel';
    End;
End;
{......................................................................................................}

{......................................................................................................}
Procedure GenerateReport(Var ARpt : TStringList; AObjectClass : IPCB_ObjectClass);
Var
    S : TPCBString;
    I : Integer;
Begin
    // need to know how many members in this class...
    S := 'Class Name: ' + AObjectClass.Name + ' Class Member Kind: ' + MbrKindToStr(AObjectClass.MemberKind);

    ARpt.Add(S);
    ARpt.Add('');

    If Not AObjectClass.SuperClass Then
    Begin
        // not a super class, so get member names
        I := 0;
        While AObjectClass.MemberName[I] <> '' Do
        Begin
            ARpt.Add(AObjectClass.MemberName[I]);
            Inc(I);
        End;
    End
    Else
    Begin
        // is a super class!
        Case AObjectClass.MemberKind Of
            eClassMemberKind_Net       : ARpt.Add('All Nets');
            eClassMemberKind_Component : ARpt.Add('All Components');
            eClassMemberKind_FromTo    : ARpt.Add('All FromTos');
            eClassMemberKind_Pad       : ARpt.Add('All Pads');
            eClassMemberKind_Layer     : ARpt.Add('All Layers');
        End;
    End;
End;
{......................................................................................................}

{......................................................................................................}
Procedure GenerateSelectedClassesReport(Dummy : Integer = 0);
Var
    ClassRpt : TStringList;
    I        : Integer;
    FileName : String;
    Document : IServerDocument;
Begin
    If Form_GetClass.Classes.Items.Count < 1 Then Exit;

    ClassRpt := TStringList.Create;
    For i := 0 To Form_GetClass.Classes.Items.Count - 1 Do
        If Form_GetClass.Classes.Selected[i] Then
            GenerateReport(ClassRpt, Form_GetClass.Classes.Items.Objects[i]);

    // Display the object class and its members report
    FileName := ChangeFileExt(Board.FileName,'.REP');
    ClassRpt.SaveToFile(Filename);
    ClassRpt.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{......................................................................................................}

{......................................................................................................}
Function ChooseClasses(Dummy : Integer = 0) : Boolean;
Begin
    Result := Form_GetClass.showmodal = mrOK;
End;
{......................................................................................................}

{......................................................................................................}
Procedure FillClassList(Dummy : Integer = 0);
Var
    ClassType : TClassMemberKind;
    Iterator  : IPCB_BoardIterator;
    c         : IPCB_ObjectClass;
Begin
    Case Form_GetClass.cb_ClassMemberKind.ItemIndex Of
        0 : ClassType := eClassMemberKind_Net;
        1 : ClassType := eClassMemberKind_Component;
        2 : ClassType := eClassMemberKind_FromTo;
        3 : ClassType := eClassMemberKind_Pad;
        4 : ClassType := eClassMemberKind_Layer;
//        5 : ClassType := eClassMemberKind_DesignChannel;
    End;

    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
    c := Iterator.FirstPCBObject;

    Form_GetClass.Classes.Clear;
    While c <> Nil Do
    Begin
        If c.MemberKind = ClassType Then
            Form_GetClass.Classes.Items.AddObject(c.Name, C);
        c := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
End;
{......................................................................................................}

{......................................................................................................}
Procedure SearchAndGenerateClassesReport;
Begin
    Pcbserver.PreProcess;
    Try
        Board := PCBServer.GetCurrentPCBBoard;
        If Not Assigned(Board) Then
        Begin
            ShowMessage('The current Document is not a PCB Document.');
            Exit;
        End;

        FillClassList;
        If ChooseClasses Then
            GenerateSelectedClassesReport;
    Finally
        Pcbserver.PostProcess;
    End;
End;
{......................................................................................................}

{......................................................................................................}
procedure Tform_GetClass.cb_ClassMemberKindChange(Sender: TObject);
begin
    FillClassList;
end;
{......................................................................................................}

{......................................................................................................}
procedure Tform_GetClass.ClassesDblClick(Sender: TObject);
begin
    ModalResult := mrOK;
end;
{......................................................................................................}

{......................................................................................................}
procedure Tform_GetClass.bCancelClick(Sender: TObject);
begin
    Close;
end;
{......................................................................................................}

{......................................................................................................}

