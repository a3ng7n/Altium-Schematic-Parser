{......................................................................................................}
Procedure UnRouteNet(Board : IPCB_Board; Net : IPCB_Net);
Var
    Iterator : IPCB_GroupIterator;
    Prim     : IPCB_Prim;
    KillList : TList;
    i        : Integer;
Begin
    Iterator := Net.GroupIterator_Create;
    Prim     := Iterator.FirstPCBObject;
    KillList := TList.Create;
    While Prim <> NIl Do
    Begin
        If Prim.IsFreePrimitive Then
            KillList.Add(Prim);
        Prim := Iterator.NextPCBObject;
    End;

    For i := 0 TO KillList.Count - 1 Do
        Board.RemovePCBObject(KillList[i]);
    KillList.Free;
End;
{......................................................................................................}

{......................................................................................................}
Procedure UnrouteClass(Board : IPCB_BOARD; NetClass : IPCB_OBjectClass);
Var
    Iterator : IPCB_BoardIterator;
    net      : IPCB_Net;
Begin
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Net := Iterator.FirstPCBObject;
    While Net <> NIl Do
    Begin
        If NetClass.IsMember(Net.Name) Then
            UnrouteNet(Board, Net);
        Net := Iterator.NextPCBObject;
    End;
    Board.ViewManager_FullUpdate;
End;
{......................................................................................................}

{......................................................................................................}
Procedure UnRouteSelectedClasses(Board : I_PCBBoard);
Var
    i : Integer;
Begin
    For i := 0 To GetNetClass.Classes.Items.Count - 1 Do
        If GetNetClass.Classes.Selected[i] Then
            UnrouteClass(Board, GetNetClass.Classes.Items.Objects[i]);
End;
{......................................................................................................}

{......................................................................................................}
Function ChooseNetCLass(Dummy : Integer = 0) : Boolean;
Begin
    Result := GetNetClass.showmodal = mrOK;
End;
{......................................................................................................}

{......................................................................................................}
Procedure FillNetClassList(Board : IPCB_Board);
Var
    Iterator : IPCB_BoardIterator;
    c        : IPCB_ObjectClass;
Begin
    Iterator := Board.BoardIterator_Create;

    Iterator.SetState_FilterAll;
    Iterator.AddFilter_ObjectSet(MkSet(eClassObject));
    c := Iterator.FirstPCBObject;
    While c <> NIl Do
    Begin
        If c.MemberKind = eClassMemberKind_Net Then
            GetNetClass.Classes.Items.AddObject(c.Name, C);
        c := Iterator.NextPCBObject;
    End;
End;
{......................................................................................................}

{......................................................................................................}
 Procedure UnRouteNetClass;
 Var
     ClassToUnroute : IPCB_OBjectClass;
     Board          : IPCB_Board;
 Begin
     Pcbserver.PreProcess;
     Try
         Board := PCBServer.GetCurrentPCBBoard;
         If Not Assigned(Board) Then
         Begin
             ShowMessage('The Current Document is not a Protel PCB Document.');
             Exit;
         End;
         FillNetClassList(Board);
         If ChooseNetClass Then
             UnrouteSelectedClasses(Board);
     Finally
         Pcbserver.PostProcess;
     End;
 End;
{......................................................................................................}

{......................................................................................................}
procedure TGetNetClass.ClassesDblClick(Sender: TObject);
begin
    ModalResult := mrOK;
end;

