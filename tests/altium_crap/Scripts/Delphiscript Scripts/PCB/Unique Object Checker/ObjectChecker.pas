{..............................................................................}
{ Summary Checks for duplicated Nets and Components on a current PCB document  }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    ObjectKind              : TObjectId;
    ObjectList              : TStringList;
    ShowReportInAMessageBox : Boolean;
    DisplayReportInDXP      : Boolean;
    PcbBoard                : IPCB_Board;
    SaveDialog              : TSaveDialog;
    RptFile                 : TextFile;
    LS                      : String;

    sRepeatedObjectReport   : String;
    sObjectsHaveUniqueNames : String;
    sObjectsAreNamed        : String;

    sObjectsTitle           : String;
    sObjectsFilter          : String;
{..............................................................................}

{..............................................................................}
Procedure CheckDuplicatedObjects(Dummy : Integer = 0);
Var
    ObjectHandle   : IPCB_Primitive;
    IteratorHandle : IPCB_BoardIterator;
    I, RepeatCount : Integer;
Begin
    If ObjectKind = eNoObject Then Exit;

    ObjectList := TStringList.Create;
    IteratorHandle := PCBBoard.BoardIterator_Create;
    IteratorHandle.AddFilter_ObjectSet(MkSet(ObjectKind));
    IteratorHandle.AddFilter_LayerSet(AllLayers);
    IteratorHandle.AddFilter_Method(eProcessAll);

    ObjectHandle := IteratorHandle.FirstPCBObject;
    While ObjectHandle <> Nil Do
    Begin
        If ObjectHandle.ObjectId = eComponentObject Then
            ObjectList.Add(ObjectHandle.Name.Text)
        Else If ObjectHandle.ObjectId = eNetObject Then
            ObjectList.Add(ObjectHandle.Name);
        ObjectHandle := IteratorHandle.NextPCBObject;
    End;
    PCBBoard.BoardIterator_Destroy(IteratorHandle);

    // Sort list (of object names) alphanumerically.
    QuickSortStringList(ObjectList, 0, ObjectList.Count - 1);

    // Check for repeated object names.
    RepeatCount := 0;
    LS := sRepeatedObjectReport;
    For I := 1 To ObjectList.Count - 1 Do
    Begin
        If ObjectList[I - 1] = ObjectList[I] Then
            Inc(RepeatCount)
        Else If RepeatCount <> 0 Then
        Begin
            LS := LS + #13#10 + IntToStr(RepeatCount + 1) + sObjectsAreNamed + ObjectList[I - 1] + '.';
            RepeatCount := 0;
        End;
    End;
    ObjectList.Free;

    // if the LS string only contains the sRepeatedObejctsReport then there are only unique objects
    If LS = sRepeatedObjectReport Then
        LS := LS + #13#10 + sObjectsHaveUniqueNames;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessComponents(Dummy : Integer = 0);
Begin
    ObjectKind              := eComponentObject;
    sRepeatedObjectReport   := 'Repeated Component Names Report:';
    sObjectsHaveUniqueNames := 'All components in this file have unique names.';
    sObjectsAreNamed        := ' components are named ';

    sObjectsTitle           := 'Save Component Names Report to File...';
    sObjectsFilter          := 'PCB Report files (*.rpt)|*.rpt|All files (*.*)|*.*';

    CheckDuplicatedObjects;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessNets(Dummy : Integer = 0);
Begin
    ObjectKind              := eNetObject;
    sRepeatedObjectReport   := 'Repeated Net Names Report:';
    sObjectsHaveUniqueNames := 'All nets in this file have unique names.';
    sObjectsAreNamed        := ' nets are named ';

    sObjectsTitle           := 'Save Net Names Report to File...';
    sObjectsFilter          := 'PCB Report files (*.rpt)|*.rpt|All files (*.*)|*.*';

    CheckDuplicatedObjects;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessAll(Dummy : Integer = 0);
Var
    TempLS : String;
Begin
    ProcessNets;

    //insert a blank line...
    TempLS := LS; //copy LS contents to TempSL;

    ProcessComponents;

    LS := TempLS + #13#10#13#10 + LS;
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateReport(Dummy : Integer = 0);
Var
    DocFileName    : String;
    ReportDocument : IServerDocument;
Begin
    If ObjectKind = eNoObject Then Exit;

    DocFileName := ChangeFileExt(PcbBoard.FileName, '.rpt');

    SaveDialog            := TSaveDialog.Create(Application);
    SaveDialog.Title      := sObjectsTitle;
    SaveDialog.Filter     := sObjectsFilter;
    SaveDialog.InitialDir := ExtractFilePath(PcbBoard.FileName);
    SaveDialog.FileName   := DocFileName;
    SaveDialog.DefaultExt := 'rpt';
    SaveDialog.Options    := MkSet(ofOverwritePrompt, ofShowHelp, OfPathMustExist);

    If SaveDialog.Execute Then
    Begin
        DocFileName := SaveDialog.FileName;
        AssignFile(RptFile, DocFileName);
        Rewrite(RptFile);
        Writeln(RptFile, LS);
        CloseFile(RptFile);
    End
    Else
    Begin
        //User pressed cancel, so no need to display information!
        ShowReportInAMessageBox := False;
        DisplayReportInDXP      := False;
    End;
    SaveDialog.Free;

    If ShowReportInAMessageBox Then
        ShowInfo(LS);

    If DisplayReportInDXP Then
    Begin
        ReportDocument := Client.OpenDocument('Text', DocFileName);
        If ReportDocument <> Nil Then
            Client.ShowDocument(ReportDocument);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure CheckObjects(ADisplay : Boolean; AShow : Boolean; AObject : String);
Begin
    BeginHourGlass;

    ObjectKind := eNoObject;
    PcbBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;


    DisplayReportInDXP := ADisplay;
    ShowReportInAMessageBox := AShow;

    If UpperCase(AObject) = 'COMPONENT' Then
        ProcessComponents
    Else If UpperCase(AObject) = 'NET' Then
        ProcessNets
    Else If UpperCase(AObject) = 'ALL' Then
        ProcessAll;

    EndHourGlass;

    GenerateReport;
End;
{..............................................................................}

{..............................................................................}
Procedure CheckNets;
Begin
    CheckObjects(True, True, 'Net');
End;
{..............................................................................}

{..............................................................................}
Procedure CheckComponents;
Begin
    CheckObjects(True, True, 'Component');
End;
{..............................................................................}

{..............................................................................}
Procedure CheckAll;
Begin
    CheckObjects(True, True, 'All');
End;
{..............................................................................}

{..............................................................................}
