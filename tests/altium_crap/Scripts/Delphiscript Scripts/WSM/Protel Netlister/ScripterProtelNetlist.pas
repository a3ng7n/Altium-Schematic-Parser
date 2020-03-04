{..............................................................................}
{ Summary Demo the use of WorkSpaceManger interfaces to generate a Protel      }
{         Netlist. The netlist file is stored in the Generated folder for      }
{         the current project.                                                 }
{                                                                              }
{         The Netlist file is generated in Generated folder for the current    }
{         Project on the Projects Panel.                                       }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    TargetFileName : ShortString;
    NetList        : TStringList;
{..............................................................................}

{..............................................................................}
Procedure WriteComponent_Version1(Component : IComponent);
Begin
    If Component <> Nil Then
    Begin
        NetList.Add('[');
        NetList.Add(Component.DM_PhysicalDesignator);
        NetList.Add(Component.DM_FootPrint);
        NetList.Add(Component.DM_PartType);
        NetList.Add('');
        NetList.Add('');
        NetList.Add('');
        NetList.Add(']');
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure WriteComponent_Version2(Component : IComponent);
Begin
    If Component <> Nil Then
    Begin
        NetList.Add('[');
        NetList.Add('Designator' );
        NetList.Add(Component.DM_PhysicalDesignator );
        NetList.Add('FOOTPRINT'  );
        NetList.Add(Component.DM_FootPrint  );
        NetList.Add('PARTTYPE'   );
        NetList.Add(Component.DM_PartType   );
        NetList.Add('DESCRIPTION');
        NetList.Add(Component.DM_Description);
        NetList.Add(']');
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure WriteComponent(Component : IComponent; Version : Integer);
Begin
    Case Version Of
        0 : WriteComponent_Version1(Component);
        1 : WriteComponent_Version2(Component);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure WriteNet_Version1(Net : INet);
Var
    I       : Integer;
    Pin     : IPin;
    PinDsgn : String;
    PinNo   : String;
Begin
    If Net.DM_PinCount >= 2 Then
    Begin
        NetList.Add('(');
        NetList.Add(Net.DM_CalculatedNetName);

        For i := 0 To Net.DM_PinCount - 1 Do
        Begin
            Pin := Net.DM_Pins(i);
            PinDsgn := Pin.DM_PhysicalPartDesignator;
            PinNo   := Pin.DM_PinNumber;
            NetList.Add(PinDsgn + '-' + PinNo);
        End;
        NetList.Add(')');
    End;
End;
{..............................................................................}

{..............................................................................}
Function ConvertElectricToString(Value : TPinElectrical) : String;
Begin
    Result := '';
    Case Value Of
        eElectricInput         : Result := 'Input';
        eElectricIO            : Result := 'IO';
        eElectricOutput        : Result := 'Output';
        eElectricOpenCollector : Result := 'OpenCollector';
        eElectricPassive       : Result := 'Passive';
        eElectricHiZ           : Result := 'HiZ';
        eElectricOpenEmitter   : Result := 'OpenEmitter';
        eElectricPower         : Result := 'Power';
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure WriteNet_Version2(Net : INet);
Var
    i   : Integer;
    Pin : IPin;
    ElectricalString : String;
Begin
    If (Net.DM_PinCount >= 2) Or (Net.DM_CountOfNonPinItems >= 1) Then
    Begin
        NetList.Add('(');
        NetList.Add(Net.DM_FullNetName);
        For i := 0 To Net.DM_PinCount - 1 Do
        Begin
            Pin := Net.DM_Pins(i);

            ElectricalString := '';
            If Pin.DM_Electric <> Nil Then
                ElectricalString := ConvertElectricToString(Pin.DM_Electric);

            NetList.Add(Pin.DM_PhysicalPartDesignator  + '-' +
                        Pin.DM_PinNumber               + ' ' +
                        Pin.DM_PartType                + '-' +
                        Pin.DM_PinName                 + ' ' +
                        ElectricalString);
        End;
        NetList.Add(')');
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure WriteNet(Net : INet; Version : Integer);
Begin
    Case Version Of
        0 : WriteNet_Version1(Net);
        1 : WriteNet_Version2(Net);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure CreateAllDirectories(Dir : String);
Var
    ldir  : Integer;
    fpath : String;
Begin
    ldir := Length(Dir);
    If ldir = 0 Then Exit;

    If Dir[ldir] = '\' Then
        Delete(Dir, ldir, 1);

    fpath := ExtractFilePath(Dir);

    If (Length(Dir) < 3) Or DirectoryExists(Dir) Or (fPath = Dir) Then Exit;

    CreateAllDirectories(fPath);
    CreateDir(Dir);
End;
{..............................................................................}

{..............................................................................}
Procedure Generate(Const Project : IProject; Const DocumentPath, TargetFolder : WideString; Version : Integer);
Var
    i              : Integer;
    Document       : IDocument;
    AFullPath      : WideString;
    Msg            : WideString;
Begin
    TargetFileName := '';
    Document := Project.DM_GetDocumentFromPath(DocumentPath);

    If Project.DM_DocumentFlattened <> Nil Then
       AFullPath := Project.DM_ProjectFullPath
    Else
    If Document <> Nil Then
       AFullPath := Document.DM_FullPath
    Else {Use Free Documents Project}
       AFullPath := Project.DM_ProjectFullPath;

    TargetFileName := Project.DM_GetOutputPath + '\' + ExtractFileNameFromPath(AFullPath) + '.net';

    If (Project.DM_DocumentFlattened = Nil) And (Document = Nil) Then Exit;

    If (Client <> Nil) And (Client.GUIManager <> Nil) Then
    Begin
        Msg := 'Writing Protel NetList to ' + ExtractFileName(TargetFileName) + '...';
        Client.GUIManager.StatusBar_SetState(0, Msg);
    End;

    CreateAllDirectories(ExtractFilePath(TargetFileName));
    // Check if project has been flattened...
    Document := Project.DM_DocumentFlattened;
    If Document = Nil Then
       Document := Project.DM_GetDocumentFromPath(DocumentPath);

    // Generate component information
    For i := 0 To Document.DM_ComponentCount - 1 Do
        WriteComponent(Document.DM_Components(i),Version);

    // Generate net information
    For i := 0 To Document.DM_NetCount - 1 Do
        WriteNet(Document.DM_Nets(i),Version);

    NetList.SaveToFile(TargetFileName);

    // Sets the Editor Type and put in the Generated folder.
    VFS_SetFileEditorName(TargetFileName,'ProtelNetlist');
    Project.DM_AddGeneratedDocument(TargetFileName);
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateNetlist(Version : Integer);
Var
    WS      : IWorkspace;
    Prj     : IProject;
Begin
    WS  := GetWorkspace;
    If WS = Nil Then Exit;

    Prj := WS.DM_FocusedProject;
    If Prj = Nil Then Exit;

    // Compile the project to fetch the connectivity
    // information for the design.
    Prj.DM_Compile;

    // Create a TStringList to store component and net data.
    NetList := TStringList.Create;

    // Generate Netlist depending on what version.
    Generate(Prj, Prj.DM_ProjectFullPath, ExtractFilePath(Prj.DM_ProjectFullPath), Version);
    NetList.Free;
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateProtelV1FormatNetlist;
Var
    Version : Integer;
Begin
    // Protel 1 Netlist format...
    GenerateNetlist(0);
End;
{..............................................................................}

{..............................................................................}
Procedure GenerateProtelV2FormatNetlist;
Var
    Version : Integer;
Begin
    // Protel 2 Netlist format...
    GenerateNetlist(1);
End;
{..............................................................................}

{..............................................................................}
