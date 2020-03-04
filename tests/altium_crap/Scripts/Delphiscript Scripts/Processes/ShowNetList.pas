{..............................................................................}
{ Summary ShowNetList - generate a Protel format net list from a Sch Project   }
{ Version 2                                                                    }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure RunNetList;
Var
    ErrorCode   : Integer;
    CommandLine : String;
    Value       : Integer;
    FileName    : String;
    AnIndex     : Integer;
Begin
    ResetParameters;
    AddStringParameter('ObjectKind','Netlist');

    // AnIndex value for GenerateReport process
    // Based on netlisters in
    // Design » Netlist For Project
    // 1 = EDIF for PCB
    // 2 = Multiwire
    // 3 = Pcad for PCB
    // 4 = Protel
    // 5 = Verilog file
    // 6 = VHDL File
    // 7 = XSPICE

    AnIndex := 4;
    AddIntegerParameter('Index',AnIndex);
    AddStringParameter('ReturnGeneratedDocuments', 'True');
    RunProcess('WorkspaceManager:GenerateReport');
    GetIntegerParameter('Result', Value);
    If Value = 0 Then Exit;

    // Invoke the notepad and display the netlist file.
    GetStringParameter('File1', FileName);
    CommandLine := 'notepad.exe ' + FileName;
    ErrorCode := RunApplication(CommandLine);
    If ErrorCode <> 0 Then
           ShowError('System cannot start : ' + CommandLine + #13#10 + GetErrorMessage(ErrorCode));
End;
{..............................................................................}

{..............................................................................}
