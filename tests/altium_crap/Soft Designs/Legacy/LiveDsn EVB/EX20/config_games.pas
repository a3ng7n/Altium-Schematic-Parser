Uses
    Rt_ClientServerInterface;

Var ServerDocumentView : IServerDocumentView;

Var CurrentDirectory : AnsiString;
Var TargetFile       : File;
Var TargetDirectory  : AnsiString;

Begin
    ServerDocumentView := Client.GetCurrentView;
    CurrentDirectory := ExtractFileDir(ServerDocumentView.GetOwnerDocument.FileName);
    TargetDirectory  := CurrentDirectory + '\HardwarePlatform\graphics\';
    ChDir(TargetDirectory);

    // rename everything back to terminal
    If Not FileExists ('terminal_font.hex') Then
    Begin
        AssignFile (TargetFile,'project_tile.hex');
        Rename     (TargetFile,'terminal_font.hex');
    End;

    // rename bocman files to common files
    If FileExists ('game_tile.hex') Then
    Begin
        AssignFile (TargetFile,'game_tile.hex');
        Rename     (TargetFile,'project_tile.hex');
    End;
End;

