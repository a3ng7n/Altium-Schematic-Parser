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

    // rename everything back to bocman 
    If Not FileExists ('game_tile.hex') Then
    Begin
        AssignFile (TargetFile,'project_tile.hex');
        Rename     (TargetFile,'game_tile.hex');
    End;

    // rename arkanoid files to common files
    If FileExists ('terminal_font.hex') Then
    Begin
        AssignFile (TargetFile,'terminal_font.hex');
        Rename     (TargetFile,'project_tile.hex');
    End;
End;

