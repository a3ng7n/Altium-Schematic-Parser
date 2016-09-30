{..............................................................................}
{ Summary Demo of inserting a schematic sheet in a new PCB Project             }
{         There needs to be a sheet1.schdoc file in C:\ folder                 }  
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    Workspace     : IWorkspace;
    Document      : IServerDocument;
    SheetFileName : TString;
Begin
    // Need to start up Schematic Server first.
    Client.StartServer('SCH');

    // issues... Workspace.DM_AddDocumentToActiveProject(Document.FileName);
    // if after a script project. causes the sch doc to be added to the Free Documents project.
    // if another PCB project is set by mouse for example, then the sch doc is added into a new PCB project correctly.

    // Create a blank project.
    Client.SendMessage('WorkspaceManager:OpenObject','Kind=PcbProject | ObjectKind=NewAnything',1024,Nil);

    SheetFileName := 'C:\Sheet1.SchDoc';
    Document      := Client.OpenDocument('Sch', SheetFileName);

    If Document <> Nil Then
    Begin
        //Add this schematic sheet in the newly created PCB project in DXP.
        Workspace := GetWorkspace;
        If Workspace <> Nil Then
        Begin
           // Workspace.DM_AddDocumentToActiveProject(Document.FileName);
           Workspace.DM_FocusedProject.DM_AddSourceDocument(Document.FileName);
        End;

        Client.ShowDocument(Document);
    End;
End;

