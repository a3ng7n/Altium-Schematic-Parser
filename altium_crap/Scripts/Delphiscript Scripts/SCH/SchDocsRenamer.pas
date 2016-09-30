{..............................................................................}
{ Summary Finds all Schematic documents in opened projects in DXP              }
{ All schematic documents found are copied and renamed in the same directory   }
{ the original docs are in.                                                    }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CopyAndRenameFile(OldFilename : WideString);
Var
    NewFileName;
Begin
    NewFileName := ChangeFileExt(OldFileName,'.sch');
    
    // CopyFile is a function from the Windows unit that is
    // exposed in the scripting system.
    CopyFile(OldFileName,NewFileName,False);
End;
{..............................................................................}

{..............................................................................}
Procedure CopyAndRenameSchDocs;
Var
    TheClient      : IClient;
    WorkSpace      : IWorkSpace;

    Project        : IProject;
    Document       : IDocument;

    I              : Integer;
    K              : Integer;

    FirstTime      : Boolean;
    ReportFileName : WideString;

    Report         : TStringList;
    ReportDocument : IServerDocument;
Begin
    // Need the workspace manager to have access to logical documents of
    // opened projects in DXP.
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;

    // Need the client to be able to open the report in DXP
    // after report has been collected and saved.
    TheClient := Client;
    If TheClient = Nil Then Exit;


    //Create a Report list.
    Report := TStringList.Create;
    Report.Add('Schematic File Copier and Renamer');
    Report.Add('__________________________________');
    Report.Add('');

    // Copy existing schematics of each project
    // with a new .sch file extension. Original sch files
    // are untouched.
    For I := 0 To WorkSpace.DM_ProjectCount - 1 Do
    Begin
        Project := WorkSpace.DM_Projects(I);
        FirstTime := True;

        For K := 0 To Project.DM_LogicalDocumentCount - 1 Do
        Begin
            Document := Project.DM_LogicalDocuments(K);
            If Document.DM_DocumentKind = 'SCH' Then
            Begin
                If FirstTime Then Report.Add(Project.DM_ProjectFullPath);
                CopyAndRenameFile(Document.DM_FullPath);
                Report.Add('    ' + Document.DM_FullPath);
                Report.Add('    ' + ChangeFileExt(Document.DM_FullPath,'.sch'));
                FirstTime := False;
            End;
        End;
        Report.Add('');
    End;

    // Save the report and display it in DXP...
    ReportFileName := 'C:\SchDocRenamer.txt';
    Report.SaveToFile(ReportFileName);
    ReportDocument := TheClient.OpenDocument('Text', ReportFileName);
    If ReportDocument <> Nil Then
        TheClient.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
