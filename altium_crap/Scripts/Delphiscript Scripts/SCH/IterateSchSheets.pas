{..............................................................................}
{ Summary Demo how to iterate all open schematics of a project                 }
{                                                                              }
{ Version 1.0                                                                  }
{ Copyright (c) 2007 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure IterateAllOpenSchematicsOfAProject;
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    S           : String;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;

    S := '';
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
        Begin
             CurrentSch := SchServer.GetSchDocumentByPath(Doc.DM_FullPath);
             If CurrentSch <> Nil Then
             Begin
                  S := S + #13 + CurrentSch.DocumentName;
             End;
        End;
    End;
    ShowMessage('Schematics:' + #13 + S);
End;

// A limitation of this script. If the project is compiled then all the
// documents are loaded in memory and these documents are reported  as opened
// even though they are not visible in the workspace ie sch tabs not showing...
