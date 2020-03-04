{..............................................................................}
{ Summary Finds currently open projects including Free projects in DXP         }
{ A free project is a group of documents that are not related to any project.  }
{ Copyright (c) 2003 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure ReportExistingProjectsInDXP;
Var
    WorkSpace      : IWorkSpace;
    Project        : IProject;
    Document       : IDocument;
    I              : Integer;
    K              : Integer;
    ReportFile     : Text;
    FileName       : TDynamicString;
    ReportDocument : IServerDocument;
Begin
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;

    // SpecialFolder_MyDesigns is a function from RT_Util that returns the path to MyDesigns
    FileName := SpecialFolder_MyDesigns + '\Project_Report.Txt';
    AssignFile(ReportFile, FileName);
    Rewrite(ReportFile);
 
    For I := 0 To WorkSpace.DM_ProjectCount - 1 Do
    Begin
        Project := WorkSpace.DM_Projects(I);
 
        Writeln(ReportFile, 'Project : ', Project.DM_ProjectFileName);
 
        For K := 0 To Project.DM_LogicalDocumentCount - 1 Do
        Begin
            Document := Project.DM_LogicalDocuments(K);
            Writeln(ReportFile, Document.DM_FullPath);
        End;
 
        Writeln(ReportFile);
    End;
    CloseFile(ReportFile);

    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}