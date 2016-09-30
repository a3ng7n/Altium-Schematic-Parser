{..............................................................................}
{ Summary Demonstration of the WorkSpace Manager interfaces                    }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure ReportWSMInterfaces;
Var
    WorkSpace      : IWorkSpace;
    Project        : IProject;
    Document       : IDocument;
    Component      : IComponent;
    Net            : INet;
    i              : Integer;
    k              : Integer;
    l              : Integer;
    ReportFile     : TStringList;
    FileName       : TDynamicString;
    ReportDocument : IServerDocument;
Begin
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;

    ReportFile := TStringList.Create;
    For i := 0 To WorkSpace.DM_ProjectCount - 1 Do
    Begin
        Project := WorkSpace.DM_Projects(i);
        Project.DM_Compile;

        ReportFile.Add('Project : ' +  Project.DM_ProjectFileName);

        For k := 0 To Project.DM_LogicalDocumentCount - 1 Do
        Begin
            Document := Project.DM_LogicalDocuments(k);
            ReportFile.Add(Document.DM_FullPath);

            // Obtain component path and ID information
            ReportFile.Add('Component Path and Info');
            For l := 0 To Document.DM_ComponentCount - 1 Do
            Begin
                Component := Document.DM_Components(l);

                ReportFile.Add(Component.DM_PhysicalPath);
                ReportFile.Add(Component.DM_UniqueId);
                ReportFile.Add(Component.DM_UniqueIdName);
                ReportFile.Add(Component.DM_UniqueIdPath);
                ReportFile.Add('');
            End;

            //Obtain net names on the document
            ReportFile.Add('Net info');
            For l := 0 to Document.DM_NetCount - 1 Do
            Begin
                Net     := Document.DM_Nets(l);
                ReportFile.Add(Net.DM_NetName);
            End;
        End;
    End;
    
    // Save the text file to the MyDesigns special folder.
    FileName := SpecialFolder_MyDesigns + '\Project_Report.Txt';    
    ReportFile.SaveToFile(Filename);
    ReportFile.Free;

    // Open the new text file in DXP
    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
End.
