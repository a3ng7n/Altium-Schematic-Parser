{..............................................................................}
{ Summary Reports PCB documents open in DXP.                                   }
{                                                                              }
{ Copyright (c) 2003 by Altium Limited                                         } 
{..............................................................................}

Var
    ReportFile : TStringList;
{..............................................................................}

{..............................................................................}
Function BooleanToString (Value : LongBool) : String;
Begin
    Result := 'True';

    If Value = True Then Result := 'True'
                    Else Result := 'False';
End;
{..............................................................................}

{..............................................................................}
Procedure ReportPCBDocumentViews;
Var
    I,J                : Integer;

    FileName           : TDynamicString;
    ReportDocument     : IServerDocument;

    ServerModule       : IServerModule;
    ServerDocument     : IServerDocument;
    ServerDocumentView : IServerDocumentView;
    S                  : TDynamicString;
Begin
    If Client = Nil Then Exit;
    S := '';
 
    ReportFile := TStringList.Create;  
    Try

        ReportFile.Add('PCB Server Documents information:');
        ReportFile.Add('=============================');
        ReportFile.Add('');

        // at least two views for each document, 
        // ie one document view and at least one panel view
        ServerModule := Client.ServerModuleByName['PCB'];
        If ServerModule = Nil Then Exit;
            
        For I := 0 to ServerModule.DocumentCount - 1 Do
        Begin
            ServerDocument := ServerModule.Documents[I];
            ReportFile.Add('Document View Count ' + IntToStr(ServerDocument.Count));
            ReportFile.Add('FileName ' + ServerDocument.FileName);
            ReportFile.Add('Kind ' + ServerDocument.Kind);
            ReportFile.Add('');
            
                ReportFile.Add('  Server Views information:');
                ReportFile.Add('  =============================');
                ReportFile.Add('');
            
                For J := 0 to ServerDocument.Count - 1 Do
                Begin
                    ServerDocumentView := ServerDocument.View[j];
                    ReportFile.Add('  View Name ' + ServerDocumentView.ViewName);
                    If Not(ServerDocumentView.IsPanel) Then 
                         ReportFile.Add('  Document Name ' + ServerDocument.FileName);
                         
                    ReportFile.Add('  Panel? ' + BooleanToString(ServerDocumentView.IsPanel));
                    ReportFile.Add('  Caption ' + ServerDocumentView.Caption);
                    ReportFile.Add('');
                End;
        End;
    Finally
        FileName := SpecialFolder_MyDesigns + '\ServerDocumentTypes_Report.Txt';
        ReportFile.SaveToFile(FileName);
        ReportFile.Free;
    End;

    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
