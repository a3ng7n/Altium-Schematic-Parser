{..............................................................................}
{ Summary Server Process Report -                                              }
{         Generate a report for all installed servers' processes.              }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

Var
    ReportFile     : TStringList;

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
Procedure ReportServerProcessInfo;
Var
    I              : Integer;
    J              : Integer;
    K              : Integer;

    FileName       : TDynamicString;
    ReportDocument : IServerDocument;

    RecordCount    : Integer;
    CommandCount   : Integer;
    ParameterCount : Integer;

    ServerRecord   : IServerRecord;
    ServerProcess  : IServerProcess;

    S              : TDynamicString;
Begin
    If Client = Nil Then Exit;
    BeginHourGlass;
    S := '';
    
    ReportFile := TStringList.Create;
    Try
        ReportFile.Add('Server Processes information:');
        ReportFile.Add('=============================');
        ReportFile.Add('');

        RecordCount := Client.GetServerRecordCount;
        ReportFile.Add(' Server Record Count : ' + IntToStr(RecordCount));
        ReportFile.Add(' =========================');
        ReportFile.Add('');
     
        For I := 0 to RecordCount - 1 Do
        Begin

            ServerRecord := Client.GetServerRecord(I);
     
            ReportFile.Add('  Server Name = ' + ServerRecord.GetName + ' #' + IntToStr(I + 1));
            ReportFile.Add('  ===============================================================');
            ReportFile.Add('    Server Version ' + ServerRecord.GetVersion);
            ReportFile.Add('    Server Copyright info ' + ServerRecord.GetCopyRight);
            ReportFile.Add('    Server Date ' + ServerRecord.GetDate);
            ReportFile.Add('    Server Info ' + ServerRecord.GetGeneralInfo);

            ReportFile.Add('    RCS path '   + ServerRecord.GetRCSFilePath);
            ReportFile.Add('    Ins Path '   + ServerRecord.GetInsPath);
            ReportFile.Add('    Exe Path'    + ServerRecord.GetExePath);

            ReportFile.Add('    Number of document types ' + IntToStr(ServerRecord.GetWindowKindCount));
            ReportFile.Add('    Number of commands '       + IntToStr(ServerRecord.GetCommandCount));

            ReportFile.Add('    =====================================');

            CommandCount := ServerRecord.GetCommandCount;
            For J := 0 To CommandCount - 1 Do
            Begin
                ServerProcess := ServerRecord.GetCommand(J);
                ReportFile.Add('        Process #' + IntToStr(J + 1) + ' Name = '  +
                    ServerProcess.GetOriginalId + ' LongSummary = ' + ServerProcess.GetLongSummary);

                S := '';
                ParameterCount := ServerProcess.GetParameterCount;
                For K := 0 To ParameterCount - 1 Do
                    S := S + ServerProcess.GetParameter(K) + ', ';

                ReportFile.Add('        Parameters = ' + S);
            End;
            ReportFile.Add('');
            S := '';
        End;

    Finally
        FileName := SpecialFolder_MyDesigns + '\ServerProcesses_Report.Txt';
        ReportFile.SaveToFile(FileName);
        ReportFile.Free;
    End;

    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);

    EndHourGlass;
End;
{..............................................................................}

{..............................................................................}
