Function BooleanToString (Value : LongBool) : String;
Begin
    Result := 'True';

    If Value = True Then Result := 'True'
                    Else Result := 'False';
End;
{..................................................................................................}

{..................................................................................................}
Procedure ReportClientServerInterfaces;
Var
    CSData          : TStringList;
    I,J,K,L,M       : Integer;

    FileName        : AnsiString;
    ReportDocument  : IServerDocument;
    RecordCount     : Integer;
    ServerRecord    : IServerRecord;
    OwnerDocument   : IServerDocument;
    CurrentView     : IServerDocumentView;
    WindowKind      : IServerWindowKind;
    ServerProcess   : IServerProcess;
    ServerPanelInfo : IServerPanelInfo;

Begin
    If Client = Nil Then Exit;

    CSData := TStringList.Create;
    Try
        CSData.Add('Client Interface information:');
        CSData.Add('=============================');
        CSData.Add('');

        CSData.Add('Application Handle ' + IntToStr(Client.ApplicationHandle));
        CSData.Add('Main Window Handle ' + IntToStr(Client.MainWindowHandle));

    //    Log(' CommandLauncher',Client.CommandLauncher);

        CSData.Add('');
        CurrentView   := Client.CurrentView;
        If CurrentView <> Nil Then
        Begin
            OwnerDocument := Client.CurrentView.OwnerDocument;
            CSData.Add('Current Document View''s server ' + OwnerDocument.ServerModule.ModuleName);
            CSData.Add('Current Document View Name '      + CurrentView.ViewName);
    //        Log('Current Document View Kind '     ,CurrentView.Kind);
            CSData.Add('Current Document View Caption '   + CurrentView.Caption);
        End;

        CSData.Add('');
        CSData.Add('Current process depth ' + IntToStr(Client.ProcessControl.ProcessDepth));
        CSData.Add('Status Bar Index 0 '    + Client.GUIManager.StatusBar_GetState(0));

        CSData.Add('');
        K :=  Client.Count;
        CSData.Add('Number of active servers in DXP : ' + IntToStr(K)); // number of servers.

        For K := 0 to K - 1 Do
            CSData.Add('Server module name' + Client.ServerModule[K].ModuleName);


        CSData.Add('');
        RecordCount := Client.GetServerRecordCount;
        CSData.Add(' Server Record Count : ' +  IntToStr(RecordCount));
        CSData.Add(' =========================');
        CSData.Add('');

        For I := 0 to RecordCount - 1 Do
        Begin
            ServerRecord := Client.GetServerRecord(I);

            CSData.Add('  Server Name = ' + ServerRecord.GetName + ' #' + IntToStr(I + 1));
            CSData.Add('  ===============================================================');
            CSData.Add('    Server Version '               + ServerRecord.GetVersion);
            CSData.Add('    Server Copyright info '        + ServerRecord.GetCopyRight);
            CSData.Add('    Server Date '                  + ServerRecord.GetDate);
            CSData.Add('    Server Info '                  + ServerRecord.GetGeneralInfo);

            CSData.Add('    DDB Read Supported? '          + BooleanToString(ServerRecord.GetSupportsDDBRead));
            CSData.Add('    DDB Write Supported? '         + BooleanToString(ServerRecord.GetSupportsDDBWrite));
            CSData.Add('    System Extension? '            + BooleanToString(ServerRecord.GetSystemExtension));

            CSData.Add('    INS Path '                     + ServerRecord.GetInsPath);
            CSData.Add('    RCS path '                     + ServerRecord.GetRCSFilePath);

            CSData.Add('    Description '                  + ServerRecord.GetDescription);
            CSData.Add('    Server File Exists? '          + BooleanToString(ServerRecord.GetServerFileExist));
            CSData.Add('    Is server a document wizard? ' + BooleantoString(ServerRecord.GetIsDocumentWizard));

            // Infomration about WIndow Kinds
            CSData.Add('    Number of document types '     + IntToStr(ServerRecord.GetWindowKindCount));

            For J := 0 to ServerRecord.GetWindowKindCount - 1 Do
            Begin
                WindowKind := ServerRecord.GetWindowKind(J);
                CSData.Add('      Window Kind Name ' + WindowKind.GetName);
                CSData.Add('      New Window Caption ' + WindowKind.GetNewWindowCaption);
                CSData.Add('      New Window Extension ' + WindowKind.GetNewWindowExtension);
                CSData.Add('      Window Kind Description ' + WindowKind.GetWindowKindDescription);
                CSData.Add('      Icon Name ' + WindowKind.GetIconName);
                CSData.Add('      Is Domain? ' + BooleanToString(WindowKind.GetIsDomain));
                CSData.Add('      Is Document Editor? ' + BooleanToString(WindowKind.GetIsDocumentEditor));
                CSData.Add('');

                CSData.Add('      Window Kind Class Count ' + IntToStr(WindowKind.GetWindowKindClassCount));
                For L := 0 to WindowKind.GetWindowKindClassCount - 1 Do
                    CSData.Add('        Window Kind Class ' + WindowKind.GetWindowKindClass(L));

                CSData.Add('');
            End;

            // Information about commands
            CSData.Add('    Number of commands '           + IntToStr(ServerRecord.GetCommandCount));
            For J := 0 to ServerRecord.GetCommandCount - 1 Do
            Begin
                // IServerProcess = Server's commands
                ServerProcess := ServerRecord.GetCommand(J);
                CSData.Add('      Original Command Id: ' + ServerProcess.GetOriginalId);
                CSData.Add('      Get Long Summary: ' + ServerProcess.GetLongSummary);

                For M := 0 to ServerProcess.GetParameterCount - 1 Do
                    CSData.Add('        Server Parameter' + ServerProcess.GetParameter(M));

                CSData.Add('');
            End;

            // Informaiton about panels
            CSData.Add('    Number of Panels '             + IntToStr(ServerRecord.GetPanelInfoCount));
            For J := 0 to ServerRecord.GetPanelInfoCount - 1 Do
            Begin
                // IServerPanelInfo
                ServerPanelInfo := ServerRecord.GetPanelInfo(J);
                CSData.Add('      Panel Name ' + ServerPanelInfo.GetName);
                CSData.Add('      Panel Category ' + ServerPanelInfo.GetCategory);
                CSData.Add('      Panel Bitmap ' + ServerPanelInfo.GetBitmap);
                CSData.Add('      Panel Hot Key ' + ServerPanelInfo.GetHotkey);
                CSData.Add('      Panel Button Visible ' + BooleanToString(ServerPanelInfo.GetButtonVisible));
                CSData.Add('      Panel Multiple Creation ' + BooleanToString(ServerPanelInfo.GetMultipleCreation));
                CSData.Add('      Panel Creation Class Name ' + ServerPanelInfo.GetCreationClassName);
                CSData.Add('      Panel Dock Vertical? ' + BooleanToString(ServerPanelInfo.GetCanDockVertical));
                CSData.Add('      Panel Dock Horizontal? ' + BooleanToString(ServerPanelInfo.GetCanDockHorizontal));

                CSData.Add('');
            End;


            CSData.Add('  =====================================');
            CSData.Add('');

        End;

        FileName := SpecialFolder_MyDesigns + '\Project_Report.Txt';
    Finally
        CSData.SaveToFile(FileName);
        CSData.Free;
    End;

    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..................................................................................................}

{..................................................................................................}
