'..............................................................................
' Summary Server Process Report -
'         Generate a report for all installed servers" processes.
' Copyright (c) 2003 by Altium Limited
'..............................................................................

Dim ReportFile

'..............................................................................

'..............................................................................
Sub Log(Description, Data)
    Call ReportFile.Write(Description)
    Call ReportFile.WriteLine(Data)
End Sub
'..............................................................................

'..............................................................................
Function BooleanToString (Value)
    Result = "True"

    If Value = True Then Result = "True" Else Result = "False"
End Function
'..............................................................................

'..............................................................................
Sub ReportServerProcessInfo
    Dim I
    Dim J
    Dim K

    Dim FileName
    Dim ReportDocument

    Dim RecordCount
    Dim CommandCount
    Dim ParameterCount

    Dim ServerRecord
    Dim ServerProcess

    Dim S

    Dim fso

    If Client Is Nothing Then Exit Sub
    BeginHourGlass
    S = ""

    FileName = SpecialFolder_MyDesigns + "\ServerProcesses_Report.Txt"

    Set fso = CreateObject("Scripting.FileSystemObject")
    Set ReportFile = fso.CreateTextFile(FileName, True)

    Call Log("Server Processes information:","")
    Call Log("=============================","")
    Call Log("","")

    RecordCount = Client.GetServerRecordCount
    Call Log(" Server Record Count : ", IntToStr(RecordCount))
    Call Log(" =========================","")
    Call Log("","")

    For I = 0 to RecordCount - 1
        ServerRecord = Client.GetServerRecord(I)

        Call Log("  Server Name = ",ServerRecord.GetName + " #" + IntToStr(I + 1))
        Call Log("  ===============================================================","")
        Call Log("    Server Version ",ServerRecord.GetVersion)
        Call Log("    Server Copyright info ",ServerRecord.GetCopyRight)
        Call Log("    Server Date ",ServerRecord.GetDate)
        Call Log("    Server Info ",ServerRecord.GetGeneralInfo)

        Call Log("    RCS path "   ,ServerRecord.GetRCSFilePath)
        Call Log("    Ins Path "   ,ServerRecord.GetInsPath)
        Call Log("    Exe Path"    ,ServerRecord.GetExePath)

        Call Log("    Number of document types ",IntToStr(ServerRecord.GetWindowKindCount))
        Call Log("    Number of commands "      ,IntToStr(ServerRecord.GetCommandCount))

        Call Log("    =====================================","")

        CommandCount = ServerRecord.GetCommandCount
        For J = 0 To CommandCount - 1
            ServerProcess = ServerRecord.GetCommand(J)
            Call Log("        Process #" + IntToStr(J + 1) + " Name = ", ServerProcess.GetOriginalId + " LongSummary = " + ServerProcess.GetLongSummary)

            ParameterCount = ServerProcess.GetParameterCount
            For K = 0 To ParameterCount - 1
                S = S + ServerProcess.GetParameter(K) + ", "
            Next
            Call Log("        Parameters = ",S)
        Next
        Call Log("","")
        S = ""
    Next

    ReportFile.Close

    Set ReportDocument = Client.OpenDocument("Text", FileName)
    If Not (ReportDocument Is Nothing) Then
        Client.ShowDocument(ReportDocument)
    End If

    EndHourGlass
End Sub
'..............................................................................

'..............................................................................
