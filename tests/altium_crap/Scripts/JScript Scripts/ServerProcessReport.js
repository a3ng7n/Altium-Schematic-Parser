/*..............................................................................*/
/* Summary Server Process Report -                                              */
/*         Generate a report for all installed servers" processes.              */
/* Copyright (c) 2003 by Altium Limited                                         */
/*..............................................................................*/

var ReportFile;

/*..............................................................................*/

/*..............................................................................*/
function Log(Description, Data)
{
    ReportFile.Write(Description);
    ReportFile.WriteLine(Data);
}
/*..............................................................................*/

/*..............................................................................*/
function BooleanToString (Value)
{
    Result = "True";

    if(Value == true)
        Result = "True";
    else
        Result = "False";
}
/*..............................................................................*/

/*..............................................................................*/
function ReportServerProcessInfo()
{
    var I;
    var J;
    var K;

    var FileName;
    var ReportDocument;

    var RecordCount;
    var CommandCount;
    var ParameterCount;

    var ServerRecord;
    var ServerProcess;

    var S;

    var fso;

    if(Client == null)
        return;

    BeginHourGlass;
    S = "";

    FileName = SpecialFolder_MyDesigns + "\\ServerProcesses_Report.Txt";

    fso = new ActiveXObject("Scripting.FileSystemObject");
    ReportFile = fso.CreateTextFile(FileName, true);

    Log("Server Processes information:","");
    Log("=============================","");
    Log("","");

    RecordCount = Client.GetServerRecordCount;
    Log(" Server Record Count : ", IntToStr(RecordCount));
    Log(" =========================","");
    Log("","");

    for(I = 0; I < RecordCount; I++)
    {
        ServerRecord = Client.GetServerRecord(I);

        Log("  Server Name = ",ServerRecord.GetName + " #" + IntToStr(I + 1));
        Log("  ===============================================================","");
        Log("    Server Version ",ServerRecord.GetVersion);
        Log("    Server Copyright info ",ServerRecord.GetCopyRight);
        Log("    Server Date ",ServerRecord.GetDate);
        Log("    Server Info ",ServerRecord.GetGeneralInfo);

        Log("    RCS path "   ,ServerRecord.GetRCSFilePath);
        Log("    Ins Path "   ,ServerRecord.GetInsPath);
        Log("    Exe Path"    ,ServerRecord.GetExePath);

        Log("    Number of document types ",IntToStr(ServerRecord.GetWindowKindCount));
        Log("    Number of commands "      ,IntToStr(ServerRecord.GetCommandCount));

        Log("    =====================================","");

        CommandCount = ServerRecord.GetCommandCount;
        for(J = 0; J < CommandCount; J++)
        {
            ServerProcess = ServerRecord.GetCommand(J);
            Log("        Process #" + IntToStr(J + 1) + " Name = ", ServerProcess.GetOriginalId + " LongSummary = " + ServerProcess.GetLongSummary);

            ParameterCount = ServerProcess.GetParameterCount;
            for(K = 0; K < ParameterCount; K++)
                S = S + ServerProcess.GetParameter(K) + ", ";
            Log("        Parameters = ",S);
        }
        Log("","");
        S = "";
    }

    ReportFile.Close();

    ReportDocument = Client.OpenDocument("Text", FileName);
    if(ReportDocument != null)
        Client.ShowDocument(ReportDocument)

    EndHourGlass;
}
/*..............................................................................*/

/*..............................................................................*/

