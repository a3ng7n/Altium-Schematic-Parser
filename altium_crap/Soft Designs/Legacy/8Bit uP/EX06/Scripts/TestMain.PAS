Unit Form;

Interface

Uses
    Windows,
    System,
    StrUtils,
    Registry,
    Messages,
    StrUtils,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Utility;

{..........................................................................................................}
Type
TAutoTester = Class(TForm)
    TabControl1     : TTabControl;
    EditBox         : TEdit;
    Panel1          : TPanel;
    ProgressBar     : TProgressBar;
    Panel2          : TPanel;
    Button_Go       : TButton;
    TabControl1     : TTabControl;
    Memo_LogFile    : TMemo;
    Button_Close    : TButton;
    Panel_MemoTop   : TPanel;
    Panel_MemoLeft  : TPanel;
    Panel_ScrollBar : TPanel;
    PageControl1    : TPageControl;
    TabSheet1       : TTabSheet;
    TabSheet2       : TTabSheet;
    Panel3          : TPanel;
    Panel2          : TPanel;
    Panel5          : TPanel;
    Panel6          : TPanel;
    Panel4          : TPanel;
    RichEdit1       : TRichEdit;
    Panel1          : TPanel;
    Memo_LogFile    : TMemo;

    Procedure AutoTesterShow     (Sender : TObject);
    Procedure Button_GoClick     (Sender : TObject);
    Procedure Button_CloseClick  (Sender : TObject);
    Procedure AutoTesterMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y: Integer);
    Procedure AutoTesterMouseUp  (Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    Procedure AutoTesterMouseMove(Sender : TObject; Shift                        : TShiftState; X, Y : Integer);
Public
Private
    FDeviceLink : Pointer;
    FDragging   : Boolean;
    FLastX      : Integer;
    FLastY      : Integer;
End;
{..........................................................................................................}

Const
    kWindowsDrive        = 'C:\';
    kPassedReport        = 'Nanoboard IO Passed Project.txt';
    kFailedReport        = 'Nanoboard IO Failed Project.txt';
    kCommentLine = '{............................................................................................................................}';
    kBlankLine   = '';
Var
    AutoTester: TAutoTester;

{..........................................................................................................}

Implementation
    Function  SoftDeviceCount: Integer; Forward;
    Function  ProjectPath    : String ; Forward;

    Function  RunFPGAFlow         (Project  : String     ): Boolean; Forward;
    Function  DownloadToFPGA      (Project  : String     ): Boolean; Forward;
    Function  ProjectPassed       (Project  : String     ): Boolean; Forward;
    Function  ReadIOValues        (ValueList: TStringList): Boolean; Forward;
    Function  GetDeviceName       (AIndex   : Integer    ): String ; Forward;
    Function  GetDeviceDescription(AIndex   : Integer)    : String ; Forward;

    Procedure SetupForTesting;         Forward;
    Procedure ResetProcessor;          Forward;
    Procedure CloseEverything;         Forward;
    Procedure SaveAll;                 Forward;
    Procedure TestNanoBoardIOProjects; Forward;
    Procedure OpenDevicesPage;         Forward;
    Procedure ShowOutputPanel;         Forward;
    Procedure WriteReportHeaders;      Forward;
    Procedure FinaliseTesting;         Forward;
    Procedure ShowAllInstruments;      Forward;

    Procedure CloseProjectTree    (ProjectName    : String     ); Forward;
    Procedure ProjectUnderTest    (ProjectName    : String     ); Forward;
    Function  OpenProject         (ProjectName    : String     ); Forward;
    Procedure ShowInstrument      (AIndex         : Integer    ); Forward;

    Procedure ShowProgressForm    (ProgressMessage: String ; StepProgress   : Boolean = True);    Forward;
    Procedure SaveReport          (TempReportName : String ; CurrentProject : String );           Forward;
    Function  ReadValueFromDevice (ChannelNo      : Integer; DeviceNo       : Integer): LongWord; Forward;


    Procedure WriteSectionHeader  (Var Report     : StringList ; CurrentProject : String ); Forward;
    Procedure WriteReportFooter   (Var Report     : StringList ; CurrentProject : String ); Forward;

    Procedure WriteReportSection  (    Project        : String;
                                       Report         : String;
                                       Operation      : String); Forward;

{..........................................................................................................}
{$R *.DFM}
{..........................................................................................................}
Procedure TestNanoBoardIOProjects;
Var
    ValueList: TStringList;
Begin
    ValueList := TStringList.Create;
    ValueList.LoadFromFile(ProjectPath + 'Scripts\ValueList.txt');

    SetupForTesting;

    WriteReportHeaders;

    OpenDevicesPage;

    ProjectUnderTest(ProjectPath);

    If RunFPGAFlow(ProjectPath)    Then WriteReportSection(ProjectPath, kPassedReport, 'Run FPGA Flow')
                                   Else WriteReportSection(ProjectPath, kFailedReport, 'Run FPGA Flow');

    If DownloadToFPGA(ProjectPath) Then WriteReportSection(ProjectPath, kPassedReport, 'Download To FPGA')
                                   Else WriteReportSection(ProjectPath, kFailedReport, 'Download To FPGA');

    ShowAllInstruments;

    If ReadIOValues(ValueList)     Then WriteReportSection(ProjectPath, kPassedReport, 'IO Test')
                                   Else WriteReportSection(ProjectPath, kFailedReport, 'IO Test');

    ShowProgressForm('');
    ShowProgressForm('Finalising testing for Nanoboard IO Projects');
    FinaliseTesting;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure WriteReportSection(Project : String; Report : String; Operation : String);
Var
    List : TStringList;
Begin
    List := TStringList.Create;

    If FileExists(ProjectPath + Report) Then
      List.LoadFromFile(ProjectPath + Report);

    WriteSectionHeader(List, Project);

    If AnsiCompareText(Report, 'Passed') Then List.Add(Operation + ' ' + Project + ' Passed')
                                         Else List.Add(Operation + ' ' + Project + ' Failed');

    ShowProgressForm('');
    ShowProgressForm('Creating report for project: ' + Project);

    List.SaveToFile(ProjectPath + Report);

    List.Free;
End;
{..........................................................................................................}

{..........................................................................................................}
Function RunFPGAFlow(Project : String): Boolean;
Var
    NexusWorkBench,
    DeviceLink,
    ProcessFlow;
Begin
    Result := False;

    NexusWorkBench := GetNexusWorkbench;

    DeviceLink := NexusWorkBench.GetCurrentHardDevice;

    ProcessFlow := DeviceLink.ProcessFlow;

    ShowProgressForm('Running FPGA Flow: ' + Project);
    If ProcessFlow.RunAll(NexusWorkBench) = 3 Then Result := True;

    SaveAll;
End;
{..........................................................................................................}

{..........................................................................................................}
Function DownloadToFPGA(Project : String): Boolean;
Var
    NexusWorkBench,
    DeviceLink,
    DownloadFlow;
Begin
    Result := False;

    NexusWorkBench := GetNexusWorkbench;

    DeviceLink := NexusWorkBench.GetCurrentHardDevice;

    DownloadFlow := DeviceLink.DownloadFlow;

    ShowProgressForm('Downloading To FPGA: ' + Project);
    If DownloadFlow.RunAll(NexusWorkBench) = 3 Then Result := True;

    SaveAll;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ResetProcessor;
Var
    NexusWorkBench    : INexusWorkbench;
    NexusDevice       : INexusDevice;
    DeviceLink        : IDeviceLink;
    ProcessorRegister : IProcessorRegister;
Begin
    NexusWorkBench := GetNexusWorkbench;

    DeviceLink := NexusWorkBench.GetCurrentHardDevice;

    NexusDevice := DeviceLink.GetNexusDevice;

    NexusDevice.ResetDevice;
End;
{..........................................................................................................}

{..........................................................................................................}
Function ReadValueFromDevice(ChannelNo : Integer; DeviceNo: Integer): LongWord;
Var
    NexusWorkBench    : INexusWorkbench;
    NexusDevice       : INexusDevice;
    DeviceLink        : IDeviceLink;
    ProcessorRegister : IProcessorRegister;
Begin
    Result := 0;

    NexusWorkBench := GetNexusWorkbench;

    DeviceLink := NexusWorkBench.GetSoftDevice(DeviceNo);

    NexusDevice := DeviceLink.GetNexusDevice;

    ProcessorRegister := NexusDevice.GetProcessorRegister(ChannelNo);

    If ProcessorRegister <> Nil Then
    Begin
        Try
            NexusDevice.TemporarySuspend;

            Result := ProcessorRegister.GetValue;

            NexusDevice.ContinueFromTemporarySuspend;  //If the device is a processor then reading this value will pause the device
        Finally
        End;
    End;
End;
{..........................................................................................................}

{..........................................................................................................}
Function SoftDeviceCount : Integer;
Var
    NexusWorkBench    : INexusWorkbench;
Begin
    NexusWorkBench := GetNexusWorkbench;

    If NexusWorkBench.GetSoftDeviceCount > 0 Then Result := NexusWorkBench.GetSoftDeviceCount
                                             Else Result := 0;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ShowOutputPanel;
Begin
    ResetParameters;

    AddStringParameter('ObjectKind','Panel');
    AddStringParameter('Index','4');
    AddStringParameter('Category','4');
    AddStringParameter('Action','Hide');

    RunProcess('Client:ViewToolsMenu');

    ResetParameters;

    AddStringParameter('ObjectKind','Panel');
    AddStringParameter('Index','6');
    AddStringParameter('Category','4');
    AddStringParameter('Action','Show');

    RunProcess('Client:ViewToolsMenu');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure SetupForTesting;
Begin
    Memo_LogFile.Lines.Clear;

    ProgressBar.Max := 8;

    DeleteFile(ProjectPath + 'Nanoboard IO Passed Projects Report.txt');
    DeleteFile(ProjectPath + 'Nanoboard IO Failed Projects Report.txt');

    ShowProgressForm('Starting testing...');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ShowProgressForm(ProgressMessage: String; StepProgress : Boolean = True);
Begin
    Memo_LogFile.Lines.Add(ProgressMessage);
    If StepProgress Then ProgressBar.Position := ProgressBar.Position + 1;
    Memo_LogFile.Lines.SaveToFile(ProjectPath + 'Comparison Log.txt');
    AutoTester.Show;
    AutoTester.Repaint;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure SaveReport(TempReportName : String; CurrentProject : String);
Var
    MainReport : TStringList;
    TempReport : TStringList;
    i          : Integer;
    MainReportName : String;
Begin
    MainReport := TStringList.Create;
    TempReport := TStringList.Create;

    If ProjectPassed(TempReportName) Then MainReportName := ProjectPath + 'Passed Projects Report.txt'
                                     Else MainReportName := ProjectPath + 'Failed Projects Report.txt';

    If FileExists(TempReportName) Then TempReport.LoadFromFile(TempReportName);
    If FileExists(MainReportName) Then MainReport.LoadFromFile(MainReportName);

    WriteSectionHeader(MainReport, CurrentProject);

    For i := 0 To TempReport.Count -1 Do
    Begin
        MainReport.Add(TempReport.Strings[i]);
    End;

    WriteReportFooter(MainReport, CurrentProject);

    MainReport.SaveToFile(MainReportName);
End;
{..........................................................................................................}

{..........................................................................................................}
Function ProjectPassed(Project : String): Boolean;
Begin
    Result := True;
    If FileExists(Project) Then
    Begin
        Result := False;
    End
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure WriteSectionHeader(Var Report : StringList; CurrentProject : String);
Begin
    Report.Add(kCommentLine);
    Report.Add(kBlankLine);
    Report.Add(kCommentLine);
    Report.Add(CurrentProject);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure WriteReportHeaders;
Var
    Report : TStringList;
Begin
    Report := TStringList.Create;

    If Not FileExists(ProjectPath + kPassedReport) Then
    Begin
        Report.Add('Nanoboard IO Passed Projects');
        Report.Add('The following Nanoboard project(s) were successfully tested:');
        Report.SaveToFile(ProjectPath + kPassedReport);
    End;

    Report.Clear;

    If Not FileExists(ProjectPath + kFailedReport) Then
    Begin
        Report.Add('Nanoboard IO Failed Projects');
        Report.Add('The following Nanoboard project(s) failed testing:');
        Report.SaveToFile(ProjectPath + kFailedReport);
    End;

    Report.Free;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure WriteReportFooter(Var Report : StringList; CurrentProject : String);
Begin
    Report.Add(kCommentLine);
    Report.Add(kBlankLine  );
    Report.Add(kCommentLine);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure OpenProject(ProjectName : String);
Begin
    Result := True;

    If Not FileExists(ProjectName) Then Result := False;
    ResetParameters;

    AddStringParameter('ObjectKind','Project');
    AddStringParameter('FileName', ProjectName);

    RunProcess('WorkspaceManager:OpenObject');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure SaveAll;
Begin
    ResetParameters;

    AddStringParameter('ObjectKind','All');

    RunProcess('WorkspaceManager:SaveObject');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Button_GoClick (Sender : TObject);
Begin
    TestNanoBoardIOProjects;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure CloseEverything;
Begin
    ResetParameters;
    AddStringParameter('ObjectKind','All');
    RunProcess('WorkspaceManager:CloseObject');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure CloseProjectTree(ProjectName : String);
Begin
    ResetParameters;

    AddStringParameter('ObjectKind','FocusedProjectTree');
    RunProcess('WorkspaceManager:CloseObject');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.Button_CloseClick(Sender: TObject);
Begin
    Close;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
    //Allows form to be moved by clicking anywhere on the form
    FDragging := True;
    FLastx := Mouse.CursorPos.X;
    FLastY := Mouse.CursorPos.Y;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterMouseMove (Sender : TObject; Shift : TShiftState; X, Y : Integer);
Begin
    //Allows form to be moved by clicking anywhere on the form
    If Not FDragging Then Exit;
    AutoTester.Left := AutoTester.Left + (Mouse.CursorPos.X - FLastX);
    AutoTester.Top  := AutoTester.Top  + (Mouse.CursorPos.Y - FLastY);

    FLastx := Mouse.CursorPos.X;
    FLastY := Mouse.CursorPos.Y;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure TAutoTester.AutoTesterMouseUp (Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Begin
    //Allows form to be moved by clicking anywhere on the form
    FDragging := False;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure OpenDevicesPage;
Begin
    ResetParameters;

    AddStringParameter('ObjectKind','ControlPanel');
    AddStringParameter('Index', '1');

    RunProcess('WorkspaceManager:OpenObject');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure FinaliseTesting;
Begin
    ShowProgressForm('Testing complete.');
    ProgressBar.Position := 0;
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ProjectUnderTest(ProjectName : String);
Var
    List : TStringList;
Begin
    List := TStringList.Create;
    List.Add(ProjectName);
    List.SaveToFile(ProjectPath + 'ProjectUnderTest.txt');
    List.Free;
End;
{..........................................................................................................}

{..........................................................................................................}
Function GetDeviceName(AIndex : Integer) : String;
Var
    NexusWorkBench    : INexusWorkbench;
    DeviceLink        : IDeviceLink;
Begin
    NexusWorkBench := GetNexusWorkbench;

    DeviceLink := NexusWorkBench.GetSoftDevice(AIndex);

    Result := DeviceLink.GetDeviceName;
End;
{..........................................................................................................}

{..........................................................................................................}
Function GetDeviceDescription(AIndex : Integer) : String;
Var
    NexusWorkBench    : INexusWorkbench;
    DeviceLink        : IDeviceLink;
Begin
    NexusWorkBench := GetNexusWorkbench;

    DeviceLink := NexusWorkBench.GetSoftDevice(AIndex);

    Result := DeviceLink.GetShortDescription;
End;
{..........................................................................................................}

{..........................................................................................................}
Function DeviceDesignator(AIndex : Integer) : String;
Var
    NexusWorkBench    : INexusWorkbench;
    DeviceLink        : IDeviceLink;
    NexusCore         : INexusCore;
Begin
    NexusWorkBench := GetNexusWorkbench;

    DeviceLink     := NexusWorkBench.GetSoftDevice(AIndex);

    NexusCore      := DeviceLink.GetNexusCore;

    Result         := NexusCore.GetComponentDesignator;
End;
{..........................................................................................................}

{..........................................................................................................}
Function ReadIOValues(ValueList : TStringList) : Boolean;
Var
    i           : Integer;
    j           : Integer;
    AValue      : Integer;
    DeviceValue : Integer;
    ValueString : String;
Begin
    Result := True;

    j := SoftDeviceCount;

    For j := 0 To SoftDeviceCount -1 Do
    Begin
        If Pos( 'TESTID', UpperCase(DeviceDesignator(j))) <> 0 Then
        Begin
            For i := 0 To 2 Do
            Begin
                Begin
                    AValue := StrToInt(ValueList[0]);
                    ValueList.Delete(0);

                    ShowProgressForm('Reading From Soft Device ' + GetDeviceDescription(j) + ' - Channel ' + IntToStr(i) + ': ');

                    DeviceValue := ReadValueFromDevice(i, j);
                    If Not SameText(IntToHex(DeviceValue, 2), IntToHex(AValue, 2)) Then
                    Begin
                        ShowProgressForm('Value = ' + IntToHex(ReadValueFromDevice(i, j), 2));
                        ShowProgressForm('This does not match the predetermined value of ' + IntToHex(AValue, 2) + '. Test Failed.');
                        Result := False;
                    End
                    Else
                    Begin
                        ShowProgressForm('Value = ' + IntToHex(ReadValueFromDevice(i, j), 2));
                        ShowProgressForm('This matches the predetermined value of ' + IntToHex(AValue, 2) + '. Test Passed.');
                    End;

                    ShowProgressForm(kCommentLine);
                    ShowProgressForm(kBlankLine  );
                    ShowProgressForm(kCommentLine);
                End;
            End;
        End;
    End;
End;
{..........................................................................................................}

{..........................................................................................................}
Function ProjectPath : String;
Var
    Workspace   : IWorkspace;
    Project     : IProject;
Begin
    Workspace := GetWorkspace;
    Project   := Workspace.DM_FocusedProject;
    Result    := ExtractFilePath(Project.DM_ProjectFullPath);
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ShowInstrument(AIndex : Integer);
Begin
    ResetParameters;

    AddStringParameter('Target', 'SoftDevice'     );
    AddStringParameter('Action', 'ShowViewer'     );
    AddStringParameter('Index' ,  IntToStr(AIndex));

    RunProcess('FPGAFlow:DeviceAction');
End;
{..........................................................................................................}

{..........................................................................................................}
Procedure ShowAllInstruments;
Var
    i : Integer;
Begin
    For i := 0 To SoftDeviceCount -1 Do
    Begin
        ShowInstrument(i);
    End;
End;
{..........................................................................................................}

{..........................................................................................................}

End.


