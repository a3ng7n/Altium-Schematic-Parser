Const
    c_NumberOfTasks = 3;
Var
    TranslatorDialog : TTranslatorDialog;

procedure TTranslatorDialog.OnFormCreate(Sender: TObject);
begin
    ProgressBarStatus.Min := 0;
    ProgressBarStatus.Max := c_NumberOfTasks;
    InputFileCtrl.Text := '';
    OutputDirCtrl.Text := '';
end;

procedure TTranslatorDialog.ButtonCancelClick(Sender: TObject);
begin
    Close;
end;


procedure TTranslatorDialog.ButtonTranslateClick(Sender: TObject);
Var
    input_file  : TDynamicString;
    output_file : TDynamicString;
    output_dir  : TDynamicString;

    setting_table : TStringList;
    sch_document  : ISch_Document;
    document      : IServerDocument;
    filename      : TDynamicString;

Begin
    ProgressBarStatus.Position := 0;
    LabelStatus.Caption := '';

    input_file  := InputFileCtrl.Text;
    output_dir  := OutputDirCtrl.Text;

    If (input_file = '') Or (output_dir = '') Then
    Begin
        LabelStatus.Caption := 'Please set input file and output dir';
        Exit;
    End;

    Try
         BeginHourGlass;

         LabelStatus.Caption := 'Validate input file';

         If Not ValidateFile(input_file) Then
         Begin
              LabelStatus.Caption := 'Input file is not a CirCad data file.';
              Exit;
         End;
         ProgressBarStatus.StepBy(1);

         filename := CreateOutputFileName(input_file);
         If filename = '' Then
         Begin
              LabelStatus.Caption := 'Cannot construct output file name';
              Exit;
         End;

         LabelStatus.Caption := 'Translating to ' + filename;

         output_file := IncludeTrailingPathDelimiter(output_file) + filename;
         document := CreateDocument(output_file, cDocKind_Sch, 'BINARY');
         If document = nil Then
         Begin
              LabelStatus.Caption := 'Cannot create documment ' + output_file;
              Exit;
         End;

         ProgressBarStatus.StepBy(1);

         setting_table            := TStringList.Create;
         setting_table.Sorted     := True;
         setting_table.Duplicates := dupIgnore;

         InitSettingTable(setting_table);
         ReadDesignSettings(input_file, setting_table);
         g_BusLayerNum := GetBusLayerNum(setting_table);

         sch_document := SchServer.GetSchDocumentByPath(output_file);
         SetupSchDocument(sch_document, setting_table);

         TranslatePrimaryData(sch_document, input_file);

        sch_document.CustomX := g_WorkspaceWidth;
         sch_document.CustomY := g_WorkspaceHeight;
         sch_document.UpdateDocumentProperties;
         ProgressBarStatus.StepBy(1);

         LabelStatus.Caption := 'Save document';

         SaveDocument(document, 'BINARY');
         DestroyStringList(setting_table);

         ProgressBarStatus.StepBy(1);
         LabelStatus.Caption := 'Done';
    Finally
        EndHourGlass;
    End;
End;

procedure TTranslatorDialog.InputFileBrowseClick(Sender: TObject);
begin
    If OpenDialogCtrl.Execute Then
        InputFileCtrl.Text := OpenDialogCtrl.FileName;
end;


