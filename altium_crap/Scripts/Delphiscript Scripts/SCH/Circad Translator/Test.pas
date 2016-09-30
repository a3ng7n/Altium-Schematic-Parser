Const
//     c_TestFolder   = 'C:\DXP\FileTranslator\Installation\Test\CirCad\';
     c_TestFolder   = 'C:\Temp\Circad files\';
     c_OutputFolder = 'C:\Temp\Circad files\'
///////////////////////////////////////////////////////////////////////////////
Function TestStrTokenize(const InputFile    : TDynamicString;
                               UseAsciiCode : Boolean) : Boolean;
Var
   in_file   : Text;
   str1      : TDynamicString;
   str2      : TDynamicString;
   delimiter : TDynamicString;
   str_list  : TStringList;
   i         : Integer;
   count     : Integer;
   char_code : Integer;
Begin
     str_list := TStringList.Create;

     AssignFile(in_file, InputFile);
     Reset(in_file);
     While Not Eof(in_file) Do
     Begin
          ReadLn(in_file, str1);
          If (str1 <> Null) And (Length(str1) > 0) Then
          Begin
               If Pos('//', str1) <> 1 Then
               Begin
                    count := StrToInt(str1);
                    ReadLn(in_file, str2);  // delimiter info
                    If (str2 = Null) Or (Length(str2) = 0) Then
                    Begin
                        str_list.Free;
                        CloseFile(in_file);
                        Result := False;
                        Exit;
                    End;

                    If UseAsciiCode Then
                    Begin
                        char_code := StrToInt(str2);
                        delimiter := Chr(char_code);
                    End
                    Else
                        delimiter := str2;

                    While count > 0 Do
                    Begin
                         ReadLn(in_file, str1);
                         str_list.Clear;
                         Tokenize(str1, delimiter, str_list);
                         Log(' ');
                         Log(str1);
                         LogUnderline(str1);
                         LogStringList(str_list);
                         count := count - 1;
                    End;
               End;
          End;
     End;
     CloseFile(in_file);
     str_list.Free;
     Result := True;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TestStrToCoord(const InputFile : TDynamicString) : Boolean;
Var
   in_file : Text;
   str1    : TDynamicString;
   str2    : TDynamicString;
   str3    : TDynamicString;
   coord   : TCoord;
   i       : Integer;
Begin
     Result := True;

     AssignFile(in_file, InputFile);
     Reset(in_file);
     While Not Eof(in_file) Do
     Begin
          ReadLn(in_file, str1);
          If (str1 <> Null) And (Length(str1) > 0) Then
          Begin
               i := Pos('//', str1);
               If i > 1 Then
               Begin
                    str2 := Copy(str1, 1, i - 1);
                    i := i + 2;
                    str3 := Copy(str1, i, Length(str1) - i + 1);

                    str2 := Trim(str2);
                    str3 := Trim(str3);

                    If StrToCoord(str2, coord) Then
                    Begin
                        Log(str2 + ' = ' + FloatToStr(coord));
                        If (Length(str3) And (FloatToStr(coord) <> str3)) then
                        Begin
                             Log('Incorrect conversion ' + str2);
                             Result := False;
                        End;
                    End
                    Else
                    Begin
                         Log('Error converting ' + str2);
                         If Length(str3) And (str3 <> 'Error') Then
                         Begin
                              Result := False;
                         End;
                    End;
               End;
          End;
     End;
     CloseFile(in_file);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TestTranslateLine(const InputFile : TDynamicString;
                           const OutDir    : TDynamicString) : Boolean;
Var
   out_filepath : TDynamicString;
Begin
   Result := False;
   If Not TranslateSch(InputFile,
                       OutDir,
                       out_filepath) Then
   Begin
        Log('Cannot translate ' + InputFile);
        Exit;
   End;
   Result := True;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TestTranslate(const InputFile : TDynamicString;
                       const OutDir    : TDynamicString) : Boolean;
Var
   out_filepath : TDynamicString;
Begin
   Result := False;
   If Not TranslateSch(InputFile,
                       OutDir,
                       out_filepath) Then
   Begin
        Log('Cannot translate ' + InputFile);
        Exit;
   End;
   Result := True;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function TestGrid(const InputFile : TDynamicString;
                  const OutDir    : TDynamicString) : Boolean;
Var
   out_filepath : TDynamicString;
   str          : TDynamicString;
   sch_document : ISch_Document;
   value        : TCoord;
   setting_table : TStringList;
   items         : TStringList;
Begin
   Result := False;

   If Not TranslateSch(InputFile,
                       OutDir,
                       out_filepath) Then
   Begin
        Log('Cannot translate ' + InputFile);
        Exit;
   End;

   sch_document := SchServer.GetSchDocumentByPath(out_filepath);
   If sch_document = Nil Then
   Begin
        Log('Unable to open document ' + out_filepath);
        Exit;
   End;

   setting_table            := TStringList.Create;
   setting_table.Sorted     := True;
   setting_table.Duplicates := dupIgnore;

   InitSettingTable(setting_table);
   ReadDesignSettings(InputFile, setting_table);

   If LookupSetting(setting_table, c_SetupSection, c_GridSetting, str) Then
   Begin
        items := TStringList.Create;
        Tokenize(str, ',', items);
        If (items.Count = 0) Or (Not StrToCoord(items[0], value)) Then
        Begin
             Log('Cannot get grid spacing setting from ' + InputFile);
        End
        Else
        Begin
             If value = sch_document.VisibleGridSize Then
             Begin
                  Result := True;
             End
             Else
             Begin
                  Log('Grid spacing is not the same');
             End;
        End;
        items.Free;
   End;
   DestroyStringList(setting_table);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetAllComponent(const InputFile : TDynamicString) : Boolean;
Var
   in_file     : Text;
   str         : TDynamicString;
   log_text    : TDynamicString;
   object_type : Integer;
   data        : TStringList;
   comp_name   : TDynamicString;
Begin
     Result := True;

     log_text := 'Component in file: ' + InputFile;
     Log(log_text);
     LogUnderLine(log_text);

     data := TStringList.Create;

     AssignFile(in_file, InputFile);
     Reset(in_file);
     ReadUntilPrimaryDataSection(in_file, str);

     g_Source    := c_InputFileSource;
     g_InFile    := in_file;
     g_LookAhead := str;

     While GetNextObjectData(object_type, data) Do
     Begin
         If object_type = c_ComponentObjectCode Then
         Begin
            If GetComponentParameter(data, str) Then
            Begin
                comp_name := GetComponentProperty(str, c_ComponentNameIndex);
                Log(comp_name);
            End;
         End;
     End;
     CloseFile(in_file);
     data.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure RunTest;
Var
   log_summary : TStringList;
   test_case   : TDynamicString;
Begin
     g_DisableLog := False;
     log_summary := TStringList.Create;
     SetupLog(c_OutputFolder + 'log.txt', 'Begin Test Log');

{
     test_case := 'Test StrToCoord';
     Log(test_case);
     LogUnderLine(test_case);
     If TestStrToCoord(c_TestFolder + 'StrToCoord.dat') Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');
}

     //////////////////////////////////////
     test_case := 'Test Tokenize';
     Log(test_case);
     LogUnderLine(test_case);
     If TestStrTokenize(c_TestFolder + 'Tokenize.dat', False) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Tokenize by Ascii code';
     Log('---' + test_case + '---');
     If TestStrTokenize(c_TestFolder + 'TokenizeByAsciiCode.dat', True) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Grid 100 mil';
     Log(test_case);
     LogUnderLine(test_case);
     If TestGrid(c_TestFolder + '100MilGrid.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Grid 222 mil';
     Log(test_case);
     LogUnderLine(test_case);
     If TestGrid(c_TestFolder + '222MilGrid.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Line';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslateLine(c_TestFolder + 'Line.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Arc';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'Arc.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Elliptical Arc';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'Ellipse.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Filled Solid';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'FillSolid.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Text';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'Text.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Ports';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'port.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Power Objects';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'power.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Bus';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'bus.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Component';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'component.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');


     //////////////////////////////////////
     test_case := 'Test GetAllComponent';
     Log(test_case);
     LogUnderLine(test_case);
     If GetAllComponent(c_TestFolder + '6788057B\6788057b.sh2') Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate Pin connectivity';
     Log(test_case);
     LogUnderLine(test_case);
     If TestTranslate(c_TestFolder + 'pin_conn.sch', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     //////////////////////////////////////
     test_case := 'Test Translate 6788057b.sh1';
     Log('---' + test_case + '---');
     If TestTranslate(c_TestFolder + '6788057B\6788057b.sh1', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     test_case := 'Test Translate 6788057b.sh2';
     Log('---' + test_case + '---');
     If TestTranslate(c_TestFolder + '6788057B\6788057b.sh2', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     test_case := 'Test Translate 7078917_.sh1';
     Log('---' + test_case + '---');
     If TestTranslate(c_TestFolder + '7078917\7078917_.sh1', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     test_case := 'Test Translate 7078917_.sh2';
     Log('---' + test_case + '---');
     If TestTranslate(c_TestFolder + '7078917\7078917_.sh2', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     test_case := 'Test Translate 7078917_.sh3';
     Log('---' + test_case + '---');
     If TestTranslate(c_TestFolder + '7078917\7078917_.sh3', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     test_case := 'Test Translate 7078917_.sh4';
     Log('---' + test_case + '---');
     If TestTranslate(c_TestFolder + '7078917\7078917_.sh4', c_OutputFolder) Then
        log_summary.Add(test_case + ' SUCCESS')
     Else
        log_summary.Add(test_case + ' FAILED');

     Log('Log Summary');
     LogUnderline('Log Summary');
     LogStringList(log_summary);
     CloseLog('End');

     log_summary.Free;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure Demo;
Begin
     g_DisableLog := True;
     TestTranslate(c_TestFolder + '7078917\7078917_.sh4', c_OutputFolder);
End;
///////////////////////////////////////////////////////////////////////////////

