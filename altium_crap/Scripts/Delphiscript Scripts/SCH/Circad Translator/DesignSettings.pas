///////////////////////////////////////////////////////////////////////////////
Function LookupSetting(const SettingTable : TStringList;
                       const Section      : TDynamicString;
                       const SettingType  : TDynamicString;
                       var   Setting      : TDyanmicString) : Boolean;
Var
   index    : Integer;
   i        : Integer;
   str_list : TStringList;
   str      : TDynamicString;
   prefix   : TDynamicString;
Begin
     Result := False;
     If SettingTable.Find(Section, index) Then
     Begin
          str_list := SettingTable.Objects[index];
          For i := 0 To (str_list.Count - 1) Do
          Begin
               str := str_list[i];
               prefix := Section + SettingType;
               If (1 = Pos(prefix, str)) Then
               Begin
                    Setting := Copy(str, Length(prefix) + 1, Length(str) - Length(prefix));
                    Result  := True;
                    Exit;
               End;
          End;
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function ReadDesignSettings(const InputFile    : TDynamicString;
                                  SettingTable : TStringList) : Boolean;
Var
   in_file  : Text;
   str      : TDynamicString;
   section  : TDynamicString;
   str_list : TStringList;
Var
   index : Integer;
Begin
     Result := False;
     AssignFile(in_file, InputFile);
     Reset(in_file);
     While Not Eof(in_file) Do
     Begin
          ReadLn(in_file, str);
          If str <> Null Then
          Begin
               str := Trim(str);
               If Length(str) > 0 Then
               Begin
                    If (str[1] >= Chr('0')) and (str[1] <= Chr('9')) Then  // Is digit
                    Begin
                         Break;
                    End;

                    section := Copy(str, 1, 2);
                    If SettingTable.Find(section, index) Then
                    Begin
                         str_list := SettingTable.Objects[index];
                         str_list.Add(str);
                    End;
               End;
          End;
     End;
     CloseFile(in_file);
     Result := True;
End;
///////////////////////////////////////////////////////////////////////////////

