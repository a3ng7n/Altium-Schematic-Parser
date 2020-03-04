Const
    c_InputFileSource  = 0;
    c_StringListSource = 1;
Var
   char_set    : array[0..255];

   g_Source    : Integer;  // 0 for input file, 1 for string list
   g_StrList   : TStringList;
   g_Index     : Integer;
   g_InFile    : Text;
   g_LookAhead : TDynamicString;

///////////////////////////////////////////////////////////////////////////////
Function GetNextLine(var Str : TDynamicString) : Boolean;
Begin
    Result := False;

    If (Length(g_LookAhead) > 0) Then
    Begin
        Str := g_LookAhead;
        g_LookAhead := '';
        Result := True;
        Exit;
    End;

    If (g_Source = c_InputFileSource) Then
    Begin
        While (Not Eof(g_InFile)) Do
        Begin
            ReadLn(g_InFile, Str);
            If (Str <> Null) Then
            Begin
                Result := True;
                Break;
            End;
        End;
    End
    Else If (g_Source = c_StringListSource) Then
    Begin
        If (g_Index < g_StrList.Count) Then
        Begin
            Str := g_StrList[g_Index];
            Result := True;
            Inc(g_Index);
        End;
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure DestroyStringList(var str_list : TStringList);
Var
   i     : LongInt;
   items : TStringList;
Begin
     For i := 0 To (str_list.Count - 1) Do
     Begin
          items := str_list.Objects[i];
          items.Free;
     End;
     str_list.Free;
     str_list := Nil;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure TokenizeString(const InputStr : TDynamicString;
                               ch       : Integer;
                               StrList  : TStringList;
                               MaxCount : Integer);
Var
   i        : Integer;
   start_i  : Integer;
   count    : Integer;
   token    : TDynamicString;
Begin
     If InputStr = Null Then
         Exit;
     token   := '';
     count   := 0;
     start_i := 0;

     StrList.Clear;

     i := 1;
     While i <= Length(InputStr) Do
     Begin
          If ch <> Ord(InputStr[i]) Then
          Begin
               If count = 0 Then
                  start_i := i;
               count := count + 1;
          End
          Else
          Begin
               If count > 0 Then
               Begin
                    If StrList.Count = MaxCount Then
                    Begin
                        token := Copy(InputStr, start_i, Length(InputStr) - start_i + 1);
                        i := Length(InputStr);
                    End
                    Else
                        token := Copy(InputStr, start_i, count);

                    StrList.Add(token);
                    count := 0;
               End;
               start_i := i;
          End;
          i := i + 1;
     End;

     If count > 0 Then
     Begin
          token := Copy(InputStr, start_i, count);
          StrList.Add(token);
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure Tokenize(const InputStr     : TDynamicString;
                   const DelimiterStr : TDynamicString;
                         StrList      : TStringList);
Var
   i        : Integer;
   max      : Integer;
   j        : Integer;
   start_i  : Integer;
   count    : Integer;
   token    : TDynamicString;
Begin
     If Length(DelimiterStr) = 1 Then
     Begin
        TokenizeString(InputStr, Ord(DelimiterStr[1]), StrList, -1);
        Exit;
     End;

     max := 255;
     For i := 0 To max Do
     Begin
          char_set[i] := True;
     End;

     For i := 1 To Length(DelimiterStr) Do
     Begin
          j := Ord(DelimiterStr[i]);
          If (j >= 0) And (j <= max) Then
          Begin
               char_set[j] := False;
          End;
     End;

     token   := '';
     count   := 0;
     start_i := 0;

     StrList.Clear;

     i := 1;
     While i <= Length(InputStr) Do
     Begin
          j := Ord(InputStr[i]);
          If char_set[j] Then
          Begin
               If Count = 0 Then
                  start_i := i;
               count := count + 1;
          End
          Else
          Begin
               If count > 0 Then
               Begin
                    token := Copy(InputStr, start_i, count);
                    StrList.Add(token);
                    count := 0;
               End;
               start_i := i;
          End;
          i := i + 1;
     End;

     If count > 0 Then
     Begin
          token := Copy(InputStr, start_i, count);
          StrList.Add(token);
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function StrToCoord(const InputStr  : TDynamicString;
                     var  Coord     : TCoord) : Boolean;
Var
   i     : Integer;
   str   : TDynamicString;
   u     : Integer;
   value : TDouble;
Begin
     Result := True;

     If (InputStr = Null) Or (Length(InputStr) = 0) Then
     Begin
          Result := False;
          Exit;
     End;

     Try
          i := Pos('"', InputStr);
          If i > 0 Then
          Begin
               str   := Copy(InputStr, 1, i - 1);
               value := StrToFloat(str);
               Coord := InchesToCoord(value);
               Exit;
          End;

          i := Pos('mm', InputStr);
          If i > 0 Then
          Begin
               str   := Copy(InputStr, 1, i - 1);
               value := StrToFloat(str);
               Coord := MMsToCoord(value);
               Exit;
          End;

          i := Pos('mil', InputStr);
          If i > 0 Then
          Begin
               str   := Copy(InputStr, 1, i - 1);
               value := StrToFloat(str);
               Coord := MilsToCoord(value);
               Exit;
          End;

          value := StrToFloat(InputStr);
          Coord := InchesToCoord(value);
     Except
           Result := False;
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Function GetField(const InputStr   : TDynamicString;
                        Index      : Integer;
                        Delimiter  : Integer;
                  var   StartIndex : Integer;
                  var   EndIndex   : Integer)
                  : Boolean;
Begin
    Result     := False;
    StartIndex := -1;

    While (Index <= Length(InputStr)) Do
    Begin
        If (Ord(InputStr[Index]) = Delimiter) Then
        Begin
            If StartIndex <> -1 Then
            Begin
                Break;
            End;
        End
        Else
        Begin
            If StartIndex = -1 Then
                StartIndex := Index;
        End;
        Inc(Index);
    End;

    If StartIndex <> -1 Then
    Begin
        EndIndex := Index - 1;
        Result   := True;
    End;
End;
///////////////////////////////////////////////////////////////////////////////



