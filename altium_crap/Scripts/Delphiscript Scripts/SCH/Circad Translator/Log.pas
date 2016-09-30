Var
   g_LogFile    : Text;
   g_DisableLog : Boolean;

///////////////////////////////////////////////////////////////////////////////
Procedure Log(const Msg : TDynamicString);
Begin
     If g_DisableLog Then
        Exit;
     WriteLn(g_LogFile, Msg);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure LogUnderLine(const InputStr : TDynamicString);
Var
    i       : Integer;
    str_len : Integer;
    str     : TDynamicString;
Begin
    str_len := Length(InputStr);
    For i := 1 To str_len Do
    Begin
        str := str + '-';
    End;
    If Length(str) > 0 Then
    Begin
        Log(str);
    End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure SetupLog(const LogFileName : TDynamicString;
                   const Msg         : TDynamicString);
Begin
     If g_DisableLog Then
        Exit;
     AssignFile(g_LogFile, LogFileName);
     ReWrite(g_LogFile);
     If Length(Msg) > 0 Then
     Begin
        Log(Msg);
        LogUnderline(Msg);
     End;
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure CloseLog(const Msg : TDynamicString);
Begin
    If g_DisableLog Then
        Exit;
    If Length(Msg) > 0 Then
    Begin
        LogUnderline(Msg);
        Log(Msg);
    End;
    CloseFile(g_LogFile);
End;
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
Procedure LogStringList(StrList : TStringList);
Var
   i : Integer;
Begin
     If g_DisableLog Then
        Exit;
     For i := 0 To (StrList.Count - 1) Do
     Begin
         Log(StrList[i]);
     End;
End;
///////////////////////////////////////////////////////////////////////////////

