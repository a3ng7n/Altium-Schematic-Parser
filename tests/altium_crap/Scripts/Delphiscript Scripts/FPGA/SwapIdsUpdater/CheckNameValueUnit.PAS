{..............................................................................}

{..............................................................................}
Function CheckName(ARecord : String; AName : String) : Boolean;
Begin
    Result := False;
    If ARecord = '' Then Exit;
    If AName   = '' Then Exit;
    AName  := UpperCase(AName);

    If Pos(AName+'=',ARecord) > 0 Then
            Result := True;
End;
{..............................................................................}

{..............................................................................}
Function CheckNameAndValue(ARecord : String; AName : String; AValue : String) : Boolean;
Begin
    Result := False;
    If ARecord = '' Then Exit;
    If AName   = '' Then Exit;

    AName  := UpperCase(AName);
    AValue := UpperCase(AValue);

    If Pos(AName+'='+AValue,ARecord) > 0 Then
            Result := True;
End;
{..............................................................................}

{..............................................................................}
Function WithNameCheckValue(ARecord : String; AName : String; AnOldValue : String) : String;
Const
    Delimiter = '|';
Var
    APos, ANameLength, AFullLength  : Integer;
    AValuePos      : Integer;
    SubString      : String;
    Finished       : Boolean;
Begin
    Result := '';
    If AName = '' Then Exit;

    AName      := UpperCase(AName) + '=';
    AnOldValue := UpperCase(AnOldValue);

    // Function from SysUtils that trims leading/trailing spaces
    // and control characters from a string
    ARecord := Trim(ARecord);

    // Both AName and AValue parameters exist...
    If AnOldValue <> '' Then
        If Pos(AName+AnOldValue,ARecord) > 0 Then
    Begin
        Result := ARecord;
        Exit;
    End;

    // Only AName parameter exist...
    If Pos(AName,ARecord) > 0 Then
    Begin
        // retrieve AValue up to (but not including) the
        // delimiter or to the end of the string

        AFullLength := Length(ARecord);

        //AValuePos is the starting point of the AValue substring
        AnOldValue := '';
        AValuePos  := Pos(AName,ARecord) + Length(AName);

        Finished   := False;
        Repeat
             AnOldValue := AnOldValue + ARecord[AValuePos];
             Inc(AValuePos);

             // Delimiter encountered or end of string reached...
             If (AValuePos          = AFullLength + 1) Or
                (ARecord[AValuePos] = DeLimiter)   Then
                 Finished := True;
         Until Finished;

         Result := AnOldValue;
    End;
End;
{..............................................................................}

{..............................................................................}
Function UpdateOrInsertNameValue(ARecord : String; AName : String; AValue : String) : String;
Var
    Temp          : String;
    ExistingValue : String;

    AnOldValue    : String;
    RetrievedValue : String;

    OldPattern    : String;
    NewPattern    : String;
Begin
    If ARecord = '' Then Exit;
    If AName   = '' Then Exit;
    If AValue  = '' Then Exit;

    AName  := UpperCase(AName);
    AValue := UpperCase(AValue);

    AnOldValue := '';
    RetrievedValue := WithNameCheckValue(ARecord,AName,AnOldValue);

    AnOldValue := '';
    RetrievedValue := WithNameCheckValue(ARecord,AName,AnOldValue);
    If RetrievedValue <> '' Then
    Begin
       //1. Old Value exists, replace that with the new value.
       OldPattern := AName + '=' + RetrievedValue;
       NewPattern := AName + '=' + AValue;
       Result := StringReplace(ARecord,OldPattern,NewPattern,MkSet(rfReplaceAll));
    End
    Else
    Begin
        // 2. the AName doesnt exist in the record, thus append AName = AValue to the record string
        Result := ARecord + ' | ' + AName + '=' + AValue;
    End;
End;
{..............................................................................}

{..............................................................................}

