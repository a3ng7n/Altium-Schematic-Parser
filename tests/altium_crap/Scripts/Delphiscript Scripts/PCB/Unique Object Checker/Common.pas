{..............................................................................}
{ Summary Common routines to use for the PCB_Scripts project                   }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Const
    Great_Equal = 1;
    Less_Equal  = 2;
    Less_Than   = 3;
    Great_Than  = 4;
{..............................................................................}

{..............................................................................}
Procedure RemoveSpaces(Var Str : AnsiString);
Var
    SpacePos : Integer;
Begin
    Repeat
        SpacePos := Pos(' ', Str);
        If SpacePos <> 0 Then
            Delete(Str, SpacePos, 1);
    Until SpacePos = 0;
End;
{..............................................................................}

{..............................................................................}
Function IsNumber(AChar : Char) : Boolean;
Begin
    Result := ('0' <= AChar) And (AChar <= '9');
End;
{..............................................................................}

{..............................................................................}
Function Strip(Var Str : AnsiString) : AnsiString;
Var
    t       : AnsiString;
    LenStr  : Integer;
    Counter : Integer;
    Number  : Boolean;
Begin
    If Str = '' Then
        Result := ''
    Else
    Begin
        LenStr  := Length(Str);
        Number  := IsNumber(Str[1]);
        counter := 1;
        While (Counter <= LenStr) And (Number = IsNumber(Str[Counter])) Do
            Inc(Counter);
        t := Copy(Str, 1, Counter - 1);
        If Counter <= LenStr Then
            Str := Copy(Str, Counter, LenStr - Counter + 1)
        Else
            Str := '';
        If Number Then
            t := Copy('00000000' + t, Counter, 8);
        Result := t;
    End;
End;
{..............................................................................}

{..............................................................................}
Function CompareString(a, b : AnsiString; Comparison : Integer) : Boolean;
Var
    a1, b1 : AnsiString;
Begin
    Result := False;

    RemoveSpaces(a);
    RemoveSpaces(b);
    Repeat
        a1 := Strip(a);
        b1 := Strip(b);
    Until ((a1 <> b1) Or (a1 = '') And (b1 = ''));
    Case Comparison Of
        Great_Equal : Result := a1 >= b1;
        Less_Equal  : Result := a1 <= b1;
        Less_Than   : Result := a1 < b1;
        Great_Than  : Result := a1 > b1;
    End;
End;
{..............................................................................}

{..............................................................................}
Function LessThan(Const a, b : AnsiString) : Boolean;
Begin
    Result := CompareString(a, b, Less_Than);
End;
{..............................................................................}

{..............................................................................}
Function GreatThan(Const a, b : AnsiString) : Boolean;
Begin
    Result := CompareString(a, b, Great_Than);
End;
{..............................................................................}

{..............................................................................}
Function SortedListCompare(Const S1, S2 : AnsiString) : Integer;
Begin
    If S1 = S2 Then
        Result := 0
    Else If LessThan(S1, S2) Then
        Result := -1
    Else If GreatThan(S1, S2) Then
        Result := +1
    Else
    Begin
        {Handle the special case N01 and N001 - suffix is numerically same}
        {but alphanumerically different}
        {So resort to using straight string comparison}
        If S1 < S2 Then
            Result := -1
        Else If S1 > S2 Then
            Result := +1
        Else
            Result := 0;
    End;
End;
{..............................................................................}

{..............................................................................}
Function ListSort(List : TStringList; Index1, Index2 : Integer) : integer;
Begin
    Result := SortedListCompare(UpperCase(List[Index1]), UpperCase(List[Index2]));
End;
{..............................................................................}

{..............................................................................}
Procedure ExchangeItems(AList : TStringList; L, R : Integer);
Var
    tmp : String;
Begin
    // note - objects are not exchanged!
    tmp      := AList[L];
    AList[L] := AList[R];
    AList[R] := tmp;
End;
{..............................................................................}

{..............................................................................}
Procedure QuickSortStringList(AList : TStringList; L, R : Integer);
Var
   I, J, P: Integer;
Begin
    If AList.Count = 0 Then Exit;
    Repeat
        I := L;
        J := R;
        P := (L + R) shr 1;
        Repeat
            While ListSort(AList, I, P) < 0 do Inc(I);
            While ListSort(AList, J, P) > 0 do Dec(J);
            If I <= J Then
            Begin
                ExchangeItems(AList, I, J);
                If P = I Then
                   P := J
                Else if P = J Then
                  P := I;
                Inc(I);
                Dec(J);
            End;
        Until I > J;

        If L < J Then 
            QuickSortStringList(AList, L, J);
        L := I;
    Until I >= R;
End;
{..............................................................................}

{..............................................................................}
