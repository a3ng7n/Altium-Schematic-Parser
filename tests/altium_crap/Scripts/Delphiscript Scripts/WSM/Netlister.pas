{..............................................................................}
// Summary Netlister - Generate a PackingList format file for a Schematic project.
// Copyright (c) 2003 by Altium Limited
{..............................................................................}

Var
    FileName    : TDynamicString;
    PackingList : TStringList;
    Project     : IProject;

    // CompsList representing the list of components of a Schematic Project
    // Each entry in the CompsKist contains two objects:
    // 1. component designator string object
    // 2. CompData as a TList object
    CompsList   : TStringList;

    //CompData (TList) contains CompStrings and PadsStrings (TSTringLIsts)
    CompData    : TList;
    CompStrings : TStringList;
    PadsStrings : TStringList;

{..............................................................................}
Const
    //Typed Constants not supported in DelphiScript.
    //Numbers : Set Of Char = ['0'..'9'];
    Numbers = '0123456789';

    Great_Equal = 0;
    Less_Equal  = 1;
    Less_Than   = 2;
    Great_Than  = 3;
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

        // Number  := Str[1] In Numbers;
        Number := Pos(Str[1],Numbers) > 0;

        counter := 1;

        //While (Counter <= LenStr) And (Number = (Str[Counter] In Numbers)) Do
        //    Inc(Counter);

        While (Counter <= LenStr) And (Number = (Pos(Str[Counter],Numbers) > 0)) Do
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
//Function CompareString(a, b : AnsiString; Comparison : TComparison) : Boolean;
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
Function ListSort(List : TStringList;Index1,Index2 : Integer) : integer;
Begin
    Result := SortedListCompare(UpperCase(List[Index1]),UpperCase(List[Index2]));
End;
{..............................................................................}

{..............................................................................}
procedure QuickSort(StringList : TStringList; L, R: Integer);
Var
  I, J, P : Integer;
Begin
    P := StringList.Count;
    If (L >= P) or (R >= P) Then Exit;

    Repeat
        I := L;
        J := R;
        P := (L + R) div 2;
        Repeat
            While ListSort(StringList, I, P) < 0 do Inc(I);
            While ListSort(StringList, J, P) > 0 do Dec(J);

            If I <= J then
            Begin
                StringList.Exchange(I,J);
                If P = I Then
                    P := J
                Else If P = J Then
                    P := I;
                Inc(I);
                Dec(J);
            End;
        Until I > J;

        If L < J Then QuickSort(StringList, L, J);
        L := I;
    Until I >= R;
End;
{..............................................................................}

{..............................................................................}
Procedure SortList(StringList : TStringList);
Begin
    QuickSort(StringList, 0, StringList.Count - 1)
End;
{..............................................................................}

{..............................................................................}
Procedure FormatNets(Value);
Var
   I , J : Integer;
   CompData : TList;
   SL1, SL2 : TStringList;
begin
   For I := 0 To CompsList.Count - 1 Do
   Begin
      PackingList.Add('(');

      // Component name
      PackingList.Add(CompsList.Strings[I]);

      // Other Component strings
      CompData := TList(CompsList.Objects[I]);
      SL1 := TStringList(CompData.Items[0]);
      For J := 0 to SL1.Count - 1 Do
         PackingList.Add(SL1.Strings[J]);

      //Pad strings for this component
      PackingList.Add(' {');
      SL2 := TStringList(CompData.Items[1]);
      For J := 0 to SL2.Count - 1 Do
      Begin
          PackingList.Add(SL2.Strings[J]);
      End;
      PackingList.Add(' }');

      // end of current component
      PackingList.Add(')');
   End;
End;
{..............................................................................}

{..............................................................................}
Procedure WriteOut(Value);
Var
    ReportDocument : IServerDocument;
Begin
    If PackingList.Count > 0 Then
    Begin
        PackingList.SaveToFile(FileName);

        ReportDocument := Client.OpenDocument('Text', FileName);
        If ReportDocument <> Nil Then
            Client.ShowDocument(ReportDocument);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure CompileProject(Value);
Begin
    Project := GetWorkspace.DM_FocusedProject;

    // do a compile so the logical documents get expanded into physical documents.
    Project.DM_Compile;

    FileName := Project.DM_ProjectFullPath;
    FileName := ChangeFileExt(FileName,'.SCHPCK');
End;
{..............................................................................}

{..............................................................................}
procedure FetchNets(Value);
Var
    I,J,K  : Integer;
    Doc  : IDocument;
    Comp : IComponent;
    Pin  : IPin;
    PinName : TDynamicString;
Begin
    For I := 0 to Project.DM_PhysicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_PhysicalDocuments(I);
        For J := 0 to Doc.DM_ComponentCount - 1 Do
        Begin
            Comp := Doc.DM_Components(J);

            CompStrings := TStringList.Create;
            CompStrings.Add(' ' + Comp.DM_Footprint);
            CompStrings.Add(' ' + Comp.DM_Comment);

            //Three blank lines ...
            CompStrings.Add('');
            CompStrings.Add('');
            CompStrings.Add('');

            PadsStrings               := TStringList.Create;
            PadsStrings.Sorted        := False;
            PadsStrings.CaseSensitive := False;

            For K := 0 to Comp.DM_PinCount - 1 Do
            Begin
                Pin := Comp.DM_Pins(K);
                PinName := Pin.DM_PinNumber;
                //Right Justify...
                Case Length(PinName) Of
                  1 : PinName := '   ' + PinName;
                  2 : PinName := '  '  + PinName;
                  3 : PinName := ' '   + PinName;
                End;

                // Check for parts of a multi-part component that are not used in the project
                // then add 'No Net' for unused pins...
                If Pin.DM_FlattenedNetName = '?' Then
                    PadsStrings.Add('  ' + PinName + ' : ' + 'No Net')
                Else
                    PadsStrings.Add('  ' + PinName + ' : ' + Pin.DM_FlattenedNetName);
             End;
             SortList(PadsStrings);

             // Add the CompsStrings and PadsStrings (both TSTringList objects)
             // in CompData (TList object)
             CompData := TList.Create;
             CompData.Add(CompStrings);
             CompData.Add(PadsStrings);

             // Add Component Designator (String) and CompData(TList)
             // in CompList (TSTringsList)
             CompsList.AddObject(' ' + Comp.DM_PhysicalDesignator,CompData);
        End;
    End;
    SortList(CompsList);
End;
{..............................................................................}

{..............................................................................}
Procedure RunPackingListGenerator;
Var
    I       : Integer;
    SL1,SL2 : TStringList;
    L1      : TList;
Begin
    BeginHourGlass;
    PackingList := TStringList.Create;

    //Store components here.
    CompsList := TStringList.Create;
    CompsList.Sorted        := False;
    CompsList.CaseSensitive := False;

    CompileProject(1);
    FetchNets(2);
    FormatNets(3);
    WriteOut(4);

    If CompsList.Count > 0 Then
    Begin
        //Go thru pin lists and comp lists and free them respectively...
        For I := 0 to CompsList.Count - 1 Do
        Begin
            L1 := TList(CompsList.Objects[I]);
            L1.Free;
        End;
    End;
    CompsList.Free;

    PackingList.Free;
    EndHourGlass;
End;
{..............................................................................}

{..............................................................................}
