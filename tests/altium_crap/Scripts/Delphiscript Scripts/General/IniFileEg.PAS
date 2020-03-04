{..............................................................................}
{ Summary Demo the use of TIniFile object and the Read/Write methods           }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

Uses
    IniFiles;
Var
    IniFileName : String;

{..............................................................................}

{..............................................................................}
Procedure ReadFromIniFile(AFileName : String);
Var
    IniFile : TIniFile;
    I,J     : Integer;
    Data    : String;
Begin
    IniFile := TIniFile.Create(AFileName);
    For I := 1 to 2 Do
       For J := 1 to 2 Do
          Data := Data + #13 + IniFile.ReadString('Section'+IntToStr(I),'Key' + IntToStr(I) + '_' + IntToStr(J),'Blank');

    IniFile.Free;
    ShowMessage('The IniFile has : ' +  Data);
End;
{..............................................................................}

{..............................................................................}
Procedure WriteToIniFile(AFileName : String);
Var
    IniFile : TIniFile;
    I,J     : Integer;
Begin
    IniFile := TIniFile.Create(AFileName);
    For I := 1 to 2 Do
       For J := 1 to 2 Do
          IniFile.WriteString('Section'+IntToStr(I),'Key' + IntToStr(I) + '_' + IntToStr(J),'Value' + IntToStr(I));

    (* Generates a text file of the following format
    [Section1]
    Key1_1=Value1
    Key1_2=Value1
    [Section2]
    Key2_1=Value2
    Key2_2=Value2
    *)
    IniFile.Free;
End;
{..............................................................................}

{..............................................................................}
Procedure DemoIniFile;
Begin
    IniFileName := 'C:\Test.ini';

    ShowMessage('Creating and writing data to an inifile');
    WriteToIniFile(IniFileName);

    ShowMessage('Reading data from an inifile');
    ReadFromIniFile(IniFileName);
End;
{..............................................................................}

{..............................................................................}

