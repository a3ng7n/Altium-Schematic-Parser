{..............................................................................}
{ Summary Use the CreateLibCompInfoReader method to extract component          }
{         data of a specified Sch Library.                                     }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure LibraryCompInfoReader;
Var
    CurrentLib     : ISch_Lib;
    ALibCompReader : ILibCompInfoReader;
    CompInfo       : IComponentInfo;

    FileName       : String;
    CompNum, J     : Integer;

    ReportInfo     : TStringList;
    Document       : IServerDocument;
Begin
    If SchServer = Nil Then Exit;
    CurrentLib := SchServer.GetCurrentSchDocument;
    If CurrentLib = Nil Then Exit;

    // CHeck if CurrentLib is a Library document or not
    If CurrentLib.ObjectID <> eSchLib Then
    Begin
         ShowError('Please open schematic library.');
         Exit;
    End;

    FileName := CurrentLib.DocumentName;
    // Set up Library Component Reader object.
    ALibCompReader := SchServer.CreateLibCompInfoReader(FileName);
    If ALibCompReader = Nil Then Exit;

    ALibCompReader.ReadAllComponentInfo;

    ReportInfo := TStringList.Create;

    // Obtain the number of components in the specified sch library.
    CompNum := ALibCompReader.NumComponentInfos;

    // Go thru each component obtained by the LibCompReader interface.
    For J := 0 To CompNum - 1 Do
    Begin
        ReportInfo.Add(FileName);
        CompInfo := ALibCompReader.ComponentInfos[J];
        ReportInfo.Add(' Name : '         + CompInfo.CompName);
        ReportInfo.Add('  Alias Name : '  + CompInfo.AliasName);
        ReportInfo.Add('  Part Count : '  + IntToStr(CompInfo.PartCount));
        ReportInfo.Add('  Description : ' + CompInfo.Description);
        ReportInfo.Add('  Offset : '      + IntToStr(CompInfo.Offset));
        ReportInfo.Add('');
    End;
    SchServer.DestroyCompInfoReader(ALibCompReader);

    ReportInfo.Add('');
    ReportInfo.Insert(0,'Schematic Libraries and Their Components Report');
    ReportInfo.Insert(1,'-----------------------------------------------');
    ReportInfo.Insert(2,'');

    ReportInfo.SaveToFile('C:\SchLibCompReport.txt');

    // Open and display the Component data in DXP.
    If Client = Nil Then Exit;
    Document := Client.OpenDocument('Text','c:\SchLibCompReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);

    ReportInfo.Free;
End;
{..............................................................................}

{..............................................................................}
