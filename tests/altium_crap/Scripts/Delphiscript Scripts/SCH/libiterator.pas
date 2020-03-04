{..............................................................................}
{ Summary Demo how to iterate through a schematic library.                     }
{                                                                              }
{ Version 1.1                                                                  }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure GenerateReport(Report : TStringList);
Var
    Document : IServerDocument;
Begin
    Report.Insert(0,'Schematic Library Alias Report');
    Report.Insert(1,'------------------------------');
    Report.SaveToFile('C:\LibraryReport.txt');

    Document := Client.OpenDocument('Text','C:\LibraryReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
Procedure LookInsideALibrary;
Var
    CurrentLib      : ISch_Lib;
    LibraryIterator : ISch_Iterator;
    AnIndex         : Integer;
    i               : integer;
    LibComp         : ISch_Component;
    S               : TDynamicString;
    ReportInfo      : TStringList;
Begin
    If SchServer = Nil Then Exit;
    CurrentLib := SchServer.GetCurrentSchDocument;
    If CurrentLib = Nil Then Exit;

    // check if the document is a schematic library and if not
    // exit.
    If CurrentLib.ObjectID <> eSchLib Then
    Begin
         ShowError('Please open schematic library.');
         Exit;
    End;

    // get the library object for the library iterator.
    LibraryIterator := CurrentLib.SchLibIterator_Create;

    // Note MkSet function to create a set compatible with the
    // Scripting engine since sets not supported.
    LibraryIterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    // Create a TStringList object to store data
    ReportInfo := TStringList.Create;

    // use of Try / Finally / End exception block to
    // trap exceptions and exit gracefully.
    Try
        // find the aliases for the current library component.
        LibComp := LibraryIterator.FirstSchObject;
        While LibComp <> Nil Do
        Begin
            ReportInfo.Add(LibComp.LibReference + ' ' + LibComp.Designator.Text);

            AnIndex := LibComp.AliasCount;
            If AnIndex = 0 Then
                ReportInfo.Add('No Aliases found...')
            Else
            For i := 0 to AnIndex - 1 do
                ReportInfo.Add('Aliasname= ' + LibComp.Alias[i]);

            ReportInfo.Add('');
            // obtain the next schematic symbol in the library
            LibComp := LibraryIterator.NextSchObject;
        End;
    Finally
        // we are finished fetching symbols of the current library.
        CurrentLib.SchIterator_Destroy(LibraryIterator);
    End;

    GenerateReport(ReportInfo);
    ReportInfo.Free;
End;
{..............................................................................}

{..............................................................................}
End.

// Synopsis
// --------
// This library iterator iterates through a schematic library and checks each component for its alias.
// A component might have variations for example different power consumption and switching speeds but have the
// same functionality. For example 74 series might have a 74LS and 74S variations.

// A good example to find aliases of library components is the 4 Port Serial Interface.SchLib file.
