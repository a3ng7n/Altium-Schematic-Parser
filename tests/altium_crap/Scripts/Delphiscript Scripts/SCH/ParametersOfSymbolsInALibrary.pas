{..............................................................................}
{ Summary Demo how to iterate through a schematic library and fetching         }
{         parameters for each symbol in this library.                          }
{                                                                              }
{ Version 1.1                                                                  }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure GenerateReport(Report : TStringList);
Var
    Document : IServerDocument;
Begin
    Report.Insert(0,'Schematic Library Parameters Report');
    Report.Insert(1,'-----------------------------------');
    Report.SaveToFile('c:\LibraryParamReport.txt');

    Document := Client.OpenDocument('Text','c:\LibraryParamReport.txt');
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
Procedure LookInsideALibraryForParameters;
Var
    CurrentLib      : ISch_Lib;
    LibraryIterator : ISch_Iterator;
    AnIndex         : Integer;
    i               : integer;
    LibComp         : ISch_Component;
    S               : TDynamicString;
    PIterator       : ISch_Iterator;
    Parameter       : ISch_Parameter;
    ReportInfo      : TstringList;
Begin
    If SchServer = Nil Then Exit;
    CurrentLib := SchServer.GetCurrentSchDocument;
    If CurrentLib = Nil Then Exit;

    // check if the document is a schematic library
    If CurrentLib.ObjectID <> eSchLib Then
    Begin
         ShowError('Please open schematic library.');
         Exit;
    End;


    // Create a TStringList object to store data
    ReportInfo := TStringList.Create;
    ReportInfo.Clear;

    // create a library iterator to look for
    // symbols in the currently focussed library.
    LibraryIterator := CurrentLib.SchLibIterator_Create;
    LibraryIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Try
        // obtain the first symbol in the library
        LibComp := LibraryIterator.FirstSchObject;
        While LibComp <> Nil Do
        Begin
            ReportInfo.Add(LibComp.LibReference + ' ' + LibComp.Designator.Text);
            // look for parameters associated with this symbol in a library.
            Try
                ReportInfo.Add('Parameters for this symbol');

                PIterator := LibComp.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                Parameter := PIterator.FirstSchObject;
                While Parameter <> Nil Do
                Begin
                    ReportInfo.Add(' ' + Parameter.CalculatedValueString);
                    Parameter := PIterator.NextSchObject;
                End;
            Finally
                LibComp.SchIterator_Destroy(PIterator);
            End;
            ReportInfo.Add('');
            // obtain the next symbol in the library
            LibComp := LibraryIterator.NextSchObject;
        End;
    Finally
        // done with looking for symbols and parameters.
        // destroy the library iterator.
        CurrentLib.SchIterator_Destroy(LibraryIterator);
    End;

    GenerateReport(ReportInfo);
    ReportInfo.Free;
End;
{..............................................................................}

{..............................................................................}
