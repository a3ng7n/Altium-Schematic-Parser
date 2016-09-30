{..............................................................................}
{ Summary Using an iterator to look for sheet symbols and then                 }
{ within each sheet symbol, use the sheet symbol's iterator to                 }
{ look for sheet entries.                                                      }
{                                                                              }
{ Version 1.1                                                                  }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure RunSheetSymbolIterator;
Var
    CurrentSheet   : ISch_Document;
    SheetSymbol    : ISch_SheetSymbol;
    ChildIterator  : ISch_Iterator;
    ParentIterator : ISch_Iterator;
    SheetEntry     : ISch_SheetEntry;
    EntriesNames   : TDynamicString;
    LS             : TDynamicString;

    SheetSymbolCount : Integer;
    EntryCount       : Integer;
Begin
    // Checks if the current document is a Schematic document
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;


    SheetSymbolCount := 0;
    EntryCount       := 0;
    LS               := '';
    EntriesNames     := '';

    // Look for sheet symbols (parent objects) and its sheet entries (child objects)
    ParentIterator := CurrentSheet.SchIterator_Create;
    ParentIterator.AddFilter_ObjectSet(MkSet(eSheetSymbol));
    Try
        SheetSymbol := ParentIterator.FirstSchObject;

        While SheetSymbol <> Nil Do
        Begin
            Inc(SheetSymbolCount);
            // Look for sheet entries (child objects) within a sheet symbol object.
            ChildIterator := SheetSymbol.SchIterator_Create;
            If ChildIterator <> Nil Then
            Begin
                ChildIterator.AddFilter_ObjectSet(MkSet(eSheetEntry));
                Try
                    SheetEntry := ChildIterator.FirstSchObject;
                    While SheetEntry <> Nil Do
                    Begin
                        Inc(EntryCount);
                        EntriesNames := SheetEntry.Name + ' ' + EntriesNames;
                        SheetEntry   := ChildIterator.NextSchObject;
                    End;
                Finally
                    SheetSymbol.SchIterator_Destroy(ChildIterator);
                End;
            End;
            LS := LS + SheetSymbol.SheetName.Text + ' Sheet Symbol contains ' + IntToStr(EntryCount) + ' sheet entries: ' + Entriesnames + #13 + #13;
            EntriesNames := '';
            EntryCount   := 0;
            SheetSymbol := ParentIterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(ParentIterator);
    End;

    If SheetSymbolCount > 0 Then
        ShowInfo('The schematic document has ' + IntToStr(SheetSymbolCount) + ' Sheetsymbols ' + #13 + LS)
    Else
        ShowInfo('No sheet symbols found.');

End;
{..............................................................................}

{..............................................................................}
End.
