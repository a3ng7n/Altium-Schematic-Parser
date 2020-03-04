/*..............................................................................*/
/* Summary Using an iterator to look for sheet symbols and then                 */
/* within each sheet symbol, use the sheet symbol"s iterator to                 */
/* look for sheet entries.                                                      */
/* Copyright (c) 2004 by Altium Limited                                         */
/*..............................................................................*/
/*                                                                              */
/*..............................................................................*/
function RunSheetSymbolIterator()
{
    var CurrentSheet;
    var SheetSymbol;
    var ChildIterator;
    var ParentIterator;
    var SheetEntry;
    var EntriesNames;

    // Checks if the current document is a Schematic document
    if(SchServer == null)
        return;
    CurrentSheet = SchServer.GetCurrentSchDocument;
    if(CurrentSheet == null)
        return;

    ParentIterator = CurrentSheet.SchIterator_Create;

    EntriesNames = "";
    ParentIterator.AddFilter_ObjectSet(MkSet(eSheetSymbol));

    //Look for sheet symbols (parent objects)
    SheetSymbol = ParentIterator.FirstSchObject;

    while(SheetSymbol != null)
    {
        // Look for sheet entries (child objects) within a sheet symbol object.
        ChildIterator = SheetSymbol.SchIterator_Create;
        if(ChildIterator != null)
        {
            ChildIterator.AddFilter_ObjectSet(MkSet(eSheetEntry));
            SheetEntry = ChildIterator.FirstSchObject;
            while(SheetEntry != null)
            {
                EntriesNames = SheetEntry.Name + Chr(13) + EntriesNames;
                SheetEntry = ChildIterator.NextSchObject;
            }
            SheetSymbol.SchIterator_Destroy(ChildIterator);
        }

        if(EntriesNames != "")
            ShowInfo("The sheet symbol, " + SheetSymbol.SheetName.Text + ", contains sheet entries \n" + EntriesNames);
        EntriesNames = "";
        SheetSymbol = ParentIterator.NextSchObject;
    }
    CurrentSheet.SchIterator_Destroy(ParentIterator);
}
/*..............................................................................*/
/*                                                                              */
/*..............................................................................*/

