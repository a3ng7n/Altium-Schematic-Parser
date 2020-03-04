'..............................................................................
' Summary Using an iterator to look for sheet symbols and then
' within each sheet symbol, use the sheet symbol"s iterator to
' look for sheet entries.
' Copyright (c) 2004 by Altium Limited
'..............................................................................

'..............................................................................
Sub RunSheetSymbolIterator
    Dim CurrentSheet
    Dim SheetSymbol
    Dim ChildIterator
    Dim ParentIterator
    Dim SheetEntry
    Dim EntriesNames

    ' Checks if the current document is a Schematic document
    If SchServer Is Nothing Then Exit Sub
    Set CurrentSheet = SchServer.GetCurrentSchDocument
    If CurrentSheet Is Nothing Then Exit Sub

    Set ParentIterator = CurrentSheet.SchIterator_Create

    EntriesNames = ""
    ParentIterator.AddFilter_ObjectSet(MkSet(eSheetSymbol))

    'Look for sheet symbols (parent objects)
    Set SheetSymbol = ParentIterator.FirstSchObject

    While Not (SheetSymbol Is Nothing)
        ' Look for sheet entries (child objects) within a sheet symbol object.
        ChildIterator = SheetSymbol.SchIterator_Create
        If Not (ChildIterator Is Nothing) Then
            ChildIterator.AddFilter_ObjectSet(MkSet(eSheetEntry))
            Set SheetEntry = ChildIterator.FirstSchObject
            While Not (SheetEntry Is Nothing)
                EntriesNames = SheetEntry.Name + Chr(13) + EntriesNames
                Set SheetEntry = ChildIterator.NextSchObject
            WEnd
            SheetSymbol.SchIterator_Destroy(ChildIterator)
        End If

        If EntriesNames <> "" Then
            ShowInfo("The sheet symbol, " + SheetSymbol.SheetName.Text + ", contains sheet entries " + Chr(13) + Entriesnames)
        End If
        EntriesNames = ""
        Set SheetSymbol = ParentIterator.NextSchObject
    WEnd
    CurrentSheet.SchIterator_Destroy(ParentIterator)
End Sub
'..............................................................................

'..............................................................................

