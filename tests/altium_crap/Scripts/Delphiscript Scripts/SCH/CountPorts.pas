{..............................................................................}
{ Summary CountPadsUsingIterator - Use an iterator to count ports.             }
{ Copyright (c) 2003 by Altium Limited                                         }  
{..............................................................................}

{..............................................................................}
Procedure CountPortObjects;
Var
    Port         : ISch_Port;
    CurrentSheet : ISch_Document;
    Iterator     : ISch_Iterator;
    PortNumber   : Integer;
Begin
    // Check if schematic server exists or not.
    If SchServer = Nil Then Exit;

    // Obtain the current schematic sheet interface.
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;


    // Set the number of ports found to 0.
    PortNumber := 0;


    // Set up an iterator to look for port objects only.
    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePort));

    // Using a Try Finally block to avoid exception errors.
    Try
        Port := Iterator.FirstSchObject;
        While Port <> Nil Do
        Begin
            If Port.ObjectId = ePort Then
                PortNumber := PortNumber + 1;

            Port := Iterator.NextSchObject;
        End;

    Finally
        CurrentSheet.SchIterator_Destroy(Iterator);
    End;


    // Display the results on a ShowInfo modal dialog.
    ShowInfo ('The number of ports on the current schematic is : ' +
               IntToStr(PortNumber));

End;
{..............................................................................}

{..............................................................................}
