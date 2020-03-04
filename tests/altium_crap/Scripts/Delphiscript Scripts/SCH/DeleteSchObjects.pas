{..............................................................................}
{ Summary Deleting Schematic Objects and Updating the Undo System              }
{ Use of the RobotManager interface to send schematic messages                 }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure DeleteSchObjects;
Var
    OldPort       : ISch_Port;
    Port          : ISch_Port;
    CurrentSheet  : ISch_Document;
    Iterator      : ISch_Iterator;
Begin
    // Obtain the schematic server interface.
    If SchServer = Nil Then Exit;

    // Then obtain the current schematic sheet interface.
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;


    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(CurrentSheet, '');

    // Set up iterator to look for Port objects only
    Iterator := CurrentSheet.SchIterator_Create;
    If Iterator = Nil Then Exit;
    Iterator.AddFilter_ObjectSet(MkSet(ePort));

    Try
        Port := Iterator.FirstSchObject;
        While Port <> Nil Do
        Begin
            OldPort := Port;
            Port    := Iterator.NextSchObject;

            CurrentSheet.RemoveSchObject(OldPort);
            
            SchServer.RobotManager.SendMessage(CurrentSheet.I_ObjectAddress,c_BroadCast,
                                               SCHM_PrimitiveRegistration,OldPort.I_ObjectAddress);
         End;
    Finally
         CurrentSheet.SchIterator_Destroy(Iterator);
    End;


    // Clean up robots in Schematic editor.
    SchServer.ProcessControl.PostProcess(CurrentSheet, '');

    // Refresh the screen
    CurrentSheet.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}





