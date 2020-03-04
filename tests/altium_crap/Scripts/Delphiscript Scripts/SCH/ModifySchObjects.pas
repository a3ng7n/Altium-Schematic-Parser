{..............................................................................}
{ Summary Fetching and modifying Schematic objects.                            }
{                                                                              }
{ Use of the RobotManager interface to send schematic messages                 }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure FetchAndModifyObjects;
Var
    AnObject        : ISch_GraphicalObject;
    Iterator        : ISch_Iterator;
    Doc             : ISch_Document;
Begin
    If SchServer = Nil Then Exit;

    Doc             := SchServer.GetCurrentSchDocument;
    If Doc = Nil Then Exit;

    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(Doc, '');

    Iterator        := Doc.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePort, eWire));

    If Iterator = Nil Then Exit;
    Try
        AnObject := Iterator.FirstSchObject;
        While AnObject <> Nil Do
        Begin
            SchServer.RobotManager.SendMessage(AnObject.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
            Case AnObject.ObjectId Of
               eWire   : AnObject.Color     := $0000FF; //red color in bgr format
               ePort   : AnObject.AreaColor := $00FF00; //green color in bgr format
            End;
            SchServer.RobotManager.SendMessage(AnObject.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);

            AnObject := Iterator.NextSchObject;
        End;
    Finally
        Doc.SchIterator_Destroy(Iterator);
    End;

    // Clean up the robots in Schematic editor
    SchServer.ProcessControl.PostProcess(Doc, '');

    // Refresh schematic sheet
    Doc.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
