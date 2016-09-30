{..............................................................................}
{ Summary Deleting Cross Sheet Connectors and replacing them with Ports        }
{                                                                              }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure ReplaceCrossSheetsWithPorts;
Var
    OldCrossConn  : ISch_CrossSheetConnector;
    CrossConn     : ISch_CrossSheetConnector;

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
    Iterator.AddFilter_ObjectSet(MkSet(eCrossSheetConnector));

    Try
        CrossConn := Iterator.FirstSchObject;
        While CrossConn <> Nil Do
        Begin
            OldCrossConn := CrossConn;

            //create a new port object to replace this cross sheet connector.
            Port := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
            If Port = Nil Then Exit;

            // grab CrossConn location and pass onto the new port's location
            Port.Location  := CrossConn.Location;

            // Port alignment is determined by the CrossConnector's Style.
            If CrossConn.CrossSheetStyle = eCrossSheetRight Then
                Port.Alignment := eRightAlign
            Else
                Port.Alignment := eLeftAlign;

            // assume port.IOType is Bidirectional;
            Port.IOType    := ePortBidirectional;

            
            // Port Style determined by CrossConn orientation.
            If CrossConn.Orientation = eRotate0   Then Port.Style := ePortRight;
            If CrossConn.Orientation = eRotate90  Then Port.Style := ePortTop;
            If CrossConn.Orientation = eRotate180 Then Port.Style := ePortLeft;
            If CrossConn.Orientation = eRotate270 Then Port.Style := ePortBottom;


            // Assume Width of 500 Mils =
            Port.Width     := MilsToCoord(500);

            // assume white for area and black for text
            Port.AreaColor := $FFFFFF;
            Port.TextColor := $0;

            //Port Netname = Cross Sheet Connector's Text property.
            Port.Name      := CrossConn.Text;

            // Add a port object onto the existing schematic document
            CurrentSheet.RegisterSchObjectInContainer(Port);

            CrossConn    := Iterator.NextSchObject;

            CurrentSheet.RemoveSchObject(OldCrossConn);
            SchServer.RobotManager.SendMessage(CurrentSheet.I_ObjectAddress,c_BroadCast,
                                               SCHM_PrimitiveRegistration,OldCrossConn.I_ObjectAddress);
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





