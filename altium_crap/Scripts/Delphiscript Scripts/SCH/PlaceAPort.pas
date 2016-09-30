{..............................................................................}
{ Summary Placing a new port object.                                           }
{ Copyright (c) 2004 by Altium Limited                                         } 
{..............................................................................}

{..............................................................................}
Procedure PlaceAPort;
Var
    SchPort     : ISch_Port;
    FSchDoc     : ISch_Document;
    CurView     : IServerDocumentView;
Begin
    // Check if Schematic server exists or not.
    If SchServer = Nil Then Exit;

    // Obtain the Schematid sheet interfac.e
    FSchDoc := SchServer.GetCurrentSchDocument;
    If FSchDoc = Nil Then Exit;

    // Create a new port object
    SchPort := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
    If SchPort = Nil Then Exit;

    // Set up parameters for the port object.
    // the port is placed at 500,500 mils respectively.
    SchPort.Location  := Point(MilsToCoord(500),MilsToCoord(500));
    SchPort.Style     := ePortRight;
    SchPort.IOType    := ePortBidirectional;
    SchPort.Alignment := eHorizontalCentreAlign;

    SchPort.Width     := MilsToCoord(1000);

    SchPort.AreaColor := 0;
    SchPort.TextColor := $FFFFFF;
    SchPort.Name      := 'A new port with no net.';

    // Add a port object onto the existing schematic document
    FSchDoc.RegisterSchObjectInContainer(SchPort);

    // Refresh the schematic sheet.
    FSchDoc.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}

