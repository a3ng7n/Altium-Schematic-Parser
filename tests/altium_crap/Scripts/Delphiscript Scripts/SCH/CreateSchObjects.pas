{..............................................................................}
{ Summary Creating Sch Objects two ways                                        }
{ Use of the RobotManager interface to send schematic messages                 }
{ the first procedure places two ports and only one undo will remove both      }
{ the second procedure places two ports and need two undos to remove both.     }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CreateSchObjectsWithOneUndos;
Var
    ANewDoc     : IServerDocument;
    Doc         : ISch_Document;
    AName       : TDynamicString;
    Orientation : TRotationBy90;
    AElectrical : TPinElectrical;
    SchPort     : ISch_Port;
    Loc         : TLocation;
Begin
    // Check if Schematic server loaded in DXP.
    If SchServer = Nil Then Exit;

    ANewDoc := CreateNewDocumentFromDocumentKind('SCH');
    If ANewDoc = Nil Then Exit;

    Doc := SchServer.GetCurrentSchDocument;
    If Doc = Nil Then Exit;


    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(Doc, '');

    // First new port created.
    SchPort := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
    If SchPort = Nil Then Exit;
    SchPort.Location  := Point(MilsToCoord(1500),MilsToCoord(1500));
    SchPort.Style     := ePortRight;
    SchPort.IOType    := ePortBidirectional;
    SchPort.Alignment := eHorizontalCentreAlign;
    SchPort.Width     := MilsToCoord(500);
    SchPort.AreaColor := 0;
    SchPort.TextColor := $00FF00;
    SchPort.Name      := 'New Port 3';

    // Add a new port object in the existing Schematic document.
    Doc.RegisterSchObjectInContainer(SchPort);
    SchServer.RobotManager.SendMessage(Doc.I_ObjectAddress,c_BroadCast,
                                       SCHM_PrimitiveRegistration,SchPort.I_ObjectAddress);

    // Second new port created.
    SchPort := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
    If SchPort = Nil Then Exit;
    SchPort.Location  := Point(MilsToCoord(2500),MilsToCoord(2500));
    SchPort.Style     := ePortRight;
    SchPort.IOType    := ePortBidirectional;
    SchPort.Alignment := eHorizontalCentreAlign;
    SchPort.Width     := MilsToCoord(500);
    SchPort.AreaColor := 0;
    SchPort.TextColor := $FF00FF;
    SchPort.Name      := 'New Port 4';

    // Add a new port object in the existing Schematic document.
    Doc.RegisterSchObjectInContainer(SchPort);
    SchServer.RobotManager.SendMessage(Doc.I_ObjectAddress,c_BroadCast,
                                       SCHM_PrimitiveRegistration,SchPort.I_ObjectAddress);



    // Clean up the robots in Schematic editor
    SchServer.ProcessControl.PostProcess(Doc, '');

    // Refresh the screen
    Doc.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
Procedure CreateSchObjectsWithTwoUndo;
Var
    ANewDOc     : IServerDocument;
    Doc         : ISch_Document;
    AName       : TDynamicString;
    Orientation : TRotationBy90;
    AElectrical : TPinElectrical;
    SchPort     : ISch_Port;
    Loc         : TLocation;
Begin
    // Check if Schematic server loaded in DXP.
    If SchServer = Nil Then Exit;

    ANewDoc := CreateNewDocumentFromDocumentKind('SCH');
    If ANewDoc = Nil Then Exit;

    Doc := SchServer.GetCurrentSchDocument;
    If Doc = Nil Then Exit;

    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(Doc, '');

    // First new port created.
    SchPort := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
    If SchPort = Nil Then Exit;
    SchPort.Location  := Point(MilsToCoord(1000),MilsToCoord(1000));
    SchPort.Style     := ePortRight;
    SchPort.IOType    := ePortBidirectional;
    SchPort.Alignment := eHorizontalCentreAlign;
    SchPort.Width     := MilsToCoord(500);
    SchPort.AreaColor := 0;
    SchPort.TextColor := $FFFF00;
    SchPort.Name      := 'New Port 1';

    // Add a new port object in the existing Schematic document.
    Doc.RegisterSchObjectInContainer(SchPort);
    SchServer.RobotManager.SendMessage(Doc.I_ObjectAddress,c_BroadCast,
                                       SCHM_PrimitiveRegistration,SchPort.I_ObjectAddress);

    // Clean up the robots in Schematic editor
    SchServer.ProcessControl.PostProcess(Doc, '');



    // Second new port created.
    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(Doc, '');

    SchPort := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
    If SchPort = Nil Then Exit;
    SchPort.Location  := Point(MilsToCoord(2000),MilsToCoord(2000));
    SchPort.Style     := ePortRight;
    SchPort.IOType    := ePortBidirectional;
    SchPort.Alignment := eHorizontalCentreAlign;
    SchPort.Width     := MilsToCoord(500);
    SchPort.AreaColor := 0;
    SchPort.TextColor := $00FFFF;
    SchPort.Name      := 'New Port 2';

    // Add a new port object in the existing Schematic document.
    Doc.RegisterSchObjectInContainer(SchPort);
    SchServer.RobotManager.SendMessage(Doc.I_ObjectAddress,c_BroadCast,
                                       SCHM_PrimitiveRegistration,SchPort.I_ObjectAddress);

    // Clean up the robots in Schematic editor
    SchServer.ProcessControl.PostProcess(Doc, '');

    // Refresh the screen
    Doc.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
