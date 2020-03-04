{..............................................................................}
{ Summary Demo how to create a new footprint in the PCB library                }
{                                                                              }
{ Creates a Resistor footprint and has a Resistor name                         }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    CurrentLib : IPCB_Library;
{..............................................................................}

{..............................................................................}
Procedure CreateALibComponent;
Var
    NewPCBLibComp : IPCB_LibComponent;
    NewPad        : IPCB_Pad;
    NewTrack      : IPCB_Track;
Begin
    If PCBServer = Nil Then Exit;
    CurrentLib := PcbServer.GetCurrentPCBLibrary;
    If CurrentLib = Nil Then Exit;


    NewPCBLibComp := PCBServer.CreatePCBLibComp;
    NewPcbLibComp.Name := 'Resistor';

    CurrentLib.RegisterComponent(NewPCBLibComp);


    PCBServer.PreProcess;

    NewPad := PcbServer.PCBObjectFactory(ePadObject,eNoDimension,eCreate_Default);
    NewPad.X        := MilsToCoord(0);
    NewPad.Y        := MilsToCoord(0);
    NewPad.TopXSize := MilsToCoord(62);
    NewPad.TopYSize := MilsToCoord(62);
    NewPad.HoleSize := MilsToCoord(28);
    NewPad.Layer    := eMultiLayer;
    NewPad.Name     := '1';
    NewPCBLibComp.AddPCBObject(NewPad);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);


    NewPad := PcbServer.PCBObjectFactory(ePadObject,eNoDimension,eCreate_Default);
    NewPad.X        := MilsToCoord(400);
    NewPad.Y        := MilsToCoord(0);
    NewPad.TopXSize := MilsToCoord(62);
    NewPad.TopYSize := MilsToCoord(62);
    NewPad.HoleSize := MilsToCoord(28);
    NewPad.Layer    := eMultiLayer;
    NewPad.Name     := '2';
    NewPCBLibComp.AddPCBObject(NewPad);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);


    NewTrack := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    NewTrack.X1 := MilsToCoord(40);
    NewTrack.Y1 := MilsToCoord(0);
    NewTrack.X2 := MilsToCoord(80);
    NewTrack.Y2 := MilsToCoord(0);
    NewTrack.Layer := eTopOverlay;
    NewTrack.Width := MilsToCoord(10);
    NewPCBLibComp.AddPCBObject(NewTrack);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

    NewTrack := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    NewTrack.X1 := MilsToCoord(80);
    NewTrack.Y1 := MilsToCoord(40);
    NewTrack.X2 := MilsToCoord(80);
    NewTrack.Y2 := MilsToCoord(-40);
    NewTrack.Layer := eTopOverlay;
    NewTrack.Width := MilsToCoord(10);
    NewPCBLibComp.AddPCBObject(NewTrack);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

    NewTrack := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    NewTrack.X1 := MilsToCoord(80);
    NewTrack.Y1 := MilsToCoord(40);
    NewTrack.X2 := MilsToCoord(320);
    NewTrack.Y2 := MilsToCoord(40);
    NewTrack.Layer := eTopOverlay;
    NewTrack.Width := MilsToCoord(10);
    NewPCBLibComp.AddPCBObject(NewTrack);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

    NewTrack := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    NewTrack.X1 := MilsToCoord(80);
    NewTrack.Y1 := MilsToCoord(-40);
    NewTrack.X2 := MilsToCoord(320);
    NewTrack.Y2 := MilsToCoord(-40);
    NewTrack.Layer := eTopOverlay;
    NewTrack.Width := MilsToCoord(10);
    NewPCBLibComp.AddPCBObject(NewTrack);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

    NewTrack := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    NewTrack.X1 := MilsToCoord(320);
    NewTrack.Y1 := MilsToCoord(40);
    NewTrack.X2 := MilsToCoord(320);
    NewTrack.Y2 := MilsToCoord(-40);
    NewTrack.Layer := eTopOverlay;
    NewTrack.Width := MilsToCoord(10);
    NewPCBLibComp.AddPCBObject(NewTrack);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

    NewTrack := PcbServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    NewTrack.X1 := MilsToCoord(320);
    NewTrack.Y1 := MilsToCoord(0);
    NewTrack.X2 := MilsToCoord(360);
    NewTrack.Y2 := MilsToCoord(0);
    NewTrack.Layer := eTopOverlay;
    NewTrack.Width := MilsToCoord(10);
    NewPCBLibComp.AddPCBObject(NewTrack);
    PCBServer.SendMessageToRobots(NewPCBLibComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);



    PCBServer.SendMessageToRobots(CurrentLib.Board.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPCBLibComp.I_ObjectAddress);
    PCBServer.PostProcess;

    CurrentLib.CurrentComponent := NewPcbLibComp;
    CurrentLib.Board.ViewManager_FullUpdate;
End;
{..............................................................................}

{..............................................................................}
End.

