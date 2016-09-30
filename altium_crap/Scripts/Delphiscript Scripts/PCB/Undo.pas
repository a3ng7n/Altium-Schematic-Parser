{..............................................................................}
{ Summary Demonstration of the PCB's Undo system.                              }
{                                                                              }
{ Three procedures to demonstrate how the Undo system works -                  }
{  as one large Undo or multiple smaller Undos, and create,modify and delete   }
{                                                                              }
{ 1/ Execute UndoEach to undo each object repeatedly with several undos        }
{ 2/ Execute UndoAll procedure to remove all objects with one Undo             }
{ 3/ Create, Modify and Undo this new object                                   }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure Create2PCBObjectsWithTwoUndos;
Var
    Depth : Integer;
    Board : IPCB_Board;
    Fill1 : IPCB_Fill1;
    Fill2 : IPCB_Fill2;
Begin
    If PCBServer = Nil Then Exit;

    CreateNewDocumentFromDocumentKind('PCB');
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Initialize robots in PCB Server
    PCBServer.PreProcess;
    Fill1            := PCBServer.PCBObjectFactory(eFillObject, eNoDimension, eCreate_Default);
    Fill1.X1Location := MilsToCoord(4000);
    Fill1.Y1Location := MilsToCoord(4000);
    Fill1.X2Location := MilsToCoord(4400);
    Fill1.Y2Location := MilsToCoord(4400);
    Fill1.Rotation   := 0;
    Fill1.Layer      := eTopLayer;
    Board.AddPCBObject(Fill1);

    // Notify the robots that the pcb object has been registered.
    PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Fill1.I_ObjectAddress);
    // Clean up robots in PCB Server
    PCBServer.PostProcess;

    // Initialize robots in PCB Server
    PCBServer.PreProcess;
    Fill2     := PCBServer.PCBObjectFactory(eFillObject, eNoDimension, eCreate_Default);
    Fill2.X1Location := MilsToCoord(5000);
    Fill2.Y1Location := MilsToCoord(3000);
    Fill2.X2Location := MilsToCoord(5500);
    Fill2.Y2Location := MilsToCoord(4000);
    Fill2.Rotation   := 45;
    Fill2.Layer      := eTopLayer;
    Board.AddPCBObject(Fill2);

    // Notify the robots that the pcb object has been registered.
    PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Fill2.I_ObjectAddress);

    // Clean up robots in PCB Server
    PCBServer.PostProcess;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..................................................................................................}

{..................................................................................................}
Procedure Create2PCBObjectsWithOneUndo;
Var
    Board : IPCB_Board;
    Fill1 : IPCB_Fill1;
    Fill2 : IPCB_Fill2;
Begin
    CreateNewDocumentFromDocumentKind('PCB');
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    PCBServer.PreProcess;
    Fill1            := PCBServer.PCBObjectFactory(eFillObject, eNoDimension, eCreate_Default);
    Fill1.X1Location := MilsToCoord(4000);
    Fill1.Y1Location := MilsToCoord(4000);
    Fill1.X2Location := MilsToCoord(4400);
    Fill1.Y2Location := MilsToCoord(4400);
    Fill1.Rotation   := 0;
    Fill1.Layer      := eTopLayer;
    Board.AddPCBObject(Fill1);
    // notify the event manager that the pcb object has been registered.
    PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Fill1.I_ObjectAddress);


    Fill2     := PCBServer.PCBObjectFactory(eFillObject, eNoDimension, eCreate_Default);
    Fill2.X1Location := MilsToCoord(5000);
    Fill2.Y1Location := MilsToCoord(3000);
    Fill2.X2Location := MilsToCoord(5500);
    Fill2.Y2Location := MilsToCoord(4000);
    Fill2.Rotation   := 45;
    Fill2.Layer      := eTopLayer;
    Board.AddPCBObject(Fill2);
    // notify the event manager that the pcb object has been registered.
    PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Fill2.I_ObjectAddress);

    PCBServer.PostProcess;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..................................................................................................}

{..................................................................................................}
Var
    Fill : IPCB_Fill;
{..................................................................................................}

{..................................................................................................}
Procedure CreateObject(Dummy : Integer = 0);
Begin
    PCBServer.PreProcess;
    Fill     := PCBServer.PCBObjectFactory(eFillObject, eNoDimension, eCreate_Default);
    Fill.X1Location := MilsToCoord(4000);
    Fill.Y1Location := MilsToCoord(4000);
    Fill.X2Location := MilsToCoord(4400);
    Fill.Y2Location := MilsToCoord(4400);
    Fill.Rotation   := 0;
    Fill.Layer      := eTopLayer;

    // Adds the Fill object into the PCB database for the current PCB document.
    Board.AddPCBObject(Fill);
    PCBServer.PostProcess;
End;
{..................................................................................................}

{..................................................................................................}
Procedure ModifyObject(Dummy : Integer = 0);
Begin
    PCBServer.PreProcess;

    //Undo the fill object.
    PCBServer.SendMessageToRobots(Fill.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);
    Fill.Layer := eBottomLayer;
    PCBServer.SendMessageToRobots(Fill.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

    PCBServer.PostProcess;
End;
{..................................................................................................}

{..................................................................................................}
Procedure RemoveObject(Dummy : Integer = 0);
Begin
    PCBServer.PreProcess;

    //Remove the fill object.
    Board.RemovePCBObject(Fill);

    PCBServer.PostProcess;
End;
{..................................................................................................}

{..................................................................................................}
Procedure CreateModifyRemoveAObject;
Begin
    CreateNewDocumentFromDocumentKind('PCB');
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    ShowInfo('Creating an object');
    CreateObject;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    ShowInfo('Modifying this object');
    ModifyObject;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    ShowInfo('Undoing the modification');
    RemoveObject;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..................................................................................................}

{..................................................................................................}
