{..............................................................................}
{ Summary Create a new Via object on a PCB document.                           }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure ViaCreation;
Var
    Board    : IPCB_Board;
    BR       : TCoordRect;
    Sheet    : IPCB_Sheet;
    Via      : IPCB_Via;
    PadCache : TPadCache;
Begin
    // Grab the board interface representing the current PCB document in DXP.
    Board := PCBServer.GetCurrentPCBBoard;

    // If the board interface doesnt exist (no PCB document) then exit.
    If Board = Nil Then Exit;
    
    // Initialize the systems in the PCB Editor.
    PCBServer.PreProcess;

    Sheet := Board.PCBSheet;   

    // Create a Via object with the PCBObjectFactory method
    // and then with the new attributes.

    // Note we convert values in Mils to internal coordinates
    // using the MilsToCoord function. All PCB objects locations and sizes
    // have internal coordinate units where 1 mil = 10000 internal units

    Via           := PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default);

    // obtain the bottom left coordinates of the board outline
    BR := Board.BoardOutline.BoundingRectangle;
    Via.x := BR.Left   + MilsToCoord(500);
    Via.y := BR.Bottom + MilsToCoord(500);

//    Via.x         := Sheet.SheetX + MilsToCoord(500);
//    Via.y         := Sheet.SheetY + MilsToCoord(500);

    Via.Size      := MilsToCoord(50);
    Via.HoleSize  := MilsToCoord(20);


    // Assign Via to the Top layer and bottom layer.
    Via.LowLayer  := eTopLayer;
    Via.HighLayer := eBottomLayer;
    
    // Set up Cache info for Via
    // which consists mainly solder mask, paste mask and power plane values from design rules
    Padcache                           := Via.GetState_Cache;
    Padcache.ReliefAirGap              := MilsToCoord(11);
    Padcache.PowerPlaneReliefExpansion := MilsToCoord(11);
    Padcache.PowerPlaneClearance       := MilsToCoord(11);
    Padcache.ReliefConductorWidth      := MilsToCoord(11);
    Padcache.SolderMaskExpansion       := MilsToCoord(11);
    Padcache.SolderMaskExpansionValid  := eCacheManual;
    Padcache.PasteMaskExpansion        := MilsToCoord(11);
    Padcache.PasteMaskExpansionValid   := eCacheManual;

    // Assign the new pad cache to the via 
    Via.SetState_Cache                 := Padcache;

    // Put the new Via object on the board
    Board.AddPCBObject(Via);
    
    // Update the Undo System in DXP that a new VIa object has been added to the board
    PCBServer.SendMessageToRobots(Board  .I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, Via.I_ObjectAddress);
    
    // Finalize the systems in the PCB Editor.
    PCBServer.PostProcess;
    

    // Refresh PCB screen
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..............................................................................}

{..............................................................................}
