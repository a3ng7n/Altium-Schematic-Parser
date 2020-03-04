{..............................................................................}
{ Summary Creating different PCB Objects                                       }
{                                                                              }
{    CreateStrings procedure creates two different text objects on a new PCB   }
{    PlacePCBObjects procedure creates different objects on a new PCB          }
{                                                                              }
{ Copyright (c) 2007 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CreateStrings;
Var
    Sheet   : IPCB_Sheet;
    Board   : IPCB_Board;
    TextObj : IPCB_Text;
Begin
    (*create a new pcb document *)
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;
    Workspace.DM_CreateNewDocument('PCB');

    // Checks if current document is a PCB kind if not, exit.
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    Sheet := Board.PCBSheet;

    // Initialize PCB server.
    PCBServer.PreProcess;

    // Toggle Top Overlay and Bottom Overlay layers to true.
    Board.LayerIsDisplayed[eTopOverLay] := True;
    Board.LayerIsDisplayed[eBottomOverLay] := True;

    //
    //Create a first text object non True Type font.
    TextObj := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);

    // notify that the pcb object is going to be modified
    PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress ,c_Broadcast, PCBM_BeginModify , c_NoEventData);

    TextObj.XLocation := Sheet.SheetX + MilsToCoord(500);
    TextObj.YLocation := Sheet.SheetY + MilsToCoord(500);
    TextObj.Layer     := eTopOverlay;
    TextObj.Text      := 'Traditional Protel Text';
    TextObj.Size       := MilsToCoord(90);   // sets the height of the text.

    Board.AddPCBObject(TextObj);

    // notify that the pcb object has been modified
    PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_EndModify         , c_NoEventData);
    PCBServer.SendMessageToRobots(Board  .I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, TextObj.I_ObjectAddress);


    //
    // second TextObject with True Type Font enabled.
    TextObj := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);

    // notify that the pcb object is going to be modified
    PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_NoEventData);

    TextObj.XLocation := Sheet.SheetX + MilsToCoord(1000);
    TextObj.YLocation := Sheet.SheetY + MilsToCoord(1000);
    TextObj.Layer     := eBottomOverlay;

    TextObj.UseTTFonts := True;
    TextObj.Italic := True;
    TextObj.Bold := False;
    TextObj.FontName := 'ARIAL';

    // inverts the text object and a text boundary is created around the text
    // The Inverted and InvertedTTTextBorder properties are useful for situations
    // if text is to be placed on a copper region and create a cutout in the region.
    // the color of the inverted border is the layer color and the text color itself
    // is black.
    TextObj.Inverted := True;
    // The InvertedTTextBorder property determines the distance between the boundary of the
    // the text object itself to the text border boundary.
    TextObj.InvertedTTTextBorder := MilsToCoord(100);

    TextObj.Text      := 'Text with True Type Property enabled.';
    TextObj.Size       := MilsToCoord(200);    // sets the height of the text.

    Board.AddPCBObject(TextObj);

    // notify that the pcb object has been modified
    PCBServer.SendMessageToRobots(TextObj.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
    PCBServer.SendMessageToRobots(Board.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration,TextObj.I_ObjectAddress);

    // Set LayerIsUsed property to true, since text objects now exist on these Overlay layers.
    Board.LayerIsUsed[eTopOverLay]    := True;
    Board.LayerIsUsed[eBottomOverLay] := True;

    PCBServer.PostProcess;
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..............................................................................}

{..............................................................................}

{..............................................................................}

{..............................................................................}
Var
    Board     : IPCB_Board;
    WorkSpace : IWorkSpace;

{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBVia(Dummy : Integer);
Var
    Via         : IPCB_Via;
    PadCache    : TPadCache;
Begin
    (* Create a Via object*)
    Via           := PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default);
    Via.X         := MilsToCoord(2000);
    Via.Y         := MilsToCoord(2000);
    Via.Size      := MilsToCoord(50);
    Via.HoleSize  := MilsToCoord(20);
    Via.LowLayer  := eTopLayer;
    Via.HighLayer := eBottomLayer;

    (* Setup a pad cache *)
    Padcache := Via.GetState_Cache;
    Padcache.ReliefAirGap := MilsToCoord(11);
    Padcache.PowerPlaneReliefExpansion := MilsToCoord(11);
    Padcache.PowerPlaneClearance       := MilsToCoord(11);
    Padcache.ReliefConductorWidth      := MilsToCoord(11);
    Padcache.SolderMaskExpansion       := MilsToCoord(11);
    Padcache.SolderMaskExpansionValid  := eCacheManual;
    Padcache.PasteMaskExpansion        := MilsToCoord(11);
    Padcache.PasteMaskExpansionValid   := eCacheManual;

    (* Assign the new pad cache to the via *)
    Via.SetState_Cache                 := Padcache;

    Board.AddPCBObject(Via);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBTrack(X1, Y1, X2, Y2, Layer, Width);
Var
    Track         : IPCB_Track;
Begin
    (* Create a Track object*)
    Track             := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    Track.X1          := MilsToCoord(X1);
    Track.Y1          := MilsToCoord(Y1);
    Track.X2          := MilsToCoord(X2);
    Track.Y2          := MilsToCoord(Y2);
    Track.Layer       := Layer;
    Track.Width       := MilsToCoord(Width);
    Board.AddPCBObject(Track);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASimplePad(Dummy : Integer);
Var
    Pad         : IPCB_Pad;
    Padcache    : TPadCache;
Begin
    (* Create a Pad object*)
    Pad := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);

    // Location of pad on the PCB document.
    Pad.X        := MilsToCoord(4000);
    Pad.Y        := MilsToCoord(4000);

    // set up attributes of pad.
    Pad.HoleSize := MilsToCoord(38);
    Pad.Name     := 'A new simple Pad';
    Pad.Mode     := ePadMode_Simple;

    // For a simple pad, only need TopShape, TopXSize, TopYSize
    Pad.TopShape := eRounded;
    Pad.TopXSize := MilsToCoord(100);
    Pad.TopYSize := MilsToCoord(100);

    (* Setup a pad cache *)
    Padcache := Pad.GetState_Cache;
    Padcache.ReliefAirGap := MilsToCoord(11);
    Padcache.PowerPlaneReliefExpansion := MilsToCoord(11);
    Padcache.PowerPlaneClearance       := MilsToCoord(11);
    Padcache.ReliefConductorWidth      := MilsToCoord(11);
    Padcache.SolderMaskExpansion       := MilsToCoord(11);
    Padcache.SolderMaskExpansionValid  := eCacheManual;
    Padcache.PasteMaskExpansion        := MilsToCoord(11);
    Padcache.PasteMaskExpansionValid   := eCacheManual;

    (* Assign the new pad cache to the pad*)
    Pad.SetState_Cache                 := Padcache;

    Board.AddPCBObject(Pad);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceATopMidBotStackPad(Dummy : Integer);
Var
    Pad         : IPCB_Pad;
    Padcache    : TPadCache;
Begin
    (* Create a Pad object*)
    Pad := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);

    // Location of pad on the PCB document.
    Pad.X        := MilsToCoord(3000);
    Pad.Y        := MilsToCoord(3000);

    // set up attributes of pad.
    Pad.HoleSize := MilsToCoord(38);
    Pad.Name     := 'A new TopMidBot Pad';
    Pad.Mode     := ePadMode_LocalStack;

    //
    Pad.TopShape := eOctagonal;
    Pad.TopXSize := MilsToCoord(75);
    Pad.TopYSize := MilsToCoord(75);

    Pad.MidShape := eRectangular;
    Pad.MidXSize := MilsToCoord(50);
    Pad.MidYSize := MilsToCoord(50);

    Pad.BotShape := eOctagonal;
    Pad.BotXSize := MilsToCoord(100);
    Pad.BotYSize := MilsToCoord(100);


    (* Setup a pad cache *)
    Padcache := Pad.GetState_Cache;
    Padcache.ReliefAirGap := MilsToCoord(11);
    Padcache.PowerPlaneReliefExpansion := MilsToCoord(11);
    Padcache.PowerPlaneClearance       := MilsToCoord(11);
    Padcache.ReliefConductorWidth      := MilsToCoord(11);
    Padcache.SolderMaskExpansion       := MilsToCoord(11);
    Padcache.SolderMaskExpansionValid  := eCacheManual;
    Padcache.PasteMaskExpansion        := MilsToCoord(11);
    Padcache.PasteMaskExpansionValid   := eCacheManual;

    (* Assign the new pad cache to the pad*)
    Pad.SetState_Cache                 := Padcache;

    Board.AddPCBObject(Pad);
End;
{..............................................................................}

{..............................................................................}
Procedure  PlaceAFullStackPad(0);
Var
    Pad         : IPCB_Pad;
    Padcache    : TPadCache;
Begin
    (* Create a Pad object*)
    Pad := PCBServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);

    // Location of pad on the PCB document.
    Pad.X        := MilsToCoord(3500);
    Pad.Y        := MilsToCoord(3500);

    // set up attributes of pad.
    Pad.HoleSize := MilsToCoord(10);
    Pad.Name     := 'A new fullstack Pad';
    Pad.Mode     := ePadMode_ExternalStack;

    // Set up full stack pad
    Pad.XStackSizeOnLayer(eTopLayer) := MilsToCoord(50);
    Pad.YStackSizeOnLayer(eTopLayer) := MilsToCoord(50);
    Pad.StackShapeOnLayer(eTopLayer) := eRounded;

    Pad.XStackSizeOnLayer(eMidLayer1) := MilsToCoord(50);
    Pad.YStackSizeOnLayer(eMidLayer1) := MilsToCoord(50);
    Pad.StackShapeOnLayer(eMidLayer1) := eRounded;

    Pad.XStackSizeOnLayer(eMidLayer2) := MilsToCoord(50);
    Pad.YStackSizeOnLayer(eMidLayer2) := MilsToCoord(50);
    Pad.StackShapeOnLayer(eMidLayer2) := eRounded;

    Pad.XStackSizeOnLayer(eBottomLayer) := MilsToCoord(50);
    Pad.YStackSizeOnLayer(eBottomLayer) := MilsToCoord(50);
    Pad.StackShapeOnLayer(eBottomLayer) := eOctagonal;

    (* Setup a pad cache *)
    Padcache := Pad.GetState_Cache;
    Padcache.ReliefAirGap := MilsToCoord(11);
    Padcache.PowerPlaneReliefExpansion := MilsToCoord(11);
    Padcache.PowerPlaneClearance       := MilsToCoord(11);
    Padcache.ReliefConductorWidth      := MilsToCoord(11);
    Padcache.SolderMaskExpansion       := MilsToCoord(11);
    Padcache.SolderMaskExpansionValid  := eCacheManual;
    Padcache.PasteMaskExpansion        := MilsToCoord(11);
    Padcache.PasteMaskExpansionValid   := eCacheManual;

    (* Assign the new pad cache to the pad*)
    Pad.SetState_Cache                 := Padcache;

    Board.AddPCBObject(Pad);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBFill(Dummy : Integer);
Var
    Fill : IPCB_Fill;
Begin
    (* Create a Fill object*)
    Fill             := PCBServer.PCBObjectFactory(eFillObject, eNoDimension,eCreate_Default);
    Fill.X1Location  := MilsToCoord(2000);
    Fill.Y1Location  := MilsToCoord(2000);
    Fill.X2Location  := MilsToCoord(2500);
    Fill.Y2Location  := MilsToCoord(2500);
    Fill.Layer       := eBottomLayer;
    Fill.Rotation    := 45;

    Board.AddPCBObject(Fill);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBArc(Dummy : Integer);
Var
    Arc : IPCB_Arc;
Begin
    Arc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
    Arc.XCenter    := MilsToCoord(Board.XOrigin + 1800);
    Arc.YCenter    := MilsToCoord(Board.YOrigin + 1800);
    Arc.Radius     := MilsToCoord(200);
    Arc.LineWidth  := MilsToCoord(50);
    Arc.StartAngle := 0;
    Arc.EndAngle   := 270;
    Arc.Layer      := eBottomLayer;
    Board.AddPCBObject(Arc);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBComponent(Dummy : Integer);
Begin
    If IntegratedLibraryManager = Nil Then Exit;

End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBText(Dummy : Integer);
Var
    TextObj : IPCB_Text;
Begin
    (* create a text object on a top overlay *)
    Board.LayerIsDisplayed[eTopOverLay] := True;

    TextObj := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);
    TextObj.XLocation := MilsToCoord(Board.XOrigin + 4000);
    TextObj.YLocation := MilsToCoord(Board.YOrigin + 2000);
    TextObj.Layer     := eTopOverlay;
    TextObj.Text      := 'Text Object';
    TextObj.Size      := MilsToCoord(90);   // sets the height of the text.

    Board.AddPCBObject(TextObj);
End;
{..............................................................................}

{..............................................................................}
Procedure CreateANewNetClass(Dummy : Integer = 0);
Var
    Board    : IPCB_Board;
    NetClass : IPCB_ObjectClass;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    PCBServer.PreProcess;

    NetClass := PCBServer.PCBClassFactoryByClassMember(eClassMemberKind_Net);
    NetClass.SuperClass := False;
    NetClass.Name := 'NetGndClass';
    NetClass.AddMemberByName('GND');

    Board.AddPCBObject(NetClass);
    PCBServer.PostProcess;
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBRegion(Dummy : Integer = 0);
Const
     N = 10;
Var
    I         : Integer;
    Region    : IPCB_Region;
    BaseAngle : Double;
    Contour   : IPCB_Contour;
Begin
    Region := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension,eCreate_Default);
    Contour := Region.MainContour.Replicate;

    Region.Layer             := eBottomLayer;
    BaseAngle                := 2 * PI / N;
    Contour.Count := N;

    For I := 1 To Contour.Count Do
    Begin
        Contour.X[I] := MilsToCoord(200 * Sin(I * BaseAngle));
        Contour.Y[I] := MilsToCoord(200 * Cos(I * BaseAngle));
    End;
    Contour.Translate(MilsToCoord(4500), MilsToCoord(4500));

    Region.SetOutlineContour(Contour);

    // Add hole to region, hole must be totally contained within MainContour
    If Region.HoleCount = 0 Then
       Region.GeometricPolygon.AddContourIsHole(Nil, True);

    BaseAngle := 2 * PI / (N Div 2);
    Region.Holes[0].Count := (N Div 2);
    For I := 1 To Region.Holes[0].Count Do
    Begin
        Region.Holes[0].X[I] := MilsToCoord(75 * Sin(I * BaseAngle));
        Region.Holes[0].Y[I] := MilsToCoord(75 * Cos(I * BaseAngle));
    End;
    Region.Holes[0].Translate(MilsToCoord(4500), MilsToCoord(4500));


    Board.AddPCBObject(Region);
End;
{..............................................................................}

{..............................................................................}
Procedure PlacePCBObjects;
Begin
    (*create a new pcb document *)
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;
    Workspace.DM_CreateNewDocument('PCB');

    If PCBServer = Nil Then Exit;
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil then exit;

    (* Place new PCB objects*)
    PlaceAPCBVia(0);

    PlaceAPCBTrack(1000,1000,1500,1500,eTopLayer,50);
    PlaceAPCBTrack(1500,1500,1500,1000,eTopLayer,50);
    PlaceAPCBTrack(1500,1000,1000,1000,eTopLayer,50);

    PlaceASimplePad(0);
    PlaceATopMidBotStackPad(0);
    PlaceAFullStackPad(0);

    PlaceAPCBFill(0);
    PlaceAPCBArc(0);
    PlaceAPCBComponent(0);
    PlaceAPCBText(0);

    CreateANewNetClass;

    PlaceAPCBRegion;

    (* Refresh PCB workspace *)
    ResetParameters;
    AddStringParameter('Action', 'All');
    RunProcess('PCB:Zoom');
End;
{..............................................................................}

{..............................................................................}

