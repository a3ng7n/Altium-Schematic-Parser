{..............................................................................}
{                                                                              }
{ Summary Demo how to create a new component on the PCB                        }
{                                                                              }
{ 1/ Run CreateAComponentOnPCB procedure first to create a new component       }
{ 2/ Then run CreateANewCompHeight procedure to set a new height for this comp.}
{ 3/ Then run the FetchComponentMidPoints procedure to fetch mid points.       }
{ 4/ Run the RotateAComponent procedure to rotate and flip this new comp.      }
{                                                                              }
{ Copyright (c) 2007 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    Board     : IPCB_Board;
    WorkSpace : IWorkSpace;

{..............................................................................}

{..............................................................................}
Function PlaceAPCBTrack(AX1, AY1, AX2, AY2 : TCoord; AWidth : TCoord; ALayer : TLayer) : IPCB_Track;
Var
   T : IPCB_Track;
Begin
    PCBServer.PreProcess;
    T             := PCBServer.PCBObjectFactory(eTrackObject,eNoDimension,eCreate_Default);
    T.X1          := MilsToCoord(AX1);
    T.Y1          := MilsToCoord(AY1);
    T.X2          := MilsToCoord(AX2);
    T.Y2          := MilsToCoord(AY2);
    T.Layer       := ALayer;
    T.Width       := MilsToCoord(AWidth);
    PCBServer.PostProcess;

    Result := T;
End;
{..............................................................................}

{..............................................................................}
Function PlaceAPCBPad(AX,AY : TCoord; ATopXSize, ATopYSize : TCoord; AHoleSize : TCoord; ALayer : TLayer; AName : String) : IPCB_Pad;
Var
    P        : IPCB_Pad;
    PadCache : TPadCache;
Begin
    Result := Nil;

    Try
    PCBServer.PreProcess;
    P := PcbServer.PCBObjectFactory(ePadObject,eNoDimension,eCreate_Default);
    If P = Nil Then Exit;

    P.X        := MilsToCoord(AX);
    P.Y        := MilsToCoord(AY);
    P.TopXSize := MilsToCoord(ATopXSize);
    P.TopYSize := MilsToCoord(ATopYSize);
    P.HoleSize := MilsToCoord(AHoleSize);
    P.Layer    := ALayer;
    P.Name     := AName;

    (* Setup a pad cache *)
    Padcache := P.GetState_Cache;
    Padcache.ReliefAirGap := MilsToCoord(11);
    Padcache.PowerPlaneReliefExpansion := MilsToCoord(11);
    Padcache.PowerPlaneClearance       := MilsToCoord(11);
    Padcache.ReliefConductorWidth      := MilsToCoord(11);
    Padcache.SolderMaskExpansion       := MilsToCoord(11);
    Padcache.SolderMaskExpansionValid  := eCacheManual;
    Padcache.PasteMaskExpansion        := MilsToCoord(11);
    Padcache.PasteMaskExpansionValid   := eCacheManual;

    (* Assign the new pad cache to the pad*)
    P.SetState_Cache(Padcache);
    Finally
        PCBServer.PostProcess;
    End;

    Result := P;
End;
{..............................................................................}

{..............................................................................}
Procedure Place4Tracks(NewComp : IPCB_Component);
Var
    NewTrack : IPCB_Track;
Begin
     // Create a first track
     NewTrack := PlaceAPCBTrack(50,50,250,50,10,eTopOverlay);
     NewComp.AddPCBObject(NewTrack);
     PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

     // Create a second track
     NewTrack := PlaceAPCBTrack(250,50,250,-150,10,eTopOverlay);
     NewComp.AddPCBObject(NewTrack);
     PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

     // Create a third track
     NewTrack := PlaceAPCBTrack(250,-150,50,-150,10,eTopOverlay);
     NewComp.AddPCBObject(NewTrack);
     PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);

     // Create a second track
     NewTrack := PlaceAPCBTrack(50,-150,50,50,10,eTopOverlay);
     NewComp.AddPCBObject(NewTrack);
     PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewTrack.I_ObjectAddress);
End;
{..............................................................................}

{..............................................................................}
Procedure Place4Pads(NewComp : IPCB_Component);
Var
    NewPad : IPCB_Pad;
Begin
    // Create a first pad
    NewPad := PlaceAPCBPad(0,0,62,62,28,eMultiLayer,'1');
    NewComp.AddPCBObject(NewPad);
    PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);

    // Create a second pad
    NewPad := PlaceAPCBPad(0,-100,62,62,28,eMultiLayer,'2');
    NewComp.AddPCBObject(NewPad);
    PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);

    // Create a third pad
    NewPad := PlaceAPCBPad(300,0,62,62,28,eMultiLayer,'3');
    NewComp.AddPCBObject(NewPad);
    PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);

    // Create a fourth pad
    NewPad := PlaceAPCBPad(300,-100,62,62,28,eMultiLayer,'4');
    NewComp.AddPCBObject(NewPad);
    PCBServer.SendMessageToRobots(NewComp.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,NewPad.I_ObjectAddress);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceAPCBComponent(AX,AY : TCoord);
Var
    Comp : IPCB_Component;
Begin
    PCBServer.PreProcess;
    Comp := PCBServer.PCBObjectFactory(eComponentObject, eNoDimension, eCreate_Default);
    If Comp = Nil Then Exit;

    Place4Tracks(Comp);
    Place4Pads  (Comp);

    // Set the reference point of the Component
    Comp.X         := MilsToCoord(AX);
    Comp.Y         := MilsToCoord(AY);
    Comp.Layer     := eTopLayer;

    // Make the designator text visible;
    Comp.NameOn         := True;
    Comp.Name.Text      := 'Custom';
    Comp.Name.XLocation := MilsToCoord(AX) - MilsToCoord(220);
    Comp.Name.YLocation := MilsToCoord(AY) - MilsToCoord(220);

    // Make the comment text visible;
    Comp.CommentOn         := True;
    Comp.Comment.Text      := 'Comment';
    Comp.Comment.XLocation := MilsToCoord(AX) - MilsToCoord(220);
    Comp.Comment.YLocation := MilsToCoord(AY) - MilsToCoord(300);

    PCBServer.SendMessageToRobots(Board.I_ObjectAddress,c_Broadcast,PCBM_BoardRegisteration,Comp.I_ObjectAddress);
    Board.AddPCBObject(Comp);
    PCBServer.PostProcess;
End;
{..............................................................................}

{..............................................................................}
Procedure CreateAComponentOnPCB;
Var
    OffSetX  : Integer;
    OffSetY  : Integer;
    NewPad   : IPCB_Pad;
    NewTrack : IPCB_Track;
    BR       : TCoordRect;
Begin
    Client.StartServer('PCB');

    // Create a new pcb document
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;
    Workspace.DM_CreateNewDocument('PCB');

    // Check if PCB document exists
    If PCBServer = Nil Then Exit;
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil then exit;

    // obtain the offsets so we can place objects inside the PCB board outline.
    BR := Board.BoardOutline.BoundingRectangle;
    OffsetX := Trunc(CoordToMils(BR.Left));
    OffsetY := Trunc(CoordToMils(BR.Bottom)); //TReal to Integer

    PlaceAPCBComponent(OffsetX + 3000, OffsetY + 1000);

    Board.ViewManager_FullUpdate;

    // Refresh PCB screen
    Client.CommandLauncher.LaunchCommand('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;{..............................................................................}

{..............................................................................}
Procedure RotateAComponent;
Var
    Board     : IPCB_Board;
    Iterator  : IPCB_BoardIterator;
    Component : IPCB_Component;
    Layer     : TLayer;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Initialize the PCB editor.
    PCBServer.PreProcess;

    //Grab the first component from the PCB only.
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    Component := Iterator.FirstPCBObject;

    // If no components found, exit.
    If Component = Nil Then
    Begin
        Board.BoardIterator_Destroy(Iterator);
        Exit;
    End;

    // Notify the PCB that the pcb object is going to be modified
    PCBServer.SendMessageToRobots(Component.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);


    // Set the reference point of the component.
    // Note that IPCB_Component is inherited from IPCB_Group
    // and thus the X,Y properties are inherited.
    Component.X := Component.X + MilsToCoord(100);
    Component.Y := Component.Y + MilsToCoord(100);

    // Rotate a component 45 degrees.
    Component.Rotation := 45;

    // If Component is on Top layer, its placed on bottom layer and vice versa.
    Layer := Component.Layer;
    If (Layer = eTopLayer) Then
    Begin
        Component.Layer := eBottomLayer;
    End
    Else If (Layer = eBottomLayer) Then
        Component.Layer := eTopLayer;

    // Notify the PCB editor that the pcb object has been modified
    PCBServer.SendMessageToRobots(Component.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

    Board.BoardIterator_Destroy(Iterator);

    // Reset the PCB
    PCBServer.PostProcess;

    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..............................................................................}

{..............................................................................}
Var
    MinX, MinY, MaxX, MaxY : Integer;
{..............................................................................}

{..............................................................................}
Procedure ProcessObjectsOfAComponent(Const P : IPCB_Primitive);
Var
    R : TCoordRect;
Begin
    // check for comment / name objects
    If P.ObjectId <> eTextObject Then
    Begin
        R := P.BoundingRectangle;

        If R.left   < MinX Then MinX := R.left;
        If R.bottom < MinY Then MinY := R.bottom;
        If R.right  > MaxX Then MaxX := R.right;
        If R.top    > MaxY Then MaxY := R.top;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchComponentMidPoints;
Var
    MidX, MidY    : Double;
    Component     : IPCB_Component;
    Iterator      : IPCB_BoardIterator;
    Board         : IPCB_Board;
    S             : TPCBString;
    GroupIterator : IPCB_GroupIterator;
    GroupHandle   : IPCB_Primitive;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    BeginHourGlass;

    // setting extreme constants...
    MinX :=  2147483647;
    MinY :=  2147483647;
    MaxX := -2147483647;
    MaxY := -2147483647;

    // set up filter for component objects only
    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Looks for the first component and then calculates the Mid X and MidY
    // of this component taking all the prims of this component into account.
    Component := Iterator.FirstPCBObject;
    If Component <> Nil Then
    Begin
        GroupIterator := Component.GroupIterator_Create;
        GroupIterator.AddFilter_ObjectSet(AllObjects);
        GroupHandle   := GroupIterator.FirstPCBObject;
        While GroupHandle <> Nil Do
        Begin
             ProcessObjectsOfAComponent(GroupHandle);
             GroupHandle := GroupIterator.NextPCBObject;
        End;
        Component.GroupIterator_Destroy(GroupIterator);
    End;
    Board.BoardIterator_Destroy(Iterator);

    MidX := (MinX + MaxX)/2;
    MidY := (MinY + MaxY)/2;

    EndHourGlass;
    S := Component.Name.Text;
    ShowInfo('Component''s ' + S + ' midpoint X and Y are : '+ #13 +
             'MidX = ' + FloatToStr(MidX) + ', MidY = ' + FloatToStr(MidY));
End;
{..............................................................................}

{..............................................................................}
Procedure CreateANewCompHeight;
Var
    Component : IPCB_Component;
    Iterator  : IPCB_BoardIterator;
    Board     : IPCB_Board;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    Iterator := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // very first component on PCB document fetched.
    // best have a PCB with only one component.
    Component := Iterator.FirstPCBObject;
    If Component <> Nil Then
    Begin
        ShowInfo('Component Designator ' + Component.SourceDesignator + #13 +
                 'Component''s Original Height = ' + IntToStr(Component.Height));

        (* Notify PCB of a modify- the height of a component is going to be changed *)
        Try
           PCBServer.PreProcess;
           PCBServer.SendMessageToRobots(Component.I_ObjectAddress, c_Broadcast, PCBM_BeginModify, c_noEventData);

            (* objects coordinates are stored in internal coordinates values *)
            Component.Height := MilsToCoord(25);

            // notify PCB that the document is dirty bec comp height changed.
            PCBServer.SendMessageToRobots(Component.I_ObjectAddress, c_Broadcast, PCBM_EndModify, c_noEventData);
            Board.BoardIterator_Destroy(Iterator);
         Finally
             PCBServer.PostProcess;
         End;

         (* Check if component height has changed *)
          ShowInfo('Component Designator ' + Component.SourceDesignator + #13 +
                   'Component''s New Height = ' + IntToStr(CoordToMils(Component.Height)) + ' mils');
      End;
End;
{..............................................................................}

{..............................................................................}
End.

