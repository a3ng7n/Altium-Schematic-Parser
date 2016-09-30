{..............................................................................}
{ Summary                                                                      }
{ Demo the use of ChooseRectangleByCorners method and the spatial iterator.    }
{ Selects free primitives only on the focussed PCB                             }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure SearchForObjectsWithinTheBoundaryRectangle;
Var
    Board         : IPCB_Board;
    PCBObject     : IPCB_Object;

    Iterator      : IPCB_SpatialIterator;
    X1,Y1,X2,Y2   : TCoord;

    ASetOfLayers  : TLayerSet;
    ASetOfObjects : TObjectSet;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    (* The ChooseRectangleByCorners fn is an interactive function where *)
    (* you are prompted to choose two points on a PCB document.*)
    If Not (Board.ChooseRectangleByCorners( 'Choose first corner',
                                            'Choose final corner',
                                            x1,y1,x2,y2)) Then Exit;

    (* Top/Bottom Layers and Arc/Track objects defined
       for the Spatial iterator constraints *)
    ASetOfLayers  := MkSet(eTopLayer,eBottomLayer);
    ASetOfObjects := MkSet(eArcObject,eTrackObject);

    (* Setup a spatial iterator which conducts a *)
    (* search within a defined boundary by the   *)
    (* ChooseRectangleByCorners method *)
    Iterator := Board.SpatialIterator_Create;
    Iterator.AddFilter_ObjectSet(ASetOfObjects);
    Iterator.AddFilter_LayerSet(ASetOfLayers);
    Iterator.AddFilter_Area(X1,Y1,X2,Y2);

    (* Iterate for tracks and arcs on bottom/top layers *)
    PCBObject := Iterator.FirstPCBObject;
    While PCBObject <> 0 Do
    Begin
         PCBObject.Selected := True;
         PCBObject := Iterator.NextPCBObject;
    End;
    Board.SpatialIterator_Destroy(Iterator);

    (* Update PCB document *)
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..............................................................................}

{..............................................................................}
