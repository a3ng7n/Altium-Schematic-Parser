{..............................................................................}
{ Summary: Forces rebuild of all internal/split planes                         }
{          on the current PCB document                                         }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure RebuildInternalSplitPlanes;
Var
    PCBBoard      : IPCB_Board;
    TheLayerStack : IPCB_LayerStack;
    FoundPlane    : Boolean;
    LayerObj      : IPCB_LayerObject;
    L             : TLayer;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    TheLayerStack := PCBBoard.LayerStack;
    If TheLayerStack = Nil Then Exit;

    FoundPlane := False;
    LayerObj   := TheLayerStack.FirstLayer;
    Repeat
        If InSet(LayerObj.LayerID, InternalPlanes) Then
        Begin
            PCBBoard.InvalidatePlane(LayerObj.LayerID);
            FoundPlane := True;
        End;
        LayerObj := TheLayerStack.NextLayer(LayerObj);
    Until LayerObj = Nil;

    If FoundPlane Then
    Begin
        PCBBoard.ValidateInvalidPlanes;
        PCBBoard.GraphicalView_ZoomRedraw
    End
    Else ShowInfo('This Board does not contain internal planes');
End;
{..............................................................................}
