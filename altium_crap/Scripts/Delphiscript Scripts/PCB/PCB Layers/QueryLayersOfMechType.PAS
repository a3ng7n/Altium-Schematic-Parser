{..............................................................................}
{ Summary Queries the layers of IPCB_MechanicalLayer type.                     }
{         IPCB_MechanicalLayer interface introduced in Altium Designer         }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function BooleanToString(AValue : Boolean) : String;
Begin
    Result := 'False';
    If AValue Then Result := 'True';
End;
{..............................................................................}

{..............................................................................}
Procedure QueryAllMechLayers;
Var
    PCBBoard      : IPCB_Board;
    LayerIterator : IPBC_LayerObjectIterator;
    i             : Integer;
    LayerObj      : IPCB_MechanicalLayer;
    Layer         : TLayer;
    LS            : String;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    LS := '';
    LayerIterator := PCBBoard.MechanicalLayerIterator;
    While LayerIterator.Next Do
    Begin
        LayerObj := LayerIterator.LayerObject;
        Layer := LayerIterator.Layer;
        LS := LS + Layer2String(Layer) + ' linked '   + BooleanToString(LayerObj.LinkToSheet)            +
                                         ' enabled '  + BooleanToString(LayerObj.MechanicalLayerEnabled) +
                                         ' displayed in single layer mode ' + BooleanToString(LayerObj.DisplayInSingleLayerMode) + #13;
    End;
    ShowMessage(LS);
End;
{..............................................................................}

{..............................................................................}
