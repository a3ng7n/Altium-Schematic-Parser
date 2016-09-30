{..............................................................................}
{ Summary Queries the Layer Stack of the current PCB document.                 }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}
Procedure QueryTheLayerStack;
Var
    PCBBoard      : IPCB_Board;
    i             : Integer;
    LayerIterator : IPBC_LayerObjectIterator;
    LS            : String;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    LS := '';
    LayerIterator := PCBBoard.ElectricalLayerIterator;
    While LayerIterator.Next Do
    Begin
        LS       := LS + Layer2String(LayerIterator.LayerObject.LayerID) + #13#10;
    End;

    ShowInfo('The Layer Stack has :'#13#10 + LS);
End;
{..............................................................................}

{..............................................................................}
