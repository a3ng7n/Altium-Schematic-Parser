{..............................................................................}
{                                                                              }
{ Summary Queries mechanical layers of the current PCB document.               }
{ Copyright (c) 2006 by Altium Limited                                         }
{                                                                              }
{..............................................................................}

{..............................................................................}
Procedure QueryMechLayers;
Var
    Board   : IPCB_Board;
    Layer   : TLayer;
    LS      : IPCB_LayerStack;
    LObject : IPCB_LayerObject;
    LayerIterator : IPBC_LayerObjectIterator;
    S       : String;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;
    LS := Board.LayerStack;
    If LS = Nil Then Exit;
    S := '';

    LayerIterator := Board.MechanicalLayerIterator;
    While LayerIterator.Next Do
    Begin
        LObject := LayerIterator.LayerObject;

        If (LObject.IsDisplayed[Board] = True) and (LObject.UsedByPrims) Then
         S := S + LObject.Name + ' is displayed and there are objects on it.' + #13;

        If (LObject.IsDisplayed[Board] = True) and Not (LObject.UsedByPrims) Then
         S := S+ LObject.Name + ' is displayed and there are NO objects on it.' + #13;

        If (LObject.IsDisplayed[Board] = False) and (LObject.UsedByPrims) Then
          S := S + LObject.Name + ' is NOT displayed and there are objects on it.' + #13;

        If (LObject.IsDisplayed[Board] = False) and Not (LObject.UsedByPrims) Then

        S := S + LObject.Name + ' is NOT displayed and there are NO objects on it.' + #13;

    End;
    ShowMessage(S);
End;
{..............................................................................}

{..............................................................................}
