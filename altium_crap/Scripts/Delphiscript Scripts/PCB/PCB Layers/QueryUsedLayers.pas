{..............................................................................}
{ Summary Queries the current PCB document for layers that are used.           }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure QueryUsedLayers;
Var
    PCBBoard : IPCB_Board;
    Layer    : TLayer;
    LS       : String;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    LS       := '';

    (* Check for each signal layer for used/display setting *)
    For Layer := eTopLayer to eMultiLayer Do
        If PCBBoard.LayerIsUsed[Layer] Then
            If PCBBoard.LayerIsDisplayed[Layer] Then
                LS := LS + Layer2String(Layer) + ' used and displayed'     + #13
            Else
                LS := LS + Layer2String(Layer) + ' used but not displayed' + #13;

    (* Show the results *)
    ShowInfo(LS);
End;
{..............................................................................}

{..............................................................................}
