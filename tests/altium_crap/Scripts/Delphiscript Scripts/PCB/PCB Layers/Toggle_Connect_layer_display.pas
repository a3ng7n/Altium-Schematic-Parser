{..............................................................................}
{ Summary Toggle display of Connect layer                                      }
{ Copyright (c) 2003 by Altium Limited                                         }     
{..............................................................................}

{..............................................................................}
Var
    Display  : Boolean;
    Board    : IPCB_Board;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    Display := Board.LayerIsDisplayed[eConnectLayer];
    Display := Not Display;

    Board.LayerIsDisplayed[eConnectLayer] := Display;

    // Now redraw the current document.
    ResetParameters;
    AddStringParameter('Action','Redraw');
    RunProcess('PCB:Zoom');
End.
{..............................................................................}

{..............................................................................}
