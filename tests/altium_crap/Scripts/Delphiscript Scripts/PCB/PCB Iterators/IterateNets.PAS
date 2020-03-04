{..............................................................................}
{ Summary Iterates Nets of the current PCB document.                            }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}
Procedure IterateNetObjects;
Var
    Board       : IPCB_Board;
    Net         : IPCB_Net;
    Iterator    : IPCB_BoardIterator;
    LS          : TPCBString;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Net objects only
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for Net objects and get their Net Name values
    LS := '';
    Net := Iterator.FirstPCBObject;
    While (Net <> Nil) Do
    Begin
        LS := LS + Net.Name + ', ';
        Net := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    // Display the Net Names on a dialog.
    ShowInfo('Nets = ' + LS);
 End;
{..............................................................................}

{..............................................................................}

