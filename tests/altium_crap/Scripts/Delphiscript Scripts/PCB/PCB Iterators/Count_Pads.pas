{..............................................................................}
{ Summary Count the number of pads on a current PCB document.                  }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure PadCount;
Var
    Board     : IPCB_Board;
    Pad       : IPCB_Primitive;
    Iterator  : IPCB_BoardIterator;
    PadNumber : Integer;
Begin
    PadNumber       := 0;

    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // retrieve the iterator
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(ePadObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search and count pads
    Pad := Iterator.FirstPCBObject;
    While (Pad <> Nil) Do
    Begin
        Inc(PadNumber);
        Pad := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    // Display the count result on a dialog.
    ShowMessage('Pad Count = ' + IntToStr(PadNumber));
End;
{..............................................................................}

{..............................................................................}
