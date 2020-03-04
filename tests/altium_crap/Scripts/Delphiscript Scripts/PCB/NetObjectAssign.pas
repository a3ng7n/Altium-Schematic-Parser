{..............................................................................}
{ Summary A demo of how to assign nets to PCB objects.                         }
{                                                                              }
{         1/ Look for a Net object that has the GND name.                      }
{         2/ Create a Via object with the GND net and put on the PCB.          }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    Board : IPCB_Board;
{..............................................................................}

{..............................................................................}
Procedure AssignNetToNewObject(NewNet : IPCB_Net);
Var
    Via   : IPCB_Via;
    BR    : TCoordRect;
Begin
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Obtain the board outline and validate it.
    Board.BoardOutline.Invalidate;
    Board.BoardOutline.Rebuild;
    Board.BoardOutline.Validate;
    // Obtain the boundary (Left and Bottom properties) of the board outline
    // so the via object can be placed within the board outline.
    BR := Board.BoardOutline.BoundingRectangle;


    // Create a Via object using the PCBObjectFactory method.
    Via           := PCBServer.PCBObjectFactory(eViaObject, eNoDimension, eCreate_Default);
    Via.X         := BR.Left   + MilsToCoord(500);
    Via.Y         := BR.Bottom + MilsToCoord(500);

    Via.Size      := MilsToCoord(150);
    Via.HoleSize  := MilsToCoord(50);
    Via.LowLayer  := eTopLayer;
    Via.HighLayer := eBottomLayer;

    // Assign the GND net to the new Via object.
    Via.Net       := NewNet;

    // Put the new Via object in the board object (representing the current PCB document)
    Board.AddPCBObject(Via);

    // Refresh PCB screen
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);
End;
{..............................................................................}

{..............................................................................}
Procedure LookForNetsAndAssignNetsToNewObjects;
Var
    Net         : IPCB_Net;
    Iterator    : IPCB_BoardIterator;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Net objects only
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eNetObject));
    Iterator.AddFilter_LayerSet(AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for Net object that has the 'GND' name.
    Net := Iterator.FirstPCBObject;
    While (Net <> Nil) Do
    Begin
        If Net.Name = 'GND' Then
        Begin
            AssignNetToNewObject(Net);
            Break;
        End;
        Net := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);
End;
{..............................................................................}

{..............................................................................}

