{..............................................................................}
{ Summary Moves objects within the defined boundary set by the user            }
{         User can choose which objects to move.                               }
{                                                                              }
{         Version 1.2                                                          }
{..............................................................................}
Var
    Board       : IPCB_Board;
    X1,Y1,X2,Y2 : TCoord;
{..............................................................................}

{..............................................................................}
Procedure MovePCBObjects;
Begin
     // get the PCB document interface
     Board := PCBServer.GetCurrentPCBBoard;
     If Board = Nil Then
     Begin
         ShowWarning('This is not a PCB document.');
         formMovePCBObjects.Close;
     End;
End;
{..............................................................................}

{..............................................................................}
procedure TformMovePCBObjects.buttonMove1Click(Sender: TObject);
begin
    If Not (Board.ChooseRectangleByCorners( 'Choose first corner',
                                            'Choose final corner',
                                            x1,y1,x2,y2)) Then Exit;
    ExecuteMove(True, cbDesignObjects);
End;
{..............................................................................}

{..............................................................................}
procedure TformMovePCBObjects.buttonMove2Click(Sender: TObject);
begin
    If Not (Board.ChooseRectangleByCorners( 'Choose first corner',
                                            'Choose final corner',
                                            x1,y1,x2,y2)) Then Exit;

    ExecuteMove(False, cbDesignObjects);

end;
{..............................................................................}

{..............................................................................}
procedure TformMovePCBObjects.bCloseClick(Sender: TObject);
begin
    Close;
end;
{..............................................................................}

{..............................................................................}

