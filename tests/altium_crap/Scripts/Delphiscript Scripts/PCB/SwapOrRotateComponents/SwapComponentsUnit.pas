{..............................................................................}
{ Summary Swaps two similar components or rotate a component.                  }
{                                                                              }
{         A script to ask the user to select two components then               }
{         have their positions swapped                                         }
{         OR                                                                   }
{         if the same component is selected twice, have it rotated             }
{                                                                              }
{         Limitations of this script:                                          }
{         You need to move the cursor away from a component to exit            }
{                                                                              }
{                                                                              }
{         Version 1.2                                                          }
{..............................................................................}

{..............................................................................}
Procedure ChooseAndSwapComponents;
Var
    Board     : IPCB_Board;
    Comp1     : IPCB_Component;
    Comp2     : IPCB_Component;

    x,y,      : TCoord;
    x1, y1    : TCoord;
    Rotation  : TAngle;
Begin
    Pcbserver.PreProcess;

    Try
        Board := PCBServer.GetCurrentPCBBoard;
        If Not Assigned(Board) Then
        Begin
            ShowMessage('The Current Document is not a Protel PCB Document.');
            Exit;
        End;
    
        Repeat
            Board.ChooseLocation(x,y, 'Choose Component1');
            Comp1 := Board.GetObjectAtXYAskUserIfAmbiguous(x,y,MkSet(eComponentObject),AllLayers, eEditAction_Select);
            If Not Assigned(Comp1) Then Exit;

            Board.ChooseLocation(x,y, 'Choose Component2');
            Comp2 := Board.GetObjectAtXYAskUserIfAmbiguous(x,y,MkSet(eComponentObject),AllLayers, eEditAction_Select);
            If Not Assigned(Comp2) Then Exit;
    
            // Check if Component Name property exists before extracting the text
            If Comp1.Name = Nil Then Exit;
            If Comp2.Name = Nil Then Exit;

            // Check if same component selected twice
            If Comp1.Name.Text = Comp2.Name.Text
            Then
            Begin
                // Rotate the same component
                PCBServer.SendMessageToRobots(Comp1.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);
                Case Comp1.Rotation of
                     0    : Comp1.Rotation  := 180;
                     90   : Comp1.Rotation  := 270;
                     180  : Comp1.Rotation  := 0;
                     270  : Comp1.Rotation  := 90;
                     360  : Comp1.Rotation  := 180;
                End;
                PCBServer.SendMessageToRobots(Comp1.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
    
                Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
            End
            Else
            Begin
                PCBServer.SendMessageToRobots(Comp1.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);
                PCBServer.SendMessageToRobots(Comp2.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);            // Swap Components

                // swap two components
                x1 := comp1.x;
                y1 := comp1.y;

                comp1.x := comp2.x;
                comp1.y := comp2.y;

                comp2.x  := x1;
                comp2.y  := y1;

                PCBServer.SendMessageToRobots(Comp1.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
                PCBServer.SendMessageToRobots(Comp2.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);

                Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
             End;

        // click on the board to exit or RMB
        Until (Comp1 = Nil) Or (Comp2 = Nil);

    Finally
        Pcbserver.PostProcess;
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
    End;
End;
