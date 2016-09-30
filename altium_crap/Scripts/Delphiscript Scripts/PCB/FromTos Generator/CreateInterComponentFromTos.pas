{..............................................................................}
{ Summary: Creates FromTo conections between pads on two components            }
{                                                                              }
{         A script to ask the user to select two components then               }
{         connects pads with same net assignement                              }
{                                                                              }
{         Limitations of this script:                                          }
{         You need to move the cursor away from a component to exit.           }
{                                                                              }
{         No nets excluded (e.g. GND and VCC will be used to)                  }
{         All nets need to be rebuilt manually after this script               }
{         (kbd shortuts: N-H-A followed by N-S-A)                              }
{                                                                              }
{         Version 1.1                                                          }
{..............................................................................}

{..............................................................................}
Var
    Comp1, Comp2     : IPCB_Component;
    Pad1, Pad2       : IPCB_Pad;

{..............................................................................}

{..............................................................................}
Procedure CreateFromTosBetweenComponents;
Var
    Board            : IPCB_Board;
    CompIterator1    : IPCB_GroupIterator;
    CompIterator2    : IPCB_GroupIterator;
    FromTo           : IPcb_FromTo;
    x, y             : TCoord;
    PadIdx           : Integer;
    LinkToFirstFound : Boolean;
    Comp2UsedPads    : TStringList;
    Comp1PadCount    : Integer;
Begin
    Pcbserver.PreProcess;

    Try
        //Change next value to True to allow linking to unique pads in Component2
        LinkToFirstFound    := True;

        Comp2UsedPads            := TStringList.Create;
        Comp2UsedPads.Sorted     := True;
        Comp2UsedPads.Duplicates := dupIgnore;

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

            //Comment out next line to enable creation of FromTos between pads on the same component
            //If Comp1 = Comp2 Then Exit;

            CompIterator1 := Comp1.GroupIterator_Create;
            CompIterator1.SetState_FilterAll;
            CompIterator1.AddFilter_ObjectSet(MkSet(ePadObject));


            CompIterator2 := Comp2.GroupIterator_Create;
            CompIterator2.SetState_FilterAll;
            CompIterator2.AddFilter_ObjectSet(MkSet(ePadObject));

            formFromTos.Show;
            Pad1 := CompIterator1.FirstPCBObject;
            FormFromTos.Label1.Caption := 'Processing...';
            FormFromTos.Label2.Caption := '';
            FormFromTos.Label3.Caption := '';

            FormFromTos.Update;
            While Pad1 <> Nil Do
            Begin
                If (Pad1.Net <> Nil) Then
                Begin
                    Pad2 := CompIterator2.FirstPCBObject;
                    While Pad2 <> Nil Do
                    Begin
                        If (Pad1 <> Pad2) And (Pad1.Net = Pad2.Net) And (LinkToFirstFound Or Not Comp2UsedPads.Find(Pad2.GetState_PinDescriptorString, PadIdx)) Then
                        Begin
                            FromTo := PcbServer.PCBObjectFactory(eFromToObject,  eNoDimension, eCreate_Default);
                            FromTo.FromPad := Pad1.GetState_PinDescriptorString;
                            FromTo.ToPad   := Pad2.GetState_PinDescriptorString;
                            FromTo.NetName := Pad1.Net.Name;

                            formFromTos.Label1.Caption := 'Pad1: '    + Pad1.GetState_PinDescriptorString;
                            formFromTos.Label2.Caption := 'Pad2: '    + Pad2.GetState_PinDescriptorString;
                            formFromTos.Label3.Caption := 'NetName: ' + Pad1.Net.Name;
                            formFromTos.Update;

                            If Not LinkToFirstFound Then Comp2UsedPads.Add(Pad2.GetState_PinDescriptorString);

                            Board.AddPCBObject(FromTo);
                            Break;
                        End;
                        Pad2 := CompIterator2.NextPCBObject;
                    End;
                End
                Else
                Begin
                    //ShowMessage('Net is Nil for Pad: ' + Pad1.GetState_PinDescriptorString);
                End;
                Pad1 := CompIterator1.NextPCBObject;
            End;
            Comp1.GroupIterator_Destroy(CompIterator1);
            Comp2.GroupIterator_Destroy(CompIterator2);

            formFromTos.Close;

            Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
        // click on the board to exit or RMB
        Until (Comp1 = Nil) Or (Comp2 = Nil);
    Finally

        Pcbserver.PostProcess;
        Client.SendMessage('PCB:Zoom', 'Action=Redraw', 255, Client.CurrentView);
        Comp2UsedPads.Free;
    End;
End;

