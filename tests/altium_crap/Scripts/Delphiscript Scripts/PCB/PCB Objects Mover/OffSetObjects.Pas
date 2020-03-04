{..............................................................................}
{ Summary                                                                      }
{ Demo the use of ChooseRectangleByCorners method and the spatial iterator.    }
{ Moves the objects within the defined boundary on the PCB document.           }
{                                                                              }
{ Version 1.2                                                                  }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    DisplayUnit : TUnit;
{..............................................................................}

{..............................................................................}
Function String2ObjectId(AValue : String) : TObjectId;
Begin
    If AValue = 'Arc'       Then Result := eArcObject;
    If AValue = 'Track'     Then Result := eTrackObject;
    If AValue = 'Text'      Then Result := eTextObject;
    If AValue = 'Fill'      Then Result := eFillObject;
    If AValue = 'Pad'       Then Result := ePadObject;
    If AValue = 'Via'       Then Result := eViaObject;
    If AValue = 'Component' Then Result := eComponentObject;
End;
{..............................................................................}

{..............................................................................}
Procedure ExecuteMove(MoveByMethod : Boolean; AList : TChecklistBox);
Var
    i             : Integer;
    PCBObject     : IPCB_Object;
    Iterator      : IPCB_SpatialIterator;
    ASetOfObjects : TObjectSet;
    ASetOfLayers  : TLayerSet;

    XStep,YStep   : TCoord;
    DistanceStep  : TCoord;
    AngleStep     : TAngle;

Begin
    If Alist.Items.Count < 1 Then Exit;

    // set cursor to waiting.
    Screen.Cursor      := crHourGlass;

    ASetOfLayers  := AllLayers;
    ASetOfObjects := 0;
    For i := 0 To AList.Items.Count - 1 Do
      If AList.State[i] = cbChecked Then
          Include(ASetOfObjects,String2ObjectId(AList.Items[i]));

    // Setup a spatial iterator which conducts a search within a defined boundary by the
    // ChooseRectangleByCorners method
    Iterator := Board.SpatialIterator_Create;
    Iterator.AddFilter_ObjectSet(ASetOfObjects);
    Iterator.AddFilter_LayerSet(ASetOfLayers);
    Iterator.AddFilter_Area(X1,Y1,X2,Y2);

     // get the displayunit (mil,mm)
     DisplayUnit := Board.DisplayUnit;

    // convert the edit boxes strings to coord unit
    StringToCoordUnit(formMovePCBObjects.X.Text,XStep,DisplayUnit);
    StringToCoordUnit(formMovePCBObjects.Y.Text,YStep,DisplayUnit);
    StringToCoordUnit(formMovePCBObjects.Distance.Text,DistanceStep,DisplayUnit);
    AngleStep := formMovePCBObjects.Angle.Text;


    // Look for objects within the defined boundary and move them.
    Try
        PCBServer.PreProcess;
        PCBObject := Iterator.FirstPCBObject;
        While PCBObject <> 0 Do
        Begin
             PCBServer.SendMessageToRobots(PCBObject.I_ObjectAddress, c_Broadcast, PCBM_BeginModify , c_NoEventData);
             If MoveByMethod Then
                 PCBObject.MoveByXY(XStep,YStep)
             Else
             Begin
                 //Move the object
                 XStep := DistanceStep * Cos(AngleStep);
                 YStep := DistanceStep * Sin(AngleStep);

                 PcbObject.MoveByXY(XStep,YStep);
             End;
             PCBServer.SendMessageToRobots(PCBObject.I_ObjectAddress, c_Broadcast, PCBM_EndModify , c_NoEventData);
             PCBObject := Iterator.NextPCBObject;
        End;
        Board.SpatialIterator_Destroy(Iterator);
    Finally
        PCBServer.PostProcess;
    End;

    // Update PCB document
    Client.SendMessage('PCB:Zoom', 'Action=Redraw' , 255, Client.CurrentView);

    // Restore cursor to normal
    Screen.Cursor          := crArrow;
End;
{..............................................................................}

{..............................................................................}
