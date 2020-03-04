{..............................................................................}
{ Summary Fetch pad stack information for a clicked pad on PCB document        }
{ Version 1.1                                                                  }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function ShapeToString (AShape : TShape) : TPCBString;
Begin
    Case AShape Of
        eNoShape          : Result := 'NoShape';
        eRounded          : Result := 'Rounded';
        eRectangular      : Result := 'Rectangular';
        eOctagonal        : Result := 'Octagonal';
        eCircleShape      : Result := 'CircleShape';
        eArcShape         : Result := 'ArcShape';
        eTerminator       : Result := 'Terminator';
        eRoundRectShape   : Result := 'RoundRectShape';
        eRotatedRectShape : Result := 'RotatedRectShape';
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessSimplePad(APad : IPCB_Pad; Var AString : TPCBString);
Begin
    AString := AString + ' X Size : ' + IntToStr(APad.TopXSize) + #13#10 +
                         ' Y Size : ' + IntToStr(APad.TopYSize) + #13#10 +
                         ' Shape : '  + ShapeToString(APad.TopShape);
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessTopMidBotPad(APad : IPCB_Pad; Var AString : TPCBString);
Begin
    AString := AString + 'Top X Size : ' + IntToStr(APad.TopXSize)       + #13#10 +
                         'Top Y Size : ' + IntToStr(APad.TopYSize)       + #13#10 +
                         'Top Shape  : ' + ShapeToString(APad.TopShape)  + #13#10 +
                                                                         + #13#10 +
                         'Mid X Size : ' + IntToStr(APad.MidXSize)       + #13#10 +
                         'Mid Y Size : ' + IntToStr(APad.MidYSize)       + #13#10 +
                         'Mid Shape  : ' + ShapeToString(APad.MidShape)  + #13#10 +
                                                                         + #13#10 +
                         'Bot X Size : ' + IntToStr(APad.BotXSize)       + #13#10 +
                         'Bot Y Size : ' + IntToStr(APad.BotYSize)       + #13#10 +
                         'Bot Shape  : '  + ShapeToString(APad.BotShape);
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessFullStackPad(APad : IPCB_Pad; Var AString : TPCBString);
Var
   Layer : TLayer;
Begin
   // checking if layer is part of layer stack up not implemented.
   // a full stack pad is technically those layers that are defined in the
   // layer stack up.
    For Layer := MinLayer to MaxLayer Do
    Begin
        If (APad.XStackSizeOnLayer[Layer] <> 0) And
           (APad.YStackSizeOnLayer[Layer] <> 0)
        Then
        Begin
            AString := AString + Layer2String(Layer)           + ' ' +
                       IntToStr(APad.XStackSizeOnLayer[Layer]) + ' ' +
                       IntToStr(APad.YStackSizeOnLayer[Layer]) + ' ' +
                       ShapeToString(APad.StackShapeOnLayer[Layer]) + #13#10;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessRoundHole (APad : IPCB_Pad; Var AString : TPCBString);
Begin
    AString := AString + ' Rounded Hole' + #13#10;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessSquareHole(APad : IPCB_Pad; Var AString : TPCBString);
Begin
    AString := AString + ' Squared Hole' + #13#10;
End;
{..............................................................................}

{..............................................................................}
Procedure ProcessSlotHole  (APad : IPCB_Pad; Var AString : TPCBString);
Begin
    AString := AString + ' Slotted Hole' + #13#10;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchPadStackInfo;
Var
    Board       : IPCB_Board;
    PadObject   : IPCB_Pad;
    PadCache    : TPadCache;

    L           : TLayer;
    PlanesArray : TPlanesConnectArray;

    LS          : TPCBString;
Begin
    // Board.GetObjectAtCursor puts the PCB Editor into Interactive mode,
    // ie a crosshair cursor appears and the 'Choose a pad' message
    // on the status bar of DXP.

    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    PadObject := Board.GetObjectAtCursor(MkSet(ePadObject),
                                      AllLayers,
                                      'Choose a pad');
    While PadObject <> 0 Do
    Begin
        LS :=      'Pad Designator/Name: ' + PadObject.Name     + #13#10;

        // obtain net name only if it exists
        If PadObject.Net <> Nil Then
            LS := LS + 'Pad Net: '              + PadObject.Net.Name + #13#10;

        // work out the pad stack style
             If PadObject.Mode = ePadMode_Simple        Then ProcessSimplePad   (PadObject,LS)
        Else If PadObject.Mode = ePadMode_LocalStack    Then ProcessTopMidBotPad(PadObject,LS)
        Else If PadObject.Mode = ePadMode_ExternalStack Then ProcessFullStackPad(PadObject,LS);

             If PadObject.HoleType = eRoundHole  Then ProcessRoundHole (PadObject,LS)
        Else If PadObject.HoleType = eSquareHole Then ProcessSquareHole(PadObject,LS)
        Else If PadObject.HoleType = eSlotHole   Then ProcessSlotHole  (PadObject,LS);

        LS := LS + 'Pad Hole Rotation: ' + IntToStr(PadObject.HoleRotation);

        // Display the results
        ShowInfo(LS);

        // Continue the loop - ie user can click on another pad or via object.
        PadObject := Board.GetObjectAtCursor(MkSet(ePadObject), AllLayers, 'Choose a pad');
    End;
End;
