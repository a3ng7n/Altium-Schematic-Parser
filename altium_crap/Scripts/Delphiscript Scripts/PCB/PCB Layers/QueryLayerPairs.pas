{..............................................................................}
{ Summary Queries the current PCB document for PCB Layer pairs.                }
{ A layer stack can have multiple layer pairs.                                 }
{                                                                               }
{ Copyright (c) 2007 by Altium Limited                                         }
{..............................................................................}
Procedure QueryLayerPairs;
Var
    PCBBoard     : IPCB_Board;
    i            : Integer;
    LayerPairs   : TStringList;
    PCBLayerPair : IPCB_DrillLayerPair;
    LowLayerObj  : IPCB_LayerObject;
    HighLayerObj : IPCB_LayerObject;
    LowPos       : Integer;
    HighPos      : Integer;
    LS           : String;
Begin
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    // Show the current layer for the PCB document
    ShowInfo('Current Layer: ' + Layer2String(PCBBoard.CurrentLayer));

    // Create a TStringList object to store Low and High Layer Names 
    // for Drill layer Pairs.
    LayerPairs := TStringList.Create;
    For i := 0 To PCBBoard.DrillLayerPairsCount - 1 Do
    Begin
        PCBLayerPair := PCBBoard.LayerPair[i];

        LowLayerObj  := PCBBoard.LayerStack.LayerObject[PCBLayerPair.LowLayer];
        HighLayerObj := PCBBoard.LayerStack.LayerObject[PCBLayerPair.HighLayer];
        LowPos       := PCBBoard.LayerPositionInSet(SetUnion(SignalLayers,InternalPlanes), LowLayerObj);
        HighPos      := PCBBoard.LayerPositionInSet(SetUnion(SignalLayers,InternalPlanes), HighLayerObj);

        If LowPos <= HighPos Then
            LayerPairs.Add(LowLayerObj .Name + ' - ' + HighLayerObj.Name)
        Else
            LayerPairs.Add(HighLayerObj.Name + ' - ' + LowLayerObj .Name);
    End;

    // Format the layer pairs data string and display it.
    LS := '';
    For i := 0 to LayerPairs.Count - 1 Do
        LS := LS + LayerPairs[i] + #13#10;

    ShowInfo('Layer Pairs:'#13#10 + LS);

    LayerPairs.Free;
End;
{..............................................................................}

{..............................................................................}
