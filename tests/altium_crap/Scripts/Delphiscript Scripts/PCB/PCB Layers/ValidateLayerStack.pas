{..............................................................................}
{ Summary Checks the sequence of layers within the PCB file's layer stack      }
{ Copyright (c) 2005 by Altium Limited                                         }
{..............................................................................}
Procedure ValidateTheLayerStack;
Var
    PCBBoard      : IPCB_Board;
    TheLayerStack : IPCB_LayerStack;
    ordSignal     : Integer;
    ordIntPlane   : Integer;
    OK_so_far     : Boolean;
    LayerObj      : IPCB_LayerObject;
    Layer         : TLayer;
Begin
    // Obtain the PCB document interface
    PCBBoard := PCBServer.GetCurrentPCBBoard;
    If PCBBoard = Nil Then Exit;

    // Obtain the layer stack of the current PCB document
    TheLayerStack := PCBBoard.LayerStack;
    If TheLayerStack = Nil Then Exit;

    ordSignal   := Ord(eMidLayer1);
    ordIntPlane := Ord(eInternalPlane1);
    OK_so_far   := True;

    // Obtain the first layer of the layer stack
    LayerObj := TheLayerStack.FirstLayer;
    Layer    := LayerObj.NextLayer;

    // Iterate through the layer stack
    // expected layers are signal and internal plane layers.
    While Layer <> eBottomLayer And OK_so_far Do
    Begin
        LayerObj := TheLayerStack.LayerObject(Layer);

        If Ord(Layer) = ordSignal Then
            // Current layer is "expected" Signal layer
            Inc(ordSignal)
        Else If Ord(Layer) = ordIntPlane Then
            // Current layer is "expected" Internal Plane layer
            Inc(ordIntPlane)
        Else
            // Current layer is neither "expected" Signal layer nor
            // "expected" Internal Plane layer - so Layer Stack is *invalid*
            OK_so_far := False;
            
        Layer    := LayerObj.NextLayer;
    End;

    If OK_so_far Then
        ShowInfo('The current Layer Stack is valid.')
    Else
        ShowError('The current Layer Stack is NOT valid!');
End;
{..............................................................................}

{..............................................................................}
