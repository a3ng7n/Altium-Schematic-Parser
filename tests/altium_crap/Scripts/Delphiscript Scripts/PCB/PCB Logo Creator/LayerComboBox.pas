{......................................................................................................................}
Var gv_ListBoxIndexToLayerArray : TList;
{......................................................................................................................}

{......................................................................................................................}
Function GetLayerFromComboBox(ComboBox : TComboBox; Board : IPCB_Board) : TLayer;
Begin
    Result := eTopLayer;

    If ComboBox.ItemIndex < 0 Then
        Exit;

    If ComboBox.ItemIndex >= ComboBox.Items.Count Then
        Exit;

    Result := gv_ListBoxIndexToLayerArray.Items(ComboBox.ItemIndex);
End;
{......................................................................................................................}

{......................................................................................................................}
Procedure SetupComboBoxFromLayer(ComboBox : TComboBox; Board : IPCB_Board);
Var
    Layer : TLayer;
    LayerObject : IPCB_LayerObject;
    LayerIterator : IPCB_LayerObjectIterator;
Begin
    gv_ListBoxIndexToLayerArray := TList.Create;
    ComboBox.Items.Clear;

    LayerIterator := ILayer.LayerIterator_PossibleLayers;
    While LayerIterator.Next Do
    Begin
        gv_ListBoxIndexToLayerArray.Add(LayerIterator.Layer);
        ComboBox.Items.Add(ILayer.AsString(LayerIterator.Layer));

        If LayerIterator.Layer = Board.CurrentLayer Then
              ComboBox.ItemIndex := ComboBox.Items.Count - 1;
    End;
End;
{......................................................................................................................}

