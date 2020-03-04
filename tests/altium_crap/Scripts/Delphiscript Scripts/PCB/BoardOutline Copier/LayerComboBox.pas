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
    DesiredLayers : IPCB_LayerSet;
    Layer : TLayer;
    LayerObject : IPCB_LayerObject;
    LayerIterator : IPCB_LayerObjectIterator;
Begin
    gv_ListBoxIndexToLayerArray := TList.Create;
    ComboBox.Items.Clear;

    DesiredLayers := LayerSet.EmptySet;
    DesiredLayers.Include(eDrillGuide);
    DesiredLayers.Include(eDrillDrawing);
    DesiredLayers.Include(eKeepOutLayer);
    DesiredLayers.IncludeMechanicalLayers;

    LayerIterator := ILayer.LayerIterator_PossibleLayers;
    LayerIterator.AddFilter_LayerSet(DesiredLayers);

    While LayerIterator.Next Do
    Begin
        gv_ListBoxIndexToLayerArray.Add(LayerIterator.Layer);
        ComboBox.Items.Add(ILayer.AsString(LayerIterator.Layer));

        If LayerIterator.Layer = Board.CurrentLayer Then
              ComboBox.ItemIndex := ComboBox.Items.Count - 1;
    End;

    If ComboBox.ItemIndex < 0 Then
       ComboBox.ItemIndex := 0;
End;
{......................................................................................................................}

