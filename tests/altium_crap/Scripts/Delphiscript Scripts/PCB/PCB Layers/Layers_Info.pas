{..............................................................................}
{ Summary Layers information based on the current PCB layer stack              }
{         of the pcb document.                                                 }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function ConvertDielectricTypeTOString (DT : TDielectricType): String;
Begin
    Result := 'Unknown Type';
    Case DT Of
        eNoDielectric    : Result := 'No Dielectric';
        eCore            : Result := 'Core';
        ePrePreg         : Result := 'PrePreg';
        eSurfaceMaterial : Result := 'Surface Material';
    End;

End;
{..............................................................................}

{..............................................................................}
Function GetLayerInfo(BoardHandle : IPCB_Board; LayerObject : IPCB_LayerObject) : String;
Begin
    Result := Layer2String(LayerObject.LayerID) + ', ' + LayerObject.Name + ', ' +
              'Copper' + ', ' + FloatToStr(LayerObject.CopperThickness / 10000) + ', ';

    If LayerObject.Dielectric.DielectricType <> eNoDielectric Then
    Begin
       Result := Result + ConvertDielectricTypeTOString(LayerObject.Dielectric.DielectricType) + ', ' +
                 LayerObject.Dielectric.DielectricMaterial +  ', ' + FloatToStr(LayerObject.Dielectric.DielectricHeight / 10000) + ', ' +
                 FloatToStr(LayerObject.Dielectric.DielectricConstant);
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure FetchLayersInformation;
Var
    BoardHandle : IPCB_Board;
    LayerIterator : IPCB_LayerObjectIterator;
    Filename    : String;
    ReportDocument : IServerDocument;
    StringList : TStringList;
Begin
    BoardHandle := PCBServer.GetCurrentPCBBoard;
    If BoardHandle = Nil Then
    Begin
       ShowError('Current document is not PCB document');
       Exit;
    End;

    StringList := TStringList.Create;
    StringList.Add('Layer, Name, Material, Cu Thickness,  Dielectric Material, type, constant, height');

    LayerIterator := BoardHandle.ElectricalLayerIterator;
    While LayerIterator.Next Do
        StringList.Add(GetLayerInfo(BoardHandle, LayerIterator.LayerObject));

    FileName := ChangeFileExt(BoardHandle.FileName, '') + '_LayerRpt.cvs';
    StringList.SaveToFile(Filename);
    StringList.Free;

    ReportDocument := Client.OpenDocument('Text', FileName);
    If ReportDocument <> Nil Then
        Client.ShowDocument(ReportDocument);
End;
{..............................................................................}

{..............................................................................}
