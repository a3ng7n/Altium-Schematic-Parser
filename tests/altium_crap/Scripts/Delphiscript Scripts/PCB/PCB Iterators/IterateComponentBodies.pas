{..............................................................................}
{ Summary Iterates Component Bodies from the current PCB document.             }
{                                                                              }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Function GetRegionKindString(RegionK : TRegionKind) : String;
Begin
    Result := '';
    If RegionK = eRegionKind_Copper      Then Result := 'Copper';
    If RegionK = eRegionKind_Cutout      Then Result := 'Cutout';
    If RegionK = eRegionKind_NamedRegion Then Result := 'Named Region';
End;
{..............................................................................}

{..............................................................................}
Procedure IterateComponentBodies;
Var
    Board         : IPCB_Board;
    CmpBody       : IPCB_ComponentBody;
    Iterator      : IPCB_BoardIterator;

    Rpt           : TStringList;

    FileName      : TPCBString;
    Document      : IServerDocument;
    Count         : Integer;
    I             : Integer;
    J             : Integer;
Begin
    // Retrieve the current board
    Board := PCBServer.GetCurrentPCBBoard;
    If Board = Nil Then Exit;

    // Create the iterator that will look for Component Body objects only
    Iterator        := Board.BoardIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eComponentBodyObject));
    Iterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
    Iterator.AddFilter_Method(eProcessAll);

    // Search for component body objects and get their Name, Kind, Area and OverallHeight values
    Count := 0;
    Rpt   := TStringList.Create;
    CmpBody := Iterator.FirstPCBObject;
    While (CmpBody <> Nil) Do
    Begin
        Inc(Count);
        Rpt.Add('Component Body No : ' + IntToStr(Count));
        Rpt.Add('===================');
        Rpt.Add(' Component Body Name : ' + CmpBody.Name);
        Rpt.Add(' Region Kind : '        + GetRegionKindString(CmpBody.Kind));
        Rpt.Add(' Overall Height: '      + IntToStr(CmpBody.GetOverallHeight));

        Rpt.Add('');
        CmpBody := Iterator.NextPCBObject;
    End;
    Board.BoardIterator_Destroy(Iterator);

    // Display the Component Bodies report
    FileName := ChangeFileExt(Board.FileName,'.bod');
    Rpt.SaveToFile(Filename);
    Rpt.Free;

    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
        Client.ShowDocument(Document);
End;
{..............................................................................}

{..............................................................................}
