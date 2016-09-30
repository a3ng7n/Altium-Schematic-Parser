{..............................................................................}
{ Summary Create and place Schematic objects on a Schematic document.          }
{                                                                              }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Var
    SchDoc    : ISch_Document;
    WorkSpace : IWorkSpace;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchPort(Dummy : Integer);
Var
    AName       : TDynamicString;
    Orientation : TRotationBy90;
    AElectrical : TPinElectrical;
    SchPort     : ISch_Port;
    Loc         : TLocation;
    CurView     : IServerDocumentView;
Begin
    SchPort := SchServer.SchObjectFactory(ePort,eCreate_GlobalCopy);
    If SchPort = Nil Then Exit;

    SchPort.Location  := Point(MilsToCoord(1000),MilsToCoord(1000));
    SchPort.Style     := ePortRight;
    SchPort.IOType    := ePortBidirectional;
    SchPort.Alignment := eHorizontalCentreAlign;
    SchPort.Width     := MilsToCoord(1000);
    SchPort.AreaColor := 0;
    SchPort.TextColor := $FFFFFF;
    SchPort.Name      := 'Test Port';

    SchDoc.RegisterSchObjectInContainer(SchPort);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchComponent(Dummy : Integer);
Begin
    If IntegratedLibraryManager = Nil Then Exit;
    IntegratedLibraryManager.PlaceLibraryComponent(
        'Res2',
        'Miscellaneous Devices.IntLib',
        'ModelType=SIM|ModelParameterName0=Value|ModelParameterValue0=1K|Orientation=1|Location.X=5000000|Location.Y=5000000');
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchJunction(Dummy : Integer);
Var
     SchJunction      : ISch_Junction;
Begin
     SchJunction := SchServer.SchObjectFactory(eJunction,eCreate_GlobalCopy);
     If SchJunction = Nil Then Exit;

     SchJunction.Location        := Point(MilsToCoord(3000), MilsToCoord(2000));
     SchJunction.SetState_Size   := eMedium;
     SchJunction.SetState_Locked := False;

     SchDoc.RegisterSchObjectInContainer(SchJunction);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchNetLabel(Dummy : Integer);
Var
    SchNetlabel : ISch_Netlabel;
Begin
    SchNetlabel := SchServer.SchObjectFactory(eNetlabel,eCreate_GlobalCopy);
    If SchNetlabel = Nil Then Exit;

    SchNetlabel.Location    := Point(MilsToCoord(2500), MilsToCoord(2500));
    SchNetlabel.Orientation := eRotate90;
    SchNetlabel.Text        := 'Netname';
    SchDoc.RegisterSchObjectInContainer(SchNetlabel);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchLine(Dummy : Integer);
Var
    SchLine : ISch_Line;
Begin
     SchLine := SchServer.SchObjectFactory(eLine,eCreate_GlobalCopy);
     If SchLine = Nil Then Exit;

     SchLine.Location  := Point(MilsToCoord(1800), MilsToCoord(2000));
     SchLine.Corner    := Point(MilsToCoord(1800), MilsToCoord(4000));
     SchLine.LineWidth := eMedium;
     SchLine.LineStyle := eLineStyleSolid;
     SchLine.Color := $FF00FF;
     SchDoc.RegisterSchObjectInContainer(SchLine);
End;
{..............................................................................}

{..............................................................................}
Function SortVertices(WireVertices : String) : Integer;
Var
   NewValue : String;
Begin
     //X1=4540|Y1=4540|X2=4540|Y2=3450|X2=3540|Y2=4560|....
     If Pos('|', WireVertices) > 0 Then
     Begin
          NewValue := Copy(WireVertices, Pos('=', WireVertices) + 1, pos('|', WireVertices) - pos('=', WireVertices) - 1);
          Result := NewValue;
     End;
End;
{..............................................................................}

{..............................................................................}
Function VerticesTrim(WireVertices : String) : String;
Var
   NewValue : String;
Begin
     If Pos('|', WireVertices) > 0 Then
     Begin
          Delete(WireVertices, 1, pos('|', WireVertices));
          Result := WireVertices;
     End;
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceASchWire(NumberOfVertices : Integer, Vertices : String, LineWidth : Tsize);
Var
   ScriptParametres : String;
   SchWire          : ISch_Wire;
   I                : Integer;
   X                : Integer;
   Y                : Integer;
   WireVertices     : String;
Begin
     SchWire := SchServer.SchObjectFactory(eWire,eCreate_GlobalCopy);
     If SchWire = Nil Then Exit;

     // Number of vertices. Always 2 for a single wire
     WireVertices := Vertices;

     X := SortVertices(WireVertices);
     WireVertices := VerticesTrim(WireVertices);

     Y := SortVertices(WireVertices);
     WireVertices := VerticesTrim(WireVertices);

     // Set the line width based on TSize type
     SchWire.SetState_LineWidth := LineWidth;

     // Starting point for the vertex
     Schwire.Location := Point(MilsToCoord(X), MilsToCoord(Y));
     Schwire.InsertVertex := 1;
     SchWire.SetState_Vertex(1, Point(MilsToCOord(X), MilsToCoord(Y)));

     For I := 2 to NumberOfVertices Do
     Begin
          Schwire.InsertVertex := I;
          X                    := SortVertices(WireVertices);

          WireVertices         := VerticesTrim(WireVertices);

          Y                    := SortVertices(WireVertices);
          WireVertices         := VerticesTrim(WireVertices);

          SchWire.SetState_Vertex(I, Point(MilsToCoord(X), MilsToCoord(Y)));
     End;
     SchDoc.RegisterSchObjectInContainer(SchWire);
End;
{..............................................................................}

{..............................................................................}
Procedure PlaceSchematicObjects;
Begin
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;
    Workspace.DM_CreateNewDocument('SCH');

    If SchServer = Nil Then Exit;
    SchDoc := SchServer.GetCurrentSchDocument;
    If SchDoc = Nil Then Exit;

    PlaceASchPort(0);
    PlaceASchJunction(0);
    PlaceASchNetLabel(0);
    PlaceASchComponent(0);
    PlaceASchLine(0);

    PlaceASchWire(2, 'X1=2000|Y1=2000|X2=2500|Y2=3000|', eSmall);
    PlaceASchWire(2, 'X1=2500|Y1=3000|X2=3000|Y2=2000|', eMedium);
    PlaceASchWire(2, 'X1=3000|Y1=2000|X2=2000|Y2=2000|', eLarge);

    SchDoc.GraphicallyInvalidate;

    ResetParameters;
    AddStringParameter('Action', 'Document');
    RunProcess('Sch:Zoom');
End;
{..............................................................................}

{..............................................................................}
