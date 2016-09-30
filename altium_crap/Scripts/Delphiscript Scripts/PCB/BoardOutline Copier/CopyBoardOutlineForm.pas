{...............................................................................}
{ Summary Copies the board outline as tracks and arcs onto specified layer.     }
{         Layer and Width values to be specified by the user before proceeding. }
{                                                                               }
{         Version 1.1                                                           }
{                                                                               }
{ Copyright (c) 2004 by Altium Limited                                          }
{...............................................................................}

{...............................................................................}
Var
    PCB_Board : IPCB_Board;
{...............................................................................}

{...............................................................................}
Procedure CopyBoardOutline(AWidth : Coord; ALayer : TLayer);
Var
    Track     : IPCB_Track;
    Arc       : IPCB_Arc;
    I,J       : Integer;
Begin
    PCB_Board.BoardOutline.Invalidate;
    PCB_Board.BoardOutline.Rebuild;
    PCB_Board.BoardOutline.Validate;

    PCBServer.PreProcess;
    // Step through each of the vertices of the Board Outline in turn.
    For I := 0 To PCB_Board.BoardOutline.PointCount - 1 Do
    Begin
        // Set the value of J to point to the "next" vertex; this is normally
        // I + 1, but needs to be set to 0 instead for the very last vertex
        // that is processed by this loop.
        If I = PCB_Board.BoardOutline.PointCount - 1 Then
            J := 0
        Else
            J := I + 1;

        If PCB_Board.BoardOutline.Segments[I].Kind = ePolySegmentLine Then
        Begin
            // Current segment is a straight line; create a Track object.
            Track          := PCBServer.PCBObjectFactory(eTrackObject,
                                                         eNoDimension,
                                                         eCreate_Default);

            Track.X1       := PCB_Board.BoardOutline.Segments[I].vx;
            Track.Y1       := PCB_Board.BoardOutline.Segments[I].vy;
            Track.X2       := PCB_Board.BoardOutline.Segments[J].vx;
            Track.Y2       := PCB_Board.BoardOutline.Segments[J].vy;
            Track.Layer    := ALayer;
            Track.Width    := AWidth;
            PCB_Board.AddPCBObject(Track);
        End
        Else
        Begin
            // Current segment is an arc; create an Arc object.
            Arc := PCBServer.PCBObjectFactory(eArcObject,
                                              eNoDimension,
                                              eCreate_Default);

            Arc.XCenter    := PCB_Board.BoardOutline.Segments[I].cx;
            Arc.YCenter    := PCB_Board.BoardOutline.Segments[I].cy;
            Arc.Layer      := ALayer;
            Arc.LineWidth  := AWidth;
            Arc.Radius     := PCB_Board.BoardOutline.Segments[I].Radius;
            Arc.StartAngle := PCB_Board.BoardOutline.Segments[I].Angle1;
            Arc.EndAngle   := PCB_Board.BoardOutline.Segments[I].Angle2;
            PCB_Board.AddPCBObject(Arc);
        End;
    End;
    PCBServer.PostProcess;

    // Display (unconditionally) the layer selected by the user.
    PCB_Board.LayerIsDisplayed[ALayer] := True;

    // Refresh PCB workspace.
    ResetParameters;
    AddStringParameter('Action', 'Redraw');
    RunProcess('PCB:Zoom');
End;
{...............................................................................}

{...............................................................................}
Procedure TCopyBoardOutlineForm.bCancelClick(Sender: TObject);
Begin
    Close;
End;
{...............................................................................}

{...............................................................................}
Procedure TCopyBoardOutlineForm.bOKClick(Sender: TObject);
Var
    Width : TCoord;
    Layer : TLayer;
Begin
    PCB_Board       := PCBServer.GetCurrentPCBBoard;
    If PCB_Board     = Nil Then Exit;

    // Note the Width is in Coord units.
    StringToCoordUnit(eWidth.Text,Width,PCB_Board.DisplayUnit);
    Layer := String2Layer(cbLayers.Items[cbLayers.ItemIndex]);

    CopyBoardOutline(Width,Layer);

    Close;
End;
{...............................................................................}

{...............................................................................}
procedure TCopyBoardOutlineForm.CopyBoardOutlineFormCreate(Sender: TObject);
begin
    PCB_Board       := PCBServer.GetCurrentPCBBoard;
    If PCB_Board     = Nil Then Exit;

    SetupComboBoxFromLayer(cbLayers, PCB_Board);
end;
{...............................................................................}

