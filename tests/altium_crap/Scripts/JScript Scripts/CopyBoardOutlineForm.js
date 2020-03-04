/*...............................................................................*/
/* Summary Copies the board outline as tracks and arcs onto specified layer.     */
/*         Layer and Width values to be specified by the user before proceeding. */
/*                                                                               */
/*         Version 1.1                                                           */
/*                                                                               */
/* Copyright (c) 2005 by Altium Limited                                          */
/*...............................................................................*/

var PCB_Board;
/*...............................................................................*/

/*...............................................................................*/
function CopyBoardOutline(AWidth, ALayer)
{
    var Track;
    var Arc;
    var I;
    var J;

    PCB_Board.BoardOutline.Invalidate;
    PCB_Board.BoardOutline.Rebuild;
    PCB_Board.BoardOutline.Validate;

    PCBServer.PreProcess;
    // Step through each of the vertices of the Board Outline in turn.
    for(I = 0; I < PCB_Board.BoardOutline.PointCount; I++)
    {
        // the value of J to point to the "next" vertex this is normally
        // I + 1, but needs to be to 0 instead for the very last vertex
        // that is processed by this loop.
        if(I == PCB_Board.BoardOutline.PointCount - 1)
            J = 0;
        else
            J = I + 1;

        if(PCB_Board.BoardOutline.Segments(I).Kind == ePolySegmentLine)
        {
            // Current segment is a straight line create a Track object.
            Track          = PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);

            Track.X1       = PCB_Board.BoardOutline.Segments(I).vx;
            Track.Y1       = PCB_Board.BoardOutline.Segments(I).vy;
            Track.X2       = PCB_Board.BoardOutline.Segments(J).vx;
            Track.Y2       = PCB_Board.BoardOutline.Segments(J).vy;
            Track.Layer    = ALayer;
            Track.Width    = AWidth;
            PCB_Board.AddPCBObject(Track);
        }
        else
        {
            // Current segment is an arc create an Arc object.
            Arc = PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);

            Arc.XCenter    = PCB_Board.BoardOutline.Segments(I).cx;
            Arc.YCenter    = PCB_Board.BoardOutline.Segments(I).cy;
            Arc.Layer      = ALayer;
            Arc.LineWidth  = AWidth;
            Arc.Radius     = PCB_Board.BoardOutline.Segments(I).Radius;
            Arc.StartAngle = PCB_Board.BoardOutline.Segments(I).Angle1;
            Arc.EndAngle   = PCB_Board.BoardOutline.Segments(I).Angle2;
            PCB_Board.AddPCBObject(Arc);
        }
    }
    PCBServer.PostProcess;

    // Display (unconditionally) the layer selected by the user.
    PCB_Board.LayerIsDisplayed(ALayer) = true;

    // Refresh PCB workspace.
    ResetParameters;
    AddStringParameter("Action", "Redraw");
    RunProcess("PCB:Zoom");
}
/*...............................................................................*/

/*...............................................................................*/
function bCancelClick(Sender)
{
    Close;
}
/*...............................................................................*/

/*...............................................................................*/
function bOKClick(Sender)
{
    var Width;
    var Layer;

    if(PCBServer == null)
        return;

    PCB_Board = PCBServer.GetCurrentPCBBoard;
    if(PCB_Board == null)
        return;

    // Note the Width is in Coord units.
    StringToCoordUnit(eWidth.Text,Width,PCB_Board.DisplayUnit);
    Layer = String2Layer(cbLayers.Items(cbLayers.ItemIndex));

    CopyBoardOutline(Width,Layer);
    Close;
}
