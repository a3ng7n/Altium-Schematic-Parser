{......................................................................................................................}
Procedure CreateRegionsFromPicture_NoUnion(AImage : TImage; Layer : TLayer; Group : IPCB_Group; ProgressBar : TXPProgressBar; StatusBar : TXPStatusBar);
Var
    AGeometricPolygon       : IPCB_GeometricPolygon;
    Region            : IPCB_Region;
    I                 : Integer;
Begin
    AGeometricPolygon := ConstructGeometricPolygonFromPicture_NoUnion(AImage, ProgressBar, StatusBar);

    For I := 0 To AGeometricPolygon.Count - 1 Do
    Begin
        Region := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension, eCreate_Default);
        Region.SetOutlineContour(AGeometricPolygon.Contour[I]);
        Region.Layer := Layer;
        Group.AddPCBObject(Region);
    End;
End;
{......................................................................................................................}

{......................................................................................................................}
// Altium Designer assumes regions contain a single piece of connected copper, ie a single non hole contour and multiple
// nonintersecting holes. A raw IPCB_GeometricPolygon does guarantee this constraint, but after a IPCB_ContourUtilities.Clip(...)
// call it is guarenteed to contain nonintersecting IPCB_Contours. The CreateRegionsFromPicture_Union(...) function associates
// each hole with its corresponding non-hole contour and creates appropiate IPCB_Regions
Procedure CreateRegionsFromPicture_Union(AImage : TImage; Layer : TLayer; Group : IPCB_Group; ProgressBar : TXPProgressBar; StatusBar : TXPStatusBar);
Var
   AGeometricPolygon : IPCB_GeometricPolygon;
   Region            : IPCB_Region;
   I, J              : Integer;
   HoleContour       : IPCB_Contour;
   ContainingRegion  : IPCB_Region;
   MinHoleArea       : Double;
   ConnectedPolygon  : IPCB_GeometricPolygon;
   ConnectedPolygons : IInterfaceList;
Begin
    AGeometricPolygon := ConstructGeometricPolygonFromPicture_UnionOptimized2(AImage, ProgressBar, StatusBar);

    If StatusBar <> Nil Then
    Begin
        StatusBar.SimpleText := 'Splitting Polygons...';
        StatusBar.Update;
    End;

    ConnectedPolygons := CreateInterfaceList;
    PCBServer.PCBContourUtilities.SplitIntoConnectedPolygons(AGeometricPolygon, ConnectedPolygons);

    If StatusBar <> Nil Then
    Begin
        StatusBar.SimpleText := 'Creating Regions...';
        StatusBar.Update;
    End;

    For I := 0 To ConnectedPolygons.Count - 1 Do
    Begin
        ConnectedPolygon := ConnectedPolygons[I];

        Region := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension, eCreate_Default);
        Region.SetOutlineContour(ConnectedPolygon.Contour[0]);
        Region.Layer := Layer;

        For J := 1 To ConnectedPolygon.Count - 1 Do
            Region.GeometricPolygon.AddContourIsHole(ConnectedPolygon.Contour[J], True);

        Group.AddPCBObject(Region);
    End;

    ConnectedPolygons := Nil;
End;
{......................................................................................................................}

