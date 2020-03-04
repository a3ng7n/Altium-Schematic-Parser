{..............................................................................}
{ Summary Demo the use of a spatial iterator to conduct a search within        }
{         a defined region.                                                    }  
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure RunSpatialIteratorExample;
Var
    CurrentSheet    : ISch_Document;
    SpatialIterator : ISch_Iterator;
    Junction        : ISch_Junction;
    Component       : ISch_Component;
    J,J1            : String;
    P,P1            : String;
    Rect            : TCoordRect;
    GraphicalObj    : ISch_GraphicalObject;
Begin
    If SchServer = Nil Then Exit;
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;

    Rect := TCoordRect;
    P    := '';
    J    := '';

    //Using the ChooseRectangleInteractively method to capture the
    // Boundary coordinates clicked on the sheet by the user.
    If Not CurrentSheet.ChooseRectangleInteractively(Rect,
                                                     'Please select the first corner',
                                                     'Please select the second corner') Then Exit;

    // Create a spatial iterator with the boundary coordinates
    // Spatial iterator looks for Junctions and Components only.
    SpatialIterator := CurrentSheet.SchIterator_Create;
    If SpatialIterator = Nil Then Exit;
    Try
        SpatialIterator.AddFilter_ObjectSet(MkSet(eJunction,eSchComponent));
        SpatialIterator.AddFilter_Area(Rect.left, Rect.bottom, Rect.right, Rect.top);

        GraphicalObj := SpatialIterator.FirstSchObject;
        While GraphicalObj <> Nil Do
        Begin
            If GraphicalObj.ObjectId = eJunction Then
            Begin
                J1 := 'X:' + IntToStr(GraphicalObj.Location.X) +
                    ', Y:' + IntToStr(GraphicalObj.Location.Y);
                J  := J + J1 + #13;
            End
            Else If GraphicalObj.ObjectId = eSchComponent Then
            Begin
                P1 := 'X:' + IntToStr(GraphicalObj.Location.X) +
                    ', Y:' + IntToStr(GraphicalObj.Location.Y);
                P  := P + P1 + #13;
            End;

            GraphicalObj := SpatialIterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(SpatialIterator);
    End;


    // Display the results on a modal dialog
    ShowInfo('Junction objects found at' + #13 + J + #13 + #13 +
             'Part objects found at'     + #13 + P);
End;
{..............................................................................}

{..............................................................................}
