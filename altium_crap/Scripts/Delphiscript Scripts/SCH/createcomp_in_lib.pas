{..............................................................................}
{ Summary Demo how to create a new symbol in the library                       }
{ Copyright (c) 2004 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure CreateALibComponent;
Var
    CurrentLib   : ISch_Lib;
    SchComponent : ISch_Component;
    R            : ISch_Rectangle;
    P1, P2       : ISch_Pin;
Begin
    If SchServer = Nil Then Exit;
    CurrentLib := SchServer.GetCurrentSchDocument;
    If CurrentLib = Nil Then Exit;

    // Check if the document is a Schematic Libary document first
    If CurrentLib.ObjectID <> eSchLib Then
    Begin
         ShowError('Please open schematic library.');
         Exit;
    End;

    // Create a library component (a page of the library is created).
    SchComponent := SchServer.SchObjectFactory(eSchComponent, eCreate_Default);
    If SchComponent = Nil Then Exit;


    // Set up parameters for the library component.
    SchComponent.CurrentPartID := 1;
    SchComponent.DisplayMode   := 0;

    // Define the LibReference and add the component to the library.
    SchComponent.LibReference := 'Custom';

    // Create a rectangle object for the new library component.
    R := SchServer.SchObjectFactory(eRectangle, eCreate_Default);
    If R = Nil Then Exit;

    // Define the rectangle parameters.
    R.LineWidth := eSmall;
    R.Location  := Point(MilsToCoord(390), MilsToCoord(90));
    R.Corner    := Point(MilsToCoord(790), MilsToCoord(860));
    R.Color     := $00FFFF;    // YELLOW
    R.AreaColor := $000000;    // BLACK
    R.IsSolid   := True;
    R.OwnerPartId          := CurrentLib.CurrentSchComponent.CurrentPartID;
    R.OwnerPartDisplayMode := CurrentLib.CurrentSchComponent.DisplayMode;



    // Create two pin objects for the new library component.
    P1 := SchServer.SchObjectFactory(ePin, eCreate_Default);
    If P1 = Nil Then Exit;

    // Define the pin parameters.
    P1.Location             := Point(MilsToCoord(400), MilsToCoord(330));
    P1.Color                := $000000;    // YELLOW
    P1.Orientation          := eRotate180;
    P1.Designator           := '0';
    P1.Name                 := '0';
    P1.OwnerPartId          := CurrentLib.CurrentSchComponent.CurrentPartID;
    P1.OwnerPartDisplayMode := CurrentLib.CurrentSchComponent.DisplayMode;


    P2 := SchServer.SchObjectFactory(ePin, eCreate_Default);
    If P2 = Nil Then Exit;

    // Define the pin parameters.
    P2.Location             := Point(MilsToCoord(400), MilsToCoord(750));
    P2.Color                := $000000;    // YELLOW
    P2.Orientation          := eRotate180;
    P2.Designator           := '1';
    P2.Name                 := '1';
    P2.OwnerPartId          := CurrentLib.CurrentSchComponent.CurrentPartID;
    P2.OwnerPartDisplayMode := CurrentLib.CurrentSchComponent.DisplayMode;


    // Add the rectangle and the pins to the component (the new page of library component).
    SchComponent.AddSchObject(P1);
    SchComponent.AddSchObject(P2);
    SchComponent.AddSchObject(R);

    SchComponent.Designator.Text      := 'U';
    SchComponent.ComponentDescription := 'Custom IC';

    CurrentLib.AddSchComponent(SchComponent);

    // Send a system notification that a new component has been added to the library.
    SchServer.RobotManager.SendMessage(nil, c_BroadCast, SCHM_PrimitiveRegistration, SCHComponent.I_ObjectAddress);
    CurrentLib.CurrentSchComponent := SchComponent;

    // Refresh library.
    CurrentLib.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
