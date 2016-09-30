{..............................................................................}
{ Summary Demo how to use the Undo system - create a component and then a pin  }
{         for this component in two seperate steps and undo these two steps    }
{                                                                              }
{ Version 1.1                                                                  }
{ Copyright (c) 2006 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure UndoRedoOfAComponentAndItsPin;
Var
    SchDoc          : ISch_Document;
    SchematicServer : IServerModule;
    Component       : ISch_Component;
    Rect            : ISch_Rectangle;
    Pin             : ISch_Pin;
    Param           : ISch_Parameter;
Begin
    // This example describes the correct method for allowing Undo/Redo at various different
    // levels of objects (the first at adding components to the document, and the second
    // at adding parameters to the pin of a placed component).

    // Specifically this will add a constructed component to the current sheet, and then
    // a parameter to the pin.  You will then be able to do undo, at the first press of 'Undo',
    // the parameter being added to the pin and then, using undo a second time,
    // adding the component to the document

    // Grab the workspace manager interface so we can create a blank Sch document.
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;
    Workspace.DM_CreateNewDocument('SCH');


    // Grab current schematic document.
    SchDoc := SchServer.GetCurrentSchDocument;
    If SchDoc = Nil Then Exit;

    // Component is a container that holds child objects
    // Create component, and its rectangle, pin and parameter objects.
    Component := SchServer.SchObjectFactory (eSchComponent, eCreate_Default);
    Rect      := SchServer.SchObjectFactory (eRectangle   , eCreate_Default);
    Pin       := SchServer.SchObjectFactory (ePin         , eCreate_Default);
    Param     := SchServer.SchObjectFactory (eParameter   , eCreate_Default);


    // Add the component to the current schematic sheet with undo stack enabled
    // Construct a new component with a rectangle and one pin.
    Try
       // Prepare the Sch software agents for changes.
       SchServer.ProcessControl.PreProcess(SchDoc, '');

       // Define rectangle coordinates and add it to the component.
       // 1000 mil = 1 inch
       Rect.OwnerPartId          := Component.CurrentPartID;
       Rect.OwnerPartDisplayMode := Component.DisplayMode;
       Rect.Location             := Point(InchesToCoord(0.5), InchesToCoord(0.5));
       Rect.Corner               := Point(InchesToCoord(2)  , InchesToCoord(2));
       Rect.Color                := $00FF00;

       // Define a pin and add it to the component.
       Pin.OwnerPartId          := Component.CurrentPartID;
       Pin.OwnerPartDisplayMode := Component.DisplayMode;
       Pin.Location             := Point(InchesToCoord(2), InchesToCoord(1.5));

       // Add rectangle and pin objects to the component object.
       Component.AddSchObject(Rect);
       Component.AddSchObject(Pin);

       // Add the new component to the schematic document.
       SchDoc.AddSchObject(Component);
       Component.Comment.IsHidden := True;
       Component.Designator.IsHidden := True;

       // Move component by 1,1 inch in respect to document's origin.
       Component.MoveByXY(InchesToCoord(1), InchesToCoord(1));

       // Inform the software agents that the document has a new component.
       SchServer.RobotManager.SendMessage(SchDoc.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, Component.I_ObjectAddress);
   Finally
       // Clean up the Sch software agents.
       SchServer.ProcessControl.PostProcess(SchDoc, '');
   End;


   // Create a parameter object and add it to the new pin object.
   Try
       SchServer.ProcessControl.PreProcess(SchDoc, '');

       // Add the parameter to the pin with undo stack also enabled
       Param.Name := 'Added Parameter';
       Param.Text := 'Param added to the pin. Press Undo and this will disappear.  Press undo twice to remove the component';
       Param.Location := Point(InchesToCoord(3), InchesToCoord(2.4));

       Pin.AddSchObject(Param);
       SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, Param.I_ObjectAddress);
   Finally
       SchServer.ProcessControl.PostProcess(SchDoc, '');
   End;

   // Refresh the screen
   SchDoc.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
