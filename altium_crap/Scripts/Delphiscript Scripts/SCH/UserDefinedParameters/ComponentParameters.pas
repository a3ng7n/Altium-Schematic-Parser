{..............................................................................}
{ Summary                                                                      }
{ Demo how to add, modify and delete the user parameters for components        }
{                                                                              }
{ Version 1.0                                                                  }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Const
    UserDefinedName = 'User-Defined';
    UserDefinedText = 'Text';

{..............................................................................}

{..............................................................................}
Procedure AddUserDefinedParametersToComponents(SchDoc : ISch_Document);
Var
    Component       : ISch_Component;
    Param           : ISch_Parameter;
    Offset          : Integer;
    Iterator        : ISch_Iterator;
Begin
    // Create a user defined parameter object and add it to all components.
    // Look for components only
    Iterator := SchDoc.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Try
       SchServer.ProcessControl.PreProcess(SchDoc, '');
       Try
           Component := Iterator.FirstSchObject;
           While Component <> Nil Do
           Begin
              // Add the parameter to the pin with undo stack also enabled
              Param     := SchServer.SchObjectFactory (eParameter   , eCreate_Default);
              Param.Name     := UserDefinedName;
              Param.ShowName := False;
              Param.Text     := UserDefinedText;
              Param.IsHidden := False;

              // Param object is placed 0.1 Dxp Units above the component.
              Param.Location := Point(Component.Location.X, Component.Location.Y + DxpsToCoord(0.1));

              Component.AddSchObject(Param);
              SchServer.RobotManager.SendMessage(Component.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, Param.I_ObjectAddress);

              Component := Iterator.NextSchObject;
           End;

        Finally
           SchDoc.SchIterator_Destroy(Iterator);
        End;
    Finally
        SchServer.ProcessControl.PostProcess(SchDoc, '');
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure ModifyUSerDefinedParametersOfComponents(Sheet : ISch_Document; UserDefined : String);
Var
    Component     : ISch_Component;
    Parameter     : ISch_Parameter;
    Iterator      : ISch_Iterator;
    PIterator     : ISch_Iterator;
Begin
    If Sheet = Nil Then Exit;

    // Look for components only
    Iterator := Sheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(Sheet, '');
    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            Try
                PIterator := Component.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                Parameter := PIterator.FirstSchObject;
                While Parameter <> Nil Do
                Begin
                    // Check for parameters that have the UserDefinedName Name.
                    If Parameter.Name = UserDefinedName Then
                    Begin
                        SchServer.RobotManager.SendMessage(Parameter.I_ObjectAddress, c_BroadCast, SCHM_BeginModify, c_NoEventData);
                        Parameter.Text := UserDefined;
                        SchServer.RobotManager.SendMessage(Parameter.I_ObjectAddress, c_BroadCast, SCHM_EndModify  , c_NoEventData);
                    End;
                    Parameter := PIterator.NextSchObject;
                End;
            Finally
                Component.SchIterator_Destroy(PIterator);
            End;
            Component := Iterator.NextSchObject;
        End;
    Finally
        Sheet.SchIterator_Destroy(Iterator);
    End;

    // Clean up robots in Schematic editor.
    SchServer.ProcessControl.PostProcess(Sheet, '');

    // Refresh the screen
    Sheet.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
Procedure DeleteUserDefinedParametersOfComponents(SchDoc : ISch_Document);
Var
    Component     : ISch_Component;
    OldParameter  : ISch_Parameter;
    Parameter     : ISch_Parameter;
    Iterator      : ISch_Iterator;
    PIterator     : ISch_Iterator;
    I             : Integer;
    ParameterList : TInterfaceList;
Begin
    // Initialize the robots in Schematic editor.
    SchServer.ProcessControl.PreProcess(SchDoc, '');

    // Look for components only
    Iterator := SchDoc.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    ParameterList := TInterfaceList.Create;
    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            Try
                PIterator := Component.SchIterator_Create;
                PIterator.AddFilter_ObjectSet(MkSet(eParameter));

                Parameter := PIterator.FirstSchObject;
                While Parameter <> Nil Do
                Begin
                     If Parameter.Name = UserDefinedName Then
                         ParameterList.Add(Parameter);
                    Parameter := PIterator.NextSChObject;
                End;
            Finally
               Component.SchIterator_Destroy(PIterator);
            End;
            Component := Iterator.NextSchObject;
        End;
    Finally
        SchDoc.SchIterator_Destroy(Iterator);
    End;

    // Remove Parameters from their components
    Try
        For I := 0 to ParameterList.Count - 1 Do
        Begin
            Parameter := ParameterList.items[i];

            // Obtain the parent object which is the component
            // that user defined param is part of.
            Component := Parameter.Container;
            Component.RemoveSchObject(Parameter);

            SchServer.RobotManager.SendMessage(Component.I_ObjectAddress,c_BroadCast,
                                               SCHM_PrimitiveRegistration,Parameter.I_ObjectAddress);
        End;
    Finally
        ParameterList.Free;
    End;

    // Clean up robots in Schematic editor.
    SchServer.ProcessControl.PostProcess(SchDoc, '');

    // Refresh the screen
    SchDoc.GraphicallyInvalidate;
End;
{..............................................................................}

{..............................................................................}
Procedure AddComponentUserParameters;
Var
    I           : Integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SchDocument : IServerDocument;
    Project     : IProject;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;
    Project.DM_Compile;

    // selected or all documents within the project
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
           Begin
           SchDocument := Client.OpenDocument('Sch', Doc.DM_FullPath);
           If SchDocument <> Nil Then
           Begin
               Client.ShowDocument(SchDocument);
               CurrentSch := SchServer.GetCurrentSchDocument;
               If CurrentSch = Nil Then Exit;
               AddUserDefinedParametersToComponents(CurrentSch);
           End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure DeleteComponentUserParameters;
Var
    I           : Integer;
    Project     : IProject;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SchDocument : IServerDocument;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;
    Project.DM_Compile;

    // selected or all documents within the project
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
           Begin
           SchDocument := Client.OpenDocument('Sch', Doc.DM_FullPath);
           If SchDocument <> Nil Then
           Begin
               Client.ShowDocument(SchDocument);
               CurrentSch := SchServer.GetCurrentSchDocument;
               If CurrentSch = Nil Then Exit;
               DeleteUserDefinedParametersOfComponents(CurrentSch);
           End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}
Procedure ModifyComponentUserParameters;
Var
    I           : Integer;
    Doc         : IDocument;
    CurrentSch  : ISch_Document;
    SchDocument : IServerDocument;
    Project     : IProject;
Begin
    Project := GetWorkspace.DM_FocusedProject;
    If Project = Nil Then Exit;
    Project.DM_Compile;

    // selected or all documents within the project
    For I := 0 to Project.DM_LogicalDocumentCount - 1 Do
    Begin
        Doc := Project.DM_LogicalDocuments(I);
        If Doc.DM_DocumentKind = 'SCH' Then
           Begin
           SchDocument := Client.OpenDocument('Sch', Doc.DM_FullPath);
           If SchDocument <> Nil Then
           Begin
               Client.ShowDocument(SchDocument);
               CurrentSch := SchServer.GetCurrentSchDocument;
               If CurrentSch = Nil Then Exit;
               ModifyUSerDefinedParametersOfComponents(CurrentSch, 'ModifiedText');
           End;
        End;
    End;
End;
{..............................................................................}

{..............................................................................}

(*
Synopsis:
1. This script has three main procedures and two string constants;
    AddComponentUserParameters
    ModifyComponentUserParameters
    DeleteComponentUserParameters

    UserDefinedName = 'User-Defined';
    UserDefinedText = 'Text';

    This script uses Schematic API (for dealing with parameters of components) and
    Workspace Manager API (for iterating schematics of a project)

2. To use this script, load a project with a hierarchy of schematic documents then
open a schematic from this project in Altium Designer.

3. Invoke the Run Script command from the DXP » Run Script menu.

a/ Run the AddComponentUserParameters from the Select Item to Run dialog.
This procedure goes through the logical documents of a focussed project.
For each schematic document found, Parameters with a "User-Defined" Name and a "Text" Value
are added to all components on that document.

b/ To delete these Parameters with a "User-Defined" Names, invoke the DeleteComponentUserParameters.

c/ To modify these Parameters with a "User-Defined" Names, invoke the ModifyComponentUserParameters.
This procedure checks for components with the "User-Defined" Names and updates the Value to 'ModifiedText'


Limitations of this script:
1. This script does not check for existing parameters with Names of "User-Defined" so duplicates will be generated
if you run the AddComponentUserParameters repeatedly.

You are free to modify this script but use this script at your risk. Please back up your designs first before using this script.
*)
