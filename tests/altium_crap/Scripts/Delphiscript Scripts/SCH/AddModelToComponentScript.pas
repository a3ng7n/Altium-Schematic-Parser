{..............................................................................}
{ Summary   This scripts demonstrates on how to add a simulation model to      }
{           a 555 component. This is done by adding a 555.ckt file to the comp }
{                                                                              }
{                                                                              }
{ Copyright (c) 2008 by Altium Limited                                         }
{..............................................................................}

{..............................................................................}
Procedure AddSimModelToAComponent;
Var
    CurrentSheet    : ISch_Document;
    Iterator        : ISch_Iterator;
    Component       : ISch_Component;
    Model           : ISch_Implementation;
    Parameter       : ISch_Parameter;

    ModelType       : String;
    CKTFileLocation : String;
    ModelName       : String;
    Netlist         : String;
Begin
    If SchServer = Nil Then Exit;
    CurrentSheet := SchServer.GetCurrentSchDocument;
    If CurrentSheet = Nil Then Exit;

    // need the 55 Astable multivibrator example.
    //look for the component, 555
    Iterator := CurrentSheet.SchIterator_Create;
    Iterator.AddFilter_ObjectSet(MkSet(eSchComponent));

    Try
        Component := Iterator.FirstSchObject;
        While Component <> Nil Do
        Begin
            If Component.LibReference = '555' Then Break;
            Component := Iterator.NextSchObject;
        End;
    Finally
        CurrentSheet.SchIterator_Destroy(Iterator);
    End;

    If Component = Nil Then
    Begin
        ShowMessage('555 Component is not found!');
        Exit;
    End;

    //add a new implementation to the component.
    ModelName       := '555';
    ModelType       := 'SIM';

    // hard coded path to a 555.ckt file in \Examples\Circuit Simulation\ folder of Altium Designers installation.
    CKTFileLocation := 'C:\Program Files\Altium Designer Summer 08\Examples\Circuit Simulation\555 Astable Multivibrator\555.ckt';
    NetList         := '@DESIGNATOR %1 %2 %3 %4 %5 %6 %7 %8 @MODEL';

    Model := Component.AddSchImplementation;
    Model.ClearAllDatafileLinks;

    Model.ModelName   := ModelName;
    Model.ModelType   := ModelType;
    Model.Description := '555 Simulation Model';
    Model.MapAsString := '(1:1),(2:2),(3:3),(4:4),(5:5),(6:6),(7:7),(8:8)';
    Model.AddDataFileLink(ModelName, CKTFileLocation, ModelType);

    // Create Kind parameter for the component
    Parameter      := SchServer.SchObjectFactory(eParameter, eCreate_Default);
    Parameter.Name := 'Kind';
    Parameter.Text := 'General';
    Parameter.ParamType     := eParameterType_String;
    Parameter.ReadOnlyState := eReadOnly_None;
    Model.AddSchObject(Parameter);


    // Create SubKind parameter for the component
    Parameter      := SchServer.SchObjectFactory(eParameter, eCreate_Default);
    Parameter.Name := 'SubKind';
    Parameter.Text := 'Spice Subcircuit';
    Parameter.ParamType     := eParameterType_String;
    Parameter.ReadOnlyState := eReadOnly_None;
    Model.AddSchObject(Parameter);


    // Create Spice Prefix parameter for the component
    Parameter      := SchServer.SchObjectFactory(eParameter, eCreate_Default);
    Parameter.Name := 'Spice Prefix';
    Parameter.Text := 'X';
    Parameter.ParamType     := eParameterType_String;
    Parameter.ReadOnlyState := eReadOnly_None;
    Model.AddSchObject(Parameter);

    // Create Netlist parameter for the component
    Parameter      := SchServer.SchObjectFactory(eParameter, eCreate_Default);
    Parameter.Name := 'NetList';
    Parameter.Text := NetList;
    Parameter.ParamType     := eParameterType_String;
    Parameter.ReadOnlyState := eReadOnly_None;
    Model.AddSchObject(Parameter);

    Model.IsCurrent           := True;
    Model.UseComponentLibrary := False;  //Refer to the WTH of the Location section of Sim Editor dialog.

End;
